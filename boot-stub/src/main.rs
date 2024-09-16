//! UEFI stub bootloader that boots applications using the capora boot protocol.

#![no_std]
#![no_main]
#![feature(maybe_uninit_fill)]
#![feature(array_chunks)]
#![feature(maybe_uninit_write_slice)]
#![feature(maybe_uninit_slice)]
#![feature(const_slice_flatten)]
#![feature(unsafe_cell_from_mut)]
#![feature(iter_map_windows)]

use core::{
    fmt,
    mem::{self, MaybeUninit},
    ptr,
};

use arch::x86_64::{
    create_architectural_structures, enable_required_features, jump_to_context_switch,
    load_architecture_structures, setup_context_switch, test_required_feature_support,
    ArchitecturalStructures, CreateArchitecturalStructuresError, SetupContextSwitchError,
    UnsupportedFeaturesError,
};
use boot_api::{BootloaderResponse, MemoryMapEntry, MemoryMapEntryKind, ModuleEntry};
use configuration::{parse_and_interprete_configuration, ParseAndInterpretConfigurationError};
use load_application::{load_application, LoadApplicationError};
use logging::setup_post_exit_logging;
use memory_map::{AllocateEntryError, ApplicationMemoryMap, BackingMemory, Protection, Usage};
use memory_structs::{Frame, FrameRange, PhysicalAddress, VirtualAddress};
use uefi::{
    boot,
    mem::memory_map::{MemoryMap, MemoryMapMut},
    Status,
};

pub mod arch;
pub mod configuration;
pub mod load_application;
pub mod logging;
pub mod memory_map;
pub mod memory_structs;
pub mod paging;
pub mod spinlock;

/// The name of this bootloader.
const BOOTLOADER_NAME: &str = "capora-boot-stub";
/// The version of capora-boot-stub.
const BOOTLOADER_VERSION: &str = core::env!("CARGO_PKG_VERSION");

/// The number of microseconds to stall before returning when an error occurs while UEFI boot
/// services is still active.
const STALL_ON_ERROR_TIME: usize = 10_000_000;

/// The minimum address of the application base.
const MINIMUM_APPLICATION_BASE: u64 = 0xFFFFFFFF80000000;
/// The size of the region that the application can occupy.
const APPLICATION_REGION_SIZE: u64 = 0x0000000080000000;

/// The maximum number of retries for finding a elf base before giving up.
const BASE_RETRY_COUNT: u64 = 1000;
/// The size of the stack for the loaded application.
const LOADED_STACK_SIZE: usize = 64 * 1024;

#[uefi::entry]
fn entry() -> Status {
    let (
        architectural_structures,
        context_switch,
        top_level_page_table,
        stack_top,
        entry_point,
        response,
    ) = match main() {
        Ok(vars) => vars,
        Err(error) => {
            log::error!("{error}");
            boot::stall(STALL_ON_ERROR_TIME);
            return Status::LOAD_ERROR;
        }
    };

    unsafe {
        switch_to_application(
            architectural_structures,
            context_switch,
            top_level_page_table,
            stack_top,
            entry_point,
            response,
        )
    }
}

fn main() -> Result<
    (
        ArchitecturalStructures,
        VirtualAddress,
        PhysicalAddress,
        VirtualAddress,
        VirtualAddress,
        VirtualAddress,
    ),
    BootloaderError,
> {
    logging::init_logging();

    log::info!("Booting {BOOTLOADER_NAME} {BOOTLOADER_VERSION}");

    let mut application_map = ApplicationMemoryMap::new();
    let (application_name, application_bytes, modules_virt_address, module_count) =
        parse_and_interprete_configuration(&mut application_map)?;
    log::info!("Loaded {application_name}");

    let (slide, entry_point) = load_application(&mut application_map, application_bytes)?;
    log::debug!("Application loaded at {slide:#X}; Entry point at {entry_point:?}");

    test_required_feature_support()?;

    let (response_virtual_address, stack) = setup_general_mappings(&mut application_map)?;

    let (bootloader_memory_map_address, uefi_memory_map_address) =
        allocate_memory_map_regions(&mut application_map)?;
    log::debug!("Allocated bootloader memory map at {bootloader_memory_map_address:?}");
    log::debug!("Allocated uefi memory map at {uefi_memory_map_address:?}");

    let architectural_structures = create_architectural_structures(&mut application_map)?;
    let context_switch = setup_context_switch(&mut application_map)?;

    let (top_level_page, application_memory_entries) = paging::map_app(application_map);
    log::debug!("PML4E located at {top_level_page:?}");

    log::info!("Exiting boot services");
    setup_post_exit_logging();
    let mut memory_map = unsafe { boot::exit_boot_services(boot::MemoryType::LOADER_DATA) };
    log::info!("Exited boot services");

    memory_map.sort();

    for entry in application_memory_entries {
        log::trace!("{entry:X?}");
    }

    let bootloader_memory_map_entry = application_memory_entries
        .iter()
        .filter(|entry| {
            entry
                .page_range()
                .contains_address(bootloader_memory_map_address)
        })
        .next()
        .unwrap();
    let mut bootloader_memory_map = bootloader_memory_map_entry.as_slice().unwrap();
    let bootloader_memory_map = MaybeUninit::fill(
        &mut bootloader_memory_map,
        MemoryMapEntry {
            kind: MemoryMapEntryKind::RESERVED,
            base: 0,
            size: 0,
        },
    );

    let mut index = 0;
    let mut base = 0;
    let mut size = 0;
    let mut kind = MemoryMapEntryKind::RESERVED;
    for entry in memory_map.entries().copied() {
        let prev_end = base + size;

        let entry_kind = match entry.ty {
            boot::MemoryType::BOOT_SERVICES_CODE | boot::MemoryType::BOOT_SERVICES_DATA => {
                MemoryMapEntryKind::USABLE
            }
            boot::MemoryType::ACPI_RECLAIM => MemoryMapEntryKind::ACPI_RECLAIMABLE,
            boot::MemoryType::ACPI_NON_VOLATILE => MemoryMapEntryKind::ACPI_NONVOLATILE_STORAGE,
            boot::MemoryType::UNACCEPTED => MemoryMapEntryKind::UNACCEPTED,
            boot::MemoryType::CONVENTIONAL => MemoryMapEntryKind::USABLE,
            boot::MemoryType::LOADER_DATA | boot::MemoryType::LOADER_CODE => {
                MemoryMapEntryKind::BOOTLOADER
            }
            _ => MemoryMapEntryKind::RESERVED,
        };
        if entry.phys_start == prev_end && entry_kind == kind {
            size += entry.page_count * 4096;
            continue;
        }

        while size != 0 {
            let entry = 'bootloader_test: {
                if kind == MemoryMapEntryKind::BOOTLOADER {
                    let start_address = PhysicalAddress::new(base).unwrap();
                    let end_address = PhysicalAddress::new((size - 1) / 4096).unwrap();

                    let start_frame = Frame::containing_address(start_address);
                    let end_frame = Frame::containing_address(end_address);

                    let frames = FrameRange::inclusive_range(start_frame, end_frame);

                    let Some((frame_range, usage)) = application_memory_entries
                        .iter()
                        .filter_map(|entry| match entry.backing() {
                            BackingMemory::Allocated {
                                frame_range,
                                protection: _,
                                usage,
                            }
                            | BackingMemory::Unallocated {
                                frame_range,
                                protection: _,
                                usage,
                            } if frame_range.overlaps(&frames) && usage != Usage::General => {
                                Some((frame_range, usage))
                            }
                            _ => None,
                        })
                        .min_by_key(|(frame_range, _)| frame_range.start().number())
                    else {
                        break 'bootloader_test MemoryMapEntry { kind, base, size };
                    };

                    let start_overlap = core::cmp::max(frames.start(), frame_range.start());
                    let end_overlap = core::cmp::min(
                        frames.start().number() + frames.size_in_frames(),
                        frame_range.start().number() + frame_range.size_in_frames(),
                    );

                    if start_frame < start_overlap {
                        break 'bootloader_test MemoryMapEntry {
                            kind,
                            base,
                            size: frames.size_in_frames(),
                        };
                    }

                    let kind = match usage {
                        Usage::General => MemoryMapEntryKind::BOOTLOADER,
                        Usage::Application => MemoryMapEntryKind::KERNEL,
                        Usage::Module => MemoryMapEntryKind::MODULE,
                        Usage::Framebuffer => MemoryMapEntryKind::RESERVED,
                    };

                    break 'bootloader_test MemoryMapEntry {
                        kind,
                        base,
                        size: (end_overlap - start_overlap.number()) * 4096,
                    };
                }

                MemoryMapEntry { kind, base, size }
            };

            bootloader_memory_map[index] = entry;

            size -= entry.size;
            base += entry.size;

            index += 1;
        }

        base = entry.phys_start;
        size = entry.page_count * 4096;
        kind = entry_kind;
    }

    let filled_memory_map = &mut bootloader_memory_map[..index];

    for entry in filled_memory_map.iter() {
        log::trace!("{entry:X?}");
    }

    mem::forget(memory_map);

    let response = application_memory_entries
        .iter()
        .filter(|entry| {
            entry
                .page_range()
                .contains_address(response_virtual_address)
        })
        .next()
        .unwrap();
    let mut response = response.as_slice::<BootloaderResponse>().unwrap();
    let response = unsafe { response[0].assume_init_mut() };

    response.kernel_virtual_address = slide as *const core::ffi::c_void;

    response.memory_map_entries = bootloader_memory_map_address.value() as *mut MemoryMapEntry;
    response.memory_map_entry_count = filled_memory_map.len();

    response.uefi_system_table_ptr = uefi::table::system_table_raw()
        .map(|ptr| ptr.as_ptr())
        .unwrap_or(ptr::null_mut())
        .cast::<core::ffi::c_void>();

    response.module_entries = modules_virt_address.value() as *mut ModuleEntry;
    response.module_entry_count = module_count as usize;

    // TODO: Search for SM BIOS entries
    // TODO: Search for RSDP pointer
    // TODO: Allocate UEFI memory map

    Ok((
        architectural_structures,
        context_switch,
        top_level_page,
        stack,
        entry_point,
        response_virtual_address,
    ))
}

/// Various error that can occur during the execution of `capora-boot-stub`.
pub enum BootloaderError {
    /// The cpu does not supported some required features.
    CpuFeatureError(UnsupportedFeaturesError),
    /// An error ocurred while parsing and interpreting its configuration.
    ParseAndInterpreteError(ParseAndInterpretConfigurationError),
    /// An error ocurred while loading the application.
    LoadApplicationError(LoadApplicationError),
    /// An error occurred while setting up mappings.
    SetupMappingsError(SetupMappingsError),
    /// An error occurred when allocating virtual memory regions for the memory maps.
    AllocateMemoryMapRegionsError(AllocateMemoryMapRegionsError),
    /// An error ocurred when creating architectural structures.
    ArchitecturalStructuresError(CreateArchitecturalStructuresError),
    /// An error ocurred when setting up context switch routine.
    SetupContextSwitchError(SetupContextSwitchError),
}

impl From<UnsupportedFeaturesError> for BootloaderError {
    fn from(value: UnsupportedFeaturesError) -> Self {
        Self::CpuFeatureError(value)
    }
}

impl From<ParseAndInterpretConfigurationError> for BootloaderError {
    fn from(value: ParseAndInterpretConfigurationError) -> Self {
        Self::ParseAndInterpreteError(value)
    }
}

impl From<LoadApplicationError> for BootloaderError {
    fn from(value: LoadApplicationError) -> Self {
        Self::LoadApplicationError(value)
    }
}

impl From<SetupMappingsError> for BootloaderError {
    fn from(value: SetupMappingsError) -> Self {
        Self::SetupMappingsError(value)
    }
}

impl From<AllocateMemoryMapRegionsError> for BootloaderError {
    fn from(value: AllocateMemoryMapRegionsError) -> Self {
        Self::AllocateMemoryMapRegionsError(value)
    }
}

impl From<CreateArchitecturalStructuresError> for BootloaderError {
    fn from(value: CreateArchitecturalStructuresError) -> Self {
        Self::ArchitecturalStructuresError(value)
    }
}

impl From<SetupContextSwitchError> for BootloaderError {
    fn from(value: SetupContextSwitchError) -> Self {
        Self::SetupContextSwitchError(value)
    }
}

impl fmt::Display for BootloaderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CpuFeatureError(error) => {
                write!(f, "cpu does not supported required feature: {error}")
            }
            Self::SetupMappingsError(error) => {
                write!(f, "error allocating miscellaneous mappings: {error}")
            }
            Self::ParseAndInterpreteError(error) => write!(
                f,
                "error while parsing and interpreting configuration: {error}"
            ),
            Self::LoadApplicationError(error) => {
                write!(f, "error while loading application: {error}")
            }
            Self::AllocateMemoryMapRegionsError(error) => write!(
                f,
                "error occurred while allocating memory for memory maps: {error}",
            ),
            Self::ArchitecturalStructuresError(error) => {
                write!(f, "error while creating architectural structures: {error}")
            }
            Self::SetupContextSwitchError(error) => {
                write!(f, "error while setting up context switch routine: {error}")
            }
        }
    }
}

/// Perfoms a context switch to the application.
///
/// # Safety
/// `top_level_page_table` must point to the physical memory address of page table layout
/// describing an address space that identity maps the context_switch, and `stack_top`,
/// `entry_point`, and `response` must be properly setup when in the application address space.
unsafe fn switch_to_application(
    architectural_structures: ArchitecturalStructures,
    context_switch: VirtualAddress,
    top_level_page_table: PhysicalAddress,
    stack_top: VirtualAddress,
    entry_point: VirtualAddress,
    response: VirtualAddress,
) -> ! {
    log::info!("Switching to application");

    // Already checked that required features are supported.
    let _ = enable_required_features();
    load_architecture_structures(architectural_structures);

    log::trace!("Context Switch: {context_switch:?}");
    log::trace!("Top Level Page Table: {top_level_page_table:?}");
    log::trace!("Stack Top: {stack_top:?}");
    log::trace!("Entry Point: {entry_point:?}");

    unsafe {
        jump_to_context_switch(
            context_switch,
            top_level_page_table,
            stack_top,
            entry_point,
            response,
        )
    }
}

/// Allocates virtual memory regions to store the bootloader memory map and the UEFI memory map
/// obtained when exiting boot services.
pub fn allocate_memory_map_regions(
    application_map: &mut ApplicationMemoryMap,
) -> Result<(VirtualAddress, VirtualAddress), AllocateMemoryMapRegionsError> {
    let memory_map = boot::memory_map(boot::MemoryType::LOADER_DATA).unwrap();

    let bootloader_memory_map_size = memory_map
        .len()
        .saturating_mul(2)
        .saturating_mul(mem::size_of::<MemoryMapEntry>());
    let bootloader_memory_map_page_count = bootloader_memory_map_size.div_ceil(4096);
    let bootloader_memory_map = application_map.allocate(
        bootloader_memory_map_page_count,
        Protection::Readable,
        Usage::General,
    )?;
    let bootloader_memory_map_address = bootloader_memory_map.page_range().start_address();

    let uefi_memory_map_size = memory_map
        .len()
        .saturating_mul(2)
        .saturating_mul(memory_map.meta().desc_size);
    let uefi_memory_map_page_count = uefi_memory_map_size.div_ceil(4096);
    let uefi_memory_map = application_map.allocate(
        uefi_memory_map_page_count,
        Protection::Readable,
        Usage::General,
    )?;
    let uefi_memory_map_address = uefi_memory_map.page_range().start_address();

    Ok((bootloader_memory_map_address, uefi_memory_map_address))
}

/// Various errors that can occur while allocating memory map regions.
pub struct AllocateMemoryMapRegionsError(AllocateEntryError);

impl From<AllocateEntryError> for AllocateMemoryMapRegionsError {
    fn from(value: AllocateEntryError) -> Self {
        Self(value)
    }
}

impl fmt::Display for AllocateMemoryMapRegionsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "error while allocating memory for memory maps: {}",
            self.0
        )
    }
}

/// Allocates and configures various mappings necessary to successfully boot.
pub fn setup_general_mappings(
    application_map: &mut ApplicationMemoryMap,
) -> Result<(VirtualAddress, VirtualAddress), SetupMappingsError> {
    let stack_frame_count = LOADED_STACK_SIZE.div_ceil(4096);
    let stack = application_map
        .allocate(stack_frame_count, Protection::Writable, Usage::General)
        .map_err(SetupMappingsError::StackAllocationError)?;
    let stack_virtual_address =
        VirtualAddress::new(stack.page_range().start_address().value() + LOADED_STACK_SIZE)
            .unwrap();
    log::debug!("Stack allocated at {stack_virtual_address:?}");

    let bootloader_response_size =
        mem::size_of::<BootloaderResponse>() + BOOTLOADER_NAME.len() + BOOTLOADER_VERSION.len();
    let bootloader_response_page_count = bootloader_response_size.div_ceil(4096);
    let bootloader_response_mapping = application_map
        .allocate(
            bootloader_response_page_count,
            Protection::Executable,
            Usage::General,
        )
        .map_err(SetupMappingsError::BootloaderResponseAllocationError)?;
    let bootloader_response_address = bootloader_response_mapping.page_range().start_address();
    log::debug!("Miscellaneous mapping allocated at {bootloader_response_address:?}");

    let mut bootloader_strings_slice = bootloader_response_mapping.as_slice().unwrap();
    MaybeUninit::copy_from_slice(
        &mut bootloader_strings_slice[mem::size_of::<BootloaderResponse>()..]
            [..BOOTLOADER_NAME.len()],
        BOOTLOADER_NAME.as_bytes(),
    );
    MaybeUninit::copy_from_slice(
        &mut bootloader_strings_slice
            [mem::size_of::<BootloaderResponse>() + BOOTLOADER_NAME.len()..]
            [..BOOTLOADER_VERSION.len()],
        BOOTLOADER_VERSION.as_bytes(),
    );
    drop(bootloader_strings_slice);

    let bootloader_response = &mut bootloader_response_mapping.as_slice().unwrap()[0];
    bootloader_response.write(BootloaderResponse {
        bootloader_name: (bootloader_response_address.value()
            + mem::size_of::<BootloaderResponse>()) as *const u8,
        bootloader_name_length: BOOTLOADER_NAME.len(),
        bootloader_version: (bootloader_response_address.value()
            + mem::size_of::<BootloaderResponse>()
            + BOOTLOADER_NAME.len()) as *const u8,
        bootloader_version_length: BOOTLOADER_VERSION.len(),
        kernel_virtual_address: ptr::null_mut(),
        memory_map_entries: ptr::null_mut(),
        memory_map_entry_count: 0,
        sm_bios_entry_32: ptr::null(),
        sm_bios_entry_64: ptr::null(),
        rsdp_table_ptr: ptr::null(),
        uefi_system_table_ptr: ptr::null(),
        uefi_memory_map: ptr::null(),
        uefi_memory_map_size: 0,
        uefi_memory_map_descriptor_size: 0,
        uefi_memory_map_descriptor_version: 0,
        module_entries: ptr::null_mut(),
        module_entry_count: 0,
    });

    Ok((bootloader_response_address, stack_virtual_address))
}

/// Various errors that can occur while allocating general page mappings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SetupMappingsError {
    /// An error occurred while allocating memory for the application stack.
    StackAllocationError(AllocateEntryError),
    /// An error occurred while allocating memory for the bootloader response.
    BootloaderResponseAllocationError(AllocateEntryError),
}

impl fmt::Display for SetupMappingsError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

#[cfg_attr(not(test), panic_handler)]
#[cfg_attr(test, expect(dead_code))]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    log::error!("PANIC OCCURRED: {info}");

    loop {
        core::hint::spin_loop()
    }
}
