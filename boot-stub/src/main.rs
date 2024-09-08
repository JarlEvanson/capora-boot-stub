//! UEFI stub bootloader that boots applications using the capora boot protocol.

#![no_std]
#![no_main]
#![feature(maybe_uninit_fill)]
#![feature(array_chunks)]
#![feature(maybe_uninit_write_slice)]
#![feature(maybe_uninit_slice)]
#![feature(const_slice_flatten)]

use core::{
    arch::x86_64,
    error,
    fmt::{self, Write},
    mem::{self, MaybeUninit},
    ptr,
};

use boot_api::BootloaderResponse;
use configuration::parse_and_interprete_configuration;
use load_application::load_application;
use mapper::{ApplicationMemoryMap, FrameRange, PageRange, Protection, Usage};
use uefi::{
    boot,
    proto::console::text,
    system::{with_stderr, with_stdout},
    Status,
};

pub mod configuration;
pub mod load_application;
pub mod mapper;
pub mod paging;

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
const LOADED_STACK_SIZE: u64 = 64 * 1024;

#[uefi::entry]
fn main() -> Status {
    with_stdout(setup_output);
    with_stderr(setup_output);

    let _ =
        with_stdout(|stdout| writeln!(stdout, "Booting {BOOTLOADER_NAME} {BOOTLOADER_VERSION}"));

    let mut application_map = ApplicationMemoryMap::new();
    let (application_name, application_bytes, modules_virt_address, module_count) =
        match parse_and_interprete_configuration(&mut application_map) {
            Ok(result) => result,
            Err(error) => {
                let _ = with_stderr(|stderr| writeln!(stderr, "{error}"));
                let _ = with_stdout(|stdout| writeln!(stdout, "{error}"));
                boot::stall(STALL_ON_ERROR_TIME);
                return Status::LOAD_ERROR;
            }
        };
    let _ = with_stdout(|stdout| writeln!(stdout, "Loaded {application_name}"));

    let (slide, entry_point) = match load_application(&mut application_map, application_bytes) {
        Ok(result) => result,
        Err(error) => {
            let _ = with_stderr(|stderr| writeln!(stderr, "{error}"));
            let _ = with_stdout(|stdout| writeln!(stdout, "{error}"));
            boot::stall(STALL_ON_ERROR_TIME);
            return Status::LOAD_ERROR;
        }
    };

    if let Err(error) = test_required_bit_support() {
        let _ = with_stderr(|stderr| writeln!(stderr, "{error}"));
        let _ = with_stdout(|stdout| writeln!(stdout, "{error}"));
        boot::stall(STALL_ON_ERROR_TIME);
        return Status::LOAD_ERROR;
    }

    let (
        response,
        response_virtual_address,
        stack,
        gdt,
        context_switch,
        bootloader_name,
        bootloader_name_length,
        bootloader_version,
        bootloader_version_length,
    ) = match setup_general_mappings(&mut application_map) {
        Ok(result) => result,
        Err(error) => {
            let _ = with_stderr(|stderr| writeln!(stderr, "{error}"));
            let _ = with_stdout(|stdout| writeln!(stdout, "{error}"));
            boot::stall(STALL_ON_ERROR_TIME);
            return Status::LOAD_ERROR;
        }
    };

    let (top_level_page, application_memory_entries) = paging::map_app(application_map);
    let memory_map = unsafe { boot::exit_boot_services(boot::MemoryType::LOADER_DATA) };

    // Already checked that the required bits are supported.
    let _ = set_required_bits();
    load_gdt(gdt);

    unsafe {
        core::arch::asm!(
            "cli",
            "jmp {context_switch}",
            context_switch = in(reg) context_switch,
            in("rax") top_level_page,
            in("rcx") stack.wrapping_add(LOADED_STACK_SIZE),
            in("rdx") entry_point,
            in("rdi") response_virtual_address,
            options(noreturn)
        )
    }
}

/// Checks if the provided [`Output`][out] is in mode -1, and if so, searches for the mode with the
/// most rows and attempts to set `output` to that mode.
///
/// Doesn't return errors because booting shouldn't break due to a logging error.
///
/// [out]: text::Output
pub fn setup_output(output: &mut text::Output) {
    let current_mode = output.current_mode();
    match current_mode {
        Ok(None) => {
            let mode = output
                .modes()
                .max_by(|mode_0, mode_1| mode_0.rows().cmp(&mode_1.rows()))
                .expect("according to the UEFI specification, at least one mode must be supported");

            // Just ignore any errors, we can't report them and we shouldn't not continue due to
            // missing output.
            let _ = output.set_mode(mode);
        }
        Ok(Some(_)) | Err(_) => {}
    }
}

/// Checks if the required feature bits are supported.
pub fn test_required_bit_support() -> Result<(), UnsupportedFeaturesError> {
    let nxe_supported_bit = unsafe { x86_64::__cpuid(0x80000001).edx };
    if !((nxe_supported_bit & (1 << 20)) == (1 << 20)) {
        return Err(UnsupportedFeaturesError::NoExecuteEnable);
    }

    Ok(())
}

/// Sets bits required by the boot specification.
pub fn set_required_bits() -> Result<(), UnsupportedFeaturesError> {
    test_required_bit_support()?;

    unsafe {
        core::arch::asm!(
            "mov ecx, 0xC0000080",
            "rdmsr",
            "or eax, 0x400",
            "wrmsr",

            // Enable write protection
            "mov rax, cr0",
            "or rax, 0x10000",
            "mov cr0, rax",
            out("eax") _,
        )
    }

    Ok(())
}

/// Various unsupported features.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UnsupportedFeaturesError {
    /// The no-execute enable feature is not supported by this processor.
    NoExecuteEnable,
}

impl fmt::Display for UnsupportedFeaturesError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NoExecuteEnable => f.write_str("no execute enable bit is not supported"),
        }
    }
}

/// Loads the GDT mapped at `gdt`.
///
/// This GDT should have a kernel code segment mapped at offset 0x8, and a kernel data segment
/// mapped at 0x10.
#[inline(always)]
pub fn load_gdt(gdt: u64) {
    #[allow(dead_code)]
    #[repr(C)]
    struct Gdtr {
        other: [MaybeUninit<u8>; 6],
        size: u16,
        offset: u64,
    }

    let gdtr = Gdtr {
        other: [MaybeUninit::uninit(); 6],
        size: (GDT.len() - 1) as u16,
        offset: gdt,
    };

    unsafe {
        core::arch::asm!(
            "lgdt [rax]",
            "push 0x08",
            "lea rax, [rip + 5f]",
            "push rax",
            "retfq",
            "5:",
            "mov ax, 0x10",
            "mov ds, ax",
            "mov es, ax",
            "mov es, ax",
            "mov fs, ax",
            "mov gs, ax",
            "mov ss, ax",
            inout("rax") &gdtr.size => _,
        )
    }
}

impl error::Error for UnsupportedFeaturesError {}

const CONTEXT_STUB_BYTES: [u8; 13] = [
    0x48, 0x31, 0xed, // xor rbp, rbp
    0x0f, 0x22, 0xd8, // mov cr3, rax
    0x48, 0x89, 0xcc, // mov rsp, rcx
    0x6a, 0x00, // push 0x0
    0xff, 0xe2, // jmp rdx
];

const GDT: &[u8] = [
    // Null GDT entry.
    0x00_0_0_00_000000_0000u64.to_ne_bytes(),
    // Kernel code entry.
    0x00_A_F_9B_000000_FFFFu64.to_ne_bytes(),
    // Kernel data entry.
    0x00_C_F_93_000000_FFFFu64.to_ne_bytes(),
]
.as_flattened();

/// Allocates and configures various mappings necessary to successfully boot.
pub fn setup_general_mappings(
    application_map: &mut ApplicationMemoryMap,
) -> Result<
    (
        &mut BootloaderResponse,
        u64,
        u64,
        u64,
        u64,
        u64,
        usize,
        u64,
        usize,
    ),
    SetupMappingsError,
> {
    let stack_frame_count = LOADED_STACK_SIZE.div_ceil(4096);
    let stack = application_map.allocate(stack_frame_count, Protection::Writable, Usage::General);
    let stack_virtual_address = stack.page_range().virtual_address();

    let miscellaneous_size = mem::size_of::<BootloaderResponse>()
        + GDT.len()
        + CONTEXT_STUB_BYTES.len()
        + BOOTLOADER_NAME.len()
        + BOOTLOADER_VERSION.len();
    let miscellaneous_page_count = miscellaneous_size.div_ceil(4096);

    let miscellaneous_frames = boot::allocate_pages(
        boot::AllocateType::AnyPages,
        boot::MemoryType::LOADER_CODE,
        miscellaneous_page_count,
    )
    .expect("failed to allocate miscellaneous frames");
    let miscellaneous_pages = PageRange::new(
        (miscellaneous_frames.as_ptr() as u64) >> 12,
        miscellaneous_page_count as u64,
    )
    .unwrap();
    let miscellaneous_frames = FrameRange::new(
        (miscellaneous_frames.as_ptr() as u64) >> 12,
        miscellaneous_page_count as u64,
    )
    .unwrap();

    let miscellaneous_mapping = unsafe {
        application_map
            .add_region(
                miscellaneous_pages,
                miscellaneous_frames,
                Protection::Executable,
                Usage::General,
            )
            .unwrap()
    };
    let miscellaneous_virtual_address = miscellaneous_mapping.page_range().virtual_address();
    MaybeUninit::copy_from_slice(
        &mut miscellaneous_mapping.as_bytes_mut()[mem::size_of::<BootloaderResponse>()..]
            [..GDT.len()],
        GDT,
    );
    MaybeUninit::copy_from_slice(
        &mut miscellaneous_mapping.as_bytes_mut()
            [mem::size_of::<BootloaderResponse>() + GDT.len()..]
            [..mem::size_of_val(&CONTEXT_STUB_BYTES)],
        &CONTEXT_STUB_BYTES,
    );
    MaybeUninit::copy_from_slice(
        &mut miscellaneous_mapping.as_bytes_mut()
            [mem::size_of::<BootloaderResponse>() + GDT.len() + CONTEXT_STUB_BYTES.len()..]
            [..BOOTLOADER_NAME.len()],
        BOOTLOADER_NAME.as_bytes(),
    );
    MaybeUninit::copy_from_slice(
        &mut miscellaneous_mapping.as_bytes_mut()[mem::size_of::<BootloaderResponse>()
            + GDT.len()
            + CONTEXT_STUB_BYTES.len()
            + BOOTLOADER_NAME.len()..][..BOOTLOADER_VERSION.len()],
        BOOTLOADER_VERSION.as_bytes(),
    );

    let bootloader_response = unsafe {
        &mut *miscellaneous_mapping
            .as_bytes_mut()
            .as_mut_ptr()
            .cast::<MaybeUninit<BootloaderResponse>>()
    };
    let bootloader_response = bootloader_response.write(BootloaderResponse {
        bootloader_name: ptr::null(),
        bootloader_name_length: 0,
        bootloader_version: ptr::null(),
        bootloader_version_length: 0,
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

    Ok((
        bootloader_response,
        miscellaneous_virtual_address,
        stack_virtual_address,
        miscellaneous_virtual_address + mem::size_of::<BootloaderResponse>() as u64,
        miscellaneous_virtual_address + (mem::size_of::<BootloaderResponse>() + GDT.len()) as u64,
        miscellaneous_virtual_address
            + (mem::size_of::<BootloaderResponse>() + GDT.len() + CONTEXT_STUB_BYTES.len()) as u64,
        BOOTLOADER_NAME.len(),
        miscellaneous_virtual_address
            + (mem::size_of::<BootloaderResponse>()
                + GDT.len()
                + CONTEXT_STUB_BYTES.len()
                + BOOTLOADER_NAME.len()) as u64,
        BOOTLOADER_VERSION.len(),
    ))
}

/// Various errors that can occur while allocating general page mappings.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SetupMappingsError {
    /// The allocation of stack frames failed.
    StackAllocationFailed {
        /// The number of frames that the allocation requested.
        frame_count: u64,
        /// The status code returned from the failed allocation.
        status: Status,
    },
    /// The allocation of the gdt and context frames failed.
    GdtContextSwitchAllocationFailed {
        /// The status code returned from the failed allocation.
        status: Status,
    },
}

impl fmt::Display for SetupMappingsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StackAllocationFailed {
                frame_count,
                status,
            } => write!(
                f,
                "allocation of {frame_count} pages for the stack failed with code {status}"
            ),
            Self::GdtContextSwitchAllocationFailed { status } => write!(
                f,
                "allocation of 1 page for GDT and context switch failed with code {status}"
            ),
        }
    }
}

#[cfg_attr(not(test), panic_handler)]
#[cfg_attr(test, expect(dead_code))]
fn panic_handler(info: &core::panic::PanicInfo) -> ! {
    if uefi::table::system_table_boot().is_some() {
        let _ = with_stderr(|stderr| writeln!(stderr, "{info}"));
        let _ = with_stdout(|stdout| writeln!(stdout, "{info}"));
    }

    loop {
        core::hint::spin_loop()
    }
}
