//! UEFI stub bootloader that boots applications using the capora boot protocol.

#![no_std]
#![no_main]
#![feature(maybe_uninit_fill)]
#![feature(array_chunks)]

use core::{
    arch::x86_64,
    error,
    fmt::{self, Write},
    mem,
};

use configuration::parse_and_interprete_configuration;
use load_application::load_application;
use mapper::{FrameRange, PageRange, VirtualMemoryMap, VirtualMemoryMapEntry};
use uefi::{
    boot::{self, AllocateType, MemoryType},
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

const LOADED_STACK_SIZE: u64 = 64 * 1024;

#[uefi::entry]
fn main() -> Status {
    with_stdout(setup_output);
    with_stderr(setup_output);

    let _ =
        with_stdout(|stdout| writeln!(stdout, "Booting {BOOTLOADER_NAME} {BOOTLOADER_VERSION}"));

    let (application, entries) = match parse_and_interprete_configuration() {
        Ok(result) => result,
        Err(error) => {
            let _ = with_stderr(|stderr| writeln!(stderr, "{error}"));
            let _ = with_stdout(|stdout| writeln!(stdout, "{error}"));
            boot::stall(STALL_ON_ERROR_TIME);
            return Status::LOAD_ERROR;
        }
    };

    let mut virtual_map = VirtualMemoryMap::new();
    let (slide, entry_point) = match load_application(&mut virtual_map, application.data()) {
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

    let (stack, gdt, context_switch) = match setup_general_mappings(&mut virtual_map) {
        Ok(result) => result,
        Err(error) => {
            let _ = with_stderr(|stderr| writeln!(stderr, "{error}"));
            let _ = with_stdout(|stdout| writeln!(stdout, "{error}"));
            boot::stall(STALL_ON_ERROR_TIME);
            return Status::LOAD_ERROR;
        }
    };
    let memery_map = unsafe { boot::exit_boot_services(boot::MemoryType::LOADER_DATA) };

    // Already checked that the required bits are supported.
    let _ = set_required_bits();

    loop {}
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
                .expect("according to UEFI specification, at least one mode must be supported");

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

impl error::Error for UnsupportedFeaturesError {}

const CONTEXT_STUB_BYTES: [u8; 13] = [
    0x48, 0x31, 0xed, // xor rbp, rbp
    0x0f, 0x22, 0xd8, // mov cr3, rax
    0x48, 0x89, 0xcc, // mov rsp, rcx
    0x6a, 0x00, // push 0x0
    0xff, 0xe2, // jmp rdx
];

const GDT: [u64; 3] = [
    // Null GDT entry.
    0x00_0_0_00_000000_0000,
    // Kernel code entry.
    0x00_A_F_9B_000000_FFFF,
    // Kernel data entry.
    0x00_C_F_93_000000_FFFF,
];

/// Allocates and configures various mappings necessary to successfully boot.
pub fn setup_general_mappings(
    map: &mut VirtualMemoryMap,
) -> Result<(u64, u64, u64), SetupMappingsError> {
    let stack_frame_count = LOADED_STACK_SIZE.div_ceil(4096);
    let stack = boot::allocate_pages(
        AllocateType::AnyPages,
        MemoryType::LOADER_DATA,
        stack_frame_count as usize,
    )
    .map_err(|error| SetupMappingsError::StackAllocationFailed {
        frame_count: stack_frame_count,
        status: error.status(),
    })?;
    let stack_frames = FrameRange::new((stack.as_ptr() as u64) >> 12, stack_frame_count)
        .expect("broken page allocator");
    let stack_pages = generate_random_supervisor_base(map, stack_frames, true, false);
    let _ = with_stdout(|stdout| writeln!(stdout, "Stack frames: {stack_frames:X?}"));

    let gdt_context_switch =
        boot::allocate_pages(AllocateType::AnyPages, MemoryType::LOADER_CODE, 1).map_err(
            |error| SetupMappingsError::GdtContextSwitchAllocationFailed {
                status: error.status(),
            },
        )?;
    let gdt_context_switch_frames = FrameRange::new((gdt_context_switch.as_ptr() as u64) >> 12, 1)
        .expect("broken page allocator");
    map.insert(
        VirtualMemoryMapEntry::new(
            PageRange::new((gdt_context_switch.as_ptr() as u64) >> 12, 1).unwrap(),
            gdt_context_switch_frames,
            false,
            true,
        )
        .expect("broken page allocator"),
    )
    .expect("GDT context switch mapping failed");

    unsafe {
        core::ptr::copy_nonoverlapping(
            GDT.as_ptr().cast::<u8>(),
            gdt_context_switch.as_ptr(),
            mem::size_of_val(&GDT),
        )
    }
    unsafe {
        core::ptr::copy_nonoverlapping(
            CONTEXT_STUB_BYTES.as_ptr().cast::<u8>(),
            gdt_context_switch.as_ptr().add(mem::size_of_val(&GDT)),
            mem::size_of_val(&CONTEXT_STUB_BYTES),
        )
    }

    let _ = with_stdout(|stdout| {
        writeln!(
            stdout,
            "GDT + context switch frames: {gdt_context_switch_frames:X?}"
        )
    });

    Ok((
        stack_pages.virtual_address() as u64,
        gdt_context_switch.as_ptr() as u64,
        gdt_context_switch.as_ptr() as u64 + mem::size_of_val(&GDT) as u64,
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

/// Generates a random address in the supervisor section of memory, marks it as to be mapped as
/// writable or executable according to the passed objects, and then puts in into the
/// [`VirtualMemoryMap`].
pub fn generate_random_supervisor_base(
    map: &mut VirtualMemoryMap,
    frames: FrameRange,
    writable: bool,
    executable: bool,
) -> PageRange {
    loop {
        let rng = load_application::rand_u64().expect("random number generator failed")
            & PageRange::PAGE_MASK;
        let Some(pages) = PageRange::new(rng, frames.size()) else {
            continue;
        };

        let Some(entry) = VirtualMemoryMapEntry::new(pages, frames, writable, executable) else {
            continue;
        };

        if map.insert(entry).is_some() {
            return pages;
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
