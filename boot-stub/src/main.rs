//! UEFI stub bootloader that boots applications using the capora boot protocol.

#![no_std]
#![no_main]
#![feature(maybe_uninit_fill)]
#![feature(array_chunks)]

use core::{
    arch::x86_64,
    error,
    fmt::{self, Write},
};

use configuration::parse_and_interprete_configuration;
use load_application::load_application;
use mapper::VirtualMemoryMap;
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

    let mmeory_map = unsafe { boot::exit_boot_services(boot::MemoryType::LOADER_DATA) };

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
