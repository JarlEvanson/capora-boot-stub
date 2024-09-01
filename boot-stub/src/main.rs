//! UEFI stub bootloader that boots applications using the capora boot protocol.

#![no_std]
#![no_main]

use core::fmt::Write;

use configuration::parse_and_interprete_configuration;
use uefi::{
    boot,
    proto::console::text,
    system::{with_stderr, with_stdout},
    Status,
};

pub mod configuration;

/// The name of this bootloader.
const BOOTLOADER_NAME: &str = "capora-boot-stub";
/// The version of capora-boot-stub.
const BOOTLOADER_VERSION: &str = core::env!("CARGO_PKG_VERSION");

/// The number of microseconds to stall before returning when an error occurs while UEFI boot
/// services is still active.
const STALL_ON_ERROR_TIME: usize = 10_000_000;

#[uefi::entry]
fn main() -> Status {
    with_stdout(setup_output);
    with_stderr(setup_output);

    let _ =
        with_stdout(|stdout| writeln!(stdout, "Booting {BOOTLOADER_NAME} {BOOTLOADER_VERSION}"));

    match parse_and_interprete_configuration() {
        Ok(result) => result,
        Err(error) => {
            let _ = with_stderr(|stderr| writeln!(stderr, "{error}"));
            boot::stall(STALL_ON_ERROR_TIME);
            return Status::LOAD_ERROR;
        }
    }

    loop {}
}

/// Checks if the provided [`Output`] is in mode -1, and if so, searches for the mode with the most
/// rows and attempts to set `output` to that mode.
///
/// Doesn't return errors because booting shouldn't break due to a logging error.
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
