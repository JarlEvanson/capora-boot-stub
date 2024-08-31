//! Application for testing `capora-boot-stub`.

#![no_std]
#![no_main]

/// The entry point of the test application.
#[export_name = "_start"]
pub fn entry() -> ! {
    loop {}
}

/// Handles all panics.
#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}
