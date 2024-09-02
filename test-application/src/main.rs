//! Application for testing `capora-boot-stub`.

#![no_std]
#![no_main]

#[export_name = "SYMBOL"]
static mut K: u8 = 0;

/// The entry point of the test application.
#[export_name = "_start"]
pub fn entry() -> ! {
    unsafe { core::ptr::write_volatile(core::ptr::addr_of_mut!(K), 2) }

    loop {}
}

/// Handles all panics.
#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}
