//! Application for testing `capora-boot-stub`.

#![no_std]
#![no_main]

use boot_api::BootloaderRequest;

/// The version of `capora-boot-api` this application expects.
#[used]
#[link_section = ".bootloader_request"]
pub static BOOTLOADER_REQUEST: BootloaderRequest = BootloaderRequest {
    signature: boot_api::SIGNATURE,
    api_version: boot_api::API_VERSION,
};

#[export_name = "SYMBOL"]
static mut K: u8 = 0;

/// The entry point of the test application.
#[export_name = "_start"]
pub fn entry() -> ! {
    // Reference it to force the item to be kept.
    core::hint::black_box(core::ptr::addr_of!(BOOTLOADER_REQUEST));

    unsafe { core::ptr::write_volatile(core::ptr::addr_of_mut!(K), 2) }

    loop {}
}

/// Handles all panics.
#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}
