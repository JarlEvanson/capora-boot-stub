//! Application for testing `capora-boot-stub`.

#![no_std]
#![no_main]

use boot_api::{BootloaderRequest, BootloaderResponse};

/// The version of `capora-boot-api` this application expects.
#[used]
#[link_section = ".bootloader_request"]
pub static BOOTLOADER_REQUEST: BootloaderRequest = BootloaderRequest {
    signature: boot_api::SIGNATURE,
    api_version: boot_api::API_VERSION,
};

#[export_name = "RESPONSE"]
static mut BOOTLOADER_RESPONSE: *const BootloaderResponse = core::ptr::null();

/// The entry point of the test application.
#[export_name = "_start"]
pub fn entry(bootloader_response: *const BootloaderResponse) -> ! {
    unsafe { core::arch::asm!("mov rax, [0x0]") }

    unsafe {
        core::ptr::write_volatile(
            core::ptr::addr_of_mut!(BOOTLOADER_RESPONSE),
            bootloader_response,
        )
    }

    unsafe {
        core::arch::asm!("mov qword ptr [0x0123456789ABCDEF], 0");
    }

    loop {}
}

/// Handles all panics.
#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {}
}
