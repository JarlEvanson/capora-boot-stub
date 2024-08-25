//! UEFI stub bootloader that boots applications using the capora boot protocol.

#![no_std]
#![no_main]

#[export_name = "efi_main"]
pub extern "efiapi" fn main() {
    loop {}
}

#[panic_handler]
fn panic_handler(_: &core::panic::PanicInfo) -> ! {
    loop {
        core::hint::spin_loop()
    }
}
