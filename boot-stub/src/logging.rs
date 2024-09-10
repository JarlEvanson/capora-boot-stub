//! Logging for `capora-boot-stub`.

use core::{
    fmt::Write,
    sync::atomic::{AtomicBool, Ordering},
};

use log::Log;
use uefi::{
    boot::{self, ScopedProtocol},
    proto::console::{gop::GraphicsOutput, serial::Serial, text},
    system::{with_stderr, with_stdout},
};

static LOCK: AtomicBool = AtomicBool::new(false);

static mut SERIAL: Option<ScopedProtocol<Serial>> = None;
static mut GRAPHICS: Graphics = Graphics::Uninitialized;

pub fn init_logging() {
    init_serial_logger();
    init_graphical_logger();
    init_console_logger();

    log::set_logger(&Logger).unwrap();
    log::set_max_level(log::LevelFilter::Trace);
}

fn init_console_logger() {
    with_stdout(setup_output);
    with_stderr(setup_output);

    fn setup_output(output: &mut text::Output) {
        match output.current_mode() {
            Ok(None) => {
                let mode = output.modes().max_by_key(|mode| mode.rows()).expect(
                    "according to the UEFI specification, at least one mode must be supported",
                );

                // Just ignore any errors, we can't report them and a failure to log shouldn't
                // bring down the bootloader.
                let _ = output.set_mode(mode);
            }
            _ => {}
        }
    }
}

fn init_serial_logger() {
    let Ok(serial_handle) = boot::get_handle_for_protocol::<Serial>() else {
        return;
    };

    let Ok(serial) = boot::open_protocol_exclusive::<Serial>(serial_handle) else {
        return;
    };

    while LOCK.swap(true, Ordering::Acquire) {}

    unsafe { SERIAL = Some(serial) }

    LOCK.store(false, Ordering::Release);
}

fn init_graphical_logger() {
    let Ok(graphics_handle) = boot::get_handle_for_protocol::<GraphicsOutput>() else {
        return;
    };

    let Ok(mut graphics) = boot::open_protocol_exclusive::<GraphicsOutput>(graphics_handle) else {
        return;
    };

    let Some(mode) = graphics
        .modes()
        .max_by_key(|mode| mode.info().resolution().0)
    else {
        return;
    };

    let Ok(()) = graphics.set_mode(&mode) else {
        return;
    };

    while LOCK.swap(true, Ordering::Acquire) {}

    unsafe { GRAPHICS = Graphics::BootServices { proto: graphics } }

    LOCK.store(false, Ordering::Release);
}

struct Logger;

impl Log for Logger {
    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        while LOCK.swap(true, Ordering::Acquire) {}

        log_serial(record);
        log_framebuffer(record);

        let _ = with_stdout(|stdout| writeln!(stdout, "[{:?}] {}", record.level(), record.args()));

        LOCK.store(false, Ordering::Release);
    }

    fn flush(&self) {}
}

fn log_serial(record: &log::Record) {
    let serial = unsafe { core::ptr::addr_of_mut!(SERIAL) };
    let serial = unsafe { &mut *serial };
    let Some(serial) = serial.as_mut() else {
        return;
    };

    let _ = writeln!(serial, "[{:?}] {}", record.level(), record.args());
}

fn log_framebuffer(record: &log::Record) {
    let graphics = unsafe { core::ptr::addr_of_mut!(GRAPHICS) };
    let graphics = unsafe { &mut *graphics };
    let Graphics::BootServices { proto } = graphics else {
        return;
    };
}

enum Graphics {
    Uninitialized,
    BootServices {
        proto: ScopedProtocol<GraphicsOutput>,
    },
}
