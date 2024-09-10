//! Logging for `capora-boot-stub`.

use core::{
    fmt::Write,
    mem::{self, MaybeUninit},
    sync::atomic::{AtomicBool, Ordering},
};

use log::Log;
use uefi::{
    boot::{self, ScopedProtocol},
    proto::console::{
        gop::{BltOp, BltPixel, BltRegion, GraphicsOutput},
        serial::Serial,
        text,
    },
    system::{with_stderr, with_stdout},
};

static LOCK: AtomicBool = AtomicBool::new(false);

static mut SERIAL: Option<ScopedProtocol<Serial>> = None;
static mut GRAPHICS: Graphics = Graphics::Uninitialized;

/// Sets up logging mechanisms and enables logging output.
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

    let buffer_size =
        mode.info().resolution().0 * mode.info().resolution().1 * mem::size_of::<BltPixel>();
    let buffer_pages = buffer_size.div_ceil(4096);
    let Ok(buffer) = boot::allocate_pages(
        boot::AllocateType::AnyPages,
        boot::MemoryType::LOADER_DATA,
        buffer_pages,
    ) else {
        return;
    };
    let buffer = unsafe {
        core::slice::from_raw_parts_mut(
            buffer.as_ptr().cast::<MaybeUninit<BltPixel>>(),
            buffer_size / mem::size_of::<BltPixel>(),
        )
    };
    let buffer = MaybeUninit::fill(buffer, BltPixel::new(0x00, 0x00, 0x00));

    while LOCK.swap(true, Ordering::Acquire) {}

    unsafe {
        GRAPHICS = Graphics::BootServices(BootServicesGraphics {
            proto: graphics,
            buffer,
            x: 0,
            y: 0,
        })
    }

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
    let serial = core::ptr::addr_of_mut!(SERIAL);
    let serial = unsafe { &mut *serial };
    let Some(serial) = serial.as_mut() else {
        return;
    };

    let _ = writeln!(serial, "[{:?}] {}", record.level(), record.args());
}

fn log_framebuffer(record: &log::Record) {
    let graphics = core::ptr::addr_of_mut!(GRAPHICS);
    let graphics = unsafe { &mut *graphics };
    let Graphics::BootServices(graphics) = graphics else {
        return;
    };

    let _ = writeln!(graphics, "[{:?}] {}", record.level(), record.args());
    graphics.flush();
}

enum Graphics {
    Uninitialized,
    BootServices(BootServicesGraphics),
}

const FONT: simple_psf::Psf =
    match simple_psf::Psf::parse(include_bytes!("../assets/Tamzen7x13.psf")) {
        Ok(font) => font,
        Err(_) => panic!(),
    };

struct BootServicesGraphics {
    proto: ScopedProtocol<GraphicsOutput>,
    buffer: &'static mut [BltPixel],
    x: usize,
    y: usize,
}

impl Write for BootServicesGraphics {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for c in s.chars() {
            match c {
                '\n' => self.newline(),
                '\r' => self.carriage_return(),
                c => {
                    let new_x = self.x + FONT.glyph_width;
                    if new_x >= self.proto.current_mode_info().resolution().0 {
                        self.newline();
                    }
                    let new_y = self.y + FONT.glyph_height;
                    if new_y >= self.proto.current_mode_info().resolution().1 {
                        self.scroll();
                        self.x = 0;
                        self.y -= FONT.glyph_height;
                    }

                    let index = if c.is_ascii() { c as usize } else { todo!() };

                    let Some(pixels) = FONT.get_glyph_pixels(index) else {
                        self.x = new_x;
                        continue;
                    };

                    let mut x_offset = 0;
                    let mut y_offset = 0;
                    let mut first_row = true;
                    for (index, pixel_on) in pixels.enumerate() {
                        if index % FONT.glyph_width == 0 && !first_row {
                            x_offset = 0;
                            y_offset += 1;
                        }

                        let mut color = BltPixel::new(0x00, 0x00, 0x00);
                        if pixel_on {
                            color = BltPixel::new(0xD0, 0xD0, 0xD0);
                        }

                        self.buffer[(self.x + x_offset)
                            + (self.y + y_offset)
                                * self.proto.current_mode_info().resolution().0] = color;
                        x_offset += 1;
                        first_row = false;
                    }
                    self.x += FONT.glyph_width;
                }
            }
        }

        Ok(())
    }
}

impl BootServicesGraphics {
    fn scroll(&mut self) {
        let bytes_uncopied = self.proto.current_mode_info().stride() * FONT.glyph_height;

        self.buffer.copy_within(bytes_uncopied.., 0);

        let len = self.buffer.len();
        self.buffer[len - bytes_uncopied..].fill(BltPixel::new(0x00, 0x00, 0x00));
    }

    fn newline(&mut self) {
        self.y += FONT.glyph_height;
        self.carriage_return();
    }

    fn carriage_return(&mut self) {
        self.x = 0;
    }

    fn flush(&mut self) {
        let dims = self.proto.current_mode_info().resolution();
        let _ = self.proto.blt(BltOp::BufferToVideo {
            buffer: self.buffer,
            src: BltRegion::Full,
            dest: (0, 0),
            dims,
        });
    }
}
