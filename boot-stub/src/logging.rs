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
        gop::{BltOp, BltPixel, BltRegion, GraphicsOutput, PixelFormat},
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
        mode.info().stride() * mode.info().resolution().1 * mem::size_of::<BltPixel>();
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

    let (width, height) = graphics.current_mode_info().resolution();
    let stride = graphics.current_mode_info().stride();

    unsafe {
        GRAPHICS = Graphics::BootServices(BootServicesGraphics {
            proto: graphics,
            framebuffer: Framebuffer {
                buffer,
                width,
                height,
                stride,
                x: 0,
                y: 0,
                background: BltPixel::new(0x00, 0x00, 0x00),
                foreground: BltPixel::new(0xD0, 0xD0, 0xD0),
            },
        })
    }

    LOCK.store(false, Ordering::Release);
}

/// Sets up logging mechansims for after exiting boot services.
pub fn setup_post_exit_logging() {
    while LOCK.swap(true, Ordering::Acquire) {}

    unsafe { SERIAL = None }
    unsafe { GRAPHICS = setup_post_exit_graphical_logger() }

    LOCK.store(false, Ordering::Release);
}

fn setup_post_exit_graphical_logger() -> Graphics {
    let graphics = core::ptr::addr_of_mut!(GRAPHICS);
    let graphics = unsafe { &mut *graphics };
    let Graphics::BootServices(graphics) = graphics else {
        return Graphics::Uninitialized;
    };

    let (background, foreground) = match graphics.proto.current_mode_info().pixel_format() {
        PixelFormat::Rgb => (0x0000_0000, 0x00D0D0D0),
        PixelFormat::Bgr => (0x0000_0000, 0x00D0D0D0),
        PixelFormat::Bitmask | PixelFormat::BltOnly => return Graphics::Uninitialized,
    };

    let mut framebuffer = graphics.proto.frame_buffer();
    let write_surface = unsafe {
        core::slice::from_raw_parts_mut(
            framebuffer.as_mut_ptr().cast::<u32>(),
            framebuffer.size() / mem::size_of::<u32>(),
        )
    };

    let buffer = &graphics.framebuffer.buffer;
    let buffer = unsafe {
        core::slice::from_raw_parts_mut(buffer.as_ptr().cast_mut().cast::<u32>(), buffer.len())
    };
    let framebuffer = Framebuffer {
        buffer,
        width: graphics.framebuffer.width,
        height: graphics.framebuffer.height,
        stride: graphics.framebuffer.stride,
        x: graphics.framebuffer.x,
        y: graphics.framebuffer.y,
        background,
        foreground,
    };

    Graphics::Exited(ExitedGraphics {
        write_surface,
        framebuffer,
    })
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

        if uefi::table::system_table_boot().is_some() {
            let _ =
                with_stdout(|stdout| writeln!(stdout, "[{:?}] {}", record.level(), record.args()));
        }

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
    match graphics {
        Graphics::Uninitialized => return,
        Graphics::BootServices(graphics) => {
            let _ = writeln!(
                graphics.framebuffer,
                "[{:?}] {}",
                record.level(),
                record.args()
            );
            graphics.flush();
        }
        Graphics::Exited(graphics) => {
            let _ = writeln!(
                graphics.framebuffer,
                "[{:?}] {}",
                record.level(),
                record.args()
            );
            graphics.flush();
        }
    }
}

enum Graphics {
    Uninitialized,
    BootServices(BootServicesGraphics),
    Exited(ExitedGraphics),
}

const FONT: simple_psf::Psf =
    match simple_psf::Psf::parse(include_bytes!("../assets/Tamzen7x13.psf")) {
        Ok(font) => font,
        Err(_) => panic!(),
    };

struct BootServicesGraphics {
    proto: ScopedProtocol<GraphicsOutput>,
    framebuffer: Framebuffer<BltPixel>,
}

impl BootServicesGraphics {
    fn flush(&mut self) {
        let _ = self.proto.blt(BltOp::BufferToVideo {
            buffer: self.framebuffer.buffer,
            src: BltRegion::SubRectangle {
                coords: (0, 0),
                px_stride: self.framebuffer.stride,
            },
            dest: (0, 0),
            dims: (self.framebuffer.width, self.framebuffer.height),
        });
    }
}

struct ExitedGraphics {
    write_surface: &'static mut [u32],
    framebuffer: Framebuffer<u32>,
}

impl ExitedGraphics {
    fn flush(&mut self) {
        self.write_surface.copy_from_slice(self.framebuffer.buffer);
    }
}

struct Framebuffer<T: 'static + Copy> {
    buffer: &'static mut [T],
    width: usize,
    height: usize,
    stride: usize,
    x: usize,
    y: usize,
    background: T,
    foreground: T,
}

impl<T: 'static + Copy> Framebuffer<T> {
    fn write_char(&mut self, c: char) {
        match c {
            '\n' => self.newline(),
            '\r' => self.carriage_return(),
            c => {
                if !(self.x + FONT.glyph_width <= self.width) {
                    self.newline();
                }

                let index = if c.is_ascii() { c as usize } else { todo!() };
                let Some(pixels) = FONT.get_glyph_pixels(index) else {
                    self.x += FONT.glyph_width;
                    return;
                };

                let mut x_offset = 0;
                let mut y_offset = 0;
                let mut first_row = true;
                for (index, pixel_on) in pixels.enumerate() {
                    if index % FONT.glyph_width == 0 && !first_row {
                        x_offset = 0;
                        y_offset += 1;
                    }

                    let mut color = self.background;
                    if pixel_on {
                        color = self.foreground;
                    }

                    self.buffer[(self.x + x_offset) + ((self.y + y_offset) * self.stride)] = color;
                    x_offset += 1;
                    first_row = false;
                }
                self.x += FONT.glyph_width;
            }
        }
    }

    fn scroll(&mut self) {
        let bytes_uncopied = self.stride * FONT.glyph_height;

        self.buffer.copy_within(bytes_uncopied.., 0);

        let len = self.buffer.len();
        self.buffer[len - bytes_uncopied..].fill(self.background);
    }

    fn newline(&mut self) {
        self.y += FONT.glyph_height;
        self.carriage_return();

        if !(self.y + FONT.glyph_height <= self.height) {
            self.scroll();
            self.y -= FONT.glyph_height;
        }
    }

    fn carriage_return(&mut self) {
        self.x = 0;
    }
}

impl<T: 'static + Copy> Write for Framebuffer<T> {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for c in s.chars() {
            self.write_char(c);
        }

        Ok(())
    }
}
