//! Helper crate for building and testing capora boot stub.

use core::fmt;
use std::{ffi::OsString, path::PathBuf};

use cli::{parse_arguments, Action, Arch};

pub mod cli;

fn main() {
    match parse_arguments() {
        Action::Build { arch, release } => match build(arch, release) {
            Ok(path) => println!("kernel located at \"{}\"", path.display()),
            Err(error) => eprintln!("error while building kernel: {error}"),
        },
        Action::Run {
            arch,
            release,
            ovmf_code,
            ovmf_vars,
        } => match run(arch, release, ovmf_code, ovmf_vars) {
            Ok(()) => {}
            Err(error) => eprintln!("error while running kernel: {error}"),
        },
    };
}

/// Builds capora-boot-stub.
pub fn build(arch: Arch, release: bool) -> Result<PathBuf, BuildError> {
    let mut cmd = std::process::Command::new("cargo");
    cmd.arg("build");
    cmd.args(["--package", "boot-stub"]);

    cmd.args(["--target", arch.as_target_triple()]);
    if release {
        cmd.arg("--release");
    }

    let mut binary_location = PathBuf::with_capacity(50);
    binary_location.push("target");
    binary_location.push(arch.as_target_triple());
    if release {
        binary_location.push("release");
    } else {
        binary_location.push("debug");
    }
    binary_location.push("boot-stub.efi");

    let status = cmd.status()?;
    if !status.success() {
        return Err(BuildError::UnsuccessfulBuild {
            code: status.code(),
        });
    }

    Ok(binary_location)
}

/// Various errors that can occur while building capora-boot-stub.
#[derive(Debug)]
pub enum BuildError {
    /// An error occurred while launching the process.
    ProcessError(std::io::Error),
    /// The build was unsuccessful.
    UnsuccessfulBuild {
        /// The exit code of the child process that was launched.
        code: Option<i32>,
    },
}

impl From<std::io::Error> for BuildError {
    fn from(value: std::io::Error) -> Self {
        Self::ProcessError(value)
    }
}

impl fmt::Display for BuildError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ProcessError(error) => writeln!(f, "error while launching cargo: {error}"),
            Self::UnsuccessfulBuild { code: Some(code) } => {
                writeln!(f, "cargo failed with exit status {code}")
            }
            Self::UnsuccessfulBuild { code: None } => f.write_str("cargo terminated by signal"),
        }
    }
}

/// Builds and runs capora-boot-stub.
pub fn run(
    arch: Arch,
    release: bool,
    ovmf_code: PathBuf,
    ovmf_vars: PathBuf,
) -> Result<(), RunError> {
    let stub_path = build(arch, release)?;
    let fat_directory =
        build_fat_directory(arch, stub_path).map_err(RunError::BuildFatDirectoryError)?;

    let qemu_name = match arch {
        Arch::X86_64 => "qemu-system-x86_64",
    };

    let mut cmd = std::process::Command::new(qemu_name);

    // Disable unnecessary devices.
    cmd.arg("-nodefaults");

    cmd.args(["-boot", "menu=on,splash-time=0"]);
    match arch {
        Arch::X86_64 => {
            // Use a fairly modern machine to target.
            cmd.args(["-machine", "q35"]);

            // Allocate some memory.
            cmd.args(["-m", "256M"]);

            // Use vga graphics.
            cmd.args(["-vga", "std"]);

            if std::env::consts::OS == "linux" {
                cmd.arg("-enable-kvm");
            }
        }
    }

    let mut ovmf_code_arg = OsString::from("if=pflash,format=raw,readonly=on,file=");
    ovmf_code_arg.push(ovmf_code);
    cmd.arg("-drive").arg(ovmf_code_arg);

    let mut ovmf_vars_arg = OsString::from("if=pflash,format=raw,readonly=on,file=");
    ovmf_vars_arg.push(ovmf_vars);
    cmd.arg("-drive").arg(ovmf_vars_arg);

    let mut fat_drive_arg = OsString::from("format=raw,file=fat:rw:");
    fat_drive_arg.push(fat_directory);
    cmd.arg("-drive").arg(fat_drive_arg);

    let status = cmd.status()?;
    if !status.success() {
        return Err(RunError::QemuError {
            code: status.code(),
        });
    }

    Ok(())
}

/// Various errors that can occur while building and running capora-boot-stub.
#[derive(Debug)]
pub enum RunError {
    /// An error occurred while building the kernel.
    BuildError(BuildError),
    /// An error occurred while building the fat directory.
    BuildFatDirectoryError(std::io::Error),
    /// An error ocurred while launching qemu.
    ProcessError(std::io::Error),
    /// QEMU exited with a non-zero exit code.
    QemuError {
        /// The exit code of QEMU.
        code: Option<i32>,
    },
}

impl From<BuildError> for RunError {
    fn from(value: BuildError) -> Self {
        Self::BuildError(value)
    }
}

impl From<std::io::Error> for RunError {
    fn from(value: std::io::Error) -> Self {
        Self::ProcessError(value)
    }
}

impl fmt::Display for RunError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BuildError(error) => {
                writeln!(f, "error while building capora-boot-stub: {error}")
            }
            Self::BuildFatDirectoryError(error) => {
                writeln!(f, "error while building FAT directory: {error}")
            }
            Self::ProcessError(error) => writeln!(f, "error while launching QEMU: {error}"),
            Self::QemuError { code: Some(code) } => {
                writeln!(f, "qemu failed with exit status {code}")
            }
            Self::QemuError { code: None } => f.write_str("qemu terminated by signal"),
        }
    }
}

/// Builds the FAT directory structure used for running capora-boot-stub.
pub fn build_fat_directory(arch: Arch, stub_path: PathBuf) -> Result<PathBuf, std::io::Error> {
    let mut fat_directory = PathBuf::with_capacity(50);
    fat_directory.push("run");
    fat_directory.push(arch.as_str());
    fat_directory.push("fat_directory");

    let mut boot_directory = fat_directory.join("EFI");
    boot_directory.push("BOOT");
    if !boot_directory.exists() {
        std::fs::create_dir_all(&boot_directory)?;
    }

    let boot_file_name = match arch {
        Arch::X86_64 => "BOOTX64.EFI",
    };

    std::fs::copy(stub_path, boot_directory.join(boot_file_name))?;

    Ok(fat_directory)
}
