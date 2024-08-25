//! Helper crate for building and testing capora boot stub.

use std::{env, ffi::OsString, path::PathBuf};

fn main() -> Result<(), ()> {
    let subcommand = parse_arguments(env::args_os());

    match subcommand {
        Subcommand::Build { release, target } => build(target, release),
        Subcommand::Run {
            release,
            target,
            ovmf_code,
            ovmf_vars,
        } => run(target, release, ovmf_code, ovmf_vars),
    }
}

/// Builds capora uefi stub with the specified target and release mode.
pub fn build(target: Target, release: bool) -> Result<(), ()> {
    let mut cmd = std::process::Command::new("cargo");
    cmd.arg("build");
    cmd.args(["--package", "uefi-stub"]);

    cmd.args(["--target", target.as_triple()]);

    if release {
        cmd.arg("--release");
    }

    cmd.status().map(|_| ()).map_err(|_| ())
}

/// Builds capora uefi stub with the specified target and release mode, then executes the
/// uefi stub for testing purposes.
pub fn run(
    target: Target,
    release: bool,
    ovmf_code: PathBuf,
    ovmf_vars: PathBuf,
) -> Result<(), ()> {
    let mut cmd = std::process::Command::new("cargo");
    cmd.arg("build");
    cmd.args(["--package", "uefi-stub"]);

    cmd.args(["--target", target.as_triple()]);

    if release {
        cmd.arg("--release");
    }

    cmd.status().map_err(|_| ())?;

    let qemu_name = match target {
        Target::X86_64 => "qemu-system-x86_64",
    };
    let mut cmd = std::process::Command::new(qemu_name);

    // Disable necessary devices.
    cmd.arg("-nodefaults");

    cmd.args(["-boot", "menu=on,splash-time=0"]);
    match target {
        Target::X86_64 => {
            // Use a fairly modern machine as the emulation target.
            cmd.args(["-machine", "q35"]);

            // Allocate memory to emulator.
            cmd.args(["-m", "256M"]);

            // Setup graphics
            cmd.args(["-vga", "std"]);

            cmd.arg("--enable-kvm");
        }
    }

    let mut ovmf_code_arg = OsString::from("if=pflash,format=raw,readonly=on,file=");
    ovmf_code_arg.push(ovmf_code);
    cmd.arg("-drive").arg(ovmf_code_arg);

    let mut ovmf_vars_arg = OsString::from("if=pflash,format=raw,readonly=off,file=");
    ovmf_vars_arg.push(ovmf_vars);
    cmd.arg("-drive").arg(ovmf_vars_arg);

    let mut fat_drive_arg = OsString::from("format=raw,file=fat:rw:");
    fat_drive_arg.push(build_fat_directory(release, target).unwrap());
    cmd.arg("-drive").arg(fat_drive_arg);

    cmd.status().map(|_| ()).map_err(|_| ())
}

/// Setups the boot directory.
pub fn build_fat_directory(release: bool, target: Target) -> Result<PathBuf, std::io::Error> {
    let build_mode = if release { "release" } else { "debug" };

    let build_directory = PathBuf::from("target")
        .join(target.as_triple())
        .join(build_mode);

    let fat_directory = PathBuf::from("run")
        .join(target.as_str())
        .join("fat_directory");
    let boot_directory = fat_directory.join("EFI").join("BOOT");
    if !boot_directory.exists() {
        std::fs::create_dir_all(&boot_directory)?;
    }

    let boot_file_name = match target {
        Target::X86_64 => "BOOTX64.EFI",
    };

    std::fs::copy(
        build_directory.join("uefi-stub.efi"),
        boot_directory.join(boot_file_name),
    )?;

    Ok(fat_directory)
}

/// The subcommand to execute.
pub enum Subcommand {
    /// Build
    Build {
        /// Indicates that capora uefi stub should be built in release mode.
        release: bool,
        /// Indicates the architecture of capora uefi stub to be built.
        target: Target,
    },
    /// Run capora uefi stub.
    Run {
        /// Indicates the capora uefi stub should be built in release mode.
        release: bool,
        /// Indicates the architecture to build capora uefi stub and run the tests on.
        target: Target,
        /// The path to the OVMF code file used to run UEFI.
        ovmf_code: PathBuf,
        /// The path to the OVMF vars file used to run UEFI.
        ovmf_vars: PathBuf,
    },
}

/// The architectures supported by the kernel.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Target {
    /// The `x86_64` architecture.
    X86_64,
}

impl Target {
    /// Converts the given [`Target`] into its Rust target triple.
    pub fn as_triple(self) -> &'static str {
        match self {
            Self::X86_64 => "x86_64-unknown-uefi",
        }
    }

    /// Converts the given [`Target`] to its unique selector name.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::X86_64 => "x86_64",
        }
    }
}

impl clap::ValueEnum for Target {
    fn value_variants<'a>() -> &'a [Self] {
        static VALUES: &[Target] = &[Target::X86_64];

        VALUES
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(clap::builder::PossibleValue::new(self.as_str()))
    }
}

/// Parses the given arguments and constructs a [`Subcommand`].
pub fn parse_arguments(arguments: env::ArgsOs) -> Subcommand {
    let argument_matches = command_parser().get_matches_from(arguments);
    let (subcommand_name, subcommand_args) = argument_matches
        .subcommand()
        .expect("subcommand is required");

    match subcommand_name {
        "build" => parse_build_arguments(subcommand_args),
        "run" => parse_run_arguments(subcommand_args),
        other => unreachable!("unexpected subcommand: {other}"),
    }
}

/// Parses the arguments to the build subcommand and constructs its [`Subcommand`].
pub fn parse_build_arguments(subcommand_args: &clap::ArgMatches) -> Subcommand {
    let release = subcommand_args
        .get_one::<bool>("release")
        .copied()
        .unwrap_or(false);
    let target = subcommand_args
        .get_one::<Target>("target")
        .copied()
        .expect("target is required");

    Subcommand::Build { release, target }
}

/// Parses the arguments to the run subcommand and constructs its [`Subcommand::Run`].
pub fn parse_run_arguments(subcommand_args: &clap::ArgMatches) -> Subcommand {
    let release = subcommand_args
        .get_one::<bool>("release")
        .copied()
        .unwrap_or(false);
    let target = subcommand_args
        .get_one::<Target>("target")
        .copied()
        .expect("target is required");
    let ovmf_code = subcommand_args
        .get_one("ovmf-code")
        .cloned()
        .expect("ovmf-code is required");
    let ovmf_vars = subcommand_args
        .get_one("ovmf-vars")
        .cloned()
        .expect("ovmf-vars is required");

    Subcommand::Run {
        release,
        target,
        ovmf_code,
        ovmf_vars,
    }
}

/// Returns the clap command parser.
pub fn command_parser() -> clap::Command {
    let release_argument = clap::Arg::new("release")
        .help("Builds the capora kernel in release mode")
        .short('r')
        .long("release")
        .action(clap::ArgAction::SetTrue);

    let target_argument = clap::Arg::new("target")
        .long("target")
        .value_parser(clap::builder::EnumValueParser::<Target>::new())
        .required(true);

    let build_subcommand = clap::Command::new("build")
        .about("Build the capora kernel")
        .arg(release_argument.clone())
        .arg(target_argument.clone());

    let run_subcommand = clap::Command::new("run")
        .about("build capora uefi stub for in testing mode and run it in QEMU")
        .arg(release_argument)
        .arg(target_argument)
        .arg(
            clap::Arg::new("ovmf-code")
                .long("ovmf-code")
                .short('c')
                .value_parser(clap::builder::PathBufValueParser::new())
                .required(true),
        )
        .arg(
            clap::Arg::new("ovmf-vars")
                .long("ovmf-vars")
                .short('v')
                .value_parser(clap::builder::PathBufValueParser::new())
                .required(true),
        );

    clap::Command::new("xtask")
        .about("Developer utility for running various tasks in capora-kernel")
        .subcommand(build_subcommand)
        .subcommand(run_subcommand)
        .subcommand_required(true)
        .arg_required_else_help(true)
}
