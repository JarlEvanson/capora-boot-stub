//! Helper crate for building and testing capora boot stub.

use std::env;

fn main() -> Result<(), ()> {
    let subcommand = parse_arguments(env::args_os());

    match subcommand {
        Subcommand::Build { release, target } => build(target, release),
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

/// The subcommand to execute.
pub enum Subcommand {
    /// Build
    Build {
        /// Indicates that capora uefi stub should be built in release mode.
        release: bool,
        /// Indicates the architecture of capora uefi stub to be built.
        target: Target,
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
}

impl clap::ValueEnum for Target {
    fn value_variants<'a>() -> &'a [Self] {
        static VALUES: &[Target] = &[Target::X86_64];

        VALUES
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        let possible_value = match self {
            Self::X86_64 => clap::builder::PossibleValue::new("x86_64"),
        };

        Some(possible_value)
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

/// Returns the clap command parser.
pub fn command_parser() -> clap::Command {
    let target_argument = clap::Arg::new("target")
        .long("target")
        .value_parser(clap::builder::EnumValueParser::<Target>::new());

    let build_subcommand = clap::Command::new("build")
        .about("Build the capora kernel")
        .arg(target_argument.required(true))
        .arg(
            clap::Arg::new("release")
                .help("Builds the capora kernel in release mode")
                .short('r')
                .long("release")
                .action(clap::ArgAction::SetTrue),
        );

    let command = clap::Command::new("xtask")
        .about("Developer utility for running various tasks in capora-kernel")
        .subcommand(build_subcommand)
        .subcommand_required(true)
        .arg_required_else_help(true);

    command
}
