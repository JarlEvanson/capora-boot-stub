//! Helper crate for building and testing capora boot stub.

use std::env;

fn main() {
    parse_arguments(env::args_os());
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

    todo!()
}

/// Returns the clap command parser.
pub fn command_parser() -> clap::Command {
    let target_argument = clap::Arg::new("target")
        .long("target")
        .value_parser(clap::builder::EnumValueParser::<Target>::new());

    let build_subcommand = clap::Command::new("build")
        .about("Build the capora kernel")
        .arg(target_argument)
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
