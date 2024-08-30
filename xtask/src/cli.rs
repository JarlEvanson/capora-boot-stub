//! Command line parsing and command construction.

use std::path::PathBuf;

/// The action to carry out.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Action {
    /// Build capora-boot-stub.
    Build {
        /// The [`Arch`] for which capora-boot-stub should be built.
        arch: Arch,
        /// Whether capora-boot-stub should be built in release mode.
        release: bool,
    },
    /// Runs capora-boot-stub.
    Run {
        /// The [`Arch`] for which capora-boot-stub should be built.
        arch: Arch,
        /// Whether capora-boot-stub should be built in release mode.
        release: bool,
        /// The path to the OVMF code file used to run UEFI.
        ovmf_code: PathBuf,
        /// The path to the OVMF vars file used to run UEFI.
        ovmf_vars: PathBuf,
    },
}

/// Parses arguments to construct an [`Action`].
pub fn parse_arguments() -> Action {
    let mut matches = command_parser().get_matches();
    let (subcommand_name, subcommand_matches) =
        matches.remove_subcommand().expect("subcommand required");
    match subcommand_name.as_str() {
        "build" => parse_build_arguments(subcommand_matches),
        "run" => parse_run_arguments(subcommand_matches),
        name => unreachable!("unexpected subcommand {name:?}"),
    }
}

/// Parses subcommand arguments for the [`Action::Build`] subcommand.
pub fn parse_build_arguments(mut matches: clap::ArgMatches) -> Action {
    let arch = matches
        .remove_one::<Arch>("arch")
        .expect("arch is a required argument");
    let release = matches.remove_one::<bool>("release").unwrap_or(false);

    Action::Build { arch, release }
}

/// Parses subcommand arguments for the [`Action::Run`] subcommand.
pub fn parse_run_arguments(mut matches: clap::ArgMatches) -> Action {
    let arch = matches
        .remove_one::<Arch>("arch")
        .expect("arch is a required argument");
    let release = matches.remove_one::<bool>("release").unwrap_or(false);

    let ovmf_code = matches
        .remove_one::<PathBuf>("ovmf-code")
        .expect("ovmf-code is required");
    let ovmf_vars = matches
        .remove_one::<PathBuf>("ovmf-vars")
        .expect("ovmf-vars is required");

    Action::Run {
        arch,
        release,
        ovmf_code,
        ovmf_vars,
    }
}

/// Returns the clap command parser.
pub fn command_parser() -> clap::Command {
    let arch_arg = clap::Arg::new("arch")
        .long("arch")
        .value_parser(clap::builder::EnumValueParser::<Arch>::new())
        .required(true);

    let release_arg = clap::Arg::new("release")
        .help("Build capora-boot-stub in release mode")
        .long("release")
        .short('r')
        .action(clap::ArgAction::SetTrue);

    let build_subcommand = clap::Command::new("build")
        .about("Builds capora-boot-stub")
        .arg(
            arch_arg
                .clone()
                .help("The architecture for which capora-boot-stub should be built"),
        )
        .arg(release_arg.clone());

    let run_subcommand = clap::Command::new("run")
        .about("Runs capora-boot-stub")
        .arg(arch_arg.help("The architecture for which capora-boot-stub should be built and run"))
        .arg(release_arg)
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
        .about("Developer utility for running various tasks in capora-boot-stub")
        .subcommand(build_subcommand)
        .subcommand(run_subcommand)
        .subcommand_required(true)
        .arg_required_else_help(true)
}

/// The architectures supported by capora-boot-stub.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Arch {
    /// The `x86_64` architecture.
    X86_64,
}

impl Arch {
    /// Returns the [`Arch`] as its rustc target triple.
    pub fn as_target_triple(&self) -> &'static str {
        match self {
            Self::X86_64 => "x86_64-unknown-uefi",
        }
    }

    /// Returns the [`Arch`] as its textual representation.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::X86_64 => "x86_64",
        }
    }
}

impl clap::ValueEnum for Arch {
    fn value_variants<'a>() -> &'a [Self] {
        static ARCHES: &[Arch] = &[Arch::X86_64];

        ARCHES
    }

    fn to_possible_value(&self) -> Option<clap::builder::PossibleValue> {
        Some(clap::builder::PossibleValue::new(self.as_str()))
    }
}
