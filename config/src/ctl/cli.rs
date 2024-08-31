//! Command line parsing and [`Action`] construction.

use core::{error, fmt};
use std::path::PathBuf;

/// The action to carry out.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Action {
    /// Configure `capora-boot-stub`.
    Configure {
        /// The path to `capora-boot-stub`.
        path: PathBuf,
        /// An [`ArgumentEntry`] that represents the application `capora-boot-stub` will load.
        application: ArgumentEntry,
        /// A collection of [`ArgumentEntry`] representing the various modules `capora-boot-stub`
        /// will load at runtime.
        modules: Vec<ArgumentEntry>,
    },
}

/// Parses arguments to construct an [`Action`].
pub fn parse_arguments() -> Action {
    let mut matches = command_parser().get_matches();
    let (subcommand_name, subcommand_matches) =
        matches.remove_subcommand().expect("subcommand required");
    match subcommand_name.as_str() {
        "configure" => parse_configure_arguments(subcommand_matches),
        name => unreachable!("unexpected subcommand {name:?}"),
    }
}

/// Parses subcommand arguments for the [`Action::Configure`] subcommand.
pub fn parse_configure_arguments(mut matches: clap::ArgMatches) -> Action {
    let path = matches
        .remove_one::<PathBuf>("stub")
        .expect("stub is a required argument");
    let application = matches
        .remove_one::<ArgumentEntry>("application")
        .expect("application is a required argument");
    let modules = matches
        .remove_many::<ArgumentEntry>("module")
        .map(|iter| iter.collect::<Vec<ArgumentEntry>>())
        .unwrap_or(Vec::new());

    Action::Configure {
        path,
        application,
        modules,
    }
}

/// Returns the clap command parser.
pub fn command_parser() -> clap::Command {
    let stub_arg = clap::Arg::new("stub")
        .help("The path to `capora-boot`stub`")
        .long("stub")
        .short('s')
        .value_parser(clap::builder::PathBufValueParser::new())
        .value_name("STUB")
        .value_hint(clap::builder::ValueHint::FilePath)
        .required(true);

    let application_arg = clap::Arg::new("application")
        .help("The entry for the application to be loaded by `capora-boot-stub`")
        .long("application")
        .short('a')
        .value_parser(parse_argument_entry)
        .value_name("APPLICATION")
        .value_hint(clap::builder::ValueHint::Other)
        .required(true);

    let module_arg = clap::Arg::new("module")
        .help("The entry for a module to be loaded by `capora-boot-stub`")
        .long("module")
        .short('m')
        .value_parser(parse_argument_entry)
        .value_name("MODULE")
        .value_hint(clap::builder::ValueHint::Other)
        .action(clap::ArgAction::Append);

    let configure_subcommand = clap::Command::new("configure")
        .about("Configures `capora-boot-stub`")
        .arg_required_else_help(true)
        .arg(stub_arg)
        .arg(application_arg)
        .arg(module_arg);

    clap::Command::new("capora-boot-stub-ctl")
        .about("Utility for managing `capora-boot-stub`")
        .subcommand(configure_subcommand)
        .subcommand_required(true)
        .arg_required_else_help(true)
}

/// A description of how an entry should be configured.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ArgumentEntry {
    /// The entry should be embedded into the binary.
    Embedded {
        /// The name of the entry.
        name: String,
        /// The path to the entry to be embedded.
        file_path: PathBuf,
    },
}

/// Parses a supplied [`str`] and transforms it into an [`ArgumentEntry`], which specifies how an
/// entry should be loaded when `capora-boot-stub` runs.
pub fn parse_argument_entry(input: &str) -> Result<ArgumentEntry, ParseArgumentEntryError> {
    let (name, remaining) = input
        .split_once(":")
        .ok_or(ParseArgumentEntryError::MissingEntryType)?;
    let (entry_type, additional_data) = remaining
        .split_once(":")
        .map(|(entry_type, data)| (entry_type, Some(data)))
        .unwrap_or((remaining, None));

    let entry = match entry_type {
        "embedded" => {
            let Some(path) = additional_data else {
                return Err(ParseArgumentEntryError::MissingAdditionalData);
            };

            ArgumentEntry::Embedded {
                name: name.to_owned(),
                file_path: PathBuf::from(path),
            }
        }
        entry_type => {
            return Err(ParseArgumentEntryError::UnsupportedEntryType(
                entry_type.to_owned(),
            ))
        }
    };

    Ok(entry)
}

/// Various errors that can occur when parsing an [`ArgumentEntry`].
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ParseArgumentEntryError {
    /// The string does not contain an entry type specifier.
    MissingEntryType,
    /// The string does not contain an additional data section despite the entry type requiring
    /// one.
    MissingAdditionalData,
    /// The given entry type is not supported.
    UnsupportedEntryType(String),
}

impl fmt::Display for ParseArgumentEntryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingEntryType => {
                f.write_str("missing entry type: format is `name:entry_type:additional_data`")
            }
            Self::MissingAdditionalData => {
                f.write_str("missing additional data: format is `name:entry_type:additional_data`")
            }
            Self::UnsupportedEntryType(entry_type) => {
                writeln!(f, "entry type `{entry_type}` is not supported",)
            }
        }
    }
}

impl error::Error for ParseArgumentEntryError {}

/*
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
struct ArgumentEntryParser;

impl clap::builder::TypedValueParser for ArgumentEntryParser {
    type Value = ArgumentEntry;

    fn parse_ref(
        &self,
        cmd: &clap::Command,
        arg: Option<&clap::Arg>,
        value: &std::ffi::OsStr,
    ) -> Result<Self::Value, clap::Error> {
        let value: String = clap::builder::StringValueParser::new().parse_ref(cmd, arg, value)?;

        let Some((name, remaining)) = value.split_once(':') else {
            let mut err = clap::Error::new(clap::error::ErrorKind::InvalidValue).with_cmd(&cmd);

            if let Some(arg) = arg {
                err.insert(
                    clap::error::ContextKind::InvalidArg,
                    clap::error::ContextValue::String(arg.to_string()),
                );
            }
            err.insert(
                clap::error::ContextKind::InvalidValue,
                clap::error::ContextValue::String(value),
            );

            use core::fmt::Write;

            let mut styled = clap::builder::StyledStr::new();
            let _ = write!(styled, "\n  ");

            err.insert(
                clap::error::ContextKind::Usage,
                clap::error::ContextValue::StyledStr(styled)
            );
            for context in err.context() {
                println!("{context:#?}")
            }

            return Err(err);
        };

        todo!()
    }
}
*/
