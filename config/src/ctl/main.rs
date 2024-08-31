//! Helper executable for managing `capora-boot-stub`'s configuration.

use std::{io::Write, iter, mem, path::PathBuf};

use cli::{parse_arguments, ArgumentEntry};
use config_raw::{
    ConfigurationFlags, ConfigurationHeader as RawConfigurationHeader,
    EmbeddedEntry as RawEmbeddedEntry, EmbeddedEntryFlags, EntryType,
    VersionHeader as RawVersionHeader, EMBEDDED_SECTION_NAME, MAJOR_VERSION, MINOR_VERSION,
    SECTION_NAME,
};

pub mod cli;

fn main() {
    match parse_arguments() {
        cli::Action::Configure {
            path,
            application,
            modules,
        } => configure(path, application, modules),
    }
}

/// Creates and installs a configuration for `capora-boot-stub` into the file located at
/// `stub_path`, using `application` and `modules` as the configured entries.
pub fn configure(stub_path: PathBuf, application: ArgumentEntry, modules: Vec<ArgumentEntry>) {
    let mut configuration_section_file_temp = tempfile::NamedTempFile::new().unwrap();
    let configuration_section_file = configuration_section_file_temp.as_file_mut();

    let mut embedded_section_file_temp = tempfile::NamedTempFile::new().unwrap();
    let embedded_section_file = embedded_section_file_temp.as_file_mut();
    let mut current_embedded_section_offset: u64 = 0;

    let config_header = RawConfigurationHeader {
        version_header: RawVersionHeader {
            major_version: MAJOR_VERSION,
            minor_version: MINOR_VERSION,
        },
        flags: ConfigurationFlags(0),
        entry_count: (1 + modules.len()) as u64,
        entry_table_offset: mem::size_of::<RawConfigurationHeader>() as u64,
    };
    configuration_section_file
        .write_all(&config_header.version_header.major_version.to_le_bytes())
        .unwrap();
    configuration_section_file
        .write_all(&config_header.version_header.minor_version.to_le_bytes())
        .unwrap();
    configuration_section_file
        .write_all(&config_header.flags.0.to_le_bytes())
        .unwrap();
    configuration_section_file
        .write_all(&config_header.entry_count.to_le_bytes())
        .unwrap();
    configuration_section_file
        .write_all(&config_header.entry_table_offset.to_le_bytes())
        .unwrap();

    let mut current_configuration_section_offset = config_header.entry_table_offset;
    for entry in iter::once(application).chain(modules.into_iter()) {
        match entry {
            ArgumentEntry::Embedded { name, file_path } => {
                let embedded_data = std::fs::read(file_path).unwrap();
                embedded_section_file
                    .write_all(embedded_data.as_slice())
                    .unwrap();
                let data_offset = current_embedded_section_offset;
                current_embedded_section_offset += embedded_data.len() as u64;

                let current_offset = current_configuration_section_offset;

                current_configuration_section_offset +=
                    (mem::size_of::<RawEmbeddedEntry>() + name.len()) as u64;

                configuration_section_file
                    .write_all(
                        &(mem::size_of::<RawEmbeddedEntry>() as u64 + name.len() as u64)
                            .to_le_bytes(),
                    )
                    .unwrap();
                configuration_section_file
                    .write_all(&EntryType::EMBEDDED.0.to_le_bytes())
                    .unwrap();
                configuration_section_file
                    .write_all(&EmbeddedEntryFlags(0).0.to_le_bytes())
                    .unwrap();
                configuration_section_file
                    .write_all(
                        &(current_offset + mem::size_of::<RawEmbeddedEntry>() as u64).to_le_bytes(),
                    )
                    .unwrap();
                configuration_section_file
                    .write_all(&(name.len() as u64).to_le_bytes())
                    .unwrap();
                configuration_section_file
                    .write_all(&data_offset.to_le_bytes())
                    .unwrap();
                configuration_section_file
                    .write_all(&(embedded_data.len() as u64).to_le_bytes())
                    .unwrap();
                configuration_section_file
                    .write_all(name.as_bytes())
                    .unwrap();
            }
        }
    }

    embedded_section_file.flush().unwrap();
    configuration_section_file.flush().unwrap();

    let mut command = std::process::Command::new("llvm-objcopy");

    command
        .arg("--add-section")
        .arg(format!(
            "{SECTION_NAME}={}",
            configuration_section_file_temp.path().display()
        ))
        .arg("--set-section-flags")
        .arg(format!("{SECTION_NAME}=alloc,readonly,data"));

    command
        .arg("--add-section")
        .arg(format!(
            "{EMBEDDED_SECTION_NAME}={}",
            embedded_section_file_temp.path().display()
        ))
        .arg("--set-section-flags")
        .arg(format!("{EMBEDDED_SECTION_NAME}=alloc,readonly,data"));

    command.arg(stub_path);

    command.status().unwrap();
}
