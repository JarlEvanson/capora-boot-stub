//! Functionality that deals with parsing and interpreting capora-boot-stub's configuration format.

use core::{error, fmt};

use config::{
    pe::{section_header_table, LocateSectionHeaderTableError, SectionHeaderTable},
    Configuration, ParseConfigurationError,
};
use uefi::{
    boot::{image_handle, open_protocol_exclusive},
    proto::loaded_image::LoadedImage,
    Status,
};

pub fn parse_and_interprete_configuration() -> Result<(), ParseAndInterpretConfigurationError> {
    let (image_base, image_size) = get_image_data()?;

    // SAFETY:
    // We create and drop this slice as quickly as possible, and we don't interact with any
    // global variables while doing so, so this should be as safe as possible.
    let slice = unsafe { core::slice::from_raw_parts(image_base, image_size) };
    let section_header_table = section_header_table(slice)?;

    let configuration = {
        let section_header = section_header_table
            .find_section(".options")
            .ok_or(ParseAndInterpretConfigurationError::MissingConfiguration)?;
        let config_section_base =
            unsafe { image_base.add(section_header.virtual_address as usize) };
        let config_section = unsafe {
            core::slice::from_raw_parts(config_section_base, section_header.virtual_size as usize)
        };
        Configuration::parse(config_section)?
    };

    let mut entries = configuration.entries();
    let application_entry = entries
        .next()
        .ok_or(ParseAndInterpretConfigurationError::MissingApplicationEntry)?;

    todo!()
}

/// Various errors that can occur while parsing and interpreting [`Configuration`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseAndInterpretConfigurationError {
    /// An error ocurred while acquiring image data.
    GetImageDataError(GetImageDataError),
    /// An error ocurred while locating the image's [`SectionHeaderTable`][sht].
    ///
    /// [sht]: config::pe::SectionHeaderTable
    LocateSectionHeaderTableError(LocateSectionHeaderTableError),
    /// The configuration section is missing.
    MissingConfiguration,
    /// An error occurred while parsing the [`Configuration`].
    ConfigurationError(ParseConfigurationError),
    /// An [`Configuration`] flag wasn't recognized.
    UnsupportedConfigurationFlags(u32),
    /// An entry for the application to be loaded does not exist.
    MissingApplicationEntry,
}

impl From<GetImageDataError> for ParseAndInterpretConfigurationError {
    fn from(value: GetImageDataError) -> Self {
        Self::GetImageDataError(value)
    }
}

impl From<LocateSectionHeaderTableError> for ParseAndInterpretConfigurationError {
    fn from(value: LocateSectionHeaderTableError) -> Self {
        Self::LocateSectionHeaderTableError(value)
    }
}

impl From<ParseConfigurationError> for ParseAndInterpretConfigurationError {
    fn from(value: ParseConfigurationError) -> Self {
        Self::ConfigurationError(value)
    }
}

impl fmt::Display for ParseAndInterpretConfigurationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::GetImageDataError(error) => {
                write!(f, "error retrieving image information: {error}",)
            }
            Self::LocateSectionHeaderTableError(error) => {
                write!(f, "error locating section header table: {error}")
            }
            Self::MissingConfiguration => write!(f, "missing configuration section"),
            Self::ConfigurationError(error) => write!(f, "error parsing configuration: {error:?}"),
            Self::UnsupportedConfigurationFlags(flags) => {
                write!(f, "unsupported configuration flags: {:b}", flags)
            }
            Self::MissingApplicationEntry => write!(f, "missing entry for application"),
        }
    }
}

impl error::Error for ParseAndInterpretConfigurationError {}

/// Returns the base of the loaded image and the size of the image.
pub fn get_image_data() -> Result<(*const u8, usize), GetImageDataError> {
    let image_protocol = open_protocol_exclusive::<LoadedImage>(image_handle())?;

    let (image_base, image_size) = image_protocol.info();

    Ok((
        image_base.cast::<u8>(),
        TryInto::<usize>::try_into(image_size).expect("nonsensical image size"),
    ))
}

/// An error occurred while obtaining loaded image image data.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GetImageDataError(Status);

impl From<uefi::Error> for GetImageDataError {
    fn from(value: uefi::Error) -> Self {
        GetImageDataError(value.status())
    }
}

impl fmt::Display for GetImageDataError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "error acquiring `LoadedImage` protocol: {}", self.0)
    }
}

impl error::Error for GetImageDataError {}
