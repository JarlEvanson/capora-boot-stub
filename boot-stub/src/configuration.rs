//! Functionality that deals with parsing and interpreting capora-boot-stub's configuration format.

use core::{
    error, fmt,
    mem::{self, MaybeUninit},
    ptr,
    str::Utf8Error,
};

use config::{
    pe::{section_header_table, LocateSectionHeaderTableError},
    Configuration, ConfigurationFlags, Entry, EntryType, ParseConfigurationError,
    EMBEDDED_SECTION_NAME, SECTION_NAME,
};
use uefi::{
    boot::{self, image_handle, open_protocol_exclusive},
    proto::loaded_image::LoadedImage,
    Status,
};

use crate::mapper::{ApplicationMemoryMap, Protection, Usage};

/// Acquires, parses, and interprets the [`Configuration`].
pub fn parse_and_interprete_configuration(
    application_map: &mut ApplicationMemoryMap,
) -> Result<(&'static str, &'static [u8], u64, u64), ParseAndInterpretConfigurationError> {
    let (config_section, embedded_section) = get_sections()?;

    // Acquire the [`Configuration`].
    let configuration = Configuration::parse(config_section)?;

    // Check if the [`Configuration`] has any unrecognized flags.
    if configuration.flags().0 & !ConfigurationFlags::SUPPORTED.0 != 0 {
        return Err(
            ParseAndInterpretConfigurationError::UnsupportedConfigurationFlags(
                configuration.flags().0 & !ConfigurationFlags::SUPPORTED.0,
            ),
        );
    }
    // Check if the [`Configuration`] has any unsupported [`Entry`]s.
    for (index, entry) in configuration.entries().enumerate() {
        if let Entry::Unknown {
            entry_type,
            slice: _,
        } = entry
        {
            return Err(ParseAndInterpretConfigurationError::UnsupportedEntry {
                index,
                entry_type,
            });
        }
    }

    let mut entries = configuration.entries();
    let application_entry = entries
        .next()
        .ok_or(ParseAndInterpretConfigurationError::MissingApplicationEntry)?;
    let (application_name, application_size, application_data) =
        get_entry_data(configuration, application_entry)
            .map_err(ParseAndInterpretConfigurationError::LoadApplicationError)?;
    let application_page_count = application_size.div_ceil(4096);
    let application_pages = boot::allocate_pages(
        boot::AllocateType::AnyPages,
        boot::MemoryType::LOADER_DATA,
        application_page_count,
    );
    let application_pages = application_pages.map_err(|_| {
        ParseAndInterpretConfigurationError::LoadApplicationError(LoadEntryError::AllocationFailed)
    })?;
    let application_bytes = unsafe {
        core::slice::from_raw_parts_mut(
            application_pages.as_ptr().cast::<MaybeUninit<u8>>(),
            application_size,
        )
    };

    let application_bytes = load_entry(embedded_section, application_bytes, application_data)
        .map_err(ParseAndInterpretConfigurationError::LoadApplicationError)?;

    let module_count = configuration.entry_count() - 1;
    if module_count == 0 {
        return Ok((application_name, application_bytes, 0, module_count));
    }

    let mut total_name_size = 0;
    for entry in entries.clone() {
        let length = match entry {
            Entry::Embedded(embedded) => embedded.name_length(),
            Entry::BootFilesystem(boot_filesystem) => boot_filesystem.name_length(),
            _ => unreachable!("unknown entries should have caused an error already"),
        };

        total_name_size += length as usize;
    }
    let total_module_entry_size = module_count as usize * mem::size_of::<boot_api::ModuleEntry>();
    let total_size = total_module_entry_size + total_name_size;

    let module_memory_entry = application_map.allocate(
        total_size.div_ceil(4096) as u64,
        Protection::Writable,
        Usage::General,
    );
    let module_virtual_address = module_memory_entry.page_range().virtual_address();

    let (module_entry_slice, string_slice) = {
        let module_memory_slice =
            unsafe { &mut *(module_memory_entry.as_bytes_mut() as *mut [MaybeUninit<u8>]) };
        let (module_entry_slice, string_slice) =
            module_memory_slice.split_at_mut(total_module_entry_size);

        let module_entry_slice = unsafe {
            let slice = core::slice::from_raw_parts_mut(
                module_entry_slice
                    .as_mut_ptr()
                    .cast::<MaybeUninit<boot_api::ModuleEntry>>(),
                module_count as usize,
            );
            MaybeUninit::fill(
                slice,
                boot_api::ModuleEntry {
                    name: ptr::null(),
                    name_length: 0,
                    address: ptr::null(),
                    size: 0,
                },
            )
        };

        (module_entry_slice, MaybeUninit::fill(string_slice, 0))
    };

    let mut string_index = 0;
    for (index, entry) in entries.enumerate() {
        let (module_name, module_size, module_data) = get_entry_data(configuration, entry)
            .map_err(
                |error| ParseAndInterpretConfigurationError::LoadEntryError { index, error },
            )?;
        let module_page_count = module_size.div_ceil(4096);

        let memory_entry = application_map.allocate(
            module_page_count as u64,
            Protection::Writable,
            Usage::Module,
        );
        log::debug!(
            "Module {module_name} loaded at {:#X}",
            memory_entry.page_range().virtual_address()
        );
        load_entry(embedded_section, memory_entry.as_bytes_mut(), module_data).map_err(
            |error| ParseAndInterpretConfigurationError::LoadEntryError { index, error },
        )?;

        let loaded_entry = boot_api::ModuleEntry {
            name: (module_virtual_address + (total_module_entry_size + string_index) as u64)
                as *const u8,
            name_length: module_name.len(),
            address: memory_entry.page_range().virtual_address() as *const u8,
            size: module_size,
        };
        module_entry_slice[index] = loaded_entry;

        string_slice[string_index..][..module_name.len()].copy_from_slice(module_name.as_bytes());
        string_index += module_name.len();
    }

    Ok((
        application_name,
        application_bytes,
        module_virtual_address,
        module_count,
    ))
}

/// Various errors that can occur while parsing and interpreting [`Configuration`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParseAndInterpretConfigurationError {
    /// An error ocurred while acquiring the configuration section and, if present, the embedded
    /// data section.
    GetSectionsError(GetSectionsError),
    /// An error occurred while parsing the [`Configuration`].
    ConfigurationError(ParseConfigurationError),
    /// An [`Configuration`] flag wasn't recognized.
    UnsupportedConfigurationFlags(u32),
    /// An [`Entry`] is unsupported.
    UnsupportedEntry {
        /// The index of the unsupported [`Entry`].
        index: usize,
        /// The type of the unsupported [`Entry`].
        entry_type: EntryType,
    },
    /// An entry for the application to be loaded does not exist.
    MissingApplicationEntry,
    /// An error occurrred while loading the application data.
    LoadApplicationError(LoadEntryError),
    /// An error occurred while loading an entry.
    LoadEntryError {
        /// The index of the [`Entry`] whose loading failed.
        index: usize,
        /// An error occurred while loading an [`Entry`].
        error: LoadEntryError,
    },
}

impl From<GetSectionsError> for ParseAndInterpretConfigurationError {
    fn from(value: GetSectionsError) -> Self {
        Self::GetSectionsError(value)
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
            Self::GetSectionsError(error) => write!(f, "failed to get required sections: {error}",),
            Self::ConfigurationError(error) => write!(f, "error parsing configuration: {error:?}"),
            Self::UnsupportedConfigurationFlags(flags) => {
                write!(f, "unsupported configuration flags: {:b}", flags)
            }
            Self::UnsupportedEntry { index, entry_type } => write!(
                f,
                "entry {index} is of unsupported entry type {entry_type:?}"
            ),
            Self::MissingApplicationEntry => write!(f, "missing entry for application"),
            Self::LoadApplicationError(error) => write!(f, "error loading application: {error}"),
            Self::LoadEntryError { index, error } => {
                write!(f, "error loadeding entry {index}: {error}")
            }
        }
    }
}

impl error::Error for ParseAndInterpretConfigurationError {}

/// Retrives the configuration section, and, if present, the embedded data section.
fn get_sections() -> Result<(&'static [u8], Option<&'static [u8]>), GetSectionsError> {
    let (image_base, image_size) = get_image_data()?;

    let slice = unsafe { core::slice::from_raw_parts(image_base, image_size) };
    let section_header_table = section_header_table(&slice)?;

    let configuration_section = {
        let section_header = section_header_table
            .find_section(SECTION_NAME)
            .ok_or(GetSectionsError::MissingConfigurationSection)?;
        let configuration_section_base =
            unsafe { image_base.add(section_header.virtual_address as usize) };
        unsafe {
            core::slice::from_raw_parts(
                configuration_section_base,
                section_header.virtual_size as usize,
            )
        }
    };

    let embedded_section = 'embedded: {
        let Some(section_header) = section_header_table.find_section(EMBEDDED_SECTION_NAME) else {
            break 'embedded None;
        };

        let configuration_section_base =
            unsafe { image_base.add(section_header.virtual_address as usize) };
        unsafe {
            Some(core::slice::from_raw_parts(
                configuration_section_base,
                section_header.virtual_size as usize,
            ))
        }
    };

    Ok((configuration_section, embedded_section))
}

/// Various errors that can occur while retrieving the configuration section and, if present, the embedded data
/// section.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GetSectionsError {
    /// An error ocurred while acquiring image data.
    ImageDataError(GetImageDataError),
    /// An error occurred while locating the section header table of the image.
    SectionHeaderTableError(LocateSectionHeaderTableError),
    /// The configuration section is not present.
    MissingConfigurationSection,
}

impl From<GetImageDataError> for GetSectionsError {
    fn from(value: GetImageDataError) -> Self {
        Self::ImageDataError(value)
    }
}

impl From<LocateSectionHeaderTableError> for GetSectionsError {
    fn from(value: LocateSectionHeaderTableError) -> Self {
        Self::SectionHeaderTableError(value)
    }
}

impl fmt::Display for GetSectionsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ImageDataError(error) => write!(f, "failed to retrieve image data: {error}"),
            Self::SectionHeaderTableError(error) => {
                write!(f, "failed to locate image section header table: {error}")
            }
            Self::MissingConfigurationSection => write!(f, "missing configuration section",),
        }
    }
}

impl error::Error for GetSectionsError {}

/// Loads the data pointed to by [`EntryData`].
pub fn load_entry<'bytes>(
    embedded_section: Option<&[u8]>,
    bytes: &'bytes mut [MaybeUninit<u8>],
    entry_data: EntryData,
) -> Result<&'bytes mut [u8], LoadEntryError> {
    let bytes = match entry_data {
        EntryData::Embedded { data_offset } => {
            let Some(embedded_section) = embedded_section else {
                return Err(LoadEmbeddedEntryError::MissingEmbeddedSection.into());
            };

            let data_max = match data_offset.checked_add(bytes.len()) {
                Some(data_max) if data_max <= embedded_section.len() => data_max,
                _ => return Err(LoadEmbeddedEntryError::DataOutOfBounds.into()),
            };

            MaybeUninit::copy_from_slice(bytes, &embedded_section[data_offset..data_max])
        }
    };

    Ok(bytes)
}

/// Various errors that can occur while loading an entry.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoadEntryError {
    /// The name of the entry is out of bounds.
    NameOutOfBounds,
    /// The name of the entry was not valid utf8.
    InvalidName(Utf8Error),
    /// An allocation of memory to store an entry failed.
    AllocationFailed,
    /// An error occurred while loading an [`EmbeddedEntry`][ee]
    ///
    /// [ee]: config::EmbeddedEntry
    EmbeddedError(LoadEmbeddedEntryError),
}

impl From<LoadEmbeddedEntryError> for LoadEntryError {
    fn from(value: LoadEmbeddedEntryError) -> Self {
        Self::EmbeddedError(value)
    }
}

impl fmt::Display for LoadEntryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NameOutOfBounds => write!(f, "name located out of bounds",),
            Self::InvalidName(error) => write!(f, "invalid name: {error}",),
            Self::AllocationFailed => write!(f, "an allocation failed",),
            Self::EmbeddedError(error) => {
                write!(f, "error ocurred while parsing embedded entry: {error}")
            }
        }
    }
}

impl error::Error for LoadEntryError {}

/// Various errors that can ocurr while loading an [`EmbeddedEntry`][ee]
///
/// [ee]: config::EmbeddedEntry
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoadEmbeddedEntryError {
    /// The embedded data section is missing.
    MissingEmbeddedSection,
    /// The data to be loaded is out of bounds of the embedded data section.
    DataOutOfBounds,
}

impl fmt::Display for LoadEmbeddedEntryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingEmbeddedSection => write!(f, "embedded data section is missing"),
            Self::DataOutOfBounds => write!(f, "data is located out of bounds"),
        }
    }
}

/// Returns the necessary data to load an entry, including the size of the data, as well as its
/// name and other data necessary to load the data.
pub fn get_entry_data<'config>(
    configuration: Configuration<'config>,
    entry: Entry<'config>,
) -> Result<(&'config str, usize, EntryData), LoadEntryError> {
    let (name_offset, name_length, data_length, data) = match entry {
        Entry::Embedded(entry) => (
            entry.name_offset(),
            entry.name_length(),
            entry.data_length() as usize,
            EntryData::Embedded {
                data_offset: entry.data_offset() as usize,
            },
        ),
        Entry::BootFilesystem(_) => todo!(),
        Entry::Unknown {
            entry_type: _,
            slice: _,
        } => unreachable!("unknown entry type should have already been caught"),
    };

    let name_max = match name_offset.checked_add(name_length) {
        Some(name_max) if (name_max as usize) <= configuration.underlying_slice().len() => name_max,
        _ => return Err(LoadEntryError::NameOutOfBounds),
    };
    let name = core::str::from_utf8(
        &configuration.underlying_slice()[name_offset as usize..name_max as usize],
    )
    .map_err(LoadEntryError::InvalidName)?;

    Ok((name, data_length, data))
}

/// Data necessary to loaded a module.
pub enum EntryData {
    /// Data necessary to load an embedded module.
    Embedded {
        /// The offset into the embedded data section at which the module should be loaded from.
        data_offset: usize,
    },
}

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
