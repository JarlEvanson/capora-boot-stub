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

/// A loaded [`Entry`].
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct LoadedEntry {
    /// A pointer to the utf8 encoded name of the loaded [`Entry`].
    name: *const u8,
    /// The length, in bytes, of [`LoadedEntry::name`].
    name_length: usize,
    /// The address of the loaded [`Entry`].
    address: *const u8,
    /// The size, in bytes, of the loaded [`Entry`].
    size: usize,
}

impl LoadedEntry {
    /// The name of the [`LoadedEntry`].
    pub fn name(&self) -> &str {
        let name_slice = unsafe { core::slice::from_raw_parts(self.name, self.name_length) };
        unsafe { core::str::from_utf8_unchecked(name_slice) }
    }

    /// A slice of the bytes this [`LoadedEntry`] controls.
    pub fn data(&self) -> &[u8] {
        unsafe { core::slice::from_raw_parts(self.address, self.size) }
    }

    /// A mutable slice of the bytes this [`LoadedEntry`] controls.
    pub fn data_mut(&mut self) -> &[u8] {
        unsafe { core::slice::from_raw_parts_mut(self.address.cast_mut(), self.size) }
    }
}

impl fmt::Debug for LoadedEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_struct = f.debug_struct("LoadedEntry");

        debug_struct.field("name", &self.name());
        debug_struct.field("data", &self.data());

        debug_struct.finish()
    }
}

/// Acquires, parses, and interprets the [`Configuration`].
pub fn parse_and_interprete_configuration(
) -> Result<(LoadedEntry, &'static mut [LoadedEntry]), ParseAndInterpretConfigurationError> {
    // Get base and size of the loaded image.
    let (image_base, image_size) = get_image_data()?;
    use core::fmt::Write;
    let _ = uefi::system::with_stdout(|stdout| {
        writeln!(stdout, "capora-boot-stub loaded at {image_base:p}")
    });

    // SAFETY:
    // We create and drop this slice as quickly as possible, and we don't interact with any
    // global variables while doing so, so this should be as safe as possible.
    let slice = unsafe { core::slice::from_raw_parts(image_base, image_size) };
    let section_header_table = section_header_table(slice)?;

    // Acquire the [`Configuration`].
    let configuration = {
        let section_header = section_header_table
            .find_section(SECTION_NAME)
            .ok_or(ParseAndInterpretConfigurationError::MissingConfiguration)?;
        let config_section_base =
            unsafe { image_base.add(section_header.virtual_address as usize) };
        let config_section = unsafe {
            core::slice::from_raw_parts(config_section_base, section_header.virtual_size as usize)
        };
        Configuration::parse(config_section)?
    };

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

    let embedded_section = 'section: {
        let Some(section_header) = section_header_table.find_section(EMBEDDED_SECTION_NAME) else {
            break 'section None;
        };
        let section_base = unsafe { image_base.add(section_header.virtual_address as usize) };
        let section = unsafe {
            core::slice::from_raw_parts(section_base, section_header.virtual_size as usize)
        };
        Some(section)
    };

    let mut entries = configuration.entries();
    let application_entry = entries
        .next()
        .ok_or(ParseAndInterpretConfigurationError::MissingApplicationEntry)?;
    let application = load_entry(configuration, embedded_section, application_entry)
        .map_err(ParseAndInterpretConfigurationError::LoadApplicationError)?;
    let application = LoadedEntry {
        name: application.0.as_ptr(),
        name_length: application.0.len(),
        address: application.1,
        size: application.2,
    };

    let module_count = configuration.entry_count() - 1;
    if module_count == 0 {
        return Ok((application, &mut []));
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
    let total_loaded_entry_size = module_count as usize * mem::size_of::<LoadedEntry>();
    let total_size = total_loaded_entry_size + total_name_size;

    let ptr = boot::allocate_pool(boot::MemoryType::LOADER_DATA, total_size)
        .expect("allocation failed")
        .as_ptr();

    let loaded_entry_slice = {
        let slice = unsafe {
            core::slice::from_raw_parts_mut(
                ptr.cast::<MaybeUninit<LoadedEntry>>(),
                module_count as usize,
            )
        };
        MaybeUninit::fill(
            slice,
            LoadedEntry {
                name: ptr::null(),
                name_length: 0,
                address: ptr::null(),
                size: 0,
            },
        )
    };
    let string_slice = {
        let ptr = unsafe { ptr.add(total_loaded_entry_size) };
        let slice = unsafe {
            core::slice::from_raw_parts_mut(
                ptr.cast::<core::mem::MaybeUninit<u8>>(),
                total_name_size,
            )
        };
        MaybeUninit::fill(slice, 0)
    };

    let mut string_index = 0;
    for (index, entry) in entries.enumerate() {
        let (name, base, size) =
            load_entry(configuration, embedded_section, entry).map_err(|error| {
                ParseAndInterpretConfigurationError::LoadEntryError { index, error }
            })?;

        let loaded_entry = LoadedEntry {
            name: string_slice[string_index..].as_ptr(),
            name_length: name.len(),
            address: base,
            size,
        };
        loaded_entry_slice[index] = loaded_entry;

        string_slice[string_index..][..name.len()].copy_from_slice(name.as_bytes());
        string_index += name.len();
    }

    Ok((application, loaded_entry_slice))
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

/// Loads the data referenced by the provided [`Entry`].
pub fn load_entry<'config>(
    configuration: Configuration<'config>,
    embedded_section: Option<&'static [u8]>,
    target_entry: Entry<'config>,
) -> Result<(&'config str, *mut u8, usize), LoadEntryError> {
    match target_entry {
        Entry::Embedded(entry) => {
            let name = get_name(configuration, target_entry)?;
            let Some(embedded_section) = embedded_section else {
                return Err(LoadEmbeddedEntryError::MissingEmbeddedSection.into());
            };

            let page_count = entry.data_length().div_ceil(4096) as usize;
            let pages = boot::allocate_pages(
                boot::AllocateType::AnyPages,
                boot::MemoryType::LOADER_DATA,
                page_count,
            );
            let pages = pages.map_err(|error| LoadEntryError::AllocationFailed {
                page_count,
                status: error.status(),
            })?;

            let name_max = entry.data_offset().checked_add(entry.data_length());
            if !name_max.is_some_and(|value| value as usize <= embedded_section.len()) {
                return Err(LoadEmbeddedEntryError::DataOutOfBounds.into());
            }

            unsafe {
                core::ptr::copy_nonoverlapping(
                    embedded_section[entry.data_offset() as usize..].as_ptr(),
                    pages.as_ptr(),
                    entry.data_length() as usize,
                )
            }

            Ok((name, pages.as_ptr(), entry.data_length() as usize))
        }
        Entry::BootFilesystem(_) => todo!(),
        Entry::Unknown {
            entry_type: _,
            slice: _,
        } => unreachable!("unknown entry should have caused an error already"),
    }
}

/// Various errors that can occur while loading an [`Entry`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoadEntryError {
    /// An allocation failed.
    AllocationFailed {
        /// The number of pages the failing allocation attempted to allocate.
        page_count: usize,
        /// The [`Status`] that the allocation returned.
        status: Status,
    },
    /// An error ocurred when getting the name of the [`Entry`].
    NameError(GetNameError),
    /// An error ocurred while loading an [`EmbeddedEntry`][ee].
    ///
    /// [ee]: config::EmbeddedEntry
    Embedded(LoadEmbeddedEntryError),
}

impl From<GetNameError> for LoadEntryError {
    fn from(value: GetNameError) -> Self {
        Self::NameError(value)
    }
}

impl From<LoadEmbeddedEntryError> for LoadEntryError {
    fn from(value: LoadEmbeddedEntryError) -> Self {
        Self::Embedded(value)
    }
}

impl fmt::Display for LoadEntryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AllocationFailed { page_count, status } => write!(
                f,
                "failed to allocate memory chunk of {page_count} pages: {status}"
            ),
            Self::NameError(error) => write!(f, "failed to get name: {error}"),
            Self::Embedded(error) => write!(f, "error while parsing embedded entry: {error}"),
        }
    }
}

impl error::Error for LoadEntryError {}

/// Various errors that can occur
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LoadEmbeddedEntryError {
    /// `capora-boot-stub` is missing an embedded section when an [`EmbeddedEntry`][ee] exists.
    ///
    /// [ee]: config::EmbeddedEntry
    MissingEmbeddedSection,
    /// The data referenced by the [`EmbeddedEntry`][ee] is located out of bounds.
    ///
    /// [ee]: config::EmbeddedEntry
    DataOutOfBounds,
}

impl fmt::Display for LoadEmbeddedEntryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingEmbeddedSection => f.write_str("embedded data section is missing"),
            Self::DataOutOfBounds => f.write_str("data is located out of bounds"),
        }
    }
}

impl error::Error for LoadEmbeddedEntryError {}

/// Retrives the name of the entry, checking for validity.
pub fn get_name<'config>(
    configuration: Configuration<'config>,
    entry: Entry<'config>,
) -> Result<&'config str, GetNameError> {
    let (offset, length) = match entry {
        Entry::Embedded(entry) => (entry.name_offset(), entry.name_length()),
        Entry::BootFilesystem(entry) => (entry.name_offset(), entry.name_length()),
        Entry::Unknown {
            entry_type: _,
            slice: _,
        } => unreachable!("this should never be reached"),
    };

    let name_max = offset.checked_add(length);
    if !name_max.is_some_and(|value| value as usize <= configuration.underlying_slice().len()) {
        return Err(GetNameError::NameOutOfBounds);
    }

    core::str::from_utf8(&configuration.underlying_slice()[offset as usize..])
        .map_err(GetNameError::Utf8Error)
}

/// Various errors that can occur while getting the name of an [`Entry`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GetNameError {
    /// The name is located out of bounds.
    NameOutOfBounds,
    /// The name failed to be parses as utf8.
    Utf8Error(Utf8Error),
}

impl fmt::Display for GetNameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NameOutOfBounds => f.write_str("name located out of bounds"),
            Self::Utf8Error(error) => write!(f, "name parsing error: {error}"),
        }
    }
}

impl error::Error for GetNameError {}

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
