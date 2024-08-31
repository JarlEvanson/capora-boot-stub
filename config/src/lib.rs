//! Definitions and wrappers from interacting with capora-boot-stub's configuration format.

#![no_std]

use core::{fmt, mem};

use config_raw::{
    BootFilesystemEntry as RawBootFilesystemEntry, BootFilesystemEntryFlags, ConfigurationFlags,
    ConfigurationHeader, EmbeddedEntry as RawEmbeddedEntry, EmbeddedEntryFlags, EntryBase,
    EntryType, VersionHeader as RawVersionHeader, MAJOR_VERSION,
};

pub mod pe;

/// A configuration for capora-boot-stub.
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct Configuration<'slice> {
    slice: &'slice [u8],
}

impl<'slice> Configuration<'slice> {
    /// Parses the slice and returns a [`Configuration`] if the version of the configuration is
    /// supported.
    ///
    /// This supports parsing configurations with a higher minor version than known, but cannot
    /// parse any unknown major versions.
    pub fn parse(slice: &'slice [u8]) -> Result<Self, ParseConfigurationError> {
        if !(mem::size_of::<RawVersionHeader>() <= slice.len()) {
            return Err(ParseConfigurationError::SliceTooSmall);
        }

        let configuration = Self { slice };
        if configuration.major_version() != MAJOR_VERSION {
            return Err(ParseConfigurationError::UnsupportedVersion {
                major_version: configuration.major_version(),
                minor_version: configuration.minor_version(),
            });
        }

        if !(mem::size_of::<ConfigurationHeader>() <= slice.len()) {
            return Err(ParseConfigurationError::SliceTooSmall);
        }

        let mut remaining = configuration.entry_count();
        let mut entry_offset: usize = configuration
            .entry_offset()
            .try_into()
            .map_err(|_| ParseConfigurationError::SliceTooSmall)?;
        while remaining != 0 {
            if !(entry_offset
                .checked_add(mem::size_of::<EntryBase>())
                .ok_or(ParseConfigurationError::SliceTooSmall)?
                <= slice.len())
            {
                return Err(ParseConfigurationError::SliceTooSmall);
            }

            let slice = &slice[entry_offset..];
            let entry_size = slice
                .first_chunk::<8>()
                .expect("parsing bounds checking failed");
            let entry_size = u64::from_le_bytes(*entry_size)
                .try_into()
                .map_err(|_| ParseConfigurationError::SliceTooSmall)?;

            entry_offset = entry_offset
                .checked_add(entry_size)
                .ok_or(ParseConfigurationError::SliceTooSmall)?;

            remaining -= 1;
        }

        if !(entry_offset <= slice.len()) {
            return Err(ParseConfigurationError::SliceTooSmall);
        }

        Ok(configuration)
    }

    /// The major version of the configuration format that this [`Configuration`] uses.
    pub fn major_version(&self) -> u16 {
        let major_version = self
            .slice
            .first_chunk::<2>()
            .expect("parsing bounds checking failed");
        u16::from_le_bytes(*major_version)
    }

    /// The minor version of the configuration format that this [`Configuration`] uses.
    pub fn minor_version(&self) -> u16 {
        let minor_version = self.slice[2..]
            .first_chunk::<2>()
            .expect("parsing bounds checking failed");
        u16::from_le_bytes(*minor_version)
    }

    /// Flags that affect the interpretation of the entire configuration.
    pub fn flags(&self) -> ConfigurationFlags {
        let flags = self.slice[mem::offset_of!(ConfigurationHeader, flags)..]
            .first_chunk::<4>()
            .expect("parsing bounds checking failed");
        ConfigurationFlags(u32::from_le_bytes(*flags))
    }

    /// The number of [`Entry`]s this [`Configuration`] contains.
    pub fn entry_count(&self) -> u64 {
        let entry_count = self.slice[mem::offset_of!(ConfigurationHeader, entry_count)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*entry_count)
    }

    /// An [`Iterator`] over the entries of this [`Configuration`].
    pub fn entries(&self) -> EntryIter<'slice> {
        EntryIter {
            slice: self.slice,
            next_offset: self.entry_offset(),
            remaining: self.entry_count(),
        }
    }

    fn entry_offset(&self) -> u64 {
        let entry_offset = self.slice[mem::offset_of!(ConfigurationHeader, entry_table_offset)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*entry_offset)
    }
}

/// Various errors that can occur while parsing a [`Configuration`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum ParseConfigurationError {
    /// The given slice is too small.
    SliceTooSmall,
    /// The configuration is of an unsupported configuration format version.
    UnsupportedVersion {
        /// The major version of the configuration format version.
        major_version: u16,
        /// The minor version of the configuration format version.
        minor_version: u16,
    },
}

/// An [`Iterator`] over the entries of a [`Configuration`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct EntryIter<'slice> {
    slice: &'slice [u8],
    next_offset: u64,
    remaining: u64,
}

impl<'slice> Iterator for EntryIter<'slice> {
    type Item = Entry<'slice>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }

        let next_offset = self.slice[self.next_offset as usize..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        let next_offset = self.next_offset + u64::from_le_bytes(*next_offset);

        let entry_slice = &self.slice[self.next_offset as usize..next_offset as usize];

        let entry_type = self.slice[self.next_offset as usize + 8..]
            .first_chunk::<4>()
            .expect("parsing bounds checking failed");
        let entry_type = EntryType(u32::from_le_bytes(*entry_type));
        let entry = match entry_type {
            EntryType::EMBEDDED => Entry::Embedded(EmbeddedEntry { slice: entry_slice }),
            EntryType::BOOT_FILESYSTEM => {
                Entry::BootFilesystem(BootFilesystemEntry { slice: entry_slice })
            }
            _ => Entry::Unknown {
                entry_type,
                slice: entry_slice,
            },
        };
        self.next_offset += next_offset;
        self.remaining -= 1;

        Some(entry)
    }
}

/// The different types of entries.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Entry<'slice> {
    /// An [`EmbeddedEntry`].
    Embedded(EmbeddedEntry<'slice>),
    /// A [`BootFilesystemEntry`].
    BootFilesystem(BootFilesystemEntry<'slice>),
    /// An unknown type of entry.
    Unknown {
        /// The type of the entry.
        entry_type: EntryType,
        /// The bytes between the start of this [`Entry`] and the start of the next [`Entry`].
        slice: &'slice [u8],
    },
}

/// An entry representing data embedded in capora-boot-stub.
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct EmbeddedEntry<'slice> {
    slice: &'slice [u8],
}

impl<'slice> EmbeddedEntry<'slice> {
    /// Parses the slice and returns a [`EmbeddedEntry`] if the `major_version` is supported.
    ///
    /// This supports parsing [`EmbeddedEntry`]s with a higher minor version than known, but cannot
    /// parse any unknown major verison.
    pub fn parse(slice: &'slice [u8], major_version: u16) -> Result<Self, ParseEmbeddedEntryError> {
        if major_version != MAJOR_VERSION {
            return Err(ParseEmbeddedEntryError::UnsupportedVersion { major_version });
        }

        if !(mem::size_of::<RawEmbeddedEntry>() <= slice.len()) {
            return Err(ParseEmbeddedEntryError::SliceTooSmall);
        }

        Ok(Self { slice })
    }

    /// Flags that affect the interpretation of [`EmbeddedEntry`] and its data.
    pub fn flags(&self) -> EmbeddedEntryFlags {
        let flags = self.slice[mem::offset_of!(RawEmbeddedEntry, flags)..]
            .first_chunk::<4>()
            .expect("parsing bounds checking failed");
        EmbeddedEntryFlags(u32::from_le_bytes(*flags))
    }

    /// The offset, in bytes, from the start of the configuration to the name of the entry.
    pub fn name_offset(&self) -> u64 {
        let name_offset = self.slice[mem::offset_of!(RawEmbeddedEntry, name_offset)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*name_offset)
    }

    /// The size, in bytes, of the name of this entry.
    pub fn name_length(&self) -> u64 {
        let name_length = self.slice[mem::offset_of!(RawEmbeddedEntry, name_length)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*name_length)
    }

    /// The offset, in bytes, from the start of the embedded data section to the data.
    pub fn data_offset(&self) -> u64 {
        let data_offset = self.slice[mem::offset_of!(RawEmbeddedEntry, data_offset)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*data_offset)
    }

    /// The size, in bytes, of the embedded data.
    pub fn data_length(&self) -> u64 {
        let data_length = self.slice[mem::offset_of!(RawEmbeddedEntry, data_length)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*data_length)
    }
}

impl fmt::Debug for EmbeddedEntry<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_struct = f.debug_struct("EmbeddedEntry");

        debug_struct.field("flags", &self.flags());
        debug_struct.field("name_offset", &self.name_offset());
        debug_struct.field("name_length", &self.name_length());
        debug_struct.field("data_offset", &self.data_offset());
        debug_struct.field("data_length", &self.data_length());

        debug_struct.finish()
    }
}

/// Various errors that can occur while parsing an [`EmbeddedEntry`].
pub enum ParseEmbeddedEntryError {
    /// The give slice is too small.
    SliceTooSmall,
    /// The [`EmbeddedEntry`] is of an unsupported version.
    UnsupportedVersion {
        /// The major version of the configuration format version.
        major_version: u16,
    },
}

/// An entry representing data to be loaded from the boot filesystem.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct BootFilesystemEntry<'slice> {
    slice: &'slice [u8],
}

impl<'slice> BootFilesystemEntry<'slice> {
    /// Parses the slice and returns a [`BootFilesystemEntry`] if the `major_version` is supported.
    ///
    /// This supports parsing [`BootFilesystemEntry`]s with a higher minor version than known, but cannot
    /// parse any unknown major verison.
    pub fn parse(
        slice: &'slice [u8],
        major_version: u16,
    ) -> Result<Self, ParseBootFilesystemEntryError> {
        if major_version != MAJOR_VERSION {
            return Err(ParseBootFilesystemEntryError::UnsupportedVersion { major_version });
        }

        if !(mem::size_of::<RawBootFilesystemEntry>() <= slice.len()) {
            return Err(ParseBootFilesystemEntryError::SliceTooSmall);
        }

        Ok(Self { slice })
    }

    /// Flags that affect the interpretation of [`BootFilesystemEntry`] and its data.
    pub fn flags(&self) -> BootFilesystemEntryFlags {
        let flags = self.slice[mem::offset_of!(RawBootFilesystemEntry, flags)..]
            .first_chunk::<4>()
            .expect("parsing bounds checking failed");
        BootFilesystemEntryFlags(u32::from_le_bytes(*flags))
    }

    /// The offset, in bytes, from the start of the configuration to the name of the entry.
    pub fn name_offset(&self) -> u64 {
        let name_offset = self.slice[mem::offset_of!(RawBootFilesystemEntry, name_offset)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*name_offset)
    }

    /// The size, in bytes, of the name of this entry.
    pub fn name_length(&self) -> u64 {
        let name_length = self.slice[mem::offset_of!(RawBootFilesystemEntry, name_length)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*name_length)
    }

    /// The offset, in bytes, from the start of the configuration to the path from which the data
    /// should be loaded.
    pub fn path_offset(&self) -> u64 {
        let path_offset = self.slice[mem::offset_of!(RawBootFilesystemEntry, path_offset)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*path_offset)
    }

    /// The size, in bytes, of the path.
    pub fn path_length(&self) -> u64 {
        let path_length = self.slice[mem::offset_of!(RawBootFilesystemEntry, path_size)..]
            .first_chunk::<8>()
            .expect("parsing bounds checking failed");
        u64::from_le_bytes(*path_length)
    }
}

/// Various errors that can occur while parsing an [`BootFilesystemEntry`].
pub enum ParseBootFilesystemEntryError {
    /// The give slice is too small.
    SliceTooSmall,
    /// The [`BootFilesystemEntry`] is of an unsupported version.
    UnsupportedVersion {
        /// The major version of the configuration format version.
        major_version: u16,
    },
}
