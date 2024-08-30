//! Definitions for capora-boot-stub's configuration format.

/// The major version of the configuration format.
pub const MAJOR_VERSION: u16 = 0;
/// The minor version of the configuration format.
pub const MINOR_VERSION: u16 = 0;

/// The name of the PE section storing the configuration of capora-boot-stub.
pub const SECTION_NAME: &str = ".options";
/// The name of the PE section storing embedded data for capora-boot-stub.
pub const EMBEDDED_SECTION_NAME: &str = ".embed";

/// The version header of the configuration for capora-boot-stub.
///
/// This header determines the version of the rest of the configuration. This is guaranteed to be
/// placed at the start of the configuration.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct VersionHeader {
    /// The major version of this configuration.
    pub major_version: u16,
    /// The minor version of this field
    pub minor_version: u16,
}

/// The header of the configuration for capora-boot-stub.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ConfigurationHeader {
    /// The version header of the configuration, which determines the layout and semantics of the
    /// rest of the configuration.
    pub version_header: VersionHeader,
    /// Flags that affect the entire configuration.
    pub flags: ConfigurationFlags,
    /// The number of entries the configuration contains.
    pub entry_count: u64,
    /// The offset of the start of the entries of this configuration.
    pub entry_table_offset: u64,
}

/// Flags that affect the entire configuration.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ConfigurationFlags(pub u32);

/// The header for all entries.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct EntryBase {
    /// The offset from this [`EntryBase`] to the next [`EntryBase`].
    pub entry_offset: u64,
    /// The type of the entry.
    pub entry_type: u32,
}

/// The type of an entry, which affects the size of the entry, and how the entry should be
/// interpreted.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct EntryType(pub u32);

impl EntryType {
    /// The entry is an [`EmbeddedEntry`].
    pub const EMBEDDED: Self = Self(0);
    /// The entry is a [`BootFilesystemEntry`].
    pub const BOOT_FILESYSTEM: Self = Self(1);
}

/// Entry representing data embedded in the executable.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct EmbeddedEntry {
    /// The offset from this [`EmbeddedEntry`] to the next entry.
    pub entry_offset: u64,
    /// The type of the entry.
    pub entry_type: u32,
    /// Flags that affect the interpretation of the [`EmbeddedEntry`].
    pub flags: EmbeddedEntryFlags,
    /// The offset, in bytes, from the start of the configuration to the name of this entry.
    pub name_offset: u64,
    /// The size, in bytes, of the name of this entry.
    pub name_length: u64,
    /// The offset, in bytes, from the start of the embedded section to the data.
    pub data_offset: u64,
    /// The size, in bytes, of the data.
    pub data_length: u64,
}

/// Flags that affect the associated [`EmbeddedEntry`].
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct EmbeddedEntryFlags(pub u32);

/// Entry representing data to be loaded from the boot filesystem.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct BootFilesystemEntry {
    /// The offset from this [`BootFilesystemEntry`] to the next entry.
    pub entry_offset: u64,
    /// The type of the entry.
    pub entry_type: u32,
    /// Flags that affect the interpretation of the [`BootFilesystemEntry`].
    pub flags: BootFilesystemEntryFlags,
    /// The offset, in bytes, from the start of the configuration to the name of this entry.
    pub name_offset: u64,
    /// The size, in bytes, of the name of this entry.
    pub name_length: u64,
    /// The offset, in bytes, from the start of the configuration to the path the data should be
    /// loaded from.
    pub path_offset: u64,
    /// The size, in bytes, of the path.
    pub path_size: u64,
}

/// Flags that affect the associated [`BootFilesystemEntry`].
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct BootFilesystemEntryFlags(pub u32);
