//! Definitions and interfaces to read a PE file.

use core::{error, fmt, mem};

/// A standard DOS file header.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct DosHeader {
    /// Magic number.
    pub magic_header: u16,
    /// The number of bytes on the last page of the file.
    pub last_page_byte_count: u16,
    /// The number of pages in the file.
    pub pages_in_file: u16,
    /// The number of relocations.
    pub relocations: u16,
    /// The size of the header in paragraphs.
    pub size_of_header_paragraphs: u16,
    /// The minimum extra paragraphs needed.
    pub minimum_extra_paragraphs: u16,
    /// The maximum extra paragraphs needed.
    pub maximum_extra_paragraphs: u16,
    /// The initial value in the SS register.
    pub initial_ss: u16,
    /// The initial value in the SP register.
    pub initial_sp: u16,
    /// The checksum.
    pub checksum: u16,
    /// The initial value in the IP register.
    pub initial_ip: u16,
    /// The initial value in the CS register.
    pub initial_cs: u16,
    /// The offset of the relocation table.
    pub relocation_table_offset: u16,
    /// The overlay number.
    pub overlay_number: u16,
    /// Reserved words.
    pub _reserved_1: [u16; 4],
    /// OEM identifier.
    pub oem_id: u16,
    /// Information dependent on the OEM.
    pub oem_information: u16,
    /// Reserved words.
    pub _reserved_2: [u16; 10],
    /// The offset of the COFF header.
    pub coff_file_header_offset: u32,
}

/// A standard COFF file header.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct CoffFileHeader {
    /// Magic number identifying this table as a COFF header.
    pub signature: u32,
    /// Number that identifies the type of the target machine.
    pub machine: u16,
    /// The number of sections, which indicates the size of the section
    /// table, which immediately follows the headers.
    pub number_of_sections: u16,
    /// The low 32 bits of the number of seconds since 00:00 January 1, 1970,
    /// which indicates when the file was created.
    pub time_data_stamp: u32,
    /// The file offset of the COFF symbol table, or zero if not present.
    ///
    /// This value should be zero for an image since COFF debugging information
    /// is deprecated.
    pub pointer_to_symbol_table: u32,
    /// The number of entries in the symbol table.
    pub number_of_symbols: u32,
    /// The size of the optional header, which is required for executable files
    /// but not for object files.
    pub size_of_optional_header: u16,
    /// The flags that indicate the attributes of the file.
    pub characteristics: u16,
}

/// Descriptor that identifies various items that are relevant to loading and
/// manipulating the associated section.
#[repr(C)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SectionHeader {
    /// The 8-byte, null-padded utf8 encoded string.
    ///
    /// Executable images do not support section section names
    /// longer than 8 characters.
    pub name: [u8; 8],
    /// The total size of the section when loaded into memory.
    ///
    /// If this value is greater than [`SectionHeader::size_of_raw_data`],
    /// the sectio is zero-padded.
    pub virtual_size: u32,
    /// For executable images, the address of the first byte of the section
    /// relative to the image base when the section is first loaded into memory.
    pub virtual_address: u32,
    /// The size of the section or initialized data.
    pub size_of_raw_data: u32,
    /// The file pointer to the first page of the section within the COFF file.
    pub pointer_to_raw_data: u32,
    /// The file pointer to the beginning of the relocation entires for the section.
    pub pointer_to_relocation: u32,
    /// The file pointer to the beginning of line-number entries for the section.
    pub pointer_to_line_numbers: u32,
    /// The number of relocation entries for the section.
    pub number_of_relocations: u16,
    /// The number of line-number entries for the section.
    pub number_of_line_numbers: u16,
    /// The flags that describe the characteristics of the section.
    pub characteristics: u32,
}

/// Abstraction over the table of [`SectionHeader`]'s found in a PE file.
///
/// # Safety
/// This only contains the slice of the section header table.
pub struct SectionHeaderTable<'slice> {
    slice: &'slice [u8],
    section_header_count: usize,
}

impl<'slice> SectionHeaderTable<'slice> {
    /// Returns the [`SectionHeader`] at `index`, or [`None`] if out of bounds.
    pub fn get(&self, index: usize) -> Option<SectionHeader> {
        if !((index as usize) < self.section_header_count) {
            return None;
        }

        let offset = index * mem::size_of::<SectionHeader>();
        let section_header = unsafe {
            self.slice[offset..]
                .as_ptr()
                .cast::<SectionHeader>()
                .read_unaligned()
        };

        Some(section_header)
    }

    /// Iterates over all [`SectionHeader`]s in the table, returning the [`SectionHeader`] with the
    /// given `name`.
    pub fn find_section(&self, name: &str) -> Option<SectionHeader> {
        for index in 0..self.section_header_count {
            let section_header = self.get(index)?;
            let section_name_length = section_header
                .name
                .iter()
                .position(|byte| 0u8.eq(byte))
                .unwrap_or(8);

            if name.as_bytes() == &section_header.name[..section_name_length] {
                return Some(section_header);
            }
        }

        None
    }
}

/// Locates the [`SectionHeaderTable`] inside the given `file`.
pub fn section_header_table<'slice>(
    file: &'slice [u8],
) -> Result<SectionHeaderTable<'slice>, LocateSectionHeaderTableError> {
    let coff_header_offset = file[mem::offset_of!(DosHeader, coff_file_header_offset)..]
        .first_chunk::<4>()
        .copied()
        .ok_or(LocateSectionHeaderTableError::FileTooSmall)?;
    let coff_header_offset = u32::from_ne_bytes(coff_header_offset);

    if !coff_header_offset
        .checked_add(mem::size_of::<CoffFileHeader>() as u32)
        .is_some_and(|max_offset| max_offset as usize <= file.len())
    {
        return Err(LocateSectionHeaderTableError::MalformedDosHeader {
            coff_header_offset,
            file_size: file.len(),
        });
    }

    let section_header_count = file
        [coff_header_offset as usize + mem::offset_of!(CoffFileHeader, number_of_sections)..]
        .first_chunk::<2>()
        .copied()
        .expect("bounds checking failed");
    let section_header_count = u16::from_ne_bytes(section_header_count);

    let optional_header_size = file
        [coff_header_offset as usize + mem::offset_of!(CoffFileHeader, size_of_optional_header)..]
        .first_chunk::<2>()
        .copied()
        .expect("bounds checking failed");
    let optional_header_size = u16::from_ne_bytes(optional_header_size);

    // Guaranteed not to overflow.
    let section_header_table_offset = (coff_header_offset as u64)
        + mem::size_of::<CoffFileHeader>() as u64
        + optional_header_size as u64;

    // Guaranteed not to overflow.
    let section_header_table_size =
        section_header_count as u64 * mem::size_of::<SectionHeader>() as u64;

    // Guaranteed not to overflow.
    let max_offset = section_header_table_offset + section_header_table_size;
    if !(max_offset <= file.len().try_into().unwrap()) {
        return Err(LocateSectionHeaderTableError::MalformedCoffHeader {
            section_header_table_offset,
            section_header_table_size,
            file_size: file.len(),
        });
    }

    Ok(SectionHeaderTable {
        slice: &file[section_header_table_offset as usize..max_offset as usize],
        section_header_count: section_header_count as usize,
    })
}

/// Various errors that can occur while locating a [`SectionHeaderTable`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum LocateSectionHeaderTableError {
    /// The file cannot fit a [`DosHeader`] inside of it.
    FileTooSmall,
    /// The [`DosHeader`] points to a [`CoffFileHeader`] that isn't in the file.
    MalformedDosHeader {
        /// The offset that the [`DosHeader`] specified.
        coff_header_offset: u32,
        /// The size of the file.
        file_size: usize,
    },
    ///
    MalformedCoffHeader {
        /// The offset of the [`SectionHeaderTable`].
        section_header_table_offset: u64,
        /// The total size, in bytes, of the [`SectionHeaderTable`].
        section_header_table_size: u64,
        /// The size of the file.
        file_size: usize,
    },
}

impl fmt::Display for LocateSectionHeaderTableError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FileTooSmall => f.write_str("file too small to fit DOS header"),
            Self::MalformedDosHeader {
                coff_header_offset,
                file_size,
            } => writeln!(
                f,
                "offset of {coff_header_offset} bytes given by DOS header points outside of \
                file of {file_size} bytes"
            ),
            Self::MalformedCoffHeader {
                section_header_table_offset,
                section_header_table_size,
                file_size,
            } => writeln!(
                f,
                "offset of {section_header_table_offset} bytes given by COFF header points \
                to a region of {section_header_table_size} bytes that does not fit in size the \
                file of {file_size} bytes"
            ),
        }
    }
}

impl error::Error for LocateSectionHeaderTableError {}
