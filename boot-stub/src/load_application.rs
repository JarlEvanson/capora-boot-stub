//! Functionality that deals with loading an ELF file.

use core::{
    fmt::{self, Write},
    mem::{self, MaybeUninit},
};

use boot_api::BootloaderRequest;
use elf::{
    class::Class64,
    encoding::LittleEndian,
    raw::{
        elf_header::ElfType,
        elf_program_header::{SegmentFlags, SegmentType},
    },
    ParseElfFileError,
};
use uefi::system::with_stdout;

use crate::{
    mapper::{ApplicationMemoryMap, PageRange, Protection},
    APPLICATION_REGION_SIZE, BASE_RETRY_COUNT, MINIMUM_APPLICATION_BASE,
};

/// Loads the application.
pub fn load_application(
    application_map: &mut ApplicationMemoryMap,
    slice: &[u8],
) -> Result<(u64, u64), LoadApplicationError> {
    let elf = elf::ElfFile::<Class64, LittleEndian>::parse(slice)?;
    let program_header_table = elf
        .program_header_table()
        .ok_or(LoadApplicationError::MissingHeaderTable)?;

    let slide = match elf.header().elf_type() {
        ElfType::EXECUTABLE => 0,
        ElfType::SHARED => {
            let load_headers = program_header_table
                .iter()
                .filter(|header| header.segment_type() == SegmentType::LOAD);

            let mut max_address = 0;
            let mut min_address = 0;
            let mut max_alignment = 0;
            for header in load_headers {
                min_address = min_address.min(header.virtual_address());
                max_address = max_address.max(header.virtual_address() + header.memory_size());
                max_alignment = max_alignment.max(header.alignment());
            }

            let size = max_address - min_address;
            if size > APPLICATION_REGION_SIZE {
                return Err(LoadApplicationError::ApplicationTooLarge);
            }

            let mut slide;
            'slide_block: {
                for _ in 0..BASE_RETRY_COUNT {
                    slide = rand_u64().ok_or(LoadApplicationError::RngFailure)?
                        & !MINIMUM_APPLICATION_BASE;
                    if slide.checked_add(size).is_some() {
                        break 'slide_block;
                    }
                }
                let _ = with_stdout(|stdout| writeln!(stdout, "Warning: ASLR failed"));
                slide = 0;
            }
            slide = (slide / max_alignment) * max_alignment;

            slide += MINIMUM_APPLICATION_BASE - min_address;
            slide - slide % 4096
        }
        elf_type => return Err(LoadApplicationError::UnsupportedElfType(elf_type)),
    };
    let _ = with_stdout(|stdout| writeln!(stdout, "Application Load Address: {slide:X}"));

    let mut request_found = false;
    for header in program_header_table.iter() {
        const BOOTLOADER_REQUEST_SEGMENT_TYPE: SegmentType =
            SegmentType(boot_api::BOOTLOADER_REQUEST_ELF_SEGMENT);
        match header.segment_type() {
            SegmentType::LOAD => {
                let page_containing = header.virtual_address() / 4096;
                let page_containing_end =
                    (header.virtual_address() + header.memory_size()).div_ceil(4096);
                let page_count = page_containing_end - page_containing;
                let adjusted_page = (slide / 4096 + page_containing) & PageRange::PAGE_MASK;
                let page_range =
                    PageRange::new(adjusted_page, page_count).expect("bounds checking failed");
                let protection = if header.flags().0 & SegmentFlags::WRITE.0
                    == SegmentFlags::WRITE.0
                {
                    Protection::Writable
                } else if header.flags().0 & SegmentFlags::EXECUTE.0 == SegmentFlags::EXECUTE.0 {
                    Protection::Executable
                } else {
                    Protection::Readable
                };

                let entry = application_map
                    .allocate_at(page_range, protection)
                    .ok_or(LoadApplicationError::OverlappingLoadSegments)?;

                MaybeUninit::copy_from_slice(
                    &mut entry.as_bytes_mut()[..header.file_size() as usize],
                    &slice[header.file_offset() as usize
                        ..(header.file_offset() + header.file_size()) as usize],
                );
                if header.file_size() != header.memory_size() {
                    MaybeUninit::fill(&mut entry.as_bytes_mut()[header.file_size() as usize..], 0);
                }
            }
            BOOTLOADER_REQUEST_SEGMENT_TYPE => {
                let request = &slice[header.file_offset() as usize
                    ..(header.file_offset() + header.file_size()) as usize];
                if !(request.len() <= mem::size_of::<BootloaderRequest>()) {
                    return Err(LoadApplicationError::UnuspportedApplicationRequest);
                }

                let mut array_chunks = request.array_chunks::<{ mem::size_of::<u64>() }>();
                if !((&mut array_chunks)
                    .take(3)
                    .zip(boot_api::SIGNATURE)
                    .all(|(a, b)| *a == b.to_ne_bytes()))
                {
                    return Err(LoadApplicationError::UnuspportedApplicationRequest);
                }
                if !(*array_chunks.next().unwrap() == boot_api::API_VERSION.to_ne_bytes()) {
                    return Err(LoadApplicationError::UnuspportedApplicationRequest);
                }
                request_found = true;
            }
            SegmentType::NULL
            | SegmentType::DYNAMIC
            | SegmentType::INTERP
            | SegmentType::NOTE
            | SegmentType::TLS
            | SegmentType::PHDR => {}
            segment_type => {
                let _ = with_stdout(|stdout| {
                    writeln!(
                        stdout,
                        "Warning: unrecognized segment type: {segment_type:?}"
                    )
                });
            }
        }
    }
    if !request_found {
        return Err(LoadApplicationError::UnuspportedApplicationRequest);
    }

    for header in program_header_table
        .iter()
        .filter(|header| header.segment_type() == SegmentType::DYNAMIC)
    {
        use elf::raw::elf_dynamic::{Elf64Dynamic, Elf64DynamicTag, ElfDynamicTag};

        let data = header.segment_data(elf).unwrap();

        let mut rela = None;
        let mut rela_size = None;
        let mut rela_ent = None;

        for index in 0..(data.len() / mem::size_of::<Elf64Dynamic>()) {
            let dynamic = &data[index * mem::size_of::<Elf64Dynamic>()..];
            let tag = Elf64DynamicTag(i64::from_le_bytes(*dynamic.first_chunk::<8>().unwrap()));
            let tag = tag.into();
            let value = u64::from_le_bytes(*dynamic[8..].first_chunk::<8>().unwrap());

            match tag {
                ElfDynamicTag::RELA_TABLE => rela.replace(value),
                ElfDynamicTag::RELA_SIZE => rela_size.replace(value),
                ElfDynamicTag::RELA_ENTRY_SIZE => rela_ent.replace(value),
                ElfDynamicTag::NULL => break,
                _ => Some(0),
            };
        }

        let Some(rela_offset) = rela else {
            continue;
        };
        let rela_size = rela_size.ok_or(LoadApplicationError::MissingRelaSize)?;
        let rela_entry_size = rela_ent.ok_or(LoadApplicationError::MissingRelaEntrySize)?;

        let num_entries = rela_size / rela_entry_size;
        for index in 0..num_entries {
            let memory_range = application_map
                .lookup(slide + rela_offset)
                .expect("rela table not loaded");
            let loaded_range =
                unsafe { MaybeUninit::slice_assume_init_ref(memory_range.as_bytes()) };

            let range_offset = (slide + rela_offset) - memory_range.page_range().virtual_address();
            let rela_slice = &loaded_range[range_offset as usize..];
            let rela = &rela_slice[(index * rela_entry_size) as usize..];
            let offset = u64::from_le_bytes(*rela.first_chunk::<8>().unwrap());
            let info = u64::from_le_bytes(*rela[8..].first_chunk::<8>().unwrap());
            let addend = i64::from_le_bytes(*rela[16..].first_chunk::<8>().unwrap());

            let rela_type = info & 0xFFFF_FFFF;
            match rela_type {
                8 => {
                    let value = slide.checked_add_signed(addend).unwrap();
                    let address = slide.checked_add(offset).unwrap();

                    let memory_range = application_map.lookup_mut(address).unwrap();
                    let frame_range_offset = address - memory_range.page_range().virtual_address();
                    assert!(frame_range_offset + 8 <= memory_range.size() * 4096);

                    let relocation_place = &mut memory_range.as_bytes_mut()
                        [frame_range_offset as usize..(frame_range_offset + 8) as usize];

                    MaybeUninit::copy_from_slice(relocation_place, &value.to_ne_bytes());
                }
                relocation_type => {
                    return Err(LoadApplicationError::UnsupportedRelocationType(
                        relocation_type,
                    ));
                }
            }
        }
    }

    Ok((slide, slide + elf.header().entry()))
}

/// Various errors that can occur while loading an application.
pub enum LoadApplicationError {
    /// An error occurred while parsing the application.
    ElfError(ParseElfFileError),
    /// The application is missing a header table.
    MissingHeaderTable,
    /// The application is too large when loaded.
    ApplicationTooLarge,
    /// The ELF application is of an unsuspported type.
    UnsupportedElfType(ElfType),
    /// The rng failed.
    RngFailure,
    /// [`SegmentType::LOAD`] virtual ranges overlap.
    OverlappingLoadSegments,
    /// The application is missing a boot request.
    UnuspportedApplicationRequest,
    /// A relocation table is present, but the size of the relocation cannot be determined.
    MissingRelaSize,
    /// A relocation table is present, but the size of a relocation entry cannot be determined.
    MissingRelaEntrySize,
    /// An unsupported relocation type is present.
    UnsupportedRelocationType(u64),
}

impl From<ParseElfFileError> for LoadApplicationError {
    fn from(value: ParseElfFileError) -> Self {
        Self::ElfError(value)
    }
}

impl fmt::Display for LoadApplicationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ElfError(error) => write!(f, "error ocurred when parsing elf file: {error:?}"),
            Self::MissingHeaderTable => write!(f, "missing elf program header table"),
            Self::ApplicationTooLarge => write!(f, "application is too large when loaded"),
            Self::UnsupportedElfType(elf_type) => {
                write!(f, "elf is of unsupported type {elf_type:?}")
            }
            Self::RngFailure => write!(f, "rng failed"),
            Self::OverlappingLoadSegments => write!(f, "overlapping load segments"),
            Self::UnuspportedApplicationRequest => {
                write!(f, "missing or unsupported application boot request")
            }
            Self::MissingRelaSize => write!(f, "missing dynamic rela size tag"),
            Self::MissingRelaEntrySize => write!(f, "missing dynamic rela entry size tag"),
            Self::UnsupportedRelocationType(rel_type) => {
                write!(f, "unsupported relocation entry type: {rel_type:?}")
            }
        }
    }
}

/// Returns a random [`u64`], returning [`None`] if the attempt fails.
pub fn rand_u64() -> Option<u64> {
    let rdrand_supported_bit = unsafe { core::arch::x86_64::__cpuid(7).ebx };
    if !(rdrand_supported_bit & (1 << 18) == (1 << 18)) {
        return None;
    }

    let value;
    unsafe {
        core::arch::asm!(
            "9:",
            "rdseed {}",
            "jnc 9b",
            out(reg) value,
        )
    };

    Some(value)
}
