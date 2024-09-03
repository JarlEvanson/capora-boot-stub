//! Functionality that deals with loading an ELF file.

use core::{
    fmt::{self, Write},
    mem,
};

use elf::{
    class::Class64,
    encoding::LittleEndian,
    raw::{
        elf_header::ElfType,
        elf_program_header::{SegmentFlags, SegmentType},
    },
    ParseElfFileError,
};
use uefi::{boot, system::with_stdout};

use crate::{
    mapper::{FrameRange, PageRange, VirtualMemoryMap, VirtualMemoryMapEntry},
    APPLICATION_REGION_SIZE, BASE_RETRY_COUNT, MINIMUM_APPLICATION_BASE,
};

/// Loads the application.
pub fn load_application(
    virtual_map: &mut VirtualMemoryMap,
    slice: &[u8],
) -> Result<u64, LoadApplicationError> {
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

    for header in program_header_table.iter() {
        match header.segment_type() {
            SegmentType::LOAD => {
                let page_containing = header.virtual_address() / 4096;
                let page_containing_end =
                    (header.virtual_address() + header.memory_size()).div_ceil(4096);
                let page_count = page_containing_end - page_containing;
                let adjusted_page = (slide / 4096 + page_containing) & PageRange::PAGE_MASK;
                let page_range =
                    PageRange::new(adjusted_page, page_count).expect("bounds checking failed");

                let frames = boot::allocate_pages(
                    boot::AllocateType::AnyPages,
                    boot::MemoryType::LOADER_DATA,
                    page_range.size() as usize,
                )
                .expect("allocation failed");

                let frame_range = FrameRange::new(frames.as_ptr() as u64 / 4096, page_range.size())
                    .expect("bounds checking failed");

                virtual_map
                    .insert(
                        VirtualMemoryMapEntry::new(
                            page_range,
                            frame_range,
                            header.flags().0 & SegmentFlags::WRITE.0 == SegmentFlags::WRITE.0,
                            header.flags().0 & SegmentFlags::EXECUTE.0 == SegmentFlags::EXECUTE.0,
                        )
                        .expect("mismatched page and frame range sizes"),
                    )
                    .ok_or(LoadApplicationError::OverlappingLoadSegments)?;
                let _ = with_stdout(|stdout| {
                    writeln!(
                        stdout,
                        "Segment loaded for {:X} at frame {:X}",
                        page_range.page(),
                        frame_range.frame(),
                    )
                });

                unsafe { core::ptr::write_bytes(frames.as_ptr(), 0, page_count as usize * 4096) }

                let segment_slice = unsafe {
                    core::slice::from_raw_parts_mut(frames.as_ptr(), page_count as usize * 4096)
                };

                let copy_from = &slice[header.file_offset() as usize
                    ..(header.file_offset() + header.file_size()) as usize];
                let copy_to = &mut segment_slice[header.virtual_address() as usize % 4096
                    ..(header.virtual_address() % 4096 + header.file_size()) as usize];

                copy_to.copy_from_slice(copy_from);
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
            let rela = &slice[(rela_offset + index * rela_entry_size) as usize..];
            let offset = u64::from_le_bytes(*rela.first_chunk::<8>().unwrap());
            let info = u64::from_le_bytes(*rela[8..].first_chunk::<8>().unwrap());
            let addend = i64::from_le_bytes(*rela[16..].first_chunk::<8>().unwrap());

            let rela_type = info & 0xFFFF_FFFF;

            match rela_type {
                8 => {
                    let value = slide.checked_add_signed(addend).unwrap();
                    let address = slide.checked_add(offset).unwrap();

                    let memory_range = virtual_map.lookup(address).unwrap();
                    let frame_range_offset = address - memory_range.page_range().virtual_address();
                    assert!(frame_range_offset + 8 <= memory_range.size() * 4096);

                    let address = (memory_range.frame_range().frame() << 12) + frame_range_offset;
                    unsafe { (address as *mut u64).write_unaligned(value) }
                }
                relocation_type => {
                    return Err(LoadApplicationError::UnsupportedRelocationType(
                        relocation_type,
                    ))
                }
            }
        }
    }

    Ok(slide + elf.header().entry())
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
    let success: u8;
    unsafe {
        core::arch::asm!(
            "mov eax, 7",
            "mov ecx, 0",
            "cpuid",
            "bt ebx, 18",
            "setc {}",
            out(reg_byte) success,
        )
    };
    if success == 0 {
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
