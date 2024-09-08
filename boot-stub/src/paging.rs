//! Functionality that deals with paging and virtual memory.

use core::mem::{self, MaybeUninit};

use crate::mapper::ApplicationMemoryMap;

/// Creates and allocates a full page table layout for the provided [`ApplicationMemoryMap`], returning
/// the physical memory address to be loaded into the address space specifier.
pub fn map_app(map: ApplicationMemoryMap) -> u64 {
    let mut pml4e_index = 512;
    let mut pml3e_index = 512;
    let mut pml2e_index = 512;

    let mut page_count = 1;

    for entry in map.as_slice().iter().filter(|entry| entry.present()) {
        let mut page = entry.page();
        while entry.page_range().contains(page) {
            let pml4e = get_pml4e_page_index(page);
            if pml4e != pml4e_index {
                pml4e_index = pml4e;
                page_count += 1;

                pml3e_index = 512;
                pml2e_index = 512;
            }
            let pml3e = get_pml3e_page_index(page);
            if pml3e != pml3e_index {
                pml3e_index = pml3e;
                page_count += 1;

                pml2e_index = 512;
            }
            let pml2e = get_pml2e_page_index(page);
            if pml2e != pml2e_index {
                pml2e_index = pml2e;
                page_count += 1;
            }

            page = page.wrapping_add(1);
        }
    }

    let pages = uefi::boot::allocate_pages(
        uefi::boot::AllocateType::AnyPages,
        uefi::boot::MemoryType::LOADER_DATA,
        page_count,
    )
    .expect("allocation failed");

    let pages = unsafe {
        core::slice::from_raw_parts_mut(
            pages.as_ptr().cast::<MaybeUninit<u64>>(),
            page_count * (4096 / mem::size_of::<u64>()),
        )
    };
    let pages = MaybeUninit::fill(pages, 0);

    let mut page_tables = pages.array_chunks_mut::<512>();

    let pml4e_table = page_tables.next().unwrap();
    pml4e_index = 512;
    let mut pml3e_table = &mut [0; 512];
    pml3e_index = 512;
    let mut pml2e_table = &mut [0; 512];
    pml2e_index = 512;
    let mut pml1e_table = &mut [0; 512];

    for entry in map.as_slice().iter().filter(|entry| entry.present()) {
        let mut page = entry.page();
        let mut frame = entry.frame();

        while entry.page_range().contains(page) {
            let pml4e = get_pml4e_page_index(page);
            if pml4e != pml4e_index {
                pml3e_table = page_tables.next().unwrap();
                pml4e_index = pml4e;

                pml4e_table[pml4e_index as usize] = 1 | (1 << 1) | (pml3e_table.as_ptr() as u64);

                pml3e_index = 512;
                pml2e_index = 512
            }
            let pml3e = get_pml3e_page_index(page);
            if pml3e != pml3e_index {
                pml2e_table = page_tables.next().unwrap();
                pml3e_index = pml3e;

                pml3e_table[pml3e_index as usize] = 1 | (1 << 1) | (pml2e_table.as_ptr() as u64);

                pml2e_index = 512;
            }
            let pml2e = get_pml2e_page_index(page);
            if pml2e != pml2e_index {
                pml1e_table = page_tables.next().unwrap();
                pml2e_index = pml2e;

                pml2e_table[pml2e_index as usize] = 1 | (1 << 1) | (pml1e_table.as_ptr() as u64);
            }

            pml1e_table[get_pml1e_page_index(page) as usize] = 1
                | ((entry.writable() as u64) << 1)
                | (frame << 12)
                | ((!entry.executable() as u64) << 63);

            page = page.wrapping_add(1);
            frame = frame.wrapping_add(1);
        }
    }

    pml4e_table.as_ptr() as u64
}

fn get_pml1e_page_index(page: u64) -> u16 {
    (page & 0x1FF) as u16
}

fn get_pml2e_page_index(page: u64) -> u16 {
    ((page >> 9) & 0x1FF) as u16
}

fn get_pml3e_page_index(page: u64) -> u16 {
    ((page >> 18) & 0x1FF) as u16
}

fn get_pml4e_page_index(page: u64) -> u16 {
    ((page >> 27) & 0x1FF) as u16
}

/// Gets the index into the page map level 1 table.
pub fn get_pml1e_index(address: u64) -> u16 {
    ((address >> 12) & 0x1FF) as u16
}

/// Gets the index into the page map level 2 table.
pub fn get_pml2e_index(address: u64) -> u16 {
    ((address >> 21) & 0x1FF) as u16
}

/// Gets the index into the page map level 3 table.
pub fn get_pml3e_index(address: u64) -> u16 {
    ((address >> 30) & 0x1FF) as u16
}

/// Gets the index into the page map level 4 table.
pub fn get_pml4e_index(address: u64) -> u16 {
    ((address >> 39) & 0x1FF) as u16
}
