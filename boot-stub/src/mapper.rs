//! Functionality that deals with virtual memory.

use core::{
    fmt,
    mem::{self, MaybeUninit},
    ptr::{self, NonNull},
};

use uefi::boot;

use crate::load_application::rand_u64;

/// A map of the virtual memory space into which the application will be loaded.
pub struct ApplicationMemoryMap {
    ptr: *mut Entry,
    capacity: usize,
    length: usize,
}

impl ApplicationMemoryMap {
    /// Creates a new [`ApplicationMemoryMap`].
    pub fn new() -> Self {
        Self {
            ptr: ptr::null_mut(),
            capacity: 0,
            length: 0,
        }
    }

    /// Generates a random page to be the base of the region in virtual memory, marks it to be
    /// mapped according to `protection`, and allocates physical memory to back the allocation.
    pub fn allocate(&mut self, page_count: u64, protection: Protection) -> &mut Entry {
        let virtual_address = loop {
            let page = (rand_u64().expect("random number generator failed") & PageRange::PAGE_MASK)
                | PageRange::EXTENDED_BIT_PAGE; // Enforce supervisor mode memory region.

            let Some(pages) = PageRange::new(page, page_count) else {
                continue;
            };

            match self.allocate_at(pages, protection) {
                Some(entry) => break entry.page_range().virtual_address(),
                None => continue,
            }
        };

        self.lookup_mut(virtual_address).unwrap()
    }

    /// Marks the region at `pages` to be mapped according to `protection`, and allocates physical
    /// memory to back the page region.
    pub fn allocate_at(&mut self, pages: PageRange, protection: Protection) -> Option<&mut Entry> {
        if protection == Protection::NotPresent {
            unsafe {
                return self.add_region(
                    pages,
                    FrameRange::new(1, pages.size()).unwrap(),
                    protection,
                );
            };
        }

        let mem_type = if protection == Protection::Executable {
            boot::MemoryType::LOADER_CODE
        } else {
            boot::MemoryType::LOADER_DATA
        };

        let frames = boot::allocate_pages(
            boot::AllocateType::AnyPages,
            mem_type,
            pages.size() as usize,
        )
        .expect("memory region allocation failed");
        let frames = FrameRange::new((frames.as_ptr() as u64) >> 12, pages.size())
            .expect("memory allocation function failed");

        unsafe { self.add_region(pages, frames, protection) }
    }

    /// Adds the virtual region `pages` backed by the physical region `frames`.
    ///
    /// # Safety
    /// Access to the region of `frames`, which exists as an identity mapped region, must be
    /// controlled completely by this `self`.
    ///
    /// It must be safe to read and write to the region given by `frames`.
    pub unsafe fn add_region(
        &mut self,
        pages: PageRange,
        frames: FrameRange,
        protection: Protection,
    ) -> Option<&mut Entry> {
        let index = self.check_add_reqs(pages, frames)?;
        let mut entry = Entry::new(pages.page(), frames.frame(), pages.size(), protection);

        if self.length == self.capacity {
            self.grow();
        }

        unsafe {
            core::ptr::copy(
                self.as_slice()[index..].as_ptr(),
                self.as_slice()[index + 1..].as_ptr().cast_mut(),
                self.length - index,
            )
        }

        self.length += 1;
        core::mem::swap(&mut self.as_slice_mut()[index], &mut entry);
        mem::forget(entry);

        self.as_slice_mut().get_mut(index)
    }

    fn check_add_reqs(&self, pages: PageRange, frames: FrameRange) -> Option<usize> {
        if pages.size() != frames.size() || frames.frame() == 0 {
            return None;
        }

        let Err(index) = self
            .as_slice()
            .binary_search_by_key(&pages.page(), |entry| entry.page())
        else {
            return None;
        };

        let overlaps_lower = self
            .as_slice()
            .get(index)
            .is_some_and(|entry| entry.page_range().overlaps(pages));
        let overlaps_higher = self
            .as_slice()
            .get(index + 1)
            .is_some_and(|entry| entry.page_range().overlaps(pages));

        if overlaps_lower || overlaps_higher {
            return None;
        }

        Some(index)
    }

    /// Searches the [`ApplicationMemoryMap`] for the [`Entry`] that contains `virtual_address` and
    /// returns a reference to it.
    pub fn lookup(&self, virtual_address: u64) -> Option<&Entry> {
        self.as_slice()
            .iter()
            .filter(|entry| entry.page_range().contains(virtual_address >> 12))
            .next()
    }

    /// Searches the [`ApplicationMemoryMap`] for the [`Entry`] that contains `virtual_address` and
    /// returns a mutable reference to it.
    pub fn lookup_mut(&mut self, virtual_address: u64) -> Option<&mut Entry> {
        self.as_slice_mut()
            .iter_mut()
            .filter(|entry| entry.page_range().contains(virtual_address >> 12))
            .next()
    }

    /// Returns an immutable slice over the [`Entry`] that make up the [`ApplicationMemoryMap`].
    pub fn as_slice(&self) -> &[Entry] {
        if self.length == 0 {
            return &[];
        }

        unsafe { core::slice::from_raw_parts(self.ptr, self.length) }
    }

    fn as_slice_mut(&self) -> &mut [Entry] {
        if self.length == 0 {
            return &mut [];
        }

        unsafe { core::slice::from_raw_parts_mut(self.ptr, self.length) }
    }

    fn grow(&mut self) {
        let new_capacity = if self.capacity == 0 {
            4
        } else {
            self.capacity * 2
        };

        let allocation = boot::allocate_pool(
            boot::MemoryType::LOADER_DATA,
            new_capacity * mem::size_of::<Entry>(),
        )
        .expect("memory map allocation failed");
        let new_ptr = allocation.as_ptr().cast::<Entry>();

        if let Some(old_ptr) = NonNull::new(self.ptr) {
            unsafe { core::ptr::copy_nonoverlapping(self.ptr, new_ptr, self.length) };

            unsafe { boot::free_pool(old_ptr.cast::<u8>()).expect("deallocation failed") }
        }

        self.ptr = new_ptr;
        self.capacity = new_capacity;
    }
}

/// A virtual memory region and its flags.
#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Entry {
    page: u64,
    frame: u64,
    size: u64,
}

impl Entry {
    fn new(page: u64, frame: u64, size: u64, flags: Protection) -> Self {
        Self {
            page,
            frame,
            size: size | ((flags as u64) << 62),
        }
    }

    /// The virtual memory region this [`Entry`] represents.
    pub fn page_range(&self) -> PageRange {
        PageRange {
            page_number: self.page(),
            size: self.size(),
        }
    }

    /// The backing physical memory range.
    pub fn frame_range(&self) -> FrameRange {
        FrameRange {
            frame_number: self.frame(),
            size: self.size(),
        }
    }

    /// The starting page of the virtual memory region.
    pub fn page(&self) -> u64 {
        self.page
    }

    /// The starting frame of the backing physical memory.
    pub fn frame(&self) -> u64 {
        self.frame
    }

    /// The number of pages this region covers.
    pub fn size(&self) -> u64 {
        self.size & 0x3FFF_FFFF_FFFF_FFFF
    }

    /// The settings on the virtual memory region when the application is loaded.
    pub fn protection(&self) -> Protection {
        match self.size >> 62 {
            0 => Protection::NotPresent,
            1 => Protection::Readable,
            2 => Protection::Writable,
            3 => Protection::Executable,
            _ => unreachable!(),
        }
    }

    /// Returns an immutable reference to the bytes making the physical page backing the memory.
    ///
    /// When the [`Protection`] setting is [`Protection::NotPresent`], then this returns a
    /// zero-sized slice.
    pub fn as_bytes(&self) -> &[u8] {
        if self.protection() == Protection::NotPresent {
            return &[];
        }

        unsafe {
            core::slice::from_raw_parts(
                (self.frame() << 12) as *const u8,
                self.size() as usize * 4096,
            )
        }
    }

    /// Returns a mutable reference to the bytes making the physical page backing the memory.
    ///
    /// When the [`Protection`] setting is [`Protection::NotPresent`], then this returns a
    /// zero-sized slice.
    pub fn as_bytes_mut(&mut self) -> &mut [u8] {
        if self.protection() == Protection::NotPresent {
            return &mut [];
        }

        unsafe {
            core::slice::from_raw_parts_mut(
                (self.frame() << 12) as *mut u8,
                self.size() as usize * 4096,
            )
        }
    }
}

/// The protection settings on the application virtual memory map.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Protection {
    /// The virtual memory region should not be occupied.
    NotPresent = 0,
    /// The virtual memory region should be readable.
    Readable = 1,
    /// The virtual memory region should be readable and writable.
    Writable = 2,
    /// The virtual memory region should be readable and executable.
    Executable = 3,
}

/// An ordered collection of non-overlapping virtual memory ranges and underlying physical ranges.
pub struct VirtualMemoryMap {
    map_ptr: *mut VirtualMemoryMapEntry,
    map_length: usize,
    map_capacity: usize,
}

impl VirtualMemoryMap {
    /// Creates a new [`VirtualMemoryMap`] with 0 capacity.
    pub fn new() -> Self {
        Self {
            map_ptr: ptr::null_mut(),
            map_length: 0,
            map_capacity: 0,
        }
    }

    /// Inserts `entry` into `self`, checking that `entry` doesn't overlap with an already existing
    /// [`VirtualMemoryMapEntry`].
    pub fn insert(&mut self, entry: VirtualMemoryMapEntry) -> Option<()> {
        let Err(index) = self
            .as_slice()
            .binary_search_by_key(&entry.page(), |a| a.page())
        else {
            return None;
        };

        if self
            .as_slice()
            .get(index)
            .is_some_and(|val| entry.virt_overlaps(*val))
            || self
                .as_slice()
                .get(index + 1)
                .is_some_and(|val| entry.virt_overlaps(*val))
        {
            return None;
        }

        if self.map_length == self.map_capacity {
            self.grow();
        }

        let map_length = self.map_length;
        self.maybe_unit_slice()
            .copy_within(index..map_length, index + 1);
        self.maybe_unit_slice()[index].write(entry);
        self.map_length += 1;

        Some(())
    }

    /// Returns the [`VirtualMemoryMapEntry`] that contains `address`, if there exists such an
    /// entry in this [`VirtualMemoryMap`].
    pub fn lookup(&self, address: u64) -> Option<&VirtualMemoryMapEntry> {
        self.as_slice()
            .iter()
            .find(|entry| entry.virt_contains(address))
    }

    fn grow(&mut self) {
        let new_capacity = if self.map_capacity == 0 {
            4
        } else {
            self.map_capacity * 2
        };

        let new_ptr = uefi::boot::allocate_pool(
            uefi::boot::MemoryType::LOADER_DATA,
            new_capacity * mem::size_of::<VirtualMemoryMapEntry>(),
        )
        .expect("allocation failed")
        .as_ptr()
        .cast::<VirtualMemoryMapEntry>();

        if let Some(old_ptr) = NonNull::new(self.map_ptr) {
            unsafe { core::ptr::copy_nonoverlapping(self.map_ptr, new_ptr, self.map_length) }

            unsafe { uefi::boot::free_pool(old_ptr.cast::<u8>()).expect("deallocation failed") }
        }

        self.map_ptr = new_ptr;
        self.map_capacity = new_capacity;
    }

    /// Returns a slice of the [`VirtualMemoryMapEntry`] that compose this [`VirtualMemoryMap`].
    pub fn as_slice(&self) -> &[VirtualMemoryMapEntry] {
        if self.map_capacity == 0 {
            return &[];
        }

        unsafe { core::slice::from_raw_parts(self.map_ptr, self.map_length) }
    }

    fn maybe_unit_slice(&mut self) -> &mut [MaybeUninit<VirtualMemoryMapEntry>] {
        if self.map_capacity == 0 {
            return &mut [];
        }

        unsafe { core::slice::from_raw_parts_mut(self.map_ptr.cast(), self.map_capacity) }
    }
}

impl fmt::Debug for VirtualMemoryMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_list = f.debug_list();

        debug_list.entries(self.as_slice().iter());

        debug_list.finish()
    }
}

/// A range of virtual memory with a contiguous region of physical memory underlying it.
#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct VirtualMemoryMapEntry {
    page: u64,
    frame: u64,
    size_flags: u64,
}

impl VirtualMemoryMapEntry {
    const SIZE_BITS: u8 = if PageRange::SIZE_BITS > FrameRange::SIZE_BITS {
        PageRange::SIZE_BITS
    } else {
        FrameRange::SIZE_BITS
    };
    const SIZE_MASK: u64 = (1 << Self::SIZE_BITS) - 1;

    const WRITE_OFFSET: u8 = Self::SIZE_BITS;
    const WRITE_BITS: u8 = 1;
    const WRITE_MASK: u64 = ((1 << Self::WRITE_BITS) - 1) << Self::WRITE_OFFSET;

    const EXEC_OFFSET: u8 = Self::WRITE_OFFSET + 1;
    const EXEC_BITS: u8 = 1;
    const EXEC_MASK: u64 = ((1 << Self::EXEC_BITS) - 1) << Self::EXEC_OFFSET;

    /// Constructs a new [`VirtualMemoryMapEntry`], asserting that `page_number` and `size` are
    /// well-formed.
    pub fn new(
        pages: PageRange,
        frames: FrameRange,
        writable: bool,
        executable: bool,
    ) -> Option<Self> {
        if pages.size() != frames.size() {
            return None;
        }

        let entry = Self {
            page: pages.page(),
            frame: frames.frame(),
            size_flags: pages.size()
                | ((writable as u64) << Self::WRITE_OFFSET)
                | ((executable as u64) << Self::EXEC_OFFSET),
        };

        Some(entry)
    }

    /// The page number at the start of this entry.
    pub fn page(self) -> u64 {
        self.page
    }

    /// The frame number at the start of this entry.
    pub fn frame(self) -> u64 {
        self.frame
    }

    /// The number of pages in this entry.
    pub fn size(self) -> u64 {
        self.size_flags & Self::SIZE_MASK
    }

    /// The virtual memory range is writable.
    pub fn writable(self) -> bool {
        self.size_flags & Self::WRITE_MASK == Self::WRITE_MASK
    }

    /// The virtual memory range is executable.
    pub fn executable(self) -> bool {
        self.size_flags & Self::EXEC_MASK == Self::EXEC_MASK
    }

    /// The [`PageRange`] that represents the virtual address range this [`VirtualMemoryMapEntry`]
    /// covers.
    pub fn page_range(self) -> PageRange {
        PageRange {
            page_number: self.page(),
            size: self.size(),
        }
    }

    /// The [`FrameRange`] that underlies the virtual address range.
    pub fn frame_range(self) -> FrameRange {
        FrameRange {
            frame_number: self.frame(),
            size: self.size(),
        }
    }

    /// Tests whether `self` and `entry` specify overlapping ranges.
    pub fn virt_overlaps(&self, entry: VirtualMemoryMapEntry) -> bool {
        self.page() < entry.page() + entry.size() && entry.page() < self.page() + self.size()
    }

    /// Tests whether `address` is contained in the virtual range of this
    /// [`VirtualMemoryMapEntry`].
    pub fn virt_contains(&self, address: u64) -> bool {
        let containing_page = (address / 4096) & PageRange::PAGE_MASK;
        self.page() <= containing_page && containing_page < self.page() + self.size()
    }
}

impl fmt::Debug for VirtualMemoryMapEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut debug_struct = f.debug_struct("VirtualMemoryMapEntry");

        debug_struct.field("page", &self.page());
        debug_struct.field("frame", &self.frame());
        debug_struct.field("size", &self.size());

        debug_struct.field("writable", &self.writable());
        debug_struct.field("executable", &self.executable());

        debug_struct.finish()
    }
}

/// A contiguous region of virtual memory.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct PageRange {
    page_number: u64,
    size: u64,
}

impl PageRange {
    const ADDRESS_BITS: u8 = 48;
    const MAX_PAGE: u64 = 1 << (Self::ADDRESS_BITS - 12);

    const EXTENDED_MASK: u64 = 1 << (Self::ADDRESS_BITS - 1);
    const EXTENDED_BIT_PAGE: u64 = 1 << (Self::ADDRESS_BITS - 13);

    const PAGE_OFFSET: u8 = 0;
    const PAGE_BITS: u8 = Self::ADDRESS_BITS - 12;
    /// Mask of the bits that a page number can occuy.
    pub const PAGE_MASK: u64 = ((1 << Self::PAGE_BITS) - 1) << Self::PAGE_OFFSET;

    const SIZE_BITS: u8 = Self::ADDRESS_BITS - 12;

    /// Creates a new [`PageRange`], validating that the range fits within the maximum page number
    /// limits and that it doesn't cross the `x86_64` virtual boundary.
    pub fn new(page_number: u64, size: u64) -> Option<Self> {
        if !(page_number.checked_add(size)? < Self::MAX_PAGE) {
            return None;
        }

        let page_range = Self { page_number, size };
        if page_range.contains(Self::EXTENDED_BIT_PAGE)
            && page_range.page_number != Self::EXTENDED_BIT_PAGE
        {
            return None;
        }

        Some(page_range)
    }

    /// The starting page of this [`PageRange`].
    pub const fn page(self) -> u64 {
        self.page_number
    }

    /// The virtual address at the start of this [`PageRange`].
    pub const fn virtual_address(self) -> u64 {
        ((self.page_number << 12) ^ Self::EXTENDED_MASK).wrapping_sub(Self::EXTENDED_MASK)
    }

    /// The number of pages this [`PageRange`] includes.
    pub const fn size(self) -> u64 {
        self.size
    }

    /// Checks if `page_number` is inside this [`PageRange`].
    pub const fn contains(self, page_number: u64) -> bool {
        self.page_number <= page_number && page_number < self.page_number + self.size
    }

    /// Checks if `pages` overlaps with `self`.
    pub const fn overlaps(self, pages: PageRange) -> bool {
        self.page() < pages.page() + pages.size() && pages.page() < self.page() + self.size()
    }
}

/// A contiguous region of physical memory.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct FrameRange {
    frame_number: u64,
    size: u64,
}

impl FrameRange {
    const MAX_PHYS: u8 = 52;
    const MAX_FRAME: u64 = 1 << (Self::MAX_PHYS - 12);

    const SIZE_BITS: u8 = Self::MAX_PHYS - 12;

    /// Creates a new [`FrameRange`], validating that the range fits within the maximum frame
    /// number limits.
    pub fn new(frame_number: u64, size: u64) -> Option<Self> {
        if !(frame_number.checked_add(size)? < Self::MAX_FRAME) {
            return None;
        }

        Some(Self { frame_number, size })
    }

    /// The physical address at the start of this [`FrameRange`].
    pub const fn physical_address(self) -> u64 {
        self.frame() << 12
    }

    /// The starting frame of this [`FrameRange`].
    pub const fn frame(self) -> u64 {
        self.frame_number
    }

    /// The number of frames this [`FrameRange`] includes.
    pub const fn size(self) -> u64 {
        self.size
    }
}
