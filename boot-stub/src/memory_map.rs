//! Functionality that deals with virtual memory.

use core::{
    error, fmt,
    mem::{self, MaybeUninit},
    ptr::{self, NonNull},
};

use uefi::boot::{self, AllocateType, MemoryType};

use crate::{
    load_application::rand_u64,
    memory_structs::{Frame, FrameRange, Page, PageRange, PhysicalAddress, VirtualAddress},
    spinlock::{RawSpinlock, SpinlockAcquisitionError, SpinlockGuard},
    MINIMUM_APPLICATION_BASE,
};

const ALLOCATION_RETRY_ATTEMPTS: usize = 10;

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

    /// Generates a random virtual memory region, allocates backing memory, marks it to be mapped
    /// according to [`Protection`].
    pub fn allocate(
        &mut self,
        size: usize,
        protection: Protection,
        usage: Usage,
    ) -> Result<&Entry, AllocateEntryError> {
        let Some(total_btyes) = size.checked_mul(4096) else {
            return Err(AllocateEntryError::BackingMemoryAllocationError {
                size,
                status: uefi::Status::OUT_OF_RESOURCES,
            });
        };

        let mut tries = 0;
        let address = 'exit: {
            while tries < ALLOCATION_RETRY_ATTEMPTS {
                tries += 1;

                let Some(random_number) = rand_u64() else {
                    // Move to deterministic virtual memory address allocator.
                    break;
                };

                let start_address = VirtualAddress::new_canonical(
                    random_number as usize | (1 << (VirtualAddress::MAX_BITS - 1)),
                );
                let start_page = Page::containing_address(start_address);

                let Some(end_address) = start_page.base_address().value().checked_add(total_btyes)
                else {
                    continue;
                };
                let end_address = VirtualAddress::new_canonical(end_address);
                let end_page = Page::containing_address(end_address);

                if !(end_address.value() < MINIMUM_APPLICATION_BASE as usize) {
                    // Ensure that any allocations cannot interfere with the application load section.
                    continue;
                }

                let Some(pages) = PageRange::inclusive_range(start_page, end_page) else {
                    continue;
                };

                match self.allocate_at(pages, protection, usage) {
                    Ok(entry) => break 'exit entry.page_range().start_address(),
                    Err(AllocateEntryError::AddEntryError(
                        AddEntryError::VirtualMemoryRegionOverlaps,
                    )) => continue,
                    Err(error) => return Err(error),
                };
            }

            const TOP_RANGE: PageRange = match PageRange::inclusive_range(
                Page::containing_address(VirtualAddress::new_canonical(
                    MINIMUM_APPLICATION_BASE as usize,
                )),
                Page::containing_address(VirtualAddress::new_canonical(
                    MINIMUM_APPLICATION_BASE as usize,
                )),
            ) {
                Some(range) => range,
                None => panic!(),
            };

            let Some(region) = self
                .as_slice()
                .iter()
                .map(|entry| entry.page_range())
                .filter(|entry| entry.start_address().value() > VirtualAddress::END_GAP)
                .filter(|entry| {
                    entry.start_address().value() + entry.size_in_bytes()
                        < MINIMUM_APPLICATION_BASE as usize
                })
                .chain(core::iter::once(TOP_RANGE))
                .map_windows(|[lower, upper]| {
                    let start = VirtualAddress::new_canonical(
                        lower.start_address().value() + lower.size_in_bytes(),
                    );
                    let end = VirtualAddress::new_canonical(upper.start_address().value() - 1);
                    PageRange::inclusive_range(
                        Page::containing_address(start),
                        Page::containing_address(end),
                    )
                    .unwrap()
                })
                .filter(|entry| entry.size_in_pages() >= size)
                .next()
            else {
                return Err(AddEntryError::VirtualMemoryRegionOverlaps.into());
            };

            let end_page = Page::containing_address(VirtualAddress::new_canonical(
                region.start_address().value() + total_btyes,
            ));
            let pages = PageRange::inclusive_range(region.start(), end_page).unwrap();

            return self.allocate_at(pages, protection, usage);
        };

        self.lookup(address)
            .ok_or(AddEntryError::VirtualMemoryRegionOverlaps.into())
    }

    /// Allocates a region of physical memory to back the virtual memory region `pages`, and adds
    /// it to the [`ApplicationMemoryMap`] with the given [`Protection`] and [`Usage`].
    pub fn allocate_at(
        &mut self,
        pages: PageRange,
        protection: Protection,
        usage: Usage,
    ) -> Result<&Entry, AllocateEntryError> {
        let memory_type = match protection {
            Protection::Executable | Protection::WritableExecutable => MemoryType::LOADER_CODE,
            _ => MemoryType::LOADER_DATA,
        };

        let ptr = match boot::allocate_pages(
            AllocateType::AnyPages,
            memory_type,
            pages.size_in_pages(),
        ) {
            Ok(ptr) => ptr,
            Err(error) => {
                return Err(AllocateEntryError::BackingMemoryAllocationError {
                    size: pages.size_in_pages(),
                    status: error.status(),
                })
            }
        };

        let start_frame =
            Frame::containing_address(PhysicalAddress::new(ptr.as_ptr() as u64).unwrap());
        let end_frame = Frame::containing_address(
            PhysicalAddress::new((ptr.as_ptr() as u64) + pages.size_in_pages() as u64 * 4096 - 1)
                .unwrap(),
        );

        let result = unsafe {
            self.add_entry(
                pages,
                BackingMemory::Allocated {
                    frame_range: FrameRange::inclusive_range(start_frame, end_frame),
                    protection,
                    usage,
                },
            )
        };

        match result {
            Ok(entry) => return Ok(entry),
            Err(error) => {
                let dealloc_result = unsafe { boot::free_pages(ptr, pages.size_in_pages()) };
                if let Err(error) = dealloc_result {
                    log::warn!(
                        "Failed to deallocate {} attempted identity map frames with status code: {}",
                        pages.size_in_pages(),
                        error.status()
                    );
                }
                return Err(error.into());
            }
        };
    }

    /// Allocates a region of physical memory and adds its identity map to the
    /// [`ApplicationMemoryMap`] with the given [`Protection`] flags.
    pub fn allocate_identity(
        &mut self,
        size: usize,
        protection: Protection,
    ) -> Result<&Entry, AllocateEntryError> {
        let memory_type = match protection {
            Protection::Executable | Protection::WritableExecutable => MemoryType::LOADER_CODE,
            _ => MemoryType::LOADER_DATA,
        };

        let mut tries = 0;
        let address = 'exit: {
            while tries < ALLOCATION_RETRY_ATTEMPTS {
                tries += 1;

                let ptr = match boot::allocate_pages(AllocateType::AnyPages, memory_type, size) {
                    Ok(ptr) => ptr,
                    Err(error) => {
                        return Err(AllocateEntryError::BackingMemoryAllocationError {
                            size,
                            status: error.status(),
                        })
                    }
                };

                let start_page =
                    Page::containing_address(VirtualAddress::new(ptr.as_ptr() as usize).unwrap());
                let end_page = Page::containing_address(
                    VirtualAddress::new((ptr.as_ptr() as usize) + size * 4096 - 1).unwrap(),
                );

                let start_frame =
                    Frame::containing_address(PhysicalAddress::new(ptr.as_ptr() as u64).unwrap());
                let end_frame = Frame::containing_address(
                    PhysicalAddress::new((ptr.as_ptr() as u64) + size as u64 * 4096 - 1).unwrap(),
                );

                let result = unsafe {
                    self.add_entry(
                        PageRange::inclusive_range(start_page, end_page).unwrap(),
                        BackingMemory::Allocated {
                            frame_range: FrameRange::inclusive_range(start_frame, end_frame),
                            protection,
                            usage: Usage::General,
                        },
                    )
                };

                let result = match result {
                    Ok(entry) => break 'exit entry.page_range().start_address(),
                    Err(error) => error,
                };

                let dealloc_result = unsafe { boot::free_pages(ptr, size) };
                if let Err(error) = dealloc_result {
                    log::warn!(
                    "Failed to deallocate {} attempted identity map frames with status code: {}",
                    size,
                    error.status()
                );
                }

                match result {
                    AddEntryError::GrowFailed { size: _, status: _ } => return Err(result.into()),
                    AddEntryError::VirtualMemoryRegionOverlaps => continue,
                }
            }

            return Err(AddEntryError::VirtualMemoryRegionOverlaps.into());
        };

        self.lookup(address)
            .ok_or(AddEntryError::VirtualMemoryRegionOverlaps.into())
    }

    /// Marks a virtual memory region as reserved.
    pub fn mark_reserved(&mut self, pages: PageRange) -> Result<&Entry, AddEntryError> {
        unsafe { self.add_entry(pages, BackingMemory::Unbacked) }
    }

    /// Adds the virtual memory region `pages` to the [`ApplicationMemoryMap`], checking that
    /// `backing` is of an appropriate size and that `pages` does not overlap with any other
    /// [`Entry`].
    ///
    /// # Safety
    /// If `backing` is of type [`BackingMemory::Allocated`], then this [`ApplicationMemoryMap`], and
    /// [`Entry`]s acquired from it, must have sole control over access to the identity mapped
    /// physical memory region.
    pub unsafe fn add_entry(
        &mut self,
        pages: PageRange,
        backing: BackingMemory,
    ) -> Result<&Entry, AddEntryError> {
        let index = self.check_requirements(pages, backing)?;
        let entry = Entry {
            page_range: pages,
            backing,
            lock: RawSpinlock::new(),
        };

        if self.length == self.capacity {
            self.grow()
                .map_err(|(size, status)| AddEntryError::GrowFailed { size, status })?;
        }

        unsafe {
            core::ptr::copy(
                self.ptr.add(index),
                self.ptr.add(index + 1),
                self.length - index,
            )
        }

        self.length += 1;
        let entry = self.maybe_uninit_slice_mut()[index].write(entry);

        Ok(entry)
    }

    fn check_requirements(
        &self,
        pages: PageRange,
        backing: BackingMemory,
    ) -> Result<usize, AddEntryError> {
        let Err(index) = self
            .as_slice()
            .binary_search_by_key(&pages.start(), |entry| entry.page_range.start())
        else {
            return Err(AddEntryError::VirtualMemoryRegionOverlaps);
        };

        let overlaps_lower = self
            .as_slice()
            .get(index)
            .is_some_and(|entry| entry.page_range.overlaps(&pages));
        let overlaps_upper = self
            .as_slice()
            .get(index + 1)
            .is_some_and(|entry| entry.page_range.overlaps(&pages));

        if overlaps_lower || overlaps_upper {
            return Err(AddEntryError::VirtualMemoryRegionOverlaps);
        }

        match backing {
            BackingMemory::Allocated {
                frame_range,
                protection: _,
                usage: _,
            }
            | BackingMemory::Unallocated {
                frame_range,
                protection: _,
                usage: _,
            } => assert_eq!(pages.size_in_pages() as u64, frame_range.size_in_frames()),
            _ => {}
        }

        Ok(index)
    }

    /// Searches the [`ApplicationMemoryMap`] for the [`Entry`] that contains `address` and returns
    /// a reference to it.
    ///
    /// If the `address` is not contained in one of the [`Entry`]s, then this function returns
    /// [`None`].
    pub fn lookup(&self, address: VirtualAddress) -> Option<&Entry> {
        self.as_slice()
            .iter()
            .filter(|entry| entry.page_range.contains_address(address))
            .next()
    }

    /// Returns an immutable slice over the [`Entry`]s that make up the [`ApplicationMemoryMap`].
    pub fn as_slice(&self) -> &[Entry] {
        unsafe { core::slice::from_raw_parts(self.ptr, self.length) }
    }

    fn maybe_uninit_slice_mut(&mut self) -> &mut [MaybeUninit<Entry>] {
        assert!(!self.ptr.is_null());

        unsafe {
            core::slice::from_raw_parts_mut(self.ptr.cast::<MaybeUninit<Entry>>(), self.length)
        }
    }

    fn grow(&mut self) -> Result<(), (usize, uefi::Status)> {
        let new_capacity = (self.capacity.saturating_mul(2)).max(4);
        let new_size = new_capacity.saturating_mul(mem::size_of::<Entry>());
        if new_capacity == self.capacity {
            return Err((new_size, uefi::Status::OUT_OF_RESOURCES));
        }

        let new_ptr = match boot::allocate_pool(boot::MemoryType::LOADER_DATA, new_size) {
            Ok(ptr) => ptr.as_ptr().cast::<Entry>(),
            Err(error) => return Err((new_size, error.status())),
        };

        if let Some(old_ptr) = NonNull::new(self.ptr) {
            unsafe { core::ptr::copy_nonoverlapping(self.ptr, new_ptr, self.length) }

            let free_result = unsafe { boot::free_pool(old_ptr.cast::<u8>()) };
            if let Err(error) = free_result {
                log::warn!("memory map deallocation failed: {}", error.status());
            }
        }

        self.ptr = new_ptr;
        self.capacity = new_capacity;

        Ok(())
    }
}

/// Various errors that can occur while allocating an [`Entry`] for an [`ApplicationMemoryMap`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum AllocateEntryError {
    /// The allocation of the [`BackingMemory`] failed.
    BackingMemoryAllocationError {
        /// The number of pages in the failed allocation.
        size: usize,
        /// The status code of the failed allocation.
        status: uefi::Status,
    },
    /// An error occurred when adding the [`Entry`].
    AddEntryError(AddEntryError),
}

impl From<AddEntryError> for AllocateEntryError {
    fn from(value: AddEntryError) -> Self {
        Self::AddEntryError(value)
    }
}

impl fmt::Display for AllocateEntryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BackingMemoryAllocationError { size, status } => write!(
                f,
                "failed to allocate backing physical memory region of {} frames with status code: {}",
                size,
                status,
            ),
            Self::AddEntryError(error) => write!(
                f,
                "failed to add entry to memory map: {error}",
            )
        }
    }
}

impl error::Error for AllocateEntryError {}

/// Various errors that can occur while adding an [`Entry`] to a [`ApplicationMemoryMap`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum AddEntryError {
    /// The virtual memory region of the [`Entry`] overlaps with another [`Entry`].
    VirtualMemoryRegionOverlaps,
    /// The application memory map failed to grow.
    GrowFailed {
        /// The size of the failed allocation.
        size: usize,
        /// The status code of the failed allocation.
        status: uefi::Status,
    },
}

impl fmt::Display for AddEntryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::VirtualMemoryRegionOverlaps => {
                f.pad("virtual memory region overlaps with existing region")
            }
            Self::GrowFailed { size, status } => write!(
                f,
                "failed to grow application memory map to {size} bytes with status code: {status}"
            ),
        }
    }
}

impl error::Error for AddEntryError {}

/// A virtual memory region and its flags.
#[derive(Debug)]
pub struct Entry {
    page_range: PageRange,
    backing: BackingMemory,
    lock: RawSpinlock,
}

impl Entry {
    /// The [`PageRange`] this [`Entry`] represents in the [`ApplicationMemoryMap`].
    pub fn page_range(&self) -> PageRange {
        self.page_range
    }

    /// Whether the [`PageRange`] will be mapped into the application's virtual address space.
    pub fn present(&self) -> bool {
        self.protection().is_some()
    }

    /// Whether the [`PageRange`] will be mapped as writable in the application's virtual address
    /// space.
    pub fn writable(&self) -> bool {
        self.protection().is_some_and(|protection| {
            protection == Protection::Writable || protection == Protection::WritableExecutable
        })
    }

    /// Whether the [`PageRange`] will be mapped as executable in the application's virtual address
    /// space.
    pub fn executable(&self) -> bool {
        self.protection().is_some_and(|protection| {
            protection == Protection::Executable || protection == Protection::WritableExecutable
        })
    }

    /// The settings on the [`PageRange`] when mapped into the application's virtual address space.
    fn protection(&self) -> Option<Protection> {
        match self.backing {
            BackingMemory::Allocated {
                frame_range: _,
                protection,
                usage: _,
            }
            | BackingMemory::Unallocated {
                frame_range: _,
                protection,
                usage: _,
            } => Some(protection),
            BackingMemory::Unbacked => None,
        }
    }

    /// The usage of this memory region.
    pub fn usage(&self) -> Option<Usage> {
        match self.backing {
            BackingMemory::Allocated {
                frame_range: _,
                protection: _,
                usage,
            }
            | BackingMemory::Unallocated {
                frame_range: _,
                protection: _,
                usage,
            } => Some(usage),
            BackingMemory::Unbacked => None,
        }
    }

    /// Returns a [`SpinlockGuard`] reference to the bytes backing the virtual memory allocation.
    ///
    /// If [`BackingMemory`] is not [`BackingMemory::Allocated`], then this function returns
    /// [`None`].
    pub fn as_slice<T>(&self) -> Option<SpinlockGuard<[MaybeUninit<T>]>> {
        match self.backing {
            BackingMemory::Allocated {
                frame_range,
                protection: _,
                usage: _,
            } => {
                self.lock.lock();

                Some(Self::as_slice_impl(&self.lock, frame_range))
            }
            _ => None,
        }
    }

    /// Returns a [`SpinlockGuard`] reference to the bytes backing the virtual memory allocation.
    ///
    /// If the lock is already taken, then this function returns [`TryAsSliceError::LockError`].
    ///
    /// If [`BackingMemory`] is not [`BackingMemory::Allocated`], then this function returns
    /// [`TryAsSliceError::NotAllocated`].
    pub fn try_as_slice<T>(&self) -> Result<SpinlockGuard<[MaybeUninit<T>]>, TryAsSliceError> {
        match self.backing {
            BackingMemory::Allocated {
                frame_range,
                protection: _,
                usage: _,
            } => match self.lock.try_lock() {
                Ok(()) => Ok(Self::as_slice_impl(&self.lock, frame_range)),
                Err(SpinlockAcquisitionError) => Err(TryAsSliceError::LockError),
            },
            _ => Err(TryAsSliceError::NotAllocated),
        }
    }

    fn as_slice_impl<T>(
        lock: &RawSpinlock,
        frame_range: FrameRange,
    ) -> SpinlockGuard<[MaybeUninit<T>]> {
        const { assert!(mem::align_of::<T>() <= Page::PAGE_SIZE) }

        let slice = unsafe {
            core::slice::from_raw_parts_mut(
                frame_range.start_address().value() as *mut MaybeUninit<T>,
                frame_range.size_in_bytes() as usize / mem::size_of::<T>(),
            )
        };

        let unsafe_cell = core::cell::UnsafeCell::from_mut(slice);
        unsafe { crate::spinlock::SpinlockGuard::new(&lock, unsafe_cell) }
    }
}

/// Various errors that can occur while attempting to acquire a slice of the backing memory of an
/// [`Entry`].
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum TryAsSliceError {
    /// The [`BackingMemory`] is not [`BackingMemory::Allocated`].
    NotAllocated,
    /// The memory region was already locked.
    LockError,
}

impl fmt::Display for TryAsSliceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotAllocated => f.pad("entry is not backed by allocated memory"),
            Self::LockError => f.pad("entry memory region is already locked"),
        }
    }
}

impl error::Error for TryAsSliceError {}

/// The type of backing that the [`Entry`] controls.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum BackingMemory {
    /// The virtual memory region is backed by a [`FrameRange`] that is under the exclusive control
    /// of the [`Entry`].
    Allocated {
        /// The [`FrameRange`] that forms the backing physical memory.
        frame_range: FrameRange,
        /// The [`Protection`] flags to be used on the backing's virtual memory region.
        protection: Protection,
        /// The [`Usage`] of the virtual memory region.
        usage: Usage,
    },
    /// The virtual memory region is backed by a [`FrameRange`] that is not under exclusive control
    /// of the [`Entry`].
    Unallocated {
        /// The [`FrameRange`] that forms the backing physical memory.
        frame_range: FrameRange,
        /// The [`Protection`] flags to be used on the backing's virtual memory region.
        protection: Protection,
        /// The [`Usage`] of the virtual memory region.
        usage: Usage,
    },
    /// The virtual memory is unbacked.
    Unbacked,
}

/// The protection flags to be used on the [`Entry`]'s virtual memory allocation.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Protection {
    /// The virtual memory region should be readable.
    Readable,
    /// The virtual memory region should be readable and writable.
    Writable,
    /// The virtual memory region should be readable and executable.
    Executable,
    /// The virtual memory region should be readable, writable, and executable.
    WritableExecutable,
}

/// The purpose of the [`Entry`]'s virtual memory allocation.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum Usage {
    /// The virtual memory region's purpose is general.
    General,
    /// The virtual memory region's purpose is being used to store part of the application.
    Application,
    /// The virtual memory region's purpose is being used to store a module.
    Module,
    /// The virtual memory region's purpose is to map a framebuffer.
    Framebuffer,
}
