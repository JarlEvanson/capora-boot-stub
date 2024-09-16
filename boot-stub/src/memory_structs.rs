//! Definitions of various structures for interacting with memory in an organized manner.

use core::fmt;

/// A physical memory address.
#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PhysicalAddress(u64);

impl PhysicalAddress {
    /// The maximum number of bits a `x86_64` processor can support.
    pub const MAX_BITS: u8 = 52;
    /// A bitmask for the valid values of a [`PhysicalAddress`].
    pub const ADDRESS_MASK: u64 = (1 << Self::MAX_BITS) - 1;

    /// Returns the zero [`PhysicalAddress`].
    pub const fn zero() -> Self {
        Self(0)
    }

    /// Returns the [`PhysicalAddress`] at `address` if `address` is a valid [`PhysicalAddress`].
    pub const fn new(address: u64) -> Option<Self> {
        if address & Self::ADDRESS_MASK != address {
            return None;
        }

        Some(Self(address))
    }

    /// Returns the [`PhysicalAddress`] at `address`, masking off any invalid bits.
    pub const fn new_masked(address: u64) -> Self {
        Self(address & Self::ADDRESS_MASK)
    }

    /// Returns the underlying value of this [`PhysicalAddress`].
    pub const fn value(&self) -> u64 {
        self.0
    }

    /// Returns the offset within a [`Frame`] at which this [`PhysicalAddress`] lies.
    pub const fn frame_offset(&self) -> u64 {
        self.0 % Frame::FRAME_SIZE
    }
}

impl fmt::Debug for PhysicalAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("PhysicalAddress")
            .field(&(self.0 as *const u8))
            .finish()
    }
}

/// A region of physical memory aligned to an architecture-dependent value.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Frame(u64);

impl Frame {
    /// The number of bytes that make up a [`Frame`].
    pub const FRAME_SIZE: u64 = 4096;

    /// Returns the [`Frame`] that contains the [`PhysicalAddress`].
    pub const fn containing_address(address: PhysicalAddress) -> Self {
        Self(address.value() / Self::FRAME_SIZE)
    }

    /// Returns the [`Frame`] number of this [`Frame`].
    pub const fn number(&self) -> u64 {
        self.0
    }

    /// Returns the [`PhysicalAddress`] at the base of this [`Frame`].
    pub const fn base_address(&self) -> PhysicalAddress {
        PhysicalAddress(self.0 * Self::FRAME_SIZE)
    }
}

/// A range of contiguous [`Frame`]s.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FrameRange {
    frame: Frame,
    size: u64,
}

impl FrameRange {
    /// Returns the [`FrameRange`] that starts at `start` and ends at `end`, inclusively.
    pub const fn inclusive_range(start: Frame, end: Frame) -> Self {
        let size = if end.number() < start.number() {
            0
        } else {
            end.number() - start.number() + 1
        };

        Self { frame: start, size }
    }

    /// Returns the [`Frame`] at the start of the [`FrameRange`].
    pub const fn start(&self) -> Frame {
        self.frame
    }

    /// Returns the [`PhysicalAddress`] at the start of the [`FrameRange`].
    pub const fn start_address(&self) -> PhysicalAddress {
        self.frame.base_address()
    }

    /// Returns the number of [`Frame`]s this [`FrameRange`] contains.
    pub const fn size_in_frames(&self) -> u64 {
        self.size
    }

    /// Returns number of bytes this [`FrameRange`] contains.
    pub const fn size_in_bytes(&self) -> u64 {
        self.size * Frame::FRAME_SIZE
    }

    /// Returns `true` if this [`FrameRange`] contains the given [`PhysicalAddress`].
    pub const fn contains_address(&self, address: PhysicalAddress) -> bool {
        self.start().number() <= Frame::containing_address(address).number()
            && Frame::containing_address(address).number()
                < self.start().number() + self.size_in_frames()
    }

    /// Returns the offset into this [`FrameRange`] at which the given [`PhysicalAddress`] lies.
    ///
    /// If the given [`PhysicalAddress`] is not contained in this [`FrameRange`], this function
    /// returns [`None`].
    pub const fn offset_of_address(&self, address: PhysicalAddress) -> Option<u64> {
        if !self.contains_address(address) {
            return None;
        }

        Some(address.value() - self.start_address().value())
    }

    /// Returns the [`PhysicalAddress`] located at the given `offset` in this [`FrameRange`].
    ///
    /// If the given `offset` is greater than the size in bytes of this [`FrameRange`], this
    /// function returns [`None`].
    pub const fn address_at_offset(&self, offset: u64) -> Option<PhysicalAddress> {
        if !(offset < self.size_in_bytes()) {
            return None;
        }

        Some(PhysicalAddress(self.start_address().value() + offset))
    }

    /// Returns `true` if this [`FrameRange`] fully contains the given `other` [`FrameRange`].
    pub const fn contains_range(&self, other: &FrameRange) -> bool {
        self.start().number() <= other.start().number()
            && other.start().number() + other.size_in_frames()
                < self.start().number() + self.size_in_frames()
    }

    /// Returns `true` if this [`FrameRange`] overlaps with the given `other` [`FrameRange`].
    pub const fn overlaps(&self, other: &FrameRange) -> bool {
        self.start().number() < other.start().number() + other.size_in_frames()
            && other.start().number() < self.start().number() + self.size_in_frames()
    }
}

impl IntoIterator for FrameRange {
    type Item = Frame;
    type IntoIter = FrameRangeIter;

    fn into_iter(self) -> Self::IntoIter {
        FrameRangeIter {
            frame: self.frame,
            remaining: self.size,
        }
    }
}

/// An [`Iterator`] over the [`Frame`]s that make up the [`FrameRange`].
pub struct FrameRangeIter {
    frame: Frame,
    remaining: u64,
}

impl Iterator for FrameRangeIter {
    type Item = Frame;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }

        let frame = self.frame;
        self.frame = Frame::containing_address(PhysicalAddress::new_masked(
            self.frame.base_address().value() + Frame::FRAME_SIZE,
        ));

        self.remaining -= 1;
        Some(frame)
    }
}

/// A virtual memory address.
#[repr(transparent)]
#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VirtualAddress(usize);

impl VirtualAddress {
    /// The maximum number of bits a `x86_64` processor can support.
    pub const MAX_BITS: u8 = 48;
    /// The start of the gap in the virtual address space.
    pub const START_GAP: usize = 0x0000_8000_0000_0000;
    /// The end of the gap in the virtual address space.
    pub const END_GAP: usize = 0xFFFF_7FFF_FFFF_FFFF;

    /// Returns the zero [`VirtualAddress`].
    pub const fn zero() -> Self {
        Self(0)
    }

    /// Returns the [`VirtualAddress`] at `address` if `address` is a valid [`VirtualAddress`].
    pub const fn new(address: usize) -> Option<Self> {
        let upper17 = address & !0x0000_7FFF_FFFF_FFFF;
        if !(upper17 == 0 || upper17 == !0x0000_7FFF_FFFF_FFFF) {
            return None;
        }

        Some(VirtualAddress(address))
    }

    /// Returns the [`VirtualAddress`] at `address` removing any bits that disrupt canonicality.
    pub const fn new_canonical(address: usize) -> Self {
        Self(((address << 16) as isize >> 16) as usize)
    }

    /// Returns the underlying value of this [`VirtualAddress`].
    pub const fn value(&self) -> usize {
        self.0
    }

    /// Returns the offset within a [`Page`] at which this [`VirtualAddress`] lies.
    pub const fn page_offset(&self) -> usize {
        self.0 % Page::PAGE_SIZE
    }
}

impl fmt::Debug for VirtualAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("VirtualAddress")
            .field(&(self.0 as *const u8))
            .finish()
    }
}

/// A region of virtual memory aligned to an architecture dependent value.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Page(usize);

impl Page {
    /// The number of bytes that make up a [`Page`].
    pub const PAGE_SIZE: usize = 4096;

    /// Returns the [`Page`] that contains the [`VirtualAddress`].
    pub const fn containing_address(address: VirtualAddress) -> Self {
        Self(address.value() / Self::PAGE_SIZE)
    }

    /// Returns the [`Page`] number of this [`Page`].
    pub const fn number(&self) -> usize {
        self.0
    }

    /// Returns the [`VirtualAddress`] at the base of this [`Page`].
    pub const fn base_address(&self) -> VirtualAddress {
        VirtualAddress(self.0 * Self::PAGE_SIZE)
    }
}

/// A range of contiguous [`Page`]s.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PageRange {
    page: Page,
    size: usize,
}

impl PageRange {
    /// Returns the [`PageRange`] that starts at `start` and ends at `end`, inclusively.
    ///
    /// If the [`PageRange`] would cross the virtual address space gap, this function returns
    /// [`None`].
    pub const fn inclusive_range(start: Page, end: Page) -> Option<Self> {
        if start.base_address().value() <= VirtualAddress::END_GAP
            && end.base_address().value() >= VirtualAddress::START_GAP
        {
            return None;
        }

        let size = if end.number() < start.number() {
            0
        } else {
            end.number() - start.number() + 1
        };

        Some(Self { page: start, size })
    }

    /// Returns the [`Page`] at the start of this [`PageRange`].
    pub const fn start(&self) -> Page {
        self.page
    }

    /// Returns the [`VirtualAddress`] at the start of this [`PageRange`].
    pub const fn start_address(&self) -> VirtualAddress {
        self.page.base_address()
    }

    /// Returns the number of [`Page`]s this [`PageRange`] contains.
    pub const fn size_in_pages(&self) -> usize {
        self.size
    }

    /// Returns the number of bytes this [`FrameRange`] contains.
    pub const fn size_in_bytes(&self) -> usize {
        self.size * Page::PAGE_SIZE
    }

    /// Returns `true` if this [`PageRange`] contains the given [`VirtualAddress`].
    pub const fn contains_address(&self, address: VirtualAddress) -> bool {
        self.start().number() <= Page::containing_address(address).number()
            && Page::containing_address(address).number()
                < self.start().number() + self.size_in_pages()
    }

    /// Returns the offset into this [`PageRange`] at which the given [`VirtualAddress`] lies.
    ///
    /// If the given [`VirtualAddress`] is not contained within this [`PageRange`], this function
    /// returns [`None`].
    pub const fn offset_of_address(&self, address: VirtualAddress) -> Option<usize> {
        if !self.contains_address(address) {
            return None;
        }

        Some(address.value() - self.start_address().value())
    }

    /// Returns the [`VirtualAddress`] located at the given `offset` in this [`PageRange`].
    ///
    /// If the given `offset` is greater than the size in bytes of this [`PageRange`], this
    /// function returns [`None`].
    pub const fn address_at_offset(&self, offset: usize) -> Option<VirtualAddress> {
        if !(offset < self.size_in_bytes()) {
            return None;
        }

        Some(VirtualAddress(self.start_address().value() + offset))
    }

    /// Returns `true` if this [`PageRange`] fully contains the given `other` [`PageRange`].
    pub const fn contains_range(&self, other: &PageRange) -> bool {
        self.start().number() <= other.start().number()
            && other.start().number() + other.size_in_pages()
                < self.start().number() + self.size_in_pages()
    }

    /// Returns `true` if this [`PageRange`] overlaps with the given `other` [`PageRange`].
    pub const fn overlaps(&self, other: &PageRange) -> bool {
        self.start().number() < other.start().number() + other.size_in_pages()
            && other.start().number() < self.start().number() + self.size_in_pages()
    }
}

impl IntoIterator for PageRange {
    type Item = Page;
    type IntoIter = PageRangeIter;

    fn into_iter(self) -> Self::IntoIter {
        PageRangeIter {
            page: self.page,
            remaining: self.size,
        }
    }
}

/// An [`Iterator`] over the [`Page`]s that make up the [`PageRange`].
pub struct PageRangeIter {
    page: Page,
    remaining: usize,
}

impl Iterator for PageRangeIter {
    type Item = Page;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            return None;
        }

        let page = self.page;
        self.page = Page::containing_address(VirtualAddress::new_canonical(
            self.page.base_address().value() + Page::PAGE_SIZE,
        ));

        self.remaining -= 1;
        Some(page)
    }
}
