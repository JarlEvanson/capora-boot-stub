//! Code for the `x86_64` bootloader.

use core::{
    fmt,
    mem::{self, MaybeUninit},
};

use x86_64::{
    structures::{gdt::SegmentSelector, idt::InterruptDescriptorTable},
    VirtAddr,
};

use crate::{
    memory_map::{AllocateEntryError, ApplicationMemoryMap, BackingMemory, Protection, Usage},
    memory_structs::{Frame, FrameRange, Page, PageRange, PhysicalAddress, VirtualAddress},
};

const HANDLER_BYTES: &[u8] = include_bytes!("../../handler.bin");
const FONT_BYTES: &[u8] = concat_bytes!(
    include_bytes!("../../../assets/0.bin"),
    include_bytes!("../../../assets/1.bin"),
    include_bytes!("../../../assets/2.bin"),
    include_bytes!("../../../assets/3.bin"),
    include_bytes!("../../../assets/4.bin"),
    include_bytes!("../../../assets/5.bin"),
    include_bytes!("../../../assets/6.bin"),
    include_bytes!("../../../assets/7.bin"),
    include_bytes!("../../../assets/8.bin"),
    include_bytes!("../../../assets/9.bin"),
    include_bytes!("../../../assets/A.bin"),
    include_bytes!("../../../assets/B.bin"),
    include_bytes!("../../../assets/C.bin"),
    include_bytes!("../../../assets/D.bin"),
    include_bytes!("../../../assets/E.bin"),
    include_bytes!("../../../assets/F.bin"),
);

const GDT: &[u8] = [
    // Null GDT entry.
    0x00_0_0_00_000000_0000u64.to_ne_bytes(),
    // Kernel code entry.
    0x00_A_F_9B_000000_FFFFu64.to_ne_bytes(),
    // Kernel data entry.
    0x00_C_F_93_000000_FFFFu64.to_ne_bytes(),
    // TSS entry.
    0x0u64.to_ne_bytes(),
    0x0u64.to_ne_bytes(),
]
.as_flattened();

/// Constructs architecture dependent structures.
pub fn create_architectural_structures(
    application_map: &mut ApplicationMemoryMap,
) -> Result<ArchitecturalStructures, CreateArchitecturalStructuresError> {
    let idt_virtual_address = {
        let handler_entry = application_map.allocate_identity(
            (HANDLER_BYTES.len() + 24 + FONT_BYTES.len()).div_ceil(4096),
            Protection::Executable,
        )?;
        let handler_virtual_address = handler_entry.page_range().start_address();
        log::debug!("Handler page allocated at {handler_virtual_address:?}");
        MaybeUninit::copy_from_slice(
            &mut handler_entry.as_slice().unwrap()[..HANDLER_BYTES.len()],
            HANDLER_BYTES,
        );
        let ptr = crate::logging::PTR.load(core::sync::atomic::Ordering::Acquire);
        let size = crate::logging::BUFFER.load(core::sync::atomic::Ordering::Acquire);
        let stride = crate::logging::STRIDE.load(core::sync::atomic::Ordering::Acquire);
        MaybeUninit::copy_from_slice(
            &mut handler_entry.as_slice().unwrap()[HANDLER_BYTES.len()..][..8],
            &ptr.to_ne_bytes(),
        );
        MaybeUninit::copy_from_slice(
            &mut handler_entry.as_slice().unwrap()[HANDLER_BYTES.len() + 8..][..8],
            &(size / 4).to_ne_bytes(),
        );
        MaybeUninit::copy_from_slice(
            &mut handler_entry.as_slice().unwrap()[HANDLER_BYTES.len() + 16..][..8],
            &stride.to_ne_bytes(),
        );
        MaybeUninit::copy_from_slice(
            &mut handler_entry.as_slice().unwrap()[HANDLER_BYTES.len() + 24..][..FONT_BYTES.len()],
            FONT_BYTES,
        );

        let start_virtual_address =
            Page::containing_address(VirtualAddress::new_canonical(ptr as usize));
        let end_virtual_address =
            Page::containing_address(VirtualAddress::new_canonical(ptr as usize + size as usize));
        let pages = PageRange::inclusive_range(start_virtual_address, end_virtual_address).unwrap();

        let start_physical_address = Frame::containing_address(PhysicalAddress::new_masked(ptr));
        let end_physical_address =
            Frame::containing_address(PhysicalAddress::new_masked(ptr + size));
        let frames = FrameRange::inclusive_range(start_physical_address, end_physical_address);

        let framebuffer_entry = unsafe {
            application_map
                .add_entry(
                    pages,
                    BackingMemory::Unallocated {
                        frame_range: frames,
                        protection: Protection::Writable,
                        usage: Usage::Framebuffer,
                    },
                )
                .unwrap()
        };
        log::debug!(
            "Framebuffer entry: {:?}",
            framebuffer_entry.page_range().start_address()
        );

        let idt_entry = application_map
            .allocate_identity(1, Protection::Writable)
            .unwrap();
        let idt_virtual_address = idt_entry.page_range().start_address();
        log::debug!("IDT page allocated at {idt_virtual_address:?}");
        let mut idt_lock = idt_entry.as_slice().unwrap();
        let idt = &mut idt_lock[0];

        let mut idt_table = InterruptDescriptorTable::new();
        let options = unsafe {
            idt_table
                .page_fault
                .set_handler_addr(VirtAddr::new(handler_virtual_address.value() as u64))
        };
        unsafe {
            options.set_code_selector(SegmentSelector::new(1, x86_64::PrivilegeLevel::Ring0))
        };
        unsafe { options.set_stack_index(0) };
        idt.write(idt_table);

        idt_virtual_address
    };

    let interrupt_stack_entry = application_map.allocate_identity(1, Protection::Writable)?;
    let interrupt_stack_address = interrupt_stack_entry.page_range().start_address();
    log::debug!("Interrupt stack allocated at {interrupt_stack_address:?}");

    let tss_entry = application_map.allocate_identity(1, Protection::Writable)?;
    let tss_address = tss_entry.page_range().start_address();
    let tss = [
        // Reserved
        0u32.to_ne_bytes(),
        // RSP0
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // RSP1
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Rsp2
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Reserved
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Ist 1
        ((interrupt_stack_address.value() + 4096) as u32).to_ne_bytes(),
        (((interrupt_stack_address.value() + 4096) >> 32) as u32).to_ne_bytes(),
        // Ist 2
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Ist 3
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Ist 4
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Ist 5
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Ist 6
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Ist 7
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // Reserved
        0u32.to_ne_bytes(),
        0u32.to_ne_bytes(),
        // IO base
        (104u32 << 16).to_ne_bytes(),
    ];
    let tss: &[u8] = tss.as_flattened();
    MaybeUninit::copy_from_slice(&mut tss_entry.as_slice().unwrap()[..tss.len()], tss);

    let allocation =
        application_map.allocate_identity(GDT.len().div_ceil(4096), Protection::Readable)?;

    let mut allocated_bytes = allocation
        .as_slice()
        .expect("bug in application memory map");
    MaybeUninit::copy_from_slice(&mut allocated_bytes[..GDT.len()], GDT);

    let tss_descriptor = [
        104,
        0, // 16-bit limit
        tss_address.value() as u8,
        (tss_address.value() >> 8) as u8,  // 16-bit base address
        (tss_address.value() >> 16) as u8, // 16-24 bit address
        0xC9,
        0x00,                              // Type, limit, flags
        (tss_address.value() >> 24) as u8, // 24-32 address
        (tss_address.value() >> 32) as u8,
        (tss_address.value() >> 40) as u8, // 32-48 bit address
        (tss_address.value() >> 48) as u8,
        (tss_address.value() >> 56) as u8, // 48-64 bit address
        0,
        0,
        0,
        0,
    ];
    MaybeUninit::copy_from_slice(
        &mut allocated_bytes[GDT.len() - 16..GDT.len()],
        &tss_descriptor,
    );

    Ok(ArchitecturalStructures {
        gdt: allocation.page_range().start_address(),
        idt: idt_virtual_address,
    })
}

/// Various errors that can occur while creating architectural structures.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum CreateArchitecturalStructuresError {
    /// An error occurred while allocating an [`Entry`] for architectural structures.
    AllocationError(AllocateEntryError),
}

impl From<AllocateEntryError> for CreateArchitecturalStructuresError {
    fn from(value: AllocateEntryError) -> Self {
        Self::AllocationError(value)
    }
}

impl fmt::Display for CreateArchitecturalStructuresError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::AllocationError(error) => write!(
                f,
                "error while allocating memory for architectural structures: {error}"
            ),
        }
    }
}

/// Loads the architectural structures.
pub fn load_architecture_structures(data: ArchitecturalStructures) {
    #[allow(dead_code)]
    #[repr(C)]
    struct Gdtr {
        other: [MaybeUninit<u8>; 6],
        size: u16,
        offset: u64,
    }

    let gdtr = Gdtr {
        other: [MaybeUninit::uninit(); 6],
        size: (GDT.len() - 1) as u16,
        offset: data.gdt.value() as u64,
    };

    unsafe {
        core::arch::asm!(
            "lgdt [rax]",
            "push 0x08",
            "lea rax, [rip + 5f]",
            "push rax",
            "retfq",
            "5:",
            "mov ax, 0x10",
            "mov ds, ax",
            "mov es, ax",
            "mov es, ax",
            "mov fs, ax",
            "mov gs, ax",
            "mov ss, ax",
            "mov ax, 0x18",
            "ltr ax",
            inout("rax") &gdtr.size => _,
        )
    }

    #[allow(dead_code)]
    #[repr(C)]
    struct Idtr {
        other: [MaybeUninit<u8>; 6],
        size: u16,
        offset: u64,
    }

    let idtr = Idtr {
        other: [MaybeUninit::uninit(); 6],
        size: (mem::size_of::<InterruptDescriptorTable>() - 1) as u16,
        offset: data.idt.value() as u64,
    };

    unsafe {
        core::arch::asm!(
            "lidt [rax]",
            in("rax") &idtr.size,
        )
    }
}

/// Data needed to set the architectural structures to be used.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ArchitecturalStructures {
    /// The virtual address of the global descriptor table.
    gdt: VirtualAddress,
    /// The virtual address of the interrupt descriptor table.
    idt: VirtualAddress,
}

/// Checks if the required features are supported.
pub fn test_required_feature_support() -> Result<(), UnsupportedFeaturesError> {
    let execute_disable_supported_bit = unsafe { core::arch::x86_64::__cpuid(0x80000001).edx };
    if !((execute_disable_supported_bit & (1 << 20)) == (1 << 20)) {
        return Err(UnsupportedFeaturesError::ExecuteDisable);
    }

    Ok(())
}

/// Enables the required features.
pub fn enable_required_features() -> Result<(), UnsupportedFeaturesError> {
    test_required_feature_support()?;

    unsafe {
        core::arch::asm!(
            // Enable the execute-disable feature.
            "mov ecx, 0xC0000080",
            "rdmsr",
            "or eax, 0x400",
            "wrmsr",

            // Enable write protection feature.
            "mov rax, cr0",
            "or rax, 0x10000",
            "mov cr0, rax",
            out("eax") _,
        )
    }

    Ok(())
}

/// Various unsupported features.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum UnsupportedFeaturesError {
    /// The execute-disable feature is not supported by this processor.
    ExecuteDisable,
}

impl fmt::Display for UnsupportedFeaturesError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ExecuteDisable => f.pad("no execute "),
        }
    }
}

core::arch::global_asm!(
    "context_switch_start:",
    "xor rbp, rbp",
    "mov cr3, rax",
    "mov rsp, rcx",
    "push rbp",
    "jmp rdx",
    "context_switch_end:"
);

/// Allocates memory for the context switch and maps it into the application space.
pub fn setup_context_switch(
    application_map: &mut ApplicationMemoryMap,
) -> Result<VirtualAddress, SetupContextSwitchError> {
    extern "C" {
        #[link_name = "context_switch_start"]
        static CONTEXT_SWITCH_START: core::ffi::c_void;
        #[link_name = "context_switch_end"]
        static CONTEXT_SWITCH_END: core::ffi::c_void;
    }

    let ptr = core::ptr::addr_of!(CONTEXT_SWITCH_START).cast::<u8>();
    let size: usize = unsafe { core::ptr::addr_of!(CONTEXT_SWITCH_END).byte_offset_from(ptr) }
        .try_into()
        .unwrap();
    let context_switch = unsafe { core::slice::from_raw_parts(ptr, size) };

    let page_count = size.div_ceil(4096);
    let allocation = application_map.allocate_identity(page_count, Protection::Executable)?;

    let mut allocated_bytes = allocation
        .as_slice()
        .expect("bug in application memory map");
    MaybeUninit::copy_from_slice(&mut allocated_bytes[..size], context_switch);

    Ok(allocation.page_range().start_address())
}

/// Various errors that can occur while setting up the context switch routine.
pub enum SetupContextSwitchError {
    /// An error occurred while allocating an [`Entry`] for the context switch routine.
    AllocationError(AllocateEntryError),
}

impl From<AllocateEntryError> for SetupContextSwitchError {
    fn from(value: AllocateEntryError) -> Self {
        Self::AllocationError(value)
    }
}

impl fmt::Display for SetupContextSwitchError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

/// Jumps to the context switch for the application.
///
/// # Safety
/// - The architectural structures must have been loaded.
/// - `context_switch` must point to the virtual address of a identity mapped context switch
///     routine.
/// - `top_level_page_table` must point to a properly set up paging structure.
/// - `stack_top` must be the virtual address of the top of a writable stack.
/// - `entry_point` must point to the entry point of the application.
/// - `bootloader_response` must be the virtual address of the bootloader response.
pub unsafe fn jump_to_context_switch(
    context_switch: VirtualAddress,
    top_level_page_table: PhysicalAddress,
    stack_top: VirtualAddress,
    entry_point: VirtualAddress,
    bootloader_response: VirtualAddress,
) -> ! {
    unsafe {
        core::arch::asm!(
            "cli",
            "jmp {context_switch}",
            context_switch = in(reg) context_switch.value(),
            in("rax") top_level_page_table.value(),
            in("rcx") stack_top.value(),
            in("rdx") entry_point.value(),
            in("rdi") bootloader_response.value(),
            options(noreturn, nostack)
        )
    }
}
