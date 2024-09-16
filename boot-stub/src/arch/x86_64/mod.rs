//! Code for the `x86_64` bootloader.

use core::{fmt, mem::MaybeUninit};

use crate::{
    memory_map::{AllocateEntryError, ApplicationMemoryMap, Protection},
    memory_structs::{PhysicalAddress, VirtualAddress},
};

const GDT: &[u8] = [
    // Null GDT entry.
    0x00_0_0_00_000000_0000u64.to_ne_bytes(),
    // Kernel code entry.
    0x00_A_F_9B_000000_FFFFu64.to_ne_bytes(),
    // Kernel data entry.
    0x00_C_F_93_000000_FFFFu64.to_ne_bytes(),
]
.as_flattened();

/// Constructs architecture dependent structures.
pub fn create_architectural_structures(
    application_map: &mut ApplicationMemoryMap,
) -> Result<ArchitecturalStructures, CreateArchitecturalStructuresError> {
    let allocation =
        application_map.allocate_identity(GDT.len().div_ceil(4096), Protection::Readable)?;

    let mut allocated_bytes = allocation
        .as_slice()
        .expect("bug in application memory map");
    MaybeUninit::copy_from_slice(&mut allocated_bytes[..GDT.len()], GDT);

    Ok(ArchitecturalStructures {
        gdt: allocation.page_range().start_address(),
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
            inout("rax") &gdtr.size => _,
        )
    }
}

/// Data needed to set the architectural structures to be used.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct ArchitecturalStructures {
    /// The virtual address of the global descriptor table.
    gdt: VirtualAddress,
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
