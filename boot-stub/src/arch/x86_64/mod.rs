//! Code for the `x86_64` bootloader.

use core::{fmt, mem::MaybeUninit};

use crate::mapper::{ApplicationMemoryMap, Protection, Usage};

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
    let allocation = application_map.allocate_identity(
        GDT.len().div_ceil(4096) as u64,
        Protection::Readable,
        Usage::General,
    );
    MaybeUninit::copy_from_slice(&mut allocation.as_bytes_mut()[..GDT.len()], GDT);

    Ok(ArchitecturalStructures {
        gdt: allocation.page_range().virtual_address(),
    })
}

/// Various errors that can occur while creating architectural structures.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub enum CreateArchitecturalStructuresError {}

impl fmt::Display for CreateArchitecturalStructuresError {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
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
        offset: data.gdt,
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
    gdt: u64,
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
