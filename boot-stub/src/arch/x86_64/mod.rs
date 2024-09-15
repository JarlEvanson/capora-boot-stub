//! Code for the `x86_64` bootloader.

use core::fmt;

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
