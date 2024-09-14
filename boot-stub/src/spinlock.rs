//! Simple spinlock implementation.

use core::{
    cell::UnsafeCell,
    error, fmt,
    ops::{Deref, DerefMut},
    sync::atomic::{AtomicBool, Ordering},
};

/// The locking component of a [`Spinlock`].
#[derive(Debug)]
pub struct RawSpinlock {
    /// The lock.
    lock: AtomicBool,
}

impl RawSpinlock {
    /// Creates a new [`RawSpinlock`] in the unlocked state.
    pub const fn new() -> Self {
        Self {
            lock: AtomicBool::new(false),
        }
    }

    /// Locks the [`RawSpinlock`], spinning until the lock is acquired.
    ///
    /// This function does not return until the lock has been acquired.
    pub fn lock(&self) {
        let mut was_locked = self.lock.load(Ordering::Relaxed);

        loop {
            if !was_locked {
                match self
                    .lock
                    .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
                {
                    Ok(_) => break,
                    Err(state) => was_locked = state,
                }
            }

            core::hint::spin_loop();
        }
    }

    /// Attempts to lock the [`RawSpinlock`].
    ///
    /// This function does not spin or block.
    ///
    /// # Errors
    /// If the [`RawSpinlock`] was already locked, then this calll will return an [`Err`].
    pub fn try_lock(&self) -> Result<(), SpinlockAcquisitionError> {
        if !self.lock.load(Ordering::Relaxed)
            && self
                .lock
                .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
        {
            Ok(())
        } else {
            Err(SpinlockAcquisitionError)
        }
    }

    /// Method to make unlocking of a mutex more explicit.
    pub fn unlock(&self) {
        self.lock.store(false, Ordering::Release);
    }
}

impl Default for RawSpinlock {
    fn default() -> Self {
        Self::new()
    }
}

/// A mutual exclusion primitive useful to protecting shared data.
///
/// This mutex will spin waiting for the lock to become available.
pub struct Spinlock<T: ?Sized> {
    /// The lock.
    lock: RawSpinlock,
    /// The value protected by the [`Spinlock`].
    value: UnsafeCell<T>,
}

// SAFETY:
// Nothing about `Spinlock<T>` changes whether it
// is safe to send `T` across threads.
unsafe impl<T: ?Sized + Send> Send for Spinlock<T> {}

// SAFETY:
// If `T` is safe to send across threads, then `Spinlock<T>`
// makes it safe to access from multiple threads simultaneously.
unsafe impl<T: ?Sized + Send> Sync for Spinlock<T> {}

impl<T> Spinlock<T> {
    /// Creates a new [`Spinlock`] in an unlocked state ready for use.
    pub const fn new(value: T) -> Self {
        Self {
            lock: RawSpinlock::new(),
            value: UnsafeCell::new(value),
        }
    }

    /// Consumes this mutex, returning the underlying data.
    pub fn into_inner(self) -> T {
        self.value.into_inner()
    }
}

impl<T: ?Sized> Spinlock<T> {
    /// Acquires a mutex, spinning until it is able to do so.
    ///
    /// This function will spin until it is available to acquire the mutex. Upon returning, the
    /// the context is the only context with the lock held. A RAII guard is returned to allow
    /// scoped unlock of the lock.
    pub fn lock(&self) -> SpinlockGuard<T> {
        self.lock.lock();

        SpinlockGuard {
            lock: &self.lock,
            value: &self.value,
        }
    }

    /// Attempts to acquire this lock.
    ///
    /// If the lock could not be acquired at this time, then [`Err`] is returned. Otherwise, a RAII
    /// guard is returned. The lock will be unlocked when the guard is dropped.
    ///
    /// This function does not block.
    ///
    /// # Errors
    /// If the [`Spinlock`] could not be acquire because it is already locked, then this call will
    /// return an [`Err`].
    pub fn try_lock(&self) -> Result<SpinlockGuard<T>, SpinlockAcquisitionError> {
        self.lock.try_lock().map(|()| SpinlockGuard {
            lock: &self.lock,
            value: &self.value,
        })
    }

    /// Method that makes unlocking a mutex more explicit.
    pub fn unlock(guard: SpinlockGuard<T>) {
        guard.lock.unlock()
    }

    /// Returns a mutable reference to the underlying data.
    ///
    /// Since this call borrows the [`Spinlock`] mutably, no actual locking needs to take place
    /// - the mutable borrow statically guarantees no locks exist.
    pub fn get_mut(&mut self) -> &mut T {
        self.value.get_mut()
    }
}

/// A RAII implementation of a "scoped lock" of a [`Spinlock`]. When this structure is dropped, the
/// lock will be unlocked.
///
/// The data protected by the mutex can be accessed through this guard via its [`Deref`] and
/// [`DerefMut`] implementations.
///
/// This structure is created by the [`Spinlock::lock()`] and [`Spinlock::try_lock()`] methods.
pub struct SpinlockGuard<'a, T: ?Sized> {
    lock: &'a RawSpinlock,
    value: &'a UnsafeCell<T>,
}

impl<'a, T: ?Sized> SpinlockGuard<'a, T> {
    /// Returns a new [`SpinlockGuard`] that allows for safe access to `value`.
    ///
    /// # Safety
    /// - `lock` must be locked.
    /// - `value` must be safe to return immutable or mutable references to until `lock` is
    ///     unlocked.
    pub unsafe fn new(lock: &'a RawSpinlock, value: &'a UnsafeCell<T>) -> Self {
        Self { lock, value }
    }
}

impl<T: ?Sized> Deref for SpinlockGuard<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let value_ptr = self.value.get();

        // SAFETY:
        // We have exclusive access to the value pointed to by `value_ptr`.
        unsafe { &*value_ptr }
    }
}

impl<T: ?Sized> DerefMut for SpinlockGuard<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        let value_ptr = self.value.get();

        // SAFETY:
        // We have exclusive access to the value pointed to by `value_ptr`.
        unsafe { &mut *value_ptr }
    }
}

impl<T: ?Sized> Drop for SpinlockGuard<'_, T> {
    fn drop(&mut self) {
        self.lock.unlock();
    }
}

/// Represents the failure to acquire a spinlock.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SpinlockAcquisitionError;

impl fmt::Display for SpinlockAcquisitionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad("try_lock failed because operation would block")
    }
}

impl error::Error for SpinlockAcquisitionError {}
