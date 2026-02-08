//! Arena pool for LSP parsing.
//!
//! Provides the same arena pooling functionality as typedlua-core
//! but for the LSP package which doesn't depend on typedlua-core.

use bumpalo::Bump;
use once_cell::sync::Lazy;
use parking_lot::Mutex;

/// Global pool of reusable arenas for long-lived LSP process.
static ARENA_POOL: Lazy<Mutex<Vec<Bump>>> = Lazy::new(|| Mutex::new(Vec::new()));

/// Maximum number of arenas to keep in the pool.
const MAX_POOL_SIZE: usize = 16;

/// Execute a function with a pooled arena.
pub fn with_pooled_arena<F, R>(f: F) -> R
where
    F: FnOnce(&Bump) -> R,
{
    // Try to get arena from pool (lock is held briefly)
    let mut arena = ARENA_POOL.lock().pop().unwrap_or_default();

    // Reset arena to clear previous allocations
    arena.reset();

    // Execute user code
    let result = f(&arena);

    // Return arena to pool if under capacity
    let mut pool = ARENA_POOL.lock();
    if pool.len() < MAX_POOL_SIZE {
        pool.push(arena);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_with_pooled_arena_returns_value() {
        let result = with_pooled_arena(|_arena| 42);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_pool_size_never_exceeds_max() {
        // Pool size should never exceed MAX_POOL_SIZE regardless of concurrent usage
        for _ in 0..MAX_POOL_SIZE + 10 {
            with_pooled_arena(|_arena| {});
        }
        assert!(ARENA_POOL.lock().len() <= MAX_POOL_SIZE);
    }

    #[test]
    fn test_nested_calls_use_separate_arenas() {
        let result = with_pooled_arena(|outer| {
            let inner_result = with_pooled_arena(|inner| {
                assert!(!std::ptr::eq(outer, inner));
                "inner"
            });
            (inner_result, "outer")
        });

        assert_eq!(result, ("inner", "outer"));
    }

    #[test]
    fn test_arena_allocation_works() {
        // Verify arena is usable for allocation and doesn't panic on reuse
        with_pooled_arena(|arena| {
            let val = arena.alloc(42u64);
            assert_eq!(*val, 42);
        });

        with_pooled_arena(|arena| {
            let val = arena.alloc(99u64);
            assert_eq!(*val, 99);
        });
    }
}
