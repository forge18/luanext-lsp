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
