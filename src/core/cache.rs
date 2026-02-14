//! Cache infrastructure for LSP document analysis results.
//!
//! Provides version-tracked caching primitives that ensure cached data
//! is never served stale. Every cache entry is tagged with the document
//! version that produced it, and invalidation is driven by version changes.
//!
//! # Architecture
//!
//! - [`VersionedCache<T>`]: Single-value cache with version tracking
//! - [`BoundedPositionCache<T>`]: Position-keyed cache with LRU eviction
//! - [`DocumentCache`]: Per-document container holding all cache types
//! - [`CacheStats`]: Hit/miss counters for observability

// Some cache fields still use placeholder types (diagnostics, type_info) -
// will be replaced with real types in subsequent phases.
#![allow(dead_code)]

use lsp_types::{CompletionItem, Hover, Position, SemanticTokens};
use std::collections::{HashMap, VecDeque};
use std::time::Instant;

/// A cache entry that tracks the document version it was computed from.
///
/// A cached value is valid only when the current document version matches
/// the stored version. This is the fundamental caching primitive used by
/// document-level caches (semantic tokens, diagnostics).
#[derive(Debug)]
pub struct VersionedCache<T> {
    value: Option<T>,
    version: i32,
    last_updated: Instant,
}

impl<T> VersionedCache<T> {
    /// Create a new empty cache.
    pub fn new() -> Self {
        Self {
            value: None,
            version: -1,
            last_updated: Instant::now(),
        }
    }

    /// Check if the cache holds a valid value for the given version.
    pub fn is_valid(&self, current_version: i32) -> bool {
        self.value.is_some() && self.version == current_version
    }

    /// Get the cached value if it matches the current version.
    pub fn get_if_valid(&self, current_version: i32) -> Option<&T> {
        if self.is_valid(current_version) {
            self.value.as_ref()
        } else {
            None
        }
    }

    /// Store a new value with the given version.
    pub fn set(&mut self, value: T, version: i32) {
        self.value = Some(value);
        self.version = version;
        self.last_updated = Instant::now();
    }

    /// Clear the cached value.
    pub fn invalidate(&mut self) {
        self.value = None;
    }

    /// Get the version this cache was computed from.
    pub fn version(&self) -> i32 {
        self.version
    }

    /// Get when this cache was last updated.
    pub fn last_updated(&self) -> Instant {
        self.last_updated
    }
}

impl<T> Default for VersionedCache<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// A position-keyed cache with bounded size and LRU eviction.
///
/// Stores cached values indexed by document position, with a maximum
/// capacity. When the cache is full, the least recently used entry
/// is evicted. The entire cache is invalidated when the document
/// version changes (since positions may shift).
#[derive(Debug)]
pub struct BoundedPositionCache<T> {
    entries: HashMap<Position, T>,
    access_order: VecDeque<Position>,
    max_capacity: usize,
    version: i32,
}

impl<T> BoundedPositionCache<T> {
    /// Create a new cache with the given maximum capacity.
    pub fn new(max_capacity: usize) -> Self {
        Self {
            entries: HashMap::with_capacity(max_capacity),
            access_order: VecDeque::with_capacity(max_capacity),
            max_capacity,
            version: -1,
        }
    }

    /// Get a cached value for the given position, if the version matches.
    ///
    /// Updates LRU order on hit, so this takes `&mut self`.
    pub fn get(&mut self, position: Position, current_version: i32) -> Option<&T> {
        if self.version != current_version {
            return None;
        }

        if self.entries.contains_key(&position) {
            // Move to back of access order (most recently used)
            self.access_order.retain(|p| *p != position);
            self.access_order.push_back(position);
            self.entries.get(&position)
        } else {
            None
        }
    }

    /// Store a value for the given position at the given version.
    ///
    /// Evicts the least recently used entry if at capacity.
    pub fn insert(&mut self, position: Position, value: T, version: i32) {
        // Version change means all existing entries are stale
        if self.version != version {
            self.invalidate_all();
            self.version = version;
        }

        // If already present, update in place
        if self.entries.contains_key(&position) {
            self.entries.insert(position, value);
            self.access_order.retain(|p| *p != position);
            self.access_order.push_back(position);
            return;
        }

        // Evict LRU if at capacity
        if self.entries.len() >= self.max_capacity {
            if let Some(lru_key) = self.access_order.pop_front() {
                self.entries.remove(&lru_key);
            }
        }

        self.entries.insert(position, value);
        self.access_order.push_back(position);
    }

    /// Remove entries whose positions fall within the given range (inclusive).
    pub fn invalidate_range(&mut self, start: Position, end: Position) {
        let positions_to_remove: Vec<Position> = self
            .entries
            .keys()
            .filter(|pos| **pos >= start && **pos <= end)
            .copied()
            .collect();

        for pos in &positions_to_remove {
            self.entries.remove(pos);
        }
        self.access_order
            .retain(|p| !positions_to_remove.contains(p));
    }

    /// Clear all entries.
    pub fn invalidate_all(&mut self) {
        self.entries.clear();
        self.access_order.clear();
    }

    /// Number of entries currently cached.
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// The maximum capacity.
    pub fn capacity(&self) -> usize {
        self.max_capacity
    }
}

/// Default max entries for hover cache.
const DEFAULT_HOVER_CACHE_CAPACITY: usize = 100;
/// Default max entries for completion cache.
const DEFAULT_COMPLETION_CACHE_CAPACITY: usize = 50;
/// Default max entries for type info cache.
const DEFAULT_TYPE_INFO_CACHE_CAPACITY: usize = 100;

/// Per-document cache container holding all cached analysis results.
///
/// Aggregates all cache types for a single document. Each field
/// corresponds to an LSP feature that can cache its results.
#[derive(Debug)]
pub struct DocumentCache {
    /// Cached semantic tokens for the entire document.
    pub(crate) semantic_tokens: VersionedCache<SemanticTokens>,
    /// Cached hover information per position.
    pub(crate) hover_cache: BoundedPositionCache<Hover>,
    /// Cached completion results per position.
    pub(crate) completion_cache: BoundedPositionCache<Vec<CompletionItem>>,
    /// Cached diagnostics for the document.
    pub(crate) diagnostics: VersionedCache<()>,
    /// Cached type information per position.
    pub(crate) type_info_cache: BoundedPositionCache<()>,
    /// Cache statistics for observability.
    stats: CacheStats,
}

impl DocumentCache {
    /// Create a new empty document cache.
    pub fn new() -> Self {
        Self {
            semantic_tokens: VersionedCache::new(),
            hover_cache: BoundedPositionCache::new(DEFAULT_HOVER_CACHE_CAPACITY),
            completion_cache: BoundedPositionCache::new(DEFAULT_COMPLETION_CACHE_CAPACITY),
            diagnostics: VersionedCache::new(),
            type_info_cache: BoundedPositionCache::new(DEFAULT_TYPE_INFO_CACHE_CAPACITY),
            stats: CacheStats::default(),
        }
    }

    /// Invalidate all caches (called on `didChange`).
    pub fn invalidate_all(&mut self) {
        self.semantic_tokens.invalidate();
        self.hover_cache.invalidate_all();
        self.completion_cache.invalidate_all();
        self.diagnostics.invalidate();
        self.type_info_cache.invalidate_all();
    }

    /// Invalidate position-based caches within a range.
    ///
    /// Semantic tokens and diagnostics are document-wide, so they are
    /// always invalidated. Position-based caches only clear entries
    /// that fall within the affected range.
    pub fn partial_invalidate(&mut self, start: Position, end: Position) {
        self.semantic_tokens.invalidate();
        self.diagnostics.invalidate();

        self.hover_cache.invalidate_range(start, end);
        self.completion_cache.invalidate_range(start, end);
        self.type_info_cache.invalidate_range(start, end);
    }

    /// Get a reference to the cache statistics.
    pub fn stats(&self) -> &CacheStats {
        &self.stats
    }

    /// Get a mutable reference to the cache statistics.
    pub fn stats_mut(&mut self) -> &mut CacheStats {
        &mut self.stats
    }

    /// Reset statistics counters.
    pub fn reset_stats(&mut self) {
        self.stats = CacheStats::default();
    }
}

impl Default for DocumentCache {
    fn default() -> Self {
        Self::new()
    }
}

/// Hit/miss counters for cache effectiveness monitoring.
///
/// Logged when `LUANEXT_LSP_CACHE_STATS=1` environment variable is set.
#[derive(Debug, Default, Clone)]
pub struct CacheStats {
    /// Total cache hits.
    pub hits: usize,
    /// Total cache misses.
    pub misses: usize,
}

impl CacheStats {
    /// Record a cache hit.
    pub fn record_hit(&mut self) {
        self.hits += 1;
    }

    /// Record a cache miss.
    pub fn record_miss(&mut self) {
        self.misses += 1;
    }

    /// Hit rate as a fraction (0.0 to 1.0). Returns 0.0 if no lookups.
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total > 0 {
            self.hits as f64 / total as f64
        } else {
            0.0
        }
    }

    /// Log stats if `LUANEXT_LSP_CACHE_STATS` environment variable is set.
    pub fn maybe_log(&self, cache_name: &str) {
        if std::env::var("LUANEXT_LSP_CACHE_STATS").is_ok() {
            tracing::info!(
                "Cache stats [{}]: hits={}, misses={}, hit_rate={:.1}%",
                cache_name,
                self.hits,
                self.misses,
                self.hit_rate() * 100.0
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── VersionedCache tests ──────────────────────────────────────────

    #[test]
    fn test_new_cache_is_empty() {
        let cache = VersionedCache::<String>::new();
        assert!(!cache.is_valid(0));
        assert!(cache.get_if_valid(0).is_none());
        assert_eq!(cache.version(), -1);
    }

    #[test]
    fn test_set_and_get_valid() {
        let mut cache = VersionedCache::new();
        cache.set(vec![1, 2, 3], 5);
        assert_eq!(cache.get_if_valid(5), Some(&vec![1, 2, 3]));
    }

    #[test]
    fn test_get_stale_version_returns_none() {
        let mut cache = VersionedCache::new();
        cache.set(42, 5);
        assert!(cache.get_if_valid(6).is_none());
        assert!(cache.get_if_valid(4).is_none());
    }

    #[test]
    fn test_invalidate_clears_value() {
        let mut cache = VersionedCache::new();
        cache.set("hello".to_string(), 1);
        assert!(cache.is_valid(1));
        cache.invalidate();
        assert!(!cache.is_valid(1));
        assert!(cache.get_if_valid(1).is_none());
    }

    #[test]
    fn test_set_overwrites_previous() {
        let mut cache = VersionedCache::new();
        cache.set(10, 1);
        cache.set(20, 2);
        assert!(cache.get_if_valid(1).is_none());
        assert_eq!(cache.get_if_valid(2), Some(&20));
    }

    #[test]
    fn test_version_tracking() {
        let mut cache = VersionedCache::new();
        assert_eq!(cache.version(), -1);
        cache.set((), 42);
        assert_eq!(cache.version(), 42);
    }

    #[test]
    fn test_last_updated_changes_on_set() {
        let mut cache = VersionedCache::new();
        let t1 = cache.last_updated();
        std::thread::sleep(std::time::Duration::from_millis(1));
        cache.set((), 1);
        assert!(cache.last_updated() >= t1);
    }

    #[test]
    fn test_default_trait() {
        let cache = VersionedCache::<i32>::default();
        assert!(!cache.is_valid(0));
        assert_eq!(cache.version(), -1);
    }

    #[test]
    fn test_get_if_valid_on_empty() {
        let cache = VersionedCache::<bool>::new();
        assert!(cache.get_if_valid(-1).is_none());
        assert!(cache.get_if_valid(0).is_none());
    }

    // ── BoundedPositionCache tests ────────────────────────────────────

    fn pos(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    #[test]
    fn test_bounded_insert_and_get() {
        let mut cache = BoundedPositionCache::new(10);
        cache.insert(pos(1, 5), "hover_info".to_string(), 1);
        assert_eq!(cache.get(pos(1, 5), 1), Some(&"hover_info".to_string()));
    }

    #[test]
    fn test_bounded_get_wrong_version_returns_none() {
        let mut cache = BoundedPositionCache::new(10);
        cache.insert(pos(1, 5), 42, 1);
        assert!(cache.get(pos(1, 5), 2).is_none());
    }

    #[test]
    fn test_lru_eviction_at_capacity() {
        let mut cache = BoundedPositionCache::new(3);
        cache.insert(pos(0, 0), "a", 1);
        cache.insert(pos(1, 0), "b", 1);
        cache.insert(pos(2, 0), "c", 1);
        assert_eq!(cache.len(), 3);

        // Insert a 4th entry - should evict the LRU (pos(0,0))
        cache.insert(pos(3, 0), "d", 1);
        assert_eq!(cache.len(), 3);
        assert!(cache.get(pos(0, 0), 1).is_none());
        assert_eq!(cache.get(pos(3, 0), 1), Some(&"d"));
    }

    #[test]
    fn test_access_order_updates_on_get() {
        let mut cache = BoundedPositionCache::new(3);
        cache.insert(pos(0, 0), "a", 1);
        cache.insert(pos(1, 0), "b", 1);
        cache.insert(pos(2, 0), "c", 1);

        // Access the first entry, making it most recently used
        cache.get(pos(0, 0), 1);

        // Insert a 4th entry - should evict pos(1,0) now (the LRU)
        cache.insert(pos(3, 0), "d", 1);
        assert!(cache.get(pos(1, 0), 1).is_none()); // evicted
        assert_eq!(cache.get(pos(0, 0), 1), Some(&"a")); // still there
    }

    #[test]
    fn test_bounded_invalidate_all() {
        let mut cache = BoundedPositionCache::new(10);
        cache.insert(pos(0, 0), 1, 1);
        cache.insert(pos(1, 0), 2, 1);
        cache.invalidate_all();
        assert!(cache.is_empty());
        assert_eq!(cache.len(), 0);
    }

    #[test]
    fn test_invalidate_range() {
        let mut cache = BoundedPositionCache::new(10);
        cache.insert(pos(0, 0), "line0", 1);
        cache.insert(pos(1, 5), "line1", 1);
        cache.insert(pos(2, 3), "line2", 1);
        cache.insert(pos(5, 0), "line5", 1);

        // Invalidate lines 1-3
        cache.invalidate_range(pos(1, 0), pos(3, 0));

        assert_eq!(cache.get(pos(0, 0), 1), Some(&"line0")); // before range
        assert!(cache.get(pos(1, 5), 1).is_none()); // in range
        assert!(cache.get(pos(2, 3), 1).is_none()); // in range
        assert_eq!(cache.get(pos(5, 0), 1), Some(&"line5")); // after range
    }

    #[test]
    fn test_invalidate_range_preserves_outside() {
        let mut cache = BoundedPositionCache::new(10);
        cache.insert(pos(0, 0), 1, 1);
        cache.insert(pos(10, 0), 2, 1);
        cache.invalidate_range(pos(3, 0), pos(7, 0));
        assert_eq!(cache.len(), 2); // nothing in range to remove
    }

    #[test]
    fn test_insert_same_position_updates() {
        let mut cache = BoundedPositionCache::new(10);
        cache.insert(pos(1, 0), "old", 1);
        cache.insert(pos(1, 0), "new", 1);
        assert_eq!(cache.get(pos(1, 0), 1), Some(&"new"));
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn test_version_change_clears_all() {
        let mut cache = BoundedPositionCache::new(10);
        cache.insert(pos(0, 0), "v1", 1);
        cache.insert(pos(1, 0), "v1", 1);

        // Insert with new version - should clear old entries
        cache.insert(pos(2, 0), "v2", 2);
        assert_eq!(cache.len(), 1);
        assert!(cache.get(pos(0, 0), 2).is_none());
        assert_eq!(cache.get(pos(2, 0), 2), Some(&"v2"));
    }

    #[test]
    fn test_len_and_capacity() {
        let cache = BoundedPositionCache::<i32>::new(42);
        assert_eq!(cache.capacity(), 42);
        assert_eq!(cache.len(), 0);
        assert!(cache.is_empty());
    }

    // ── DocumentCache tests ───────────────────────────────────────────

    #[test]
    fn test_new_document_cache() {
        let cache = DocumentCache::new();
        assert!(!cache.semantic_tokens.is_valid(0));
        assert!(!cache.diagnostics.is_valid(0));
        assert!(cache.hover_cache.is_empty());
        assert!(cache.completion_cache.is_empty());
        assert!(cache.type_info_cache.is_empty());
    }

    fn empty_tokens() -> SemanticTokens {
        SemanticTokens {
            result_id: None,
            data: vec![],
        }
    }

    fn test_hover() -> Hover {
        use lsp_types::{HoverContents, MarkupContent, MarkupKind};
        Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "test hover".to_string(),
            }),
            range: None,
        }
    }

    fn test_completion_items() -> Vec<CompletionItem> {
        vec![CompletionItem {
            label: "test".to_string(),
            ..Default::default()
        }]
    }

    #[test]
    fn test_document_cache_invalidate_all() {
        let mut cache = DocumentCache::new();
        cache.semantic_tokens.set(empty_tokens(), 1);
        cache.diagnostics.set((), 1);
        cache.hover_cache.insert(pos(0, 0), test_hover(), 1);
        cache
            .completion_cache
            .insert(pos(0, 0), test_completion_items(), 1);
        cache.type_info_cache.insert(pos(0, 0), (), 1);

        cache.invalidate_all();

        assert!(!cache.semantic_tokens.is_valid(1));
        assert!(!cache.diagnostics.is_valid(1));
        assert!(cache.hover_cache.is_empty());
        assert!(cache.completion_cache.is_empty());
        assert!(cache.type_info_cache.is_empty());
    }

    #[test]
    fn test_document_cache_partial_invalidate() {
        let mut cache = DocumentCache::new();
        cache.semantic_tokens.set(empty_tokens(), 1);
        cache.diagnostics.set((), 1);
        cache.hover_cache.insert(pos(0, 0), test_hover(), 1);
        cache.hover_cache.insert(pos(5, 0), test_hover(), 1);
        cache.hover_cache.insert(pos(10, 0), test_hover(), 1);

        cache.partial_invalidate(pos(3, 0), pos(7, 0));

        // Document-wide caches are always invalidated
        assert!(!cache.semantic_tokens.is_valid(1));
        assert!(!cache.diagnostics.is_valid(1));
        // Position caches: only affected range is cleared
        assert_eq!(cache.hover_cache.len(), 2); // pos(0,0) and pos(10,0) remain
    }

    #[test]
    fn test_stats_recording() {
        let mut stats = CacheStats::default();
        assert_eq!(stats.hit_rate(), 0.0);

        stats.record_hit();
        stats.record_hit();
        stats.record_miss();
        assert_eq!(stats.hits, 2);
        assert_eq!(stats.misses, 1);
        assert!((stats.hit_rate() - 2.0 / 3.0).abs() < f64::EPSILON);
    }

    #[test]
    fn test_document_cache_default_trait() {
        let cache = DocumentCache::default();
        assert!(cache.hover_cache.is_empty());
        assert_eq!(cache.hover_cache.capacity(), DEFAULT_HOVER_CACHE_CAPACITY);
        assert_eq!(
            cache.completion_cache.capacity(),
            DEFAULT_COMPLETION_CACHE_CAPACITY
        );
        assert_eq!(
            cache.type_info_cache.capacity(),
            DEFAULT_TYPE_INFO_CACHE_CAPACITY
        );
    }
}
