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
//! - [`ModuleDependencyGraph`]: Tracks import relationships for cascade invalidation
//! - [`ModuleExportsEntry`]: Cached type-checked exports for cross-file queries

use lsp_types::{CompletionItem, Diagnostic, Hover, Position, SemanticTokens};
use std::collections::{HashMap, HashSet, VecDeque};

/// Cached result of a single type-check run, extracted to owned types.
///
/// Created once per document version by `DiagnosticsProvider::ensure_type_checked()`.
/// All LSP features (hover, completion, diagnostics) query this cache
/// instead of running their own lex→parse→typecheck pass.
#[derive(Debug, Clone)]
pub struct TypeCheckResult {
    /// Symbol name → owned symbol info (kind + type display string)
    pub symbols: HashMap<String, CachedSymbolInfo>,
    /// Diagnostics produced during this type-check run
    pub diagnostics: Vec<Diagnostic>,
}

/// Owned symbol information extracted from the type checker's symbol table.
/// Stores only what LSP features need: the kind label and type display string.
#[derive(Debug, Clone)]
pub struct CachedSymbolInfo {
    /// Display label for the symbol kind ("const", "let", "function", etc.)
    pub kind: String,
    /// Display string for the symbol's type ("number", "string", "function", etc.)
    pub type_display: String,
}

/// A cache entry that tracks the document version it was computed from.
///
/// A cached value is valid only when the current document version matches
/// the stored version. This is the fundamental caching primitive used by
/// document-level caches (semantic tokens, diagnostics).
#[derive(Debug)]
pub struct VersionedCache<T> {
    value: Option<T>,
    version: i32,
}

impl<T> VersionedCache<T> {
    /// Create a new empty cache.
    pub fn new() -> Self {
        Self {
            value: None,
            version: -1,
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
    }

    /// Clear the cached value.
    pub fn invalidate(&mut self) {
        self.value = None;
    }

    /// Get the version this cache was computed from.
    pub fn version(&self) -> i32 {
        self.version
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
    #[cfg(test)]
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
    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// Whether the cache is empty.
    #[cfg(test)]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// The maximum capacity.
    #[cfg(test)]
    pub fn capacity(&self) -> usize {
        self.max_capacity
    }
}

/// Default max entries for hover cache.
const DEFAULT_HOVER_CACHE_CAPACITY: usize = 100;
/// Default max entries for completion cache.
const DEFAULT_COMPLETION_CACHE_CAPACITY: usize = 50;

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
    /// Cached type-check result (symbol table + diagnostics from a single type-check run).
    /// Populated by `DiagnosticsProvider::ensure_type_checked()`, consumed by hover/completion.
    pub(crate) type_check_result: VersionedCache<TypeCheckResult>,
    /// Last diagnostics sent to the client (for deduplication).
    /// NOT version-gated: stores whatever was last published, regardless of version.
    /// This enables skipping `PublishDiagnostics` when edits don't change errors.
    pub(crate) last_published_diagnostics: Option<Vec<Diagnostic>>,
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
            type_check_result: VersionedCache::new(),
            last_published_diagnostics: None,
            stats: CacheStats::default(),
        }
    }

    /// Invalidate all caches (called on `didChange`).
    ///
    /// Note: `last_published_diagnostics` is intentionally NOT cleared here.
    /// It stores what the client last saw and is used for deduplication.
    pub fn invalidate_all(&mut self) {
        self.semantic_tokens.invalidate();
        self.hover_cache.invalidate_all();
        self.completion_cache.invalidate_all();
        self.type_check_result.invalidate();
    }

    /// Invalidate position-based caches within a range.
    ///
    /// Semantic tokens and type-check results are document-wide, so they are
    /// always invalidated. Position-based caches only clear entries
    /// that fall within the affected range.
    #[cfg(test)]
    pub fn partial_invalidate(&mut self, start: Position, end: Position) {
        self.semantic_tokens.invalidate();
        self.type_check_result.invalidate();

        self.hover_cache.invalidate_range(start, end);
        self.completion_cache.invalidate_range(start, end);
    }

    /// Get a reference to the cache statistics.
    pub fn stats(&self) -> &CacheStats {
        &self.stats
    }

    /// Get a mutable reference to the cache statistics.
    pub fn stats_mut(&mut self) -> &mut CacheStats {
        &mut self.stats
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

/// Compute a content hash for cache staleness detection.
///
/// Uses `DefaultHasher` (non-cryptographic) to detect content changes
/// even across version resets (e.g., close + reopen a document).
pub fn content_hash(text: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    text.hash(&mut hasher);
    hasher.finish()
}

/// Cached type-checked exports for a module, shared across all consumers.
///
/// Avoids re-running lex→parse→typecheck when querying cross-file symbols.
/// Populated lazily by `DocumentManager::ensure_module_exports_cached()`.
#[derive(Debug, Clone)]
#[allow(dead_code)] // Public API - fields consumed by cross-file features
pub struct ModuleExportsEntry {
    /// Symbol name → owned type info (kind + type display)
    pub symbols: HashMap<String, CachedSymbolInfo>,
    /// Document version when this cache was computed
    pub version: i32,
    /// Content hash for detecting staleness after close+reopen (version reset)
    pub content_hash: u64,
}

/// Tracks import relationships between modules for cascade invalidation.
///
/// When module B changes and module A imports from B, A's caches must be
/// invalidated too. This graph enables efficient lookup of all affected modules.
#[derive(Debug, Default, Clone)]
pub struct ModuleDependencyGraph {
    /// module_id → set of module_ids that import from it ("who depends on me?")
    dependents: HashMap<String, HashSet<String>>,
    /// module_id → set of module_ids it imports from ("what do I depend on?")
    dependencies: HashMap<String, HashSet<String>>,
}

impl ModuleDependencyGraph {
    /// Create an empty dependency graph.
    pub fn new() -> Self {
        Self::default()
    }

    /// Record that `importer` imports from `imported`.
    pub fn add_dependency(&mut self, importer: &str, imported: &str) {
        self.dependents
            .entry(imported.to_string())
            .or_default()
            .insert(importer.to_string());
        self.dependencies
            .entry(importer.to_string())
            .or_default()
            .insert(imported.to_string());
    }

    /// Clear all dependencies for a module (called before re-indexing).
    pub fn clear_module(&mut self, module_id: &str) {
        // Remove this module from all dependents sets
        if let Some(deps) = self.dependencies.remove(module_id) {
            for dep in &deps {
                if let Some(set) = self.dependents.get_mut(dep) {
                    set.remove(module_id);
                    if set.is_empty() {
                        self.dependents.remove(dep);
                    }
                }
            }
        }
        // Also remove from dependents map (others may depend on this module)
        // But we don't remove the dependents entry itself — others still import from us
    }

    /// Get all modules that directly depend on (import from) the given module.
    #[allow(dead_code)] // Used in tests
    pub fn get_dependents(&self, module_id: &str) -> HashSet<String> {
        self.dependents.get(module_id).cloned().unwrap_or_default()
    }

    /// Get all modules transitively dependent on the given module.
    ///
    /// Uses BFS with a visited set to handle circular dependencies safely.
    pub fn get_transitive_dependents(&self, module_id: &str) -> HashSet<String> {
        let mut result = HashSet::new();
        let mut queue = VecDeque::new();

        // Seed with direct dependents
        if let Some(direct) = self.dependents.get(module_id) {
            for dep in direct {
                queue.push_back(dep.clone());
            }
        }

        while let Some(current) = queue.pop_front() {
            if !result.insert(current.clone()) {
                continue; // already visited
            }
            if let Some(next) = self.dependents.get(&current) {
                for dep in next {
                    if !result.contains(dep) {
                        queue.push_back(dep.clone());
                    }
                }
            }
        }

        result
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
        assert!(!cache.type_check_result.is_valid(0));
        assert!(cache.hover_cache.is_empty());
        assert!(cache.completion_cache.is_empty());
        assert!(cache.last_published_diagnostics.is_none());
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

    fn test_type_check_result() -> TypeCheckResult {
        let mut symbols = HashMap::new();
        symbols.insert(
            "x".to_string(),
            CachedSymbolInfo {
                kind: "let".to_string(),
                type_display: "number".to_string(),
            },
        );
        TypeCheckResult {
            symbols,
            diagnostics: vec![],
        }
    }

    #[test]
    fn test_document_cache_invalidate_all() {
        let mut cache = DocumentCache::new();
        cache.semantic_tokens.set(empty_tokens(), 1);
        cache.type_check_result.set(test_type_check_result(), 1);
        cache.hover_cache.insert(pos(0, 0), test_hover(), 1);
        cache
            .completion_cache
            .insert(pos(0, 0), test_completion_items(), 1);
        cache.last_published_diagnostics = Some(vec![]);

        cache.invalidate_all();

        assert!(!cache.semantic_tokens.is_valid(1));
        assert!(!cache.type_check_result.is_valid(1));
        assert!(cache.hover_cache.is_empty());
        assert!(cache.completion_cache.is_empty());
        // last_published_diagnostics survives invalidate_all (intentional)
        assert!(cache.last_published_diagnostics.is_some());
    }

    #[test]
    fn test_document_cache_partial_invalidate() {
        let mut cache = DocumentCache::new();
        cache.semantic_tokens.set(empty_tokens(), 1);
        cache.type_check_result.set(test_type_check_result(), 1);
        cache.hover_cache.insert(pos(0, 0), test_hover(), 1);
        cache.hover_cache.insert(pos(5, 0), test_hover(), 1);
        cache.hover_cache.insert(pos(10, 0), test_hover(), 1);

        cache.partial_invalidate(pos(3, 0), pos(7, 0));

        // Document-wide caches are always invalidated
        assert!(!cache.semantic_tokens.is_valid(1));
        assert!(!cache.type_check_result.is_valid(1));
        // Position caches: only affected range is cleared
        assert_eq!(cache.hover_cache.len(), 2); // pos(0,0) and pos(10,0) remain
    }

    #[test]
    fn test_last_published_diagnostics_survives_invalidate_all() {
        let mut cache = DocumentCache::new();
        cache.last_published_diagnostics = Some(vec![]);
        cache.invalidate_all();
        assert!(cache.last_published_diagnostics.is_some());
    }

    #[test]
    fn test_type_check_result_cached_and_invalidated() {
        let mut cache = VersionedCache::<TypeCheckResult>::new();
        let result = test_type_check_result();
        cache.set(result, 1);

        assert!(cache.is_valid(1));
        assert!(cache.get_if_valid(1).is_some());
        assert_eq!(cache.get_if_valid(1).unwrap().symbols.len(), 1);

        cache.invalidate();
        assert!(!cache.is_valid(1));
        assert!(cache.get_if_valid(1).is_none());
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
        assert!(!cache.type_check_result.is_valid(0));
        assert!(cache.last_published_diagnostics.is_none());
    }

    // ── ModuleDependencyGraph tests ─────────────────────────────────

    #[test]
    fn test_dependency_graph_add_and_get() {
        let mut graph = ModuleDependencyGraph::new();
        graph.add_dependency("/a.tl", "/b.tl");
        graph.add_dependency("/a.tl", "/c.tl");

        // /b.tl is depended on by /a.tl
        let dependents = graph.get_dependents("/b.tl");
        assert!(dependents.contains("/a.tl"));
        assert_eq!(dependents.len(), 1);

        // /c.tl is depended on by /a.tl
        let dependents = graph.get_dependents("/c.tl");
        assert!(dependents.contains("/a.tl"));
    }

    #[test]
    fn test_dependency_graph_clear_module() {
        let mut graph = ModuleDependencyGraph::new();
        graph.add_dependency("/a.tl", "/b.tl");
        graph.add_dependency("/a.tl", "/c.tl");
        graph.add_dependency("/d.tl", "/b.tl");

        graph.clear_module("/a.tl");

        // /a.tl should no longer appear as a dependent of /b.tl
        let dependents = graph.get_dependents("/b.tl");
        assert!(!dependents.contains("/a.tl"));
        // /d.tl should still be a dependent of /b.tl
        assert!(dependents.contains("/d.tl"));
        // /c.tl should have no dependents left
        assert!(graph.get_dependents("/c.tl").is_empty());
    }

    #[test]
    fn test_dependency_graph_transitive() {
        let mut graph = ModuleDependencyGraph::new();
        // A imports B, B imports C
        graph.add_dependency("/a.tl", "/b.tl");
        graph.add_dependency("/b.tl", "/c.tl");

        // Changing C should invalidate both B and A
        let transitive = graph.get_transitive_dependents("/c.tl");
        assert!(transitive.contains("/b.tl"));
        assert!(transitive.contains("/a.tl"));
        assert_eq!(transitive.len(), 2);
    }

    #[test]
    fn test_dependency_graph_circular() {
        let mut graph = ModuleDependencyGraph::new();
        // Circular: A imports B, B imports A
        graph.add_dependency("/a.tl", "/b.tl");
        graph.add_dependency("/b.tl", "/a.tl");

        // Should not infinite loop
        let transitive = graph.get_transitive_dependents("/a.tl");
        assert!(transitive.contains("/b.tl"));
        // /a.tl itself may or may not appear (B depends on A, so A is transitive of A)
        // The important thing is no infinite loop
    }

    #[test]
    fn test_dependency_graph_no_dependents() {
        let graph = ModuleDependencyGraph::new();
        assert!(graph.get_dependents("/x.tl").is_empty());
        assert!(graph.get_transitive_dependents("/x.tl").is_empty());
    }

    // ── ModuleExportsEntry tests ────────────────────────────────────

    #[test]
    fn test_module_exports_entry_creation() {
        let mut symbols = HashMap::new();
        symbols.insert(
            "foo".to_string(),
            CachedSymbolInfo {
                kind: "function".to_string(),
                type_display: "(x: number) -> string".to_string(),
            },
        );
        let entry = ModuleExportsEntry {
            symbols,
            version: 5,
            content_hash: 12345,
        };
        assert_eq!(entry.version, 5);
        assert_eq!(entry.symbols.len(), 1);
        assert_eq!(entry.symbols["foo"].kind, "function");
    }

    // ── content_hash tests ──────────────────────────────────────────

    #[test]
    fn test_content_hash_same_text() {
        let h1 = content_hash("hello world");
        let h2 = content_hash("hello world");
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_content_hash_differs() {
        let h1 = content_hash("hello world");
        let h2 = content_hash("hello world!");
        assert_ne!(h1, h2);
    }
}
