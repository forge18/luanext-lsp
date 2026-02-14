# LSP Crate Architecture

## Overview

The `luanext-lsp` crate implements a Language Server Protocol server for LuaNext. It provides IDE features like hover, completion, go-to-definition, semantic highlighting, and diagnostics.

```
src/
  core/           # Document management, caching, diagnostics, metrics
  features/       # LSP feature implementations (hover, completion, etc.)
  analysis/       # Symbol indexing and cross-file analysis
  protocol/       # LSP message types and serialization
  di/             # Dependency injection containers
  traits/         # Provider trait definitions
  message_handler.rs  # Main LSP request/notification router
  main.rs         # Server entry point
```

## Caching Architecture

### Document Lifecycle

```
didOpen  → DocumentManager::open()   → Document created, AST parsed, symbol index updated
didChange → DocumentManager::change() → Text updated, caches invalidated, AST re-parsed
didSave  → DocumentManager::save()   → Text verified, stale caches invalidated
didClose → DocumentManager::close()  → Document removed, limiter updated
```

### Cache Hierarchy

```
DocumentManager
  ├── documents: HashMap<Uri, Document>
  │     └── Document
  │           ├── text, version, ast (RefCell)
  │           └── cache: RefCell<DocumentCache>
  │                 ├── semantic_tokens: VersionedCache<SemanticTokens>
  │                 ├── type_check_result: VersionedCache<TypeCheckResult>
  │                 ├── hover_cache: BoundedPositionCache<Hover>          (cap: 50)
  │                 ├── completion_cache: BoundedPositionCache<Vec<CompletionItem>>  (cap: 20)
  │                 ├── last_published_diagnostics: Option<Vec<Diagnostic>>
  │                 └── stats: HashMap<CacheType, CacheStats>
  ├── module_exports_cache: HashMap<String, ModuleExportsEntry>
  ├── dependency_graph: ModuleDependencyGraph
  └── cache_limiter: GlobalCacheLimiter
```

### Cache Types

| Cache | Container | Key | Invalidation |
|-------|-----------|-----|-------------|
| Semantic tokens | `VersionedCache` | document version | Any text change |
| Type-check result | `VersionedCache` | document version | Any text change |
| Hover | `BoundedPositionCache` | `(Position, version)` | Text change in range, or version bump |
| Completion | `BoundedPositionCache` | `(Position, version)` | Text change in range, or version bump |
| Diagnostics | `Option<Vec>` | N/A | Never auto-cleared (deduplication) |

### Container Semantics

**VersionedCache\<T\>**: Stores a single value tagged with the document version. Returns `None` on version mismatch.

**BoundedPositionCache\<T\>**: LRU-bounded map from `Position` to cached values. Supports:
- Per-position lookup with version check
- Range invalidation (clears entries within a line range)
- Full invalidation on version change
- Capacity-bounded with LRU eviction

## Cache Invalidation Rules

| Event | Semantic Tokens | Type Check | Hover | Completion | Diagnostics |
|-------|:-:|:-:|:-:|:-:|:-:|
| `didChange` (any edit) | Invalidated | Invalidated | Range-cleared | Range-cleared | Preserved |
| `didChange` (full replace) | Invalidated | Invalidated | Cleared | Cleared | Preserved |
| `didSave` (text differs) | Invalidated | Invalidated | Cleared | Cleared | Preserved |
| Dependent module changed | - | Invalidated | - | - | - |
| `didClose` | Removed | Removed | Removed | Removed | Removed |

## Memory Management

### MemoryEstimate Trait

Each cached type implements `MemoryEstimate` to report approximate heap bytes:

```rust
pub trait MemoryEstimate {
    fn estimated_heap_bytes(&self) -> usize;
}
```

Implemented for: `CachedSymbolInfo`, `TypeCheckResult`, `SemanticTokens`, `Hover`, `Vec<CompletionItem>`.

`DocumentCache::estimated_bytes()` sums all sub-cache estimates.

### GlobalCacheLimiter

Tracks total estimated cache bytes across all open documents with LRU eviction:

- **Global limit**: 50 MB (all documents combined)
- **Per-document soft limit**: 1 MB (clamped for eviction accounting)
- **Eviction**: When total exceeds the global limit, least-recently-used documents have their caches invalidated
- **Lifecycle**: Updated on `didChange`, cleaned up on `didClose`

## Performance Monitoring

### Per-Cache-Type Metrics

Each `CacheType` (Hover, Completion, SemanticTokens, TypeCheck) tracks:
- Hit/miss counts
- Hit/miss timing (microsecond precision)
- Hit rate and average response times

### Environment Variables

| Variable | Effect |
|----------|--------|
| `LUANEXT_LSP_CACHE_STATS=1` | Log per-cache-type stats and memory estimates (every 100 parses) |
| `LUANEXT_LSP_PARSE_STATS=1` | Log incremental vs full parse metrics |
| `LUANEXT_DISABLE_INCREMENTAL=1` | Force all parses to full (disable incremental) |
| `LUANEXT_MAX_EDIT_SIZE=N` | Max edit size (bytes) for incremental parsing |
| `LUANEXT_MAX_DIRTY_REGIONS=N` | Max dirty regions before falling back to full parse |
| `LUANEXT_MAX_AFFECTED_RATIO=F` | Max ratio of affected lines for incremental |

## Troubleshooting

**LSP shows stale hover/completion data**: The language server should auto-invalidate on edits. If data appears stale, restart the language server. File a bug if the issue persists — it may indicate a cache invalidation gap.

**High memory usage**: Set `LUANEXT_LSP_CACHE_STATS=1` to monitor per-document cache sizes. The `GlobalCacheLimiter` enforces a 50 MB ceiling with LRU eviction.

**Slow response times**: Set `LUANEXT_LSP_CACHE_STATS=1` to compare `avg_hit_time_ms` vs `avg_miss_time_ms`. High miss rates indicate the cache is being invalidated too aggressively.
