//! Integration tests for semantic token caching.
//!
//! Tests the end-to-end flow: compute tokens → cache hit →
//! invalidate → recompute.

use luanext_lsp::core::cache::CacheType;
use luanext_lsp::core::document::Document;
use luanext_lsp::features::semantic::SemanticTokensProvider;

#[test]
fn test_semantic_tokens_cached_on_first_request() {
    let provider = SemanticTokensProvider::new();
    let doc = Document::new_test("local x = 42".to_string(), 1);

    // First request computes tokens
    let result = provider.provide_full(&doc);
    assert!(!result.data.is_empty());
    assert!(result.result_id.is_some());
}

#[test]
fn test_second_request_returns_cached_result() {
    let provider = SemanticTokensProvider::new();
    let doc = Document::new_test("local x = 42\nlocal y = 10".to_string(), 5);

    let result1 = provider.provide_full(&doc);
    let result2 = provider.provide_full(&doc);

    // Data should be identical
    assert_eq!(result1.data.len(), result2.data.len());
    assert_eq!(result1.result_id, result2.result_id);
    for (a, b) in result1.data.iter().zip(result2.data.iter()) {
        assert_eq!(a.delta_line, b.delta_line);
        assert_eq!(a.delta_start, b.delta_start);
        assert_eq!(a.length, b.length);
        assert_eq!(a.token_type, b.token_type);
        assert_eq!(a.token_modifiers_bitset, b.token_modifiers_bitset);
    }
}

#[test]
fn test_result_id_tracks_version() {
    let provider = SemanticTokensProvider::new();
    let doc = Document::new_test("local x = 1".to_string(), 42);

    let result = provider.provide_full(&doc);
    assert_eq!(result.result_id, Some("42".to_string()));
}

#[test]
fn test_empty_document_tokens_cached() {
    let provider = SemanticTokensProvider::new();
    let doc = Document::new_test("".to_string(), 1);

    let result1 = provider.provide_full(&doc);
    let result2 = provider.provide_full(&doc);

    assert_eq!(result1.data.len(), result2.data.len());
}

#[test]
fn test_multiple_statements_produce_tokens() {
    let provider = SemanticTokensProvider::new();
    let doc = Document::new_test(
        "local x = 1\nfunction foo() return x end\nlocal y = foo()".to_string(),
        1,
    );

    let result = provider.provide_full(&doc);

    // Should have tokens for: x (var), foo (func decl), x (ref), y (var), foo (call)
    assert!(result.data.len() >= 4);
}

#[test]
fn test_provide_full_delta_returns_valid_delta() {
    let provider = SemanticTokensProvider::new();
    let doc = Document::new_test("local x = 1".to_string(), 1);

    // Compute tokens first to populate cache
    let full = provider.provide_full(&doc);
    let result_id = full.result_id.unwrap();

    // Request delta
    let delta = provider.provide_full_delta(&doc, result_id);
    assert!(delta.result_id.is_some());
}

#[test]
fn test_provide_full_delta_with_stale_id() {
    let provider = SemanticTokensProvider::new();
    let doc = Document::new_test("local x = 1".to_string(), 1);

    let _ = provider.provide_full(&doc);

    // Request delta with a wrong result_id
    let delta = provider.provide_full_delta(&doc, "nonexistent".to_string());
    // Should still return a valid response (full replacement)
    assert!(delta.result_id.is_some());
}

#[test]
fn test_class_tokens_cached() {
    let provider = SemanticTokensProvider::new();
    let doc = Document::new_test("class Point\n  x: number\n  y: number\nend".to_string(), 1);

    let result1 = provider.provide_full(&doc);
    let result2 = provider.provide_full(&doc);

    assert!(!result1.data.is_empty());
    assert_eq!(result1.data.len(), result2.data.len());
}

#[test]
fn test_incremental_semantic_tokens_single_line_shift() {
    use luanext_lsp::features::semantic::incremental::{update_semantic_tokens, TokenTextEdit};

    // Simulate tokens for "local x = 42"
    // Token for 'x' at line 0, col 6
    let prev = lsp_types::SemanticTokens {
        result_id: Some("1".to_string()),
        data: vec![lsp_types::SemanticToken {
            delta_line: 0,
            delta_start: 6,
            length: 1,
            token_type: 5,
            token_modifiers_bitset: 1,
        }],
    };

    // Insert "abc" at position (0, 3) - "locabc x = 42"
    // Token 'x' should shift from col 6 to col 9
    let edit = TokenTextEdit {
        start_line: 0,
        start_char: 3,
        end_line: 0,
        end_char: 3,
        new_line_count: 0,
        new_last_line_length: 3,
    };

    let result = update_semantic_tokens(&prev, &edit, "2".to_string());
    assert!(result.is_some());
    let updated = result.unwrap();
    assert_eq!(updated.data[0].delta_start, 9); // 6 + 3
    assert_eq!(updated.result_id, Some("2".to_string()));
}

#[test]
fn test_incremental_multiline_returns_none() {
    use luanext_lsp::features::semantic::incremental::{update_semantic_tokens, TokenTextEdit};

    let prev = lsp_types::SemanticTokens {
        result_id: Some("1".to_string()),
        data: vec![],
    };

    let edit = TokenTextEdit {
        start_line: 0,
        start_char: 5,
        end_line: 1,
        end_char: 0,
        new_line_count: 0,
        new_last_line_length: 0,
    };

    assert!(update_semantic_tokens(&prev, &edit, "2".to_string()).is_none());
}

// ── Hover cache integration tests ────────────────────────────────

#[test]
fn test_hover_cache_integration_keyword() {
    use luanext_lsp::features::navigation::HoverProvider;
    use luanext_lsp::traits::HoverProviderTrait;

    let provider = HoverProvider::new();
    let doc = Document::new_test("local x = 42".to_string(), 1);

    // Hover on "local" keyword at position (0, 0)
    let result1 = provider.provide(&doc, lsp_types::Position::new(0, 0));
    assert!(result1.is_some());

    // Second call should return cached result
    let result2 = provider.provide(&doc, lsp_types::Position::new(0, 0));
    assert!(result2.is_some());

    // Verify cache stats recorded a hit
    let hover_stats = doc.cache().stats_for(CacheType::Hover).cloned().unwrap();
    assert!(hover_stats.hits >= 1);
}

#[test]
fn test_hover_cache_integration_different_positions() {
    use luanext_lsp::features::navigation::HoverProvider;
    use luanext_lsp::traits::HoverProviderTrait;

    let provider = HoverProvider::new();
    let doc = Document::new_test("local x = 42\nfunction foo() end".to_string(), 1);

    // Hover on "local" keyword
    let result1 = provider.provide(&doc, lsp_types::Position::new(0, 0));
    assert!(result1.is_some());

    // Hover on "function" keyword - different position, cache miss
    let result2 = provider.provide(&doc, lsp_types::Position::new(1, 3));
    assert!(result2.is_some());

    // Now hover on "local" again - should be cached
    let result3 = provider.provide(&doc, lsp_types::Position::new(0, 0));
    assert!(result3.is_some());

    let hover_stats = doc.cache().stats_for(CacheType::Hover).cloned().unwrap();
    assert!(hover_stats.hits >= 1);
    assert!(hover_stats.misses >= 2);
}

// ── Completion cache integration tests ───────────────────────────

#[test]
fn test_completion_cache_integration_statement_context() {
    use luanext_lsp::features::edit::CompletionProvider;

    let provider = CompletionProvider::new();
    let doc = Document::new_test("local x = 1".to_string(), 1);

    // First completion at statement context
    let result1 = provider.provide(&doc, lsp_types::Position::new(0, 0));
    assert!(!result1.is_empty());

    // Second call should hit cache
    let result2 = provider.provide(&doc, lsp_types::Position::new(0, 0));
    assert!(!result2.is_empty());
    assert_eq!(result1.len(), result2.len());

    let completion_stats = doc
        .cache()
        .stats_for(CacheType::Completion)
        .cloned()
        .unwrap();
    assert!(completion_stats.hits >= 1);
}

#[test]
fn test_completion_cache_integration_type_context_not_cached() {
    use luanext_lsp::features::edit::CompletionProvider;

    let provider = CompletionProvider::new();
    let doc = Document::new_test("local x: ".to_string(), 1);

    // Type annotation context - should not use cache
    let _result1 = provider.provide(&doc, lsp_types::Position::new(0, 9));
    let _result2 = provider.provide(&doc, lsp_types::Position::new(0, 9));

    // No cache stats should be recorded for cheap contexts
    assert!(doc.cache().stats_for(CacheType::Completion).is_none());
}
