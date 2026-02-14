//! Unit tests for incremental parsing heuristics
//!
//! These tests verify the ParseStrategyAnalyzer correctly detects edit patterns
//! and recommends appropriate parse strategies.

use lsp_types::{Position, Range, TextDocumentContentChangeEvent};
use luanext_lsp::core::heuristics::{IncrementalConfig, ParseStrategy, ParseStrategyAnalyzer};

fn make_change(
    start_line: u32,
    start_char: u32,
    end_line: u32,
    end_char: u32,
    text: String,
) -> TextDocumentContentChangeEvent {
    TextDocumentContentChangeEvent {
        range: Some(Range {
            start: Position {
                line: start_line,
                character: start_char,
            },
            end: Position {
                line: end_line,
                character: end_char,
            },
        }),
        text,
        range_length: None,
    }
}

#[test]
fn test_single_line_edit_detection() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    let change = make_change(10, 5, 10, 6, "x".to_string());

    let strategy = analyzer.analyze_lsp_changes(&[change], "local x = 1\nlocal y = 2\n", Some(2));
    assert_eq!(strategy, ParseStrategy::SingleLineOptimized);
}

#[test]
fn test_large_edit_forces_full_parse() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // 2000 bytes - exceeds default 1000 threshold
    let large_text = "x".repeat(2000);
    let change = make_change(0, 0, 0, 5, large_text);

    let strategy = analyzer.analyze_lsp_changes(&[change], "const x = 1", None);
    assert_eq!(strategy, ParseStrategy::FullParse);
}

#[test]
fn test_append_only_detection_empty_document() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    let change = make_change(0, 0, 0, 0, "local x = 1".to_string());

    let strategy = analyzer.analyze_lsp_changes(&[change], "", None);
    assert_eq!(strategy, ParseStrategy::AppendOnlyOptimized);
}

#[test]
fn test_append_only_detection_at_end() {
    let text = "local x = 1\nlocal y = 2";
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // Append at end of last line (line 1, position 11)
    let change = make_change(1, 11, 1, 11, "\nlocal z = 3".to_string());

    let strategy = analyzer.analyze_lsp_changes(&[change], text, Some(2));
    assert_eq!(strategy, ParseStrategy::AppendOnlyOptimized);
}

#[test]
fn test_many_edits_forces_full_parse() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // 15 edits exceeds default threshold of 10
    let changes: Vec<_> = (0..15)
        .map(|i| make_change(i, 0, i, 1, "x".to_string()))
        .collect();

    let strategy = analyzer.analyze_lsp_changes(&changes, "", None);
    assert_eq!(strategy, ParseStrategy::FullParse);
}

#[test]
fn test_full_document_replacement() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // No range = full document replacement
    let change = TextDocumentContentChangeEvent {
        range: None,
        text: "completely new content".to_string(),
        range_length: None,
    };

    let strategy = analyzer.analyze_lsp_changes(&[change], "", None);
    assert_eq!(strategy, ParseStrategy::FullParse);
}

#[test]
fn test_multiline_edit_uses_standard_incremental() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    let change = make_change(0, 0, 2, 0, "replaced text".to_string());

    let strategy = analyzer.analyze_lsp_changes(&[change], "line1\nline2\nline3", None);
    assert_eq!(strategy, ParseStrategy::Incremental);
}

#[test]
fn test_medium_edit_under_threshold() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // 500 bytes - under 1000 threshold
    let medium_text = "x".repeat(500);
    let change = make_change(0, 0, 0, 5, medium_text);

    let strategy = analyzer.analyze_lsp_changes(&[change], "const x = 1", None);
    // Should use incremental because it's under threshold and multiline
    assert_eq!(strategy, ParseStrategy::Incremental);
}

/// Tests env-var-based configuration. Combined into a single test to avoid
/// race conditions when tests run in parallel (env vars are process-global).
#[test]
fn test_config_from_env() {
    // Clean up any env vars from other tests first
    std::env::remove_var("LUANEXT_DISABLE_INCREMENTAL");

    // Part 1: custom config values
    std::env::set_var("LUANEXT_MAX_EDIT_SIZE", "500");
    std::env::set_var("LUANEXT_MAX_DIRTY_REGIONS", "5");
    std::env::set_var("LUANEXT_MAX_AFFECTED_RATIO", "0.3");

    let config = IncrementalConfig::from_env();
    assert_eq!(config.max_edit_size, 500);
    assert_eq!(config.max_dirty_regions, 5);
    assert_eq!(config.max_affected_ratio, 0.3);

    std::env::remove_var("LUANEXT_MAX_EDIT_SIZE");
    std::env::remove_var("LUANEXT_MAX_DIRTY_REGIONS");
    std::env::remove_var("LUANEXT_MAX_AFFECTED_RATIO");

    // Part 2: disable incremental
    std::env::set_var("LUANEXT_DISABLE_INCREMENTAL", "1");

    let config = IncrementalConfig::from_env();
    assert_eq!(config.max_edit_size, 0); // Forces all edits to full parse

    std::env::remove_var("LUANEXT_DISABLE_INCREMENTAL");
}

#[test]
fn test_single_line_edit_over_100_chars() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // Single line but > 100 characters
    let long_text = "x".repeat(150);
    let change = make_change(10, 5, 10, 6, long_text);

    let strategy = analyzer.analyze_lsp_changes(&[change], "local x = 1\nlocal y = 2\n", Some(2));
    // Should use standard incremental, not single-line optimized
    assert_eq!(strategy, ParseStrategy::Incremental);
}
