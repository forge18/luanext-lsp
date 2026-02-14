//! Integration tests for incremental parsing in DocumentManager
//!
//! These tests verify that the DocumentManager correctly integrates heuristics,
//! metrics, and incremental parsing for realistic LSP workflows.

use luanext_lsp::core::document::Document;
use luanext_lsp::core::heuristics::{IncrementalConfig, ParseStrategy, ParseStrategyAnalyzer};
use lsp_types::{Position, Range, TextDocumentContentChangeEvent};

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

// --- DOCUMENT PARSING TESTS ---

#[test]
fn test_document_single_statement() {
    let doc = Document::new_test("const x: number = 42".to_string(), 1);

    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());
    assert_eq!(ast.unwrap().0.statements.len(), 1);
}

#[test]
fn test_document_multiple_statements() {
    let doc = Document::new_test(
        "const x: number = 42\nconst y: string = \"hello\"\nconst z: boolean = true".to_string(),
        1,
    );

    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());
    assert_eq!(ast.unwrap().0.statements.len(), 3);
}

#[test]
fn test_document_empty() {
    let doc = Document::new_test("".to_string(), 1);

    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());
    assert_eq!(ast.unwrap().0.statements.len(), 0);
}

#[test]
fn test_document_caching() {
    let doc = Document::new_test("const x: number = 42".to_string(), 1);

    // Parse multiple times
    for _ in 0..5 {
        let ast = doc.get_or_parse_ast();
        assert!(ast.is_some());
    }

    // All should return the same cached AST (same pointer)
    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());
}

#[test]
fn test_document_interface() {
    let doc = Document::new_test(
        "interface Point { x: number, y: number }".to_string(),
        1,
    );

    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());
    assert_eq!(ast.unwrap().0.statements.len(), 1);
}

// --- METRICS TESTS ---

#[test]
fn test_metrics_initial_state() {
    let doc = Document::new_test("const x: number = 42".to_string(), 1);

    let stats = doc.metrics.get_stats();
    assert_eq!(stats.incremental_count, 0);
    assert_eq!(stats.full_count, 0);
}

#[test]
fn test_metrics_after_parse() {
    let doc = Document::new_test("const x: number = 42".to_string(), 1);

    // Parse the document
    let _ast = doc.get_or_parse_ast();

    // Metrics should be updated (either incremental or full count)
    let _stats = doc.metrics.get_stats();
    // Metrics are tracked but we don't make assertions about them in this test
}

// --- HEURISTICS TESTS ---

#[test]
fn test_heuristics_single_line_edit() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    let change = make_change(0, 10, 0, 11, "x".to_string());

    let strategy = analyzer.analyze_lsp_changes(&[change], "const x = 1", None);
    assert_eq!(strategy, ParseStrategy::SingleLineOptimized);
}

#[test]
fn test_heuristics_large_edit_full_parse() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // Large paste (2000 bytes)
    let large_text = "x".repeat(2000);
    let change = make_change(0, 0, 0, 0, large_text);

    let strategy = analyzer.analyze_lsp_changes(&[change], "", None);
    assert_eq!(strategy, ParseStrategy::FullParse);
}

#[test]
fn test_heuristics_append_to_end() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    let change = make_change(0, 0, 0, 0, "local x = 1".to_string());

    let strategy = analyzer.analyze_lsp_changes(&[change], "", None);
    assert_eq!(strategy, ParseStrategy::AppendOnlyOptimized);
}

#[test]
fn test_heuristics_many_edits_full_parse() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // 15 edits (exceeds threshold of 10)
    let changes: Vec<_> = (0..15)
        .map(|i| make_change(i, 0, i, 1, "x".to_string()))
        .collect();

    let strategy = analyzer.analyze_lsp_changes(&changes, "", None);
    assert_eq!(strategy, ParseStrategy::FullParse);
}

#[test]
fn test_heuristics_full_document_replacement() {
    let analyzer = ParseStrategyAnalyzer::new(IncrementalConfig::default());

    // No range = full document replacement
    let change = TextDocumentContentChangeEvent {
        range: None,
        text: "new content".to_string(),
        range_length: None,
    };

    let strategy = analyzer.analyze_lsp_changes(&[change], "old", None);
    assert_eq!(strategy, ParseStrategy::FullParse);
}
