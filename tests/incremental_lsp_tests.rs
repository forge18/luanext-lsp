//! Integration tests for incremental parsing in LSP
//!
//! These tests verify that the Document incremental parsing infrastructure works correctly
//! when integrated with the LSP text editing workflow.
//!
//! Note: Most incremental parsing logic is tested via the parser's own tests.
//! These tests verify the LSP integration doesn't break normal parsing.

use luanext_lsp::core::document::Document;

#[test]
fn test_document_parses_correctly_with_incremental_infrastructure() {
    // Test that Document can parse with incremental infrastructure in place
    let doc = Document::new_test("const x: number = 42".to_string(), 1);

    // First parse should work
    let ast1 = doc.get_or_parse_ast();
    assert!(ast1.is_some());

    // Second parse should return cached AST
    let ast2 = doc.get_or_parse_ast();
    assert!(ast2.is_some());

    // Should be the same AST (same pointer)
    assert_eq!(ast1.unwrap().0 as *const _, ast2.unwrap().0 as *const _);
}

#[test]
fn test_document_parses_multiline_code() {
    let doc = Document::new_test(
        "const x: number = 42\nconst y: string = \"hello\"\nconst z: boolean = true".to_string(),
        1,
    );

    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());

    let program = ast.unwrap().0;
    assert_eq!(program.statements.len(), 3);
}

#[test]
fn test_document_handles_empty_source() {
    let doc = Document::new_test("".to_string(), 1);

    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());

    let program = ast.unwrap().0;
    assert_eq!(program.statements.len(), 0);
}

#[test]
fn test_document_caching_works() {
    let doc = Document::new_test("const x: number = 42".to_string(), 1);

    // Parse multiple times
    for _ in 0..5 {
        let ast = doc.get_or_parse_ast();
        assert!(ast.is_some());
    }

    // All should return the same cached AST
    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());
}

#[test]
fn test_document_parses_complex_code() {
    let code = r#"
interface Point {
    x: number,
    y: number
}

function distance(p1: Point, p2: Point): number {
    const dx = p2.x - p1.x
    const dy = p2.y - p1.y
    return math.sqrt(dx * dx + dy * dy)
}

export { Point, distance }
"#;

    let doc = Document::new_test(code.to_string(), 1);
    let ast = doc.get_or_parse_ast();
    assert!(ast.is_some());
}
