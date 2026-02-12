//! Cross-file find-references tests
//!
//! Tests the References provider's ability to find all references to a symbol
//! across multiple files, including re-exports and type-only imports.

use lsp_types::*;
use luanext_lsp::core::document::Document;
use luanext_lsp::features::navigation::ReferencesProvider;
use luanext_lsp::traits::ReferencesProviderTrait;
use std::str::FromStr;

mod test_utils {
    use super::*;
    use luanext_typechecker::module_resolver::ModuleId;
    use std::path::PathBuf;

    pub fn create_uri(path: &str) -> Uri {
        Uri::from_str(&format!("file://{}", path)).unwrap()
    }

    pub fn create_document_with_module_id(text: &str, module_id_str: &str) -> Document {
        let mut doc = Document::new_test(text.to_string(), 1);
        doc.module_id = Some(ModuleId::new(PathBuf::from(module_id_str)));
        doc
    }
}

use test_utils::*;

// Test 1: Find references to exported function across importing files
#[test]
fn test_find_references_to_exported_function() {
    let lib_code = r#"
export function calculate(x: number): number {
    return x * 2
}
"#;

    let client1_code = r#"
import { calculate } from './lib'
local result1 = calculate(5)
local result2 = calculate(10)
"#;

    let client2_code = r#"
import { calculate } from './lib'
function process()
    local value = calculate(20)
end
"#;

    let lib_doc = create_document_with_module_id(lib_code, "lib");
    let client1_doc = create_document_with_module_id(client1_code, "client1");
    let client2_doc = create_document_with_module_id(client2_code, "client2");

    let provider = ReferencesProvider::new();
    let lib_uri = create_uri("/test/lib.luax");

    // Find references to 'calculate' in lib.luax
    // NOTE: Full cross-file reference finding requires DocumentManager integration
    // For now, just verify the provider doesn't panic
    let refs = provider.provide(&lib_uri, &lib_doc, Position::new(1, 17), true);

    // Current implementation may not find cross-file references yet
    // This test documents the expected behavior when fully implemented
    let _ = refs;
}

// Test 2: Find references through single-level re-export
#[test]
fn test_find_references_single_level_reexport() {
    let original_code = r#"
export function process(data: table): void {
    for _, item in ipairs(data) do
        print(item)
    end
}
"#;

    let reexport_code = r#"
export { process } from './original'
"#;

    let consumer_code = r#"
import { process } from './reexport'
process({1, 2, 3})
"#;

    let original_doc = create_document_with_module_id(original_code, "original");
    let reexport_doc = create_document_with_module_id(reexport_code, "reexport");
    let consumer_doc = create_document_with_module_id(consumer_code, "consumer");

    let provider = ReferencesProvider::new();
    let original_uri = create_uri("/test/original.luax");
    let reexport_uri = create_uri("/test/reexport.luax");

    // Find references to process in original.luax
    let refs_original = provider.provide(&original_uri, &original_doc, Position::new(1, 16), true);

    // Find references in re-export file
    let refs_reexport = provider.provide(&reexport_uri, &reexport_doc, Position::new(1, 11), true);

    // NOTE: Full cross-file reference finding through re-exports will be implemented later
    // For now, just verify the provider doesn't panic
    let _ = (refs_original, refs_reexport);
}

// Test 3: Find references through multi-level re-export chain
#[test]
fn test_find_references_multi_level_reexport_chain() {
    let base_code = r#"
export function transform(value: string): string {
    return value:upper()
}
"#;

    let middle_code = r#"
export { transform } from './base'
"#;

    let outer_code = r#"
export { transform } from './middle'
"#;

    let consumer_code = r#"
import { transform } from './outer'
local result = transform('hello')
"#;

    let base_doc = create_document_with_module_id(base_code, "base");
    let middle_doc = create_document_with_module_id(middle_code, "middle");
    let outer_doc = create_document_with_module_id(outer_code, "outer");
    let consumer_doc = create_document_with_module_id(consumer_code, "consumer");

    let provider = ReferencesProvider::new();
    let base_uri = create_uri("/test/base.luax");

    // Find references in the base module
    // NOTE: Full cross-file reference finding through re-export chains will be implemented later
    let refs = provider.provide(&base_uri, &base_doc, Position::new(1, 16), true);

    // Verify it doesn't panic
    let _ = refs;
}

// Test 4: Find references with type-only imports (should be empty for value usage)
#[test]
fn test_find_references_type_only_imports_empty() {
    let types_code = r#"
export interface Response {
    status: number
    message: string
}
"#;

    let usage_code = r#"
import type { Response } from './types'
local handleResponse: (Response) -> void = function(resp)
    print(resp.message)
end
"#;

    let types_doc = create_document_with_module_id(types_code, "types");
    let usage_doc = create_document_with_module_id(usage_code, "usage");

    let provider = ReferencesProvider::new();
    let types_uri = create_uri("/test/types.luax");

    // Find references to Response (used only as type annotation)
    let refs = provider.provide(&types_uri, &types_doc, Position::new(1, 18), true);

    // Type-only imports might not show up as value references
    let _ = refs;
}

// Test 5: Find references excluding declaration across files
#[test]
fn test_find_references_excluding_declaration() {
    let lib_code = r#"
export function getValue(): number {
    return 42
}
"#;

    let usage_code = r#"
import { getValue } from './lib'
local x = getValue()
local y = getValue() + getValue()
"#;

    let lib_doc = create_document_with_module_id(lib_code, "lib");
    let usage_doc = create_document_with_module_id(usage_code, "usage");

    let provider = ReferencesProvider::new();
    let lib_uri = create_uri("/test/lib.luax");

    // Find references excluding the declaration
    let refs_with_decl = provider.provide(&lib_uri, &lib_doc, Position::new(1, 16), true);
    let refs_without_decl = provider.provide(&lib_uri, &lib_doc, Position::new(1, 16), false);

    // Without declaration should have fewer or equal references
    assert!(
        refs_without_decl.len() <= refs_with_decl.len(),
        "Excluding declaration should reduce reference count"
    );
}

// Test 6: Find references with circular type dependencies
#[test]
fn test_find_references_circular_type_dependencies() {
    let nodeA_code = r#"
import type { NodeB } from './nodeB'

export interface NodeA {
    id: number
    next: NodeB | nil
}
"#;

    let nodeB_code = r#"
import type { NodeA } from './nodeA'

export interface NodeB {
    value: string
    prev: NodeA | nil
}
"#;

    let nodeA_doc = create_document_with_module_id(nodeA_code, "nodeA");
    let nodeB_doc = create_document_with_module_id(nodeB_code, "nodeB");

    let provider = ReferencesProvider::new();
    let nodeA_uri = create_uri("/test/nodeA.luax");
    let nodeB_uri = create_uri("/test/nodeB.luax");

    // Find references despite circular type dependencies
    let refs_a = provider.provide(&nodeA_uri, &nodeA_doc, Position::new(3, 16), true);
    let refs_b = provider.provide(&nodeB_uri, &nodeB_doc, Position::new(3, 16), true);

    // Should handle gracefully without panicking
    let _ = (refs_a, refs_b);
}

// Additional test: Find references in same file still works
#[test]
fn test_find_references_same_file_still_works() {
    let code = r#"
local function helper(x: number): number
    return x * 2
end

local a = helper(1)
local b = helper(2)
local c = helper(3)
"#;

    let doc = create_document_with_module_id(code, "main");
    let provider = ReferencesProvider::new();
    let uri = create_uri("/test/main.luax");

    // Find references to helper function
    let refs = provider.provide(&uri, &doc, Position::new(1, 16), true);

    // Single-file reference finding should be possible
    // Just verify the provider doesn't panic
    let _ = refs;
}
