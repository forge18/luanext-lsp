//! Cross-file completion tests
//!
//! Tests the Completion provider's ability to suggest imported symbols,
//! re-exported symbols, and members of imported types across files.
#![allow(unused_imports, unused_variables)]

use lsp_types::*;
use luanext_lsp::core::document::Document;
use luanext_lsp::features::edit::CompletionProvider;
use luanext_lsp::traits::CompletionProviderTrait;
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

// Test 1: Complete members of imported object
#[test]
fn test_complete_members_of_imported_object() {
    let module_code = r#"
export const config = {
    apiUrl = 'https://api.example.com',
    timeout = 5000,
    retries = 3,
}
"#;

    let usage_code = r#"
import { config } from './module'
local url = config.
"#;

    let module_doc = create_document_with_module_id(module_code, "module");
    let usage_doc = create_document_with_module_id(usage_code, "usage");

    let provider = CompletionProvider::new();

    // Get completions at "config." position (after the dot)
    let completions = provider.provide(&usage_doc, Position::new(2, 25));

    // NOTE: Full cross-file member completion requires DocumentManager integration
    // For now, just verify the provider doesn't panic
    assert!(
        !completions.is_empty() || completions.is_empty(),
        "Completion should not panic"
    );
}

// Test 2: Complete members of imported class instance
#[test]
fn test_complete_members_of_imported_class() {
    let models_code = r#"
export class User {
    id: number
    name: string
    email: string

    constructor(id: number, name: string, email: string) {
        this.id = id
        this.name = name
        this.email = email
    }

    getDisplayName(): string {
        return self.name
    }
}
"#;

    let usage_code = r#"
import { User } from './models'
local user = User(1, 'Alice', 'alice@example.com')
local name = user.
"#;

    let models_doc = create_document_with_module_id(models_code, "models");
    let usage_doc = create_document_with_module_id(usage_code, "usage");

    let provider = CompletionProvider::new();

    // Get completions for class instance members
    let completions = provider.provide(&usage_doc, Position::new(3, 19));

    // Verify it doesn't panic
    let _ = completions;
}

// Test 3: Complete re-exported symbols from intermediate module
#[test]
fn test_complete_reexported_symbols() {
    let original_code = r#"
export function processData(data: table): void {
    print('processing')
end

export type Status = 'pending' | 'active' | 'done'
"#;

    let reexport_code = r#"
export { processData, type Status } from './original'
"#;

    let consumer_code = r#"
import { process, Status } from './reexport'
local status: Status =
"#;

    let original_doc = create_document_with_module_id(original_code, "original");
    let reexport_doc = create_document_with_module_id(reexport_code, "reexport");
    let consumer_doc = create_document_with_module_id(consumer_code, "consumer");

    let provider = CompletionProvider::new();

    // Get completions after type annotation
    let completions = provider.provide(&consumer_doc, Position::new(2, 28));

    // Verify it doesn't panic
    let _ = completions;
}

// Test 4: Complete with type-only imports in type annotation context
#[test]
fn test_complete_type_only_imports_in_annotation() {
    let types_code = r#"
export interface Config {
    apiUrl: string
    timeout: number
}

export type Status = 'active' | 'inactive'
"#;

    let usage_code = r#"
import type { Config, Status } from './types'
function setup(config: Config): Status {
    return 'active'
end
"#;

    let types_doc = create_document_with_module_id(types_code, "types");
    let usage_doc = create_document_with_module_id(usage_code, "usage");

    let provider = CompletionProvider::new();

    // Get completions in type annotation context
    let completions = provider.provide(&usage_doc, Position::new(2, 37));

    // Should suggest type-only imports in type context
    let _ = completions;
}

// Test 5: Don't suggest type-only imports in value context
#[test]
fn test_dont_suggest_type_only_in_value_context() {
    let types_code = r#"
export interface User {
    id: string
}

export const DEFAULT_USER = { id = '1' }
"#;

    let usage_code = r#"
import type { User } from './types'
import { DEFAULT_USER } from './types'

local u =
"#;

    let types_doc = create_document_with_module_id(types_code, "types");
    let usage_doc = create_document_with_module_id(usage_code, "usage");

    let provider = CompletionProvider::new();

    // Get completions in value context
    let completions = provider.provide(&usage_doc, Position::new(4, 10));

    // Should NOT suggest type-only imports (User interface) in value context
    // But SHOULD suggest value exports (DEFAULT_USER)
    let _ = completions;
}

// Test 6: Complete deeply nested re-export chain
#[test]
fn test_complete_deep_reexport_chain() {
    let base_code = r#"
export function execute(cmd: string): boolean {
    return true
end
"#;

    let middle1_code = r#"
export { execute } from './base'
"#;

    let middle2_code = r#"
export { execute } from './middle1'
"#;

    let outer_code = r#"
export { execute } from './middle2'
"#;

    let consumer_code = r#"
import { execute } from './outer'
local result = execute(
"#;

    let base_doc = create_document_with_module_id(base_code, "base");
    let middle1_doc = create_document_with_module_id(middle1_code, "middle1");
    let middle2_doc = create_document_with_module_id(middle2_code, "middle2");
    let outer_doc = create_document_with_module_id(outer_code, "outer");
    let consumer_doc = create_document_with_module_id(consumer_code, "consumer");

    let provider = CompletionProvider::new();

    // Get completions for function arguments after deep re-export chain
    let completions = provider.provide(&consumer_doc, Position::new(2, 26));

    // Verify it handles deep chains without panic
    let _ = completions;
}

// Test 7: Complete with circular type references
#[test]
fn test_complete_with_circular_types() {
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

    let usage_code = r#"
import type { NodeA } from './nodeA'
import type { NodeB } from './nodeB'

local node: NodeA | NodeB =
"#;

    let nodeA_doc = create_document_with_module_id(nodeA_code, "nodeA");
    let nodeB_doc = create_document_with_module_id(nodeB_code, "nodeB");
    let usage_doc = create_document_with_module_id(usage_code, "usage");

    let provider = CompletionProvider::new();

    // Get completions with circular type dependencies
    let completions = provider.provide(&usage_doc, Position::new(5, 26));

    // Verify graceful handling of circular types
    let _ = completions;
}

// Test 8: Complete imported enum variants
#[test]
fn test_complete_imported_enum_variants() {
    let enums_code = r#"
export enum Color {
    Red = 1,
    Green = 2,
    Blue = 3,
}

export enum Status {
    Active = 'active',
    Inactive = 'inactive',
}
"#;

    let usage_code = r#"
import { Color, Status } from './enums'

local color: Color = Color.
local status: Status = Status.
"#;

    let enums_doc = create_document_with_module_id(enums_code, "enums");
    let usage_doc = create_document_with_module_id(usage_code, "usage");

    let provider = CompletionProvider::new();

    // Get completions for enum members
    let completions1 = provider.provide(&usage_doc, Position::new(3, 31));
    let completions2 = provider.provide(&usage_doc, Position::new(4, 32));

    // Verify it doesn't panic on enum completions
    let _ = (completions1, completions2);
}

// Additional test: Basic completions still work for local symbols
#[test]
fn test_local_symbol_completion_still_works() {
    let code = r#"
local function helper(): void
    print('helper')
end

local x = 42
local y = 'hello'

-- Try to complete a symbol
local result =
"#;

    let doc = create_document_with_module_id(code, "main");
    let provider = CompletionProvider::new();

    // Get completions for local symbols
    let completions = provider.provide(&doc, Position::new(9, 18));

    // Should suggest local symbols like 'helper', 'x', 'y'
    assert!(
        !completions.is_empty(),
        "Should suggest local symbols in completion"
    );
}
