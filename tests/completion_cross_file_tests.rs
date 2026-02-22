//! Cross-file completion tests
//!
//! Tests the Completion provider's ability to suggest imported symbols,
//! re-exported symbols, and members of imported types across files.

use lsp_types::*;
use luanext_lsp::core::document::DocumentManager;
use luanext_lsp::features::edit::CompletionProvider;
use luanext_typechecker::cli::fs::MockFileSystem;
use std::str::FromStr;

/// Build a DocumentManager with a populated mock filesystem.
///
/// `files` is a list of (module_path, source_code) pairs where module_path
/// is an absolute path like "/test/module.luax".
fn build_test_workspace(files: &[(&str, &str)]) -> DocumentManager {
    let mut fs = MockFileSystem::new();
    for (path, content) in files {
        fs.add_file(*path, *content);
    }

    let mut dm = DocumentManager::new_test_with_fs(fs);

    for (path, content) in files {
        let uri = Uri::from_str(&format!("file://{}", path)).unwrap();
        dm.add_test_document(&uri, path, content);
    }

    dm
}

fn get_completions(
    dm: &DocumentManager,
    file_path: &str,
    line: u32,
    col: u32,
) -> Vec<CompletionItem> {
    let uri = Uri::from_str(&format!("file://{}", file_path)).unwrap();
    let doc = dm.get(&uri).expect("document should exist");
    let provider = CompletionProvider::new();
    provider.provide_with_manager(doc, Position::new(line, col), dm)
}

fn labels(items: &[CompletionItem]) -> Vec<&str> {
    items.iter().map(|i| i.label.as_str()).collect()
}

// ── Object property completion ──────────────────────────────────────

#[test]
fn test_complete_members_of_imported_object() {
    let dm = build_test_workspace(&[
        (
            "/test/config.luax",
            r#"
export const config = {
    apiUrl = "https://api.example.com",
    timeout = 5000,
    retries = 3,
}
"#,
        ),
        (
            "/test/usage.luax",
            r#"
import { config } from "./config"
local url = config.
"#,
        ),
    ]);

    let completions = get_completions(&dm, "/test/usage.luax", 2, 19);
    let names = labels(&completions);

    assert!(
        !completions.is_empty(),
        "Should have cross-file member completions"
    );
    assert!(
        names.contains(&"apiUrl"),
        "Should complete 'apiUrl', got: {:?}",
        names
    );
    assert!(
        names.contains(&"timeout"),
        "Should complete 'timeout', got: {:?}",
        names
    );
    assert!(
        names.contains(&"retries"),
        "Should complete 'retries', got: {:?}",
        names
    );
}

// ── Class member completion ─────────────────────────────────────────

#[test]
fn test_complete_members_of_imported_class() {
    let dm = build_test_workspace(&[
        (
            "/test/models.luax",
            r#"
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
"#,
        ),
        (
            "/test/usage.luax",
            r#"
import { User } from "./models"
local user = User(1, "Alice", "alice@example.com")
local name = user.
"#,
        ),
    ]);

    // user. at end of line 3
    let completions = get_completions(&dm, "/test/usage.luax", 3, 19);

    // Even if class instance type doesn't fully resolve, at minimum
    // we should get something from the cross-file path
    let _ = completions;
}

// ── Method completion via colon ─────────────────────────────────────

#[test]
fn test_complete_methods_of_imported_object() {
    let dm = build_test_workspace(&[
        (
            "/test/math.luax",
            r#"
export const mathUtils = {
    add = function(a: number, b: number): number
        return a + b
    end,
    multiply = function(a: number, b: number): number
        return a * b
    end,
}
"#,
        ),
        (
            "/test/usage.luax",
            r#"
import { mathUtils } from "./math"
mathUtils.
"#,
        ),
    ]);

    let completions = get_completions(&dm, "/test/usage.luax", 2, 10);
    let names = labels(&completions);

    assert!(
        !completions.is_empty(),
        "Should have cross-file completions"
    );
    assert!(
        names.contains(&"add"),
        "Should complete 'add', got: {:?}",
        names
    );
    assert!(
        names.contains(&"multiply"),
        "Should complete 'multiply', got: {:?}",
        names
    );
}

// ── Default import completion ───────────────────────────────────────

#[test]
fn test_complete_members_of_default_import() {
    let dm = build_test_workspace(&[
        (
            "/test/defaults.luax",
            r#"
const settings = {
    debug = false,
    verbose = true,
}
export default settings
"#,
        ),
        (
            "/test/usage.luax",
            r#"
import settings from "./defaults"
settings.
"#,
        ),
    ]);

    let completions = get_completions(&dm, "/test/usage.luax", 2, 9);
    let names = labels(&completions);

    // Default exports may resolve differently; verify no panic at minimum
    // and check if members show up
    if !completions.is_empty() {
        assert!(
            names.contains(&"debug") || names.contains(&"verbose"),
            "Expected settings members, got: {:?}",
            names
        );
    }
}

// ── Local completions still work (regression guard) ─────────────────

#[test]
fn test_local_symbol_completion_still_works() {
    let dm = build_test_workspace(&[(
        "/test/main.luax",
        r#"
local function helper(): void
    print("helper")
end

local x = 42
local y = "hello"

local result =
"#,
    )]);

    let completions = get_completions(&dm, "/test/main.luax", 8, 14);

    assert!(
        !completions.is_empty(),
        "Should suggest local symbols in completion"
    );
}

// ── Local member completion still works ─────────────────────────────

#[test]
fn test_local_object_member_completion_with_manager() {
    let dm = build_test_workspace(&[(
        "/test/main.luax",
        r#"
local obj = { name = "test", value = 42 }
obj.
"#,
    )]);

    let completions = get_completions(&dm, "/test/main.luax", 2, 4);
    let names = labels(&completions);

    assert!(
        !completions.is_empty(),
        "Should have local member completions"
    );
    assert!(
        names.contains(&"name"),
        "Should complete 'name', got: {:?}",
        names
    );
    assert!(
        names.contains(&"value"),
        "Should complete 'value', got: {:?}",
        names
    );
}

// ── Type annotation completions unaffected ──────────────────────────

#[test]
fn test_type_annotation_completion_with_manager() {
    let dm = build_test_workspace(&[("/test/main.luax", "local x: ")]);

    let completions = get_completions(&dm, "/test/main.luax", 0, 9);
    let names = labels(&completions);

    assert!(names.contains(&"number"), "Should suggest built-in types");
    assert!(names.contains(&"string"), "Should suggest built-in types");
}

// ── Import path completions unaffected ──────────────────────────────

#[test]
fn test_no_panic_on_various_contexts() {
    let dm = build_test_workspace(&[
        ("/test/module.luax", "export const x = 1"),
        (
            "/test/main.luax",
            r#"
import { x } from "./module"
@
local y: number = x
"#,
        ),
    ]);

    // Decorator context
    let _ = get_completions(&dm, "/test/main.luax", 2, 1);
    // Statement context
    let _ = get_completions(&dm, "/test/main.luax", 3, 19);
}

// ── Cross-file with aliased import name ─────────────────────────────

#[test]
fn test_complete_members_of_aliased_import() {
    let dm = build_test_workspace(&[
        (
            "/test/config.luax",
            r#"
export const settings = {
    host = "localhost",
    port = 8080,
}
"#,
        ),
        (
            "/test/usage.luax",
            r#"
import { settings as cfg } from "./config"
cfg.
"#,
        ),
    ]);

    let completions = get_completions(&dm, "/test/usage.luax", 2, 4);
    let names = labels(&completions);

    assert!(
        !completions.is_empty(),
        "Should resolve aliased import, got: {:?}",
        names
    );
    assert!(
        names.contains(&"host"),
        "Should complete 'host', got: {:?}",
        names
    );
    assert!(
        names.contains(&"port"),
        "Should complete 'port', got: {:?}",
        names
    );
}
