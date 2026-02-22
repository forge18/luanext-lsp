//! Shared utilities for resolving import information across LSP features.
//!
//! Used by both Hover and Completion providers for cross-file symbol resolution.

use luanext_parser::ast::statement::{ImportClause, Statement};
use luanext_parser::string_interner::StringInterner;

/// Information about a symbol that was imported from another module.
pub struct ImportSymbolInfo {
    /// The module source path (e.g., "./math")
    pub source: String,
    /// The original exported name in the source module
    pub exported_name: String,
    /// Whether this is a type-only import
    pub is_type_only: bool,
}

/// Scan import statements to find info about an imported symbol.
pub fn find_import_info(
    statements: &[Statement],
    symbol_name: &str,
    interner: &StringInterner,
) -> Option<ImportSymbolInfo> {
    for stmt in statements {
        if let Statement::Import(import_decl) = stmt {
            let (exported_name, is_type_only) = match &import_decl.clause {
                ImportClause::Named(specs) => {
                    let name = specs.iter().find_map(|spec| {
                        let local_name = spec.local.as_ref().unwrap_or(&spec.imported);
                        if interner.resolve(local_name.node) == symbol_name {
                            Some(interner.resolve(spec.imported.node))
                        } else {
                            None
                        }
                    });
                    (name, false)
                }
                ImportClause::Default(ident) => {
                    if interner.resolve(ident.node) == symbol_name {
                        (Some("default".to_string()), false)
                    } else {
                        (None, false)
                    }
                }
                ImportClause::Namespace(ident) => {
                    if interner.resolve(ident.node) == symbol_name {
                        (Some("*".to_string()), false)
                    } else {
                        (None, false)
                    }
                }
                ImportClause::TypeOnly(specs) => {
                    let name = specs.iter().find_map(|spec| {
                        let local_name = spec.local.as_ref().unwrap_or(&spec.imported);
                        if interner.resolve(local_name.node) == symbol_name {
                            Some(interner.resolve(spec.imported.node))
                        } else {
                            None
                        }
                    });
                    (name, true)
                }
                ImportClause::Mixed { default, named } => {
                    if interner.resolve(default.node) == symbol_name {
                        (Some("default".to_string()), false)
                    } else {
                        let name = named.iter().find_map(|spec| {
                            let local_name = spec.local.as_ref().unwrap_or(&spec.imported);
                            if interner.resolve(local_name.node) == symbol_name {
                                Some(interner.resolve(spec.imported.node))
                            } else {
                                None
                            }
                        });
                        (name, false)
                    }
                }
            };

            if let Some(exported_name) = exported_name {
                return Some(ImportSymbolInfo {
                    source: import_decl.source.clone(),
                    exported_name,
                    is_type_only,
                });
            }
        }
    }
    None
}
