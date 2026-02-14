use crate::arena_pool::with_pooled_arena;
use crate::core::cache::{CachedSymbolInfo, TypeCheckResult};
use crate::core::document::Document;
use crate::traits::DiagnosticsProviderTrait;
use lsp_types::*;
use luanext_parser::ast::types::{PrimitiveType, TypeKind};
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser, Span};
use luanext_typechecker::cli::diagnostics::{
    CollectingDiagnosticHandler, DiagnosticHandler, DiagnosticLevel,
};
use luanext_typechecker::{SymbolKind, TypeChecker};
use std::collections::HashMap;
use std::sync::Arc;

/// Provides diagnostics (errors and warnings) for documents
#[derive(Clone)]
pub struct DiagnosticsProvider;

impl DiagnosticsProvider {
    pub fn new() -> Self {
        Self
    }

    /// Run type checking and cache the result, or return the cached result.
    ///
    /// This is the single entry point for type checking in the LSP.
    /// All features (diagnostics, hover, completion) should call this
    /// instead of running their own lex→parse→typecheck pipeline.
    pub fn ensure_type_checked(document: &Document) -> TypeCheckResult {
        // Check cache first — clone to release RefCell borrow before recording stats
        let cached = document
            .cache()
            .type_check_result
            .get_if_valid(document.version)
            .cloned();

        if let Some(result) = cached {
            document.cache_mut().stats_mut().record_hit();
            document.cache().stats().maybe_log("type_check");
            return result;
        }

        document.cache_mut().stats_mut().record_miss();

        // Cache miss: run full lex→parse→typecheck pipeline
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();

        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => {
                let result = TypeCheckResult {
                    symbols: HashMap::new(),
                    diagnostics: Self::convert_diagnostics(handler),
                };
                document
                    .cache_mut()
                    .type_check_result
                    .set(result.clone(), document.version);
                return result;
            }
        };

        let result = with_pooled_arena(|arena| {
            let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids, arena);
            let ast = match parser.parse() {
                Ok(a) => a,
                Err(_) => {
                    return TypeCheckResult {
                        symbols: HashMap::new(),
                        diagnostics: Self::convert_diagnostics(handler),
                    };
                }
            };

            let mut type_checker = TypeChecker::new(handler.clone(), &interner, &common_ids, arena);
            let _ = type_checker.check_program(&ast);

            // Extract symbol information while still in arena scope
            let symbol_table = type_checker.symbol_table();
            let mut symbols = HashMap::new();
            for (name, symbol) in symbol_table.all_visible_symbols() {
                let kind = Self::format_kind_display(symbol.kind);
                let type_display = Self::format_type_display(&symbol.typ.kind);
                symbols.insert(name.clone(), CachedSymbolInfo { kind, type_display });
            }

            let diagnostics = Self::convert_diagnostics(handler);
            TypeCheckResult {
                symbols,
                diagnostics,
            }
        });

        document
            .cache_mut()
            .type_check_result
            .set(result.clone(), document.version);
        result
    }

    /// Analyze a document and return diagnostics (internal method).
    /// Uses the shared type-check cache to avoid redundant work.
    pub fn provide_impl(&self, document: &Document) -> Vec<Diagnostic> {
        Self::ensure_type_checked(document).diagnostics
    }

    /// Format symbol kind as a display string.
    fn format_kind_display(kind: SymbolKind) -> String {
        match kind {
            SymbolKind::Const => "const",
            SymbolKind::Variable => "let",
            SymbolKind::Function => "function",
            SymbolKind::Class => "class",
            SymbolKind::Interface => "interface",
            SymbolKind::TypeAlias => "type",
            SymbolKind::Enum => "enum",
            SymbolKind::Parameter => "param",
            SymbolKind::Namespace => "namespace",
        }
        .to_string()
    }

    /// Format a type kind as a simple display string.
    pub fn format_type_display(kind: &TypeKind) -> String {
        match kind {
            TypeKind::Primitive(PrimitiveType::Nil) => "nil",
            TypeKind::Primitive(PrimitiveType::Boolean) => "boolean",
            TypeKind::Primitive(PrimitiveType::Number) => "number",
            TypeKind::Primitive(PrimitiveType::Integer) => "integer",
            TypeKind::Primitive(PrimitiveType::String) => "string",
            TypeKind::Primitive(PrimitiveType::Unknown) => "unknown",
            TypeKind::Primitive(PrimitiveType::Never) => "never",
            TypeKind::Primitive(PrimitiveType::Void) => "void",
            TypeKind::Primitive(PrimitiveType::Table) => "table",
            TypeKind::Primitive(PrimitiveType::Coroutine) => "coroutine",
            TypeKind::Primitive(PrimitiveType::Thread) => "thread",
            TypeKind::Literal(_) => "literal",
            TypeKind::Union(_) => "union type",
            TypeKind::Intersection(_) => "intersection type",
            TypeKind::Function(_) => "function",
            TypeKind::Object(_) => "object",
            TypeKind::Array(_) => "array",
            TypeKind::Tuple(_) => "tuple",
            TypeKind::TypeQuery(_) => "typeof",
            TypeKind::Reference(_) => "type",
            TypeKind::Nullable(_) => "nullable",
            TypeKind::IndexAccess(_, _) => "indexed access",
            TypeKind::Conditional(_) => "conditional type",
            TypeKind::Infer(_) => "infer",
            TypeKind::KeyOf(_) => "keyof",
            TypeKind::Mapped(_) => "mapped type",
            TypeKind::TemplateLiteral(_) => "template literal type",
            TypeKind::Parenthesized(inner) => return Self::format_type_display(&inner.kind),
            TypeKind::TypePredicate(_) => "type predicate",
            TypeKind::Variadic(_) => "variadic",
            TypeKind::Namespace(_) => "namespace",
        }
        .to_string()
    }

    /// Convert core diagnostics to LSP diagnostics
    fn convert_diagnostics(handler: Arc<CollectingDiagnosticHandler>) -> Vec<Diagnostic> {
        handler
            .get_diagnostics()
            .into_iter()
            .map(|d| Diagnostic {
                range: span_to_range(&d.span),
                severity: Some(match d.level {
                    DiagnosticLevel::Error => DiagnosticSeverity::ERROR,
                    DiagnosticLevel::Warning => DiagnosticSeverity::WARNING,
                    DiagnosticLevel::Info => DiagnosticSeverity::INFORMATION,
                }),
                code: None, // Core diagnostics don't have error codes yet
                source: Some("typedlua".to_string()),
                message: d.message,
                related_information: None,
                tags: None,
                code_description: None,
                data: None,
            })
            .collect()
    }
}

/// Convert a Span to an LSP Range
fn span_to_range(span: &Span) -> Range {
    Range {
        start: Position {
            line: (span.line.saturating_sub(1)) as u32,
            character: (span.column.saturating_sub(1)) as u32,
        },
        end: Position {
            // Span only has start position, so end is start + length
            line: (span.line.saturating_sub(1)) as u32,
            character: ((span.column + span.len()).saturating_sub(1)) as u32,
        },
    }
}

impl DiagnosticsProviderTrait for DiagnosticsProvider {
    fn provide(&self, document: &Document) -> Vec<Diagnostic> {
        self.provide_impl(document)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::document::Document;

    #[test]
    fn test_diagnostics_provider_new() {
        let provider = DiagnosticsProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_diagnostics_empty_document() {
        let doc = Document::new_test("".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Empty document should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_valid_code() {
        let doc = Document::new_test("local x = 1".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Valid code should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_function_declaration() {
        let doc = Document::new_test("function foo() end".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Valid function should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_with_type_annotation() {
        let doc = Document::new_test("local x: number = 1".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Correctly typed code should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_diagnostics_multiple_statements() {
        let code = "local x = 1\nlocal y = 2\nlocal z = x + y";
        let doc = Document::new_test(code.to_string(), 1);
        let provider = DiagnosticsProvider::new();

        let diagnostics = provider.provide(&doc);

        // Valid multi-statement code should have no diagnostics
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn test_span_to_range_basic() {
        let span = Span {
            start: 0,
            end: 10,
            line: 1,
            column: 1,
        };

        let range = span_to_range(&span);

        // Line and column are 0-indexed in LSP
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
        assert_eq!(range.end.line, 0);
        assert_eq!(range.end.character, 10);
    }

    #[test]
    fn test_span_to_range_multiline() {
        let span = Span {
            start: 0,
            end: 5,
            line: 5,
            column: 3,
        };

        let range = span_to_range(&span);

        assert_eq!(range.start.line, 4);
        assert_eq!(range.start.character, 2);
        assert_eq!(range.end.line, 4);
        assert_eq!(range.end.character, 7);
    }

    #[test]
    fn test_span_to_range_at_position_zero() {
        let span = Span {
            start: 0,
            end: 0,
            line: 0,
            column: 0,
        };

        let range = span_to_range(&span);

        // Should handle zero values gracefully (saturating_sub)
        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
    }

    #[test]
    fn test_diagnostics_provider_trait() {
        let doc = Document::new_test("local x = 1".to_string(), 1);
        let provider: Box<dyn DiagnosticsProviderTrait> = Box::new(DiagnosticsProvider::new());

        let diagnostics = provider.provide(&doc);

        // Should work through trait
        assert!(diagnostics.is_empty());
    }

    // ── ensure_type_checked tests ────────────────────────────────────

    #[test]
    fn test_ensure_type_checked_caches_result() {
        let doc = Document::new_test("local x = 1".to_string(), 1);

        // First call: computes and caches
        let result1 = DiagnosticsProvider::ensure_type_checked(&doc);
        assert!(result1.diagnostics.is_empty());

        // Second call: returns cached (same version)
        let result2 = DiagnosticsProvider::ensure_type_checked(&doc);
        assert!(result2.diagnostics.is_empty());

        // Cache should have recorded a hit on second call
        let stats = doc.cache().stats().clone();
        assert!(stats.hits >= 1, "Expected at least 1 cache hit");
    }

    #[test]
    fn test_ensure_type_checked_empty_document() {
        let doc = Document::new_test("".to_string(), 1);

        let result = DiagnosticsProvider::ensure_type_checked(&doc);

        assert!(result.symbols.is_empty());
        assert!(result.diagnostics.is_empty());
    }

    #[test]
    fn test_ensure_type_checked_captures_symbols() {
        let doc = Document::new_test("local x: number = 1".to_string(), 1);

        let result = DiagnosticsProvider::ensure_type_checked(&doc);

        assert!(result.diagnostics.is_empty());
        // Symbol "x" should be in the result
        assert!(
            result.symbols.contains_key("x"),
            "Expected symbol 'x' in {:?}",
            result.symbols.keys().collect::<Vec<_>>()
        );
        let sym = result.symbols.get("x").unwrap();
        assert_eq!(sym.type_display, "number");
    }

    #[test]
    fn test_ensure_type_checked_parse_error() {
        let doc = Document::new_test("local = =".to_string(), 1);

        let result = DiagnosticsProvider::ensure_type_checked(&doc);

        // Should have parse error diagnostic
        assert!(
            !result.diagnostics.is_empty(),
            "Expected diagnostics for parse error"
        );
        assert!(result.symbols.is_empty());
    }

    #[test]
    fn test_provide_impl_uses_cache() {
        let doc = Document::new_test("local x = 1".to_string(), 1);
        let provider = DiagnosticsProvider::new();

        // First call populates cache
        let d1 = provider.provide_impl(&doc);
        // Second call should hit cache
        let d2 = provider.provide_impl(&doc);

        assert_eq!(d1, d2);

        let stats = doc.cache().stats().clone();
        assert!(stats.hits >= 1, "Expected at least 1 cache hit");
    }

    #[test]
    fn test_ensure_type_checked_function_symbol() {
        let doc = Document::new_test("function foo() end".to_string(), 1);

        let result = DiagnosticsProvider::ensure_type_checked(&doc);

        assert!(result.diagnostics.is_empty());
        if let Some(sym) = result.symbols.get("foo") {
            assert_eq!(sym.kind, "function");
        }
    }
}
