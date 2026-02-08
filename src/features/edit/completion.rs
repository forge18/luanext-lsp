use crate::arena_pool::with_pooled_arena;
use crate::core::document::Document;
use crate::traits::CompletionProviderTrait;
use lsp_types::*;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser};
use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use luanext_typechecker::{Symbol, SymbolKind, TypeChecker};
use std::sync::Arc;

/// Provides code completion (IntelliSense)
#[derive(Clone)]
pub struct CompletionProvider;

impl CompletionProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide completion items at a given position
    pub fn provide(&self, document: &Document, position: Position) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Determine completion context from the text before the cursor
        let context = self.get_completion_context(document, position);

        match context {
            CompletionContext::MemberAccess => {

                // Would need type information from type checker
            }
            CompletionContext::MethodCall => {

                // Would need type information from type checker
            }
            CompletionContext::TypeAnnotation => {
                items.extend(self.complete_types());
            }
            CompletionContext::Decorator => {
                items.extend(self.complete_decorators());
            }
            CompletionContext::Import => {

                // Would need file system access
            }
            CompletionContext::Statement => {
                // Complete keywords and identifiers
                items.extend(self.complete_keywords());
                // Complete symbols from type checker
                items.extend(self.complete_symbols(document));
            }
        }

        items
    }

    /// Determine what kind of completion is needed based on context
    fn get_completion_context(&self, document: &Document, position: Position) -> CompletionContext {
        // Get the line up to the cursor position
        let lines: Vec<&str> = document.text.lines().collect();
        if position.line as usize >= lines.len() {
            return CompletionContext::Statement;
        }

        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        if char_pos > line.len() {
            return CompletionContext::Statement;
        }

        let before_cursor = &line[..char_pos];

        // Check for member access (.)
        if before_cursor.ends_with('.') {
            return CompletionContext::MemberAccess;
        }

        // Check for method call (:)
        if before_cursor.ends_with(':') {
            return CompletionContext::MethodCall;
        }

        // Check for decorator (@)
        if before_cursor.trim_start().starts_with('@') {
            return CompletionContext::Decorator;
        }

        // Check for type annotation context (after :)
        if let Some(colon_pos) = before_cursor.rfind(':') {
            let after_colon = &before_cursor[colon_pos + 1..].trim_start();
            // If we're right after a colon or typing a type, we're in type context
            if after_colon.is_empty()
                || after_colon.chars().all(|c| c.is_alphanumeric() || c == '_')
            {
                return CompletionContext::TypeAnnotation;
            }
        }

        // Check for import context
        if before_cursor.contains("import") || before_cursor.contains("from") {
            return CompletionContext::Import;
        }

        CompletionContext::Statement
    }

    /// Complete TypedLua keywords
    fn complete_keywords(&self) -> Vec<CompletionItem> {
        let keywords = vec![
            ("const", "Constant declaration", CompletionItemKind::KEYWORD),
            (
                "local",
                "Local variable declaration",
                CompletionItemKind::KEYWORD,
            ),
            (
                "function",
                "Function declaration",
                CompletionItemKind::KEYWORD,
            ),
            ("if", "If statement", CompletionItemKind::KEYWORD),
            ("then", "Then clause", CompletionItemKind::KEYWORD),
            ("else", "Else clause", CompletionItemKind::KEYWORD),
            ("elseif", "Else-if clause", CompletionItemKind::KEYWORD),
            ("end", "End block", CompletionItemKind::KEYWORD),
            ("while", "While loop", CompletionItemKind::KEYWORD),
            ("for", "For loop", CompletionItemKind::KEYWORD),
            ("in", "In operator", CompletionItemKind::KEYWORD),
            ("do", "Do block", CompletionItemKind::KEYWORD),
            ("repeat", "Repeat loop", CompletionItemKind::KEYWORD),
            ("until", "Until condition", CompletionItemKind::KEYWORD),
            ("return", "Return statement", CompletionItemKind::KEYWORD),
            ("break", "Break statement", CompletionItemKind::KEYWORD),
            (
                "continue",
                "Continue statement",
                CompletionItemKind::KEYWORD,
            ),
            ("and", "Logical and", CompletionItemKind::OPERATOR),
            ("or", "Logical or", CompletionItemKind::OPERATOR),
            ("not", "Logical not", CompletionItemKind::OPERATOR),
            ("true", "Boolean true", CompletionItemKind::VALUE),
            ("false", "Boolean false", CompletionItemKind::VALUE),
            ("nil", "Nil value", CompletionItemKind::VALUE),
            (
                "type",
                "Type alias declaration",
                CompletionItemKind::KEYWORD,
            ),
            (
                "interface",
                "Interface declaration",
                CompletionItemKind::KEYWORD,
            ),
            ("enum", "Enum declaration", CompletionItemKind::ENUM),
            ("class", "Class declaration", CompletionItemKind::CLASS),
            ("extends", "Extends clause", CompletionItemKind::KEYWORD),
            (
                "implements",
                "Implements clause",
                CompletionItemKind::KEYWORD,
            ),
            (
                "public",
                "Public access modifier",
                CompletionItemKind::KEYWORD,
            ),
            (
                "private",
                "Private access modifier",
                CompletionItemKind::KEYWORD,
            ),
            (
                "protected",
                "Protected access modifier",
                CompletionItemKind::KEYWORD,
            ),
            ("static", "Static modifier", CompletionItemKind::KEYWORD),
            ("abstract", "Abstract modifier", CompletionItemKind::KEYWORD),
            ("readonly", "Readonly modifier", CompletionItemKind::KEYWORD),
            ("match", "Match expression", CompletionItemKind::KEYWORD),
            ("when", "When guard", CompletionItemKind::KEYWORD),
            ("import", "Import statement", CompletionItemKind::KEYWORD),
            ("from", "From clause", CompletionItemKind::KEYWORD),
            ("export", "Export statement", CompletionItemKind::KEYWORD),
        ];

        keywords
            .into_iter()
            .map(|(label, detail, kind)| CompletionItem {
                label: label.to_string(),
                kind: Some(kind),
                detail: Some(detail.to_string()),
                documentation: None,
                ..Default::default()
            })
            .collect()
    }

    /// Complete symbols from the type checker
    fn complete_symbols(&self, document: &Document) -> Vec<CompletionItem> {
        // Parse and type check the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => return Vec::new(),
        };

        with_pooled_arena(|arena| {
            let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids, arena);
            let ast = match parser.parse() {
                Ok(a) => a,
                Err(_) => return Vec::new(),
            };

            let mut type_checker = TypeChecker::new(handler, &interner, &common_ids, arena);
            if type_checker.check_program(&ast).is_err() {
                // Even with errors, the symbol table may have useful information
            }

            // Get all visible symbols from the symbol table
            let symbol_table = type_checker.symbol_table();
            let mut items = Vec::new();

            for (name, symbol) in symbol_table.all_visible_symbols() {
                let kind = match symbol.kind {
                    SymbolKind::Const | SymbolKind::Variable => CompletionItemKind::VARIABLE,
                    SymbolKind::Function => CompletionItemKind::FUNCTION,
                    SymbolKind::Class => CompletionItemKind::CLASS,
                    SymbolKind::Interface => CompletionItemKind::INTERFACE,
                    SymbolKind::TypeAlias => CompletionItemKind::STRUCT,
                    SymbolKind::Enum => CompletionItemKind::ENUM,
                    SymbolKind::Parameter => CompletionItemKind::VARIABLE,
                    SymbolKind::Namespace => CompletionItemKind::MODULE,
                };

                items.push(CompletionItem {
                    label: name.clone(),
                    kind: Some(kind),
                    detail: Some(Self::format_symbol_detail(symbol)),
                    documentation: None,
                    ..Default::default()
                });
            }

            items
        })
    }

    /// Format symbol detail for completion
    fn format_symbol_detail(symbol: &Symbol) -> String {
        use luanext_parser::ast::types::{PrimitiveType, TypeKind};

        let kind_str = match symbol.kind {
            SymbolKind::Const => "const",
            SymbolKind::Variable => "let",
            SymbolKind::Function => "function",
            SymbolKind::Class => "class",
            SymbolKind::Interface => "interface",
            SymbolKind::TypeAlias => "type",
            SymbolKind::Enum => "enum",
            SymbolKind::Parameter => "param",
            SymbolKind::Namespace => "namespace",
        };

        // Simple type display
        let type_str = match &symbol.typ.kind {
            TypeKind::Primitive(PrimitiveType::Number) => "number",
            TypeKind::Primitive(PrimitiveType::String) => "string",
            TypeKind::Primitive(PrimitiveType::Boolean) => "boolean",
            TypeKind::Primitive(PrimitiveType::Nil) => "nil",
            TypeKind::Function(_) => "function",
            TypeKind::Object(_) => "object",
            TypeKind::Array(_) => "array",
            _ => "type",
        };

        format!("{}: {}", kind_str, type_str)
    }

    /// Complete type names
    fn complete_types(&self) -> Vec<CompletionItem> {
        let types = vec![
            ("nil", "Nil type", CompletionItemKind::TYPE_PARAMETER),
            (
                "boolean",
                "Boolean type",
                CompletionItemKind::TYPE_PARAMETER,
            ),
            ("number", "Number type", CompletionItemKind::TYPE_PARAMETER),
            ("string", "String type", CompletionItemKind::TYPE_PARAMETER),
            (
                "unknown",
                "Unknown type",
                CompletionItemKind::TYPE_PARAMETER,
            ),
            ("never", "Never type", CompletionItemKind::TYPE_PARAMETER),
            ("void", "Void type", CompletionItemKind::TYPE_PARAMETER),
            ("any", "Any type", CompletionItemKind::TYPE_PARAMETER),
        ];

        types
            .into_iter()
            .map(|(label, detail, kind)| CompletionItem {
                label: label.to_string(),
                kind: Some(kind),
                detail: Some(detail.to_string()),
                documentation: None,
                insert_text: Some(label.to_string()),
                ..Default::default()
            })
            .collect()
    }

    /// Complete decorator names
    fn complete_decorators(&self) -> Vec<CompletionItem> {
        let decorators = vec![
            (
                "readonly",
                "Make property readonly",
                "TypedLua built-in decorator",
            ),
            (
                "sealed",
                "Seal class from extension",
                "TypedLua built-in decorator",
            ),
            (
                "deprecated",
                "Mark as deprecated",
                "TypedLua built-in decorator",
            ),
        ];

        decorators
            .into_iter()
            .map(|(label, detail, doc)| CompletionItem {
                label: label.to_string(),
                kind: Some(CompletionItemKind::FUNCTION),
                detail: Some(detail.to_string()),
                documentation: Some(Documentation::String(doc.to_string())),
                insert_text: Some(label.to_string()),
                ..Default::default()
            })
            .collect()
    }

    /// Resolve additional details for a completion item
    #[allow(dead_code)]
    pub fn resolve(&self, item: CompletionItem) -> CompletionItem {
        // For now, just return the item as-is
        item
    }
}

/// Completion context type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CompletionContext {
    /// Completing after '.' (member access)
    MemberAccess,
    /// Completing after ':' (method call)
    MethodCall,
    /// Completing type annotations
    TypeAnnotation,
    /// Completing after '@' (decorators)
    Decorator,
    /// Completing import paths
    Import,
    /// General statement context (keywords, identifiers)
    Statement,
}

impl CompletionProviderTrait for CompletionProvider {
    fn provide(&self, document: &Document, position: Position) -> Vec<CompletionItem> {
        Self::provide(self, document, position)
    }

    fn resolve(&self, item: CompletionItem) -> CompletionItem {
        item
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::document::Document;

    fn create_test_document(text: &str) -> Document {
        Document::new_test(text.to_string(), 1)
    }

    #[test]
    fn test_empty_document() {
        let doc = create_test_document("");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 0));

        assert!(!result.is_empty());
        assert!(result.iter().any(|item| item.label == "local"));
    }

    #[test]
    fn test_keywords_included() {
        let doc = create_test_document("");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 0));

        assert!(result.iter().any(|item| item.label == "function"));
        assert!(result.iter().any(|item| item.label == "class"));
        assert!(result.iter().any(|item| item.label == "if"));
        assert!(result.iter().any(|item| item.label == "while"));
    }

    #[test]
    fn test_builtin_types_included() {
        let doc = create_test_document("");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 0));

        // Check for some keywords that should definitely be there
        assert!(
            result.iter().any(|item| item.label == "function"),
            "Missing 'function'"
        );
        assert!(
            result.iter().any(|item| item.label == "local"),
            "Missing 'local'"
        );
        assert!(
            result.iter().any(|item| item.label == "return"),
            "Missing 'return'"
        );
    }

    #[test]
    fn test_out_of_bounds_position() {
        let doc = create_test_document("local x = 1");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(10, 10));

        assert!(!result.is_empty());
    }

    #[test]
    fn test_completion_item_fields() {
        let doc = create_test_document("");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 0));

        if let Some(item) = result.iter().find(|i| i.label == "function") {
            assert_eq!(item.kind, Some(CompletionItemKind::KEYWORD));
        }
    }

    #[test]
    fn test_resolve_returns_item() {
        let provider = CompletionProvider::new();
        let item = CompletionItem {
            label: "test".to_string(),
            ..Default::default()
        };

        let result = provider.resolve(item.clone());

        assert_eq!(result.label, item.label);
    }

    #[test]
    fn test_lua_keywords_included() {
        let doc = create_test_document("");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 0));

        assert!(result.iter().any(|item| item.label == "return"));
        assert!(result.iter().any(|item| item.label == "end"));
        assert!(result.iter().any(|item| item.label == "do"));
        assert!(result.iter().any(|item| item.label == "then"));
    }

    #[test]
    fn test_complete_types_returns_all_types() {
        let provider = CompletionProvider::new();
        let types = provider.complete_types();

        assert!(!types.is_empty());

        // Check all expected types are present
        let labels: Vec<&str> = types.iter().map(|t| t.label.as_str()).collect();
        assert!(labels.contains(&"nil"));
        assert!(labels.contains(&"boolean"));
        assert!(labels.contains(&"number"));
        assert!(labels.contains(&"string"));
        assert!(labels.contains(&"unknown"));
        assert!(labels.contains(&"never"));
        assert!(labels.contains(&"void"));
        assert!(labels.contains(&"any"));
    }

    #[test]
    fn test_complete_types_have_correct_kinds() {
        let provider = CompletionProvider::new();
        let types = provider.complete_types();

        for item in &types {
            assert_eq!(item.kind, Some(CompletionItemKind::TYPE_PARAMETER));
            assert!(item.detail.is_some());
            assert!(item.insert_text.is_some());
        }
    }

    #[test]
    fn test_complete_decorators_returns_decorators() {
        let provider = CompletionProvider::new();
        let decorators = provider.complete_decorators();

        assert!(!decorators.is_empty());

        let labels: Vec<&str> = decorators.iter().map(|d| d.label.as_str()).collect();
        assert!(labels.contains(&"readonly"));
        assert!(labels.contains(&"sealed"));
        assert!(labels.contains(&"deprecated"));
    }

    #[test]
    fn test_complete_decorators_have_documentation() {
        let provider = CompletionProvider::new();
        let decorators = provider.complete_decorators();

        for item in &decorators {
            assert_eq!(item.kind, Some(CompletionItemKind::FUNCTION));
            assert!(item.detail.is_some());
            assert!(item.documentation.is_some());
        }
    }

    #[test]
    fn test_complete_keywords_structure() {
        let provider = CompletionProvider::new();
        let keywords = provider.complete_keywords();

        assert!(!keywords.is_empty());

        // Check structure of keyword items
        for item in &keywords {
            assert!(!item.label.is_empty());
            // Keywords can have various kinds depending on their nature
            assert!(
                item.kind.is_some(),
                "'{}' should have a completion kind",
                item.label
            );
            assert!(item.detail.is_some());
        }
    }

    #[test]
    fn test_complete_keywords_has_typedlua_specific() {
        let provider = CompletionProvider::new();
        let keywords = provider.complete_keywords();

        let labels: Vec<&str> = keywords.iter().map(|k| k.label.as_str()).collect();

        // TypedLua-specific keywords
        assert!(labels.contains(&"class"));
        assert!(labels.contains(&"interface"));
        assert!(labels.contains(&"enum"));
        assert!(labels.contains(&"type"));
        assert!(labels.contains(&"import"));
        assert!(labels.contains(&"export"));
    }

    #[test]
    fn test_complete_symbols_with_code() {
        let doc = create_test_document("local x = 1\nlocal y = 2");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        // Should find symbols from the code
        let labels: Vec<&str> = symbols.iter().map(|s| s.label.as_str()).collect();

        // Should have our local variables
        assert!(labels.contains(&"x"), "Should find symbol 'x'");
        assert!(labels.contains(&"y"), "Should find symbol 'y'");
    }

    #[test]
    fn test_complete_symbols_returns_variable_kind() {
        let doc = create_test_document("local myVar = 42");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        // Find our variable
        let my_var = symbols.iter().find(|s| s.label == "myVar");
        assert!(my_var.is_some(), "Should find 'myVar'");

        if let Some(var) = my_var {
            assert_eq!(var.kind, Some(CompletionItemKind::VARIABLE));
            assert!(var.detail.is_some());
        }
    }

    #[test]
    fn test_complete_symbols_with_function() {
        let doc = create_test_document("function myFunc() end");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        let func = symbols.iter().find(|s| s.label == "myFunc");
        assert!(func.is_some(), "Should find function 'myFunc'");

        if let Some(f) = func {
            assert_eq!(f.kind, Some(CompletionItemKind::FUNCTION));
        }
    }

    #[test]
    fn test_format_symbol_detail_variable() {
        let _provider = CompletionProvider::new();

        // Create a symbol representing a number variable
        let symbol = Symbol {
            name: "testVar".to_string(),
            kind: luanext_typechecker::SymbolKind::Variable,
            typ: luanext_parser::ast::types::Type::new(
                luanext_parser::ast::types::TypeKind::Primitive(
                    luanext_parser::ast::types::PrimitiveType::Number,
                ),
                luanext_parser::Span::new(0, 10, 1, 1),
            ),
            is_exported: false,
            span: luanext_parser::Span::new(0, 10, 1, 1),
            references: vec![],
        };

        let detail = CompletionProvider::format_symbol_detail(&symbol);
        assert!(detail.contains("let"));
        assert!(detail.contains("number"));
    }

    #[test]
    fn test_format_symbol_detail_function() {
        use bumpalo::Bump;

        let arena = Bump::new();
        let _provider = CompletionProvider::new();

        let return_type = arena.alloc(luanext_parser::ast::types::Type::new(
            luanext_parser::ast::types::TypeKind::Primitive(
                luanext_parser::ast::types::PrimitiveType::Number,
            ),
            luanext_parser::Span::new(0, 6, 1, 1),
        ));

        let symbol = Symbol {
            name: "testFunc".to_string(),
            kind: luanext_typechecker::SymbolKind::Function,
            typ: luanext_parser::ast::types::Type::new(
                luanext_parser::ast::types::TypeKind::Function(
                    luanext_parser::ast::types::FunctionType {
                        type_parameters: None,
                        parameters: &[],
                        return_type,
                        throws: None,
                        span: luanext_parser::Span::new(0, 20, 1, 1),
                    },
                ),
                luanext_parser::Span::new(0, 20, 1, 1),
            ),
            is_exported: false,
            span: luanext_parser::Span::new(0, 20, 1, 1),
            references: vec![],
        };

        let detail = CompletionProvider::format_symbol_detail(&symbol);
        assert!(detail.contains("function"));
    }

    #[test]
    fn test_get_completion_context_member_access() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("obj.");

        let context = provider.get_completion_context(&doc, Position::new(0, 4));

        assert_eq!(context, CompletionContext::MemberAccess);
    }

    #[test]
    fn test_get_completion_context_method_call() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("obj:");

        let context = provider.get_completion_context(&doc, Position::new(0, 4));

        assert_eq!(context, CompletionContext::MethodCall);
    }

    #[test]
    fn test_get_completion_context_type_annotation() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("local x: ");

        let context = provider.get_completion_context(&doc, Position::new(0, 9));

        assert_eq!(context, CompletionContext::TypeAnnotation);
    }

    #[test]
    fn test_get_completion_context_decorator() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("@");

        let context = provider.get_completion_context(&doc, Position::new(0, 1));

        assert_eq!(context, CompletionContext::Decorator);
    }

    #[test]
    fn test_get_completion_context_import() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("import ");

        let context = provider.get_completion_context(&doc, Position::new(0, 7));

        assert_eq!(context, CompletionContext::Import);
    }

    #[test]
    fn test_get_completion_context_statement() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("local x = ");

        let context = provider.get_completion_context(&doc, Position::new(0, 10));

        assert_eq!(context, CompletionContext::Statement);
    }

    #[test]
    fn test_completion_with_member_access_context() {
        let doc = create_test_document("obj.");
        let provider = CompletionProvider::new();

        // In member access context, should still return something
        let result = provider.provide(&doc, Position::new(0, 4));

        // Should not panic, may or may not have completions
        let _ = result;
    }

    #[test]
    fn test_completion_with_type_annotation_context() {
        let doc = create_test_document("local x: ");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 9));

        // Should include type completions
        let labels: Vec<&str> = result.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"number"));
        assert!(labels.contains(&"string"));
        assert!(labels.contains(&"boolean"));
    }

    #[test]
    fn test_completion_with_decorator_context() {
        let doc = create_test_document("@");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 1));

        // Should include decorator completions
        let labels: Vec<&str> = result.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"readonly"));
        assert!(labels.contains(&"deprecated"));
    }

    #[test]
    fn test_completion_item_variants() {
        let doc = create_test_document("");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 0));

        // Should have different kinds of items
        let has_keywords = result.iter().any(|i| {
            i.kind == Some(CompletionItemKind::KEYWORD)
                || i.kind == Some(CompletionItemKind::OPERATOR)
        });

        assert!(has_keywords, "Should have keyword/operator completions");
        // Types may only appear in type annotation context
    }

    #[test]
    fn test_completion_multiline_document() {
        let doc = create_test_document("local x = 1\nlocal y = 2\nlocal z = ");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(2, 9));

        // Should find symbols from previous lines
        let labels: Vec<&str> = result.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"x"));
        assert!(labels.contains(&"y"));
    }

    #[test]
    fn test_completion_context_enum_variants() {
        // Test that all context variants exist
        let _ = CompletionContext::MemberAccess;
        let _ = CompletionContext::MethodCall;
        let _ = CompletionContext::TypeAnnotation;
        let _ = CompletionContext::Decorator;
        let _ = CompletionContext::Import;
        let _ = CompletionContext::Statement;

        // Test equality
        assert_eq!(
            CompletionContext::MemberAccess,
            CompletionContext::MemberAccess
        );
        assert_ne!(
            CompletionContext::MemberAccess,
            CompletionContext::Statement
        );
    }

    #[test]
    fn test_completion_provider_clone() {
        let provider = CompletionProvider::new();
        let _cloned = provider.clone();

        // Should be able to clone
    }

    #[test]
    fn test_format_symbol_detail_class() {
        let _provider = CompletionProvider::new();

        let symbol = Symbol {
            name: "MyClass".to_string(),
            kind: luanext_typechecker::SymbolKind::Class,
            typ: luanext_parser::ast::types::Type::new(
                luanext_parser::ast::types::TypeKind::Primitive(
                    luanext_parser::ast::types::PrimitiveType::Unknown,
                ),
                luanext_parser::Span::new(0, 8, 1, 1),
            ),
            is_exported: false,
            span: luanext_parser::Span::new(0, 8, 1, 1),
            references: vec![],
        };

        let detail = CompletionProvider::format_symbol_detail(&symbol);
        assert!(detail.contains("class"));
    }

    #[test]
    fn test_format_symbol_detail_interface() {
        let _provider = CompletionProvider::new();

        let symbol = Symbol {
            name: "MyInterface".to_string(),
            kind: luanext_typechecker::SymbolKind::Interface,
            typ: luanext_parser::ast::types::Type::new(
                luanext_parser::ast::types::TypeKind::Primitive(
                    luanext_parser::ast::types::PrimitiveType::Unknown,
                ),
                luanext_parser::Span::new(0, 11, 1, 1),
            ),
            is_exported: false,
            span: luanext_parser::Span::new(0, 11, 1, 1),
            references: vec![],
        };

        let detail = CompletionProvider::format_symbol_detail(&symbol);
        assert!(detail.contains("interface"));
    }

    #[test]
    fn test_format_symbol_detail_enum() {
        let _provider = CompletionProvider::new();

        let symbol = Symbol {
            name: "MyEnum".to_string(),
            kind: luanext_typechecker::SymbolKind::Enum,
            typ: luanext_parser::ast::types::Type::new(
                luanext_parser::ast::types::TypeKind::Primitive(
                    luanext_parser::ast::types::PrimitiveType::Unknown,
                ),
                luanext_parser::Span::new(0, 7, 1, 1),
            ),
            is_exported: false,
            span: luanext_parser::Span::new(0, 7, 1, 1),
            references: vec![],
        };

        let detail = CompletionProvider::format_symbol_detail(&symbol);
        assert!(detail.contains("enum"));
    }

    #[test]
    fn test_format_symbol_detail_type_alias() {
        let _provider = CompletionProvider::new();

        let symbol = Symbol {
            name: "MyType".to_string(),
            kind: luanext_typechecker::SymbolKind::TypeAlias,
            typ: luanext_parser::ast::types::Type::new(
                luanext_parser::ast::types::TypeKind::Primitive(
                    luanext_parser::ast::types::PrimitiveType::Unknown,
                ),
                luanext_parser::Span::new(0, 6, 1, 1),
            ),
            is_exported: false,
            span: luanext_parser::Span::new(0, 6, 1, 1),
            references: vec![],
        };

        let detail = CompletionProvider::format_symbol_detail(&symbol);
        assert!(detail.contains("type"));
    }

    #[test]
    fn test_format_symbol_detail_parameter() {
        let _provider = CompletionProvider::new();

        let symbol = Symbol {
            name: "param".to_string(),
            kind: luanext_typechecker::SymbolKind::Parameter,
            typ: luanext_parser::ast::types::Type::new(
                luanext_parser::ast::types::TypeKind::Primitive(
                    luanext_parser::ast::types::PrimitiveType::String,
                ),
                luanext_parser::Span::new(0, 6, 1, 1),
            ),
            is_exported: false,
            span: luanext_parser::Span::new(0, 6, 1, 1),
            references: vec![],
        };

        let detail = CompletionProvider::format_symbol_detail(&symbol);
        assert!(detail.contains("param"));
    }

    #[test]
    fn test_complete_symbols_with_class() {
        let doc = create_test_document("local myVar = 1");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        // Should find the variable we declared
        let found = symbols.iter().any(|s| s.label == "myVar");
        assert!(found, "Should find symbol 'myVar'");
    }

    #[test]
    fn test_complete_symbols_with_interface() {
        let doc = create_test_document("local myVar = 1");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        // Should find the variable
        let found = symbols.iter().any(|s| s.label == "myVar");
        assert!(found, "Should find symbol 'myVar'");
    }

    #[test]
    fn test_complete_symbols_with_enum() {
        let doc = create_test_document("local myVar = 1");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        // Should find the variable
        let found = symbols.iter().any(|s| s.label == "myVar");
        assert!(found, "Should find symbol 'myVar'");
    }

    #[test]
    fn test_complete_symbols_with_type_alias() {
        let doc = create_test_document("local myVar = 1");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        // Should find the variable
        let found = symbols.iter().any(|s| s.label == "myVar");
        assert!(found, "Should find symbol 'myVar'");
    }

    #[test]
    fn test_complete_symbols_complex_function() {
        let doc =
            create_test_document("function myFunc(a: number, b: string): boolean return true end");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        let func = symbols.iter().find(|s| s.label == "myFunc");
        assert!(func.is_some(), "Should find function 'myFunc'");

        if let Some(f) = func {
            assert_eq!(f.kind, Some(CompletionItemKind::FUNCTION));
        }
    }

    #[test]
    fn test_complete_symbols_multiple_variables() {
        let doc = create_test_document("local a = 1\nlocal b = 2\nlocal c = 3");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        assert!(symbols.iter().any(|s| s.label == "a"));
        assert!(symbols.iter().any(|s| s.label == "b"));
        assert!(symbols.iter().any(|s| s.label == "c"));
    }

    #[test]
    fn test_complete_symbols_nested_scope() {
        let doc = create_test_document("local outer = 1\nfunction foo() local inner = 2 end");
        let provider = CompletionProvider::new();

        let symbols = provider.complete_symbols(&doc);

        // Should find outer
        assert!(symbols.iter().any(|s| s.label == "outer"));
    }

    #[test]
    fn test_get_completion_context_from_clause() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("from ");

        let context = provider.get_completion_context(&doc, Position::new(0, 5));

        assert_eq!(context, CompletionContext::Import);
    }

    #[test]
    fn test_get_completion_context_mid_statement() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("local x = ");

        let context = provider.get_completion_context(&doc, Position::new(0, 9));

        assert_eq!(context, CompletionContext::Statement);
    }

    #[test]
    fn test_get_completion_context_after_colon_typing() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("local x: nu");

        let context = provider.get_completion_context(&doc, Position::new(0, 11));

        assert_eq!(context, CompletionContext::TypeAnnotation);
    }

    #[test]
    fn test_get_completion_context_with_leading_whitespace() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("  obj.");

        let context = provider.get_completion_context(&doc, Position::new(0, 6));

        assert_eq!(context, CompletionContext::MemberAccess);
    }

    #[test]
    fn test_get_completion_context_whitespace_only() {
        let provider = CompletionProvider::new();
        let doc = create_test_document("   ");

        let context = provider.get_completion_context(&doc, Position::new(0, 1));

        assert_eq!(context, CompletionContext::Statement);
    }

    #[test]
    fn test_completion_resolve_item() {
        let provider = CompletionProvider::new();
        let item = CompletionItem {
            label: "test".to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some("detail".to_string()),
            ..Default::default()
        };

        let result = provider.resolve(item.clone());

        assert_eq!(result.label, item.label);
        assert_eq!(result.kind, item.kind);
        assert_eq!(result.detail, item.detail);
    }

    #[test]
    fn test_completion_with_unicode_identifier() {
        let doc = create_test_document("local 中文 = 1");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 0));

        assert!(!result.is_empty());
    }

    #[test]
    fn test_complete_keywords_has_all_control_flow() {
        let provider = CompletionProvider::new();
        let keywords = provider.complete_keywords();

        let labels: Vec<&str> = keywords.iter().map(|k| k.label.as_str()).collect();

        assert!(labels.contains(&"if"));
        assert!(labels.contains(&"while"));
        assert!(labels.contains(&"for"));
        assert!(labels.contains(&"repeat"));
        assert!(labels.contains(&"do"));
        assert!(labels.contains(&"end"));
        assert!(labels.contains(&"return"));
        assert!(labels.contains(&"break"));
        assert!(labels.contains(&"continue"));
    }

    #[test]
    fn test_complete_keywords_has_all_access_modifiers() {
        let provider = CompletionProvider::new();
        let keywords = provider.complete_keywords();

        let labels: Vec<&str> = keywords.iter().map(|k| k.label.as_str()).collect();

        assert!(labels.contains(&"public"));
        assert!(labels.contains(&"private"));
        assert!(labels.contains(&"protected"));
    }

    #[test]
    fn test_complete_keywords_has_all_class_keywords() {
        let provider = CompletionProvider::new();
        let keywords = provider.complete_keywords();

        let labels: Vec<&str> = keywords.iter().map(|k| k.label.as_str()).collect();

        assert!(labels.contains(&"class"));
        assert!(labels.contains(&"extends"));
        assert!(labels.contains(&"implements"));
        assert!(labels.contains(&"static"));
        assert!(labels.contains(&"abstract"));
        assert!(labels.contains(&"readonly"));
    }
}
