use crate::document::Document;
use lsp_types::*;
use std::sync::Arc;
use typedlua_typechecker::diagnostics::CollectingDiagnosticHandler;
use typedlua_parser::string_interner::StringInterner;
use typedlua_typechecker::{Symbol, SymbolKind, TypeChecker};
use typedlua_parser::{Lexer, Parser};

/// Provides code completion (IntelliSense)
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
            ("enum", "Enum declaration", CompletionItemKind::KEYWORD),
            ("class", "Class declaration", CompletionItemKind::KEYWORD),
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

        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids);
        let mut ast = match parser.parse() {
            Ok(a) => a,
            Err(_) => return Vec::new(),
        };

        let mut type_checker = TypeChecker::new(handler, &interner, &common_ids);
        if type_checker.check_program(&mut ast).is_err() {
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
    }

    /// Format symbol detail for completion
    fn format_symbol_detail(symbol: &Symbol) -> String {
        use typedlua_parser::ast::types::{PrimitiveType, TypeKind};

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
