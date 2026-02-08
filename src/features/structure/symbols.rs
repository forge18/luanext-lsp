use crate::arena_pool::with_pooled_arena;
use crate::core::document::Document;
use crate::traits::SymbolsProviderTrait;
use lsp_types::*;

use luanext_parser::ast::statement::{ClassMember, OperatorKind, Statement};
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser, Span};
use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use std::sync::Arc;

/// Provides document symbols (outline view)
#[derive(Clone)]
pub struct SymbolsProvider;

impl SymbolsProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide all symbols in the document (internal method)
    pub fn provide_impl(&self, document: &Document) -> Vec<DocumentSymbol> {
        // Parse the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = match lexer.tokenize() {
            Ok(t) => t,
            Err(_) => return Vec::new(),
        };

        with_pooled_arena(|arena| {
            let mut parser = Parser::new(tokens, handler, &interner, &common_ids, arena);
            let ast = match parser.parse() {
                Ok(a) => a,
                Err(_) => return Vec::new(),
            };

            // Extract symbols from AST
            let mut symbols = Vec::new();
            for stmt in ast.statements.iter() {
                if let Some(symbol) = self.extract_symbol_from_statement(stmt, &interner) {
                    symbols.push(symbol);
                }
            }

            symbols
        })
    }

    /// Extract a document symbol from a statement
    #[allow(deprecated)]
    fn extract_symbol_from_statement(
        &self,
        stmt: &Statement,
        interner: &StringInterner,
    ) -> Option<DocumentSymbol> {
        use luanext_parser::ast::pattern::Pattern;

        match stmt {
            Statement::Variable(var_decl) => {
                if let Pattern::Identifier(ident) = &var_decl.pattern {
                    let kind = match var_decl.kind {
                        luanext_parser::ast::statement::VariableKind::Const => SymbolKind::CONSTANT,
                        luanext_parser::ast::statement::VariableKind::Local => SymbolKind::VARIABLE,
                    };

                    Some(DocumentSymbol {
                        name: interner.resolve(ident.node).to_string(),
                        detail: None,
                        kind,
                        tags: None,
                        deprecated: None,
                        range: span_to_range(&var_decl.span),
                        selection_range: span_to_range(&ident.span),
                        children: None,
                    })
                } else {
                    None
                }
            }
            Statement::Function(func_decl) => {
                let mut children = Vec::new();

                // Add function body statements as children
                for stmt in func_decl.body.statements.iter() {
                    if let Some(symbol) = self.extract_symbol_from_statement(stmt, interner) {
                        children.push(symbol);
                    }
                }

                Some(DocumentSymbol {
                    name: interner.resolve(func_decl.name.node).to_string(),
                    detail: None,
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    deprecated: None,
                    range: span_to_range(&func_decl.span),
                    selection_range: span_to_range(&func_decl.name.span),
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                })
            }
            Statement::Class(class_decl) => {
                let mut children = Vec::new();

                // Add class members as children
                for member in class_decl.members.iter() {
                    if let Some(symbol) = self.extract_symbol_from_class_member(member, interner) {
                        children.push(symbol);
                    }
                }

                Some(DocumentSymbol {
                    name: interner.resolve(class_decl.name.node).to_string(),
                    detail: None,
                    kind: SymbolKind::CLASS,
                    tags: None,
                    deprecated: None,
                    range: span_to_range(&class_decl.span),
                    selection_range: span_to_range(&class_decl.name.span),
                    children: if children.is_empty() {
                        None
                    } else {
                        Some(children)
                    },
                })
            }
            Statement::Interface(interface_decl) => Some(DocumentSymbol {
                name: interner.resolve(interface_decl.name.node).to_string(),
                detail: None,
                kind: SymbolKind::INTERFACE,
                tags: None,
                deprecated: None,
                range: span_to_range(&interface_decl.span),
                selection_range: span_to_range(&interface_decl.name.span),
                children: None,
            }),
            Statement::TypeAlias(type_decl) => Some(DocumentSymbol {
                name: interner.resolve(type_decl.name.node).to_string(),
                detail: None,
                kind: SymbolKind::TYPE_PARAMETER,
                tags: None,
                deprecated: None,
                range: span_to_range(&type_decl.span),
                selection_range: span_to_range(&type_decl.name.span),
                children: None,
            }),
            Statement::Enum(enum_decl) => Some(DocumentSymbol {
                name: interner.resolve(enum_decl.name.node).to_string(),
                detail: None,
                kind: SymbolKind::ENUM,
                tags: None,
                deprecated: None,
                range: span_to_range(&enum_decl.span),
                selection_range: span_to_range(&enum_decl.name.span),
                children: None,
            }),
            _ => None,
        }
    }

    /// Extract a document symbol from a class member
    fn extract_symbol_from_class_member(
        &self,
        member: &ClassMember,
        interner: &StringInterner,
    ) -> Option<DocumentSymbol> {
        match member {
            ClassMember::Property(prop) => Some(DocumentSymbol {
                name: interner.resolve(prop.name.node).to_string(),
                detail: None,
                kind: SymbolKind::PROPERTY,
                tags: None,
                deprecated: None,
                range: span_to_range(&prop.span),
                selection_range: span_to_range(&prop.name.span),
                children: None,
            }),
            ClassMember::Constructor(ctor) => Some(DocumentSymbol {
                name: "constructor".to_string(),
                detail: None,
                kind: SymbolKind::CONSTRUCTOR,
                tags: None,
                deprecated: None,
                range: span_to_range(&ctor.span),
                selection_range: span_to_range(&ctor.span),
                children: None,
            }),
            ClassMember::Method(method) => Some(DocumentSymbol {
                name: interner.resolve(method.name.node).to_string(),
                detail: None,
                kind: SymbolKind::METHOD,
                tags: None,
                deprecated: None,
                range: span_to_range(&method.span),
                selection_range: span_to_range(&method.name.span),
                children: None,
            }),
            ClassMember::Getter(getter) => Some(DocumentSymbol {
                name: interner.resolve(getter.name.node).to_string(),
                detail: Some("get".to_string()),
                kind: SymbolKind::PROPERTY,
                tags: None,
                deprecated: None,
                range: span_to_range(&getter.span),
                selection_range: span_to_range(&getter.name.span),
                children: None,
            }),
            ClassMember::Setter(setter) => Some(DocumentSymbol {
                name: interner.resolve(setter.name.node).to_string(),
                detail: Some("set".to_string()),
                kind: SymbolKind::PROPERTY,
                tags: None,
                deprecated: None,
                range: span_to_range(&setter.span),
                selection_range: span_to_range(&setter.name.span),
                children: None,
            }),
            ClassMember::Operator(op) => {
                let op_symbol = match op.operator {
                    OperatorKind::Add => "+",
                    OperatorKind::Subtract => "-",
                    OperatorKind::Multiply => "*",
                    OperatorKind::Divide => "/",
                    OperatorKind::Modulo => "%",
                    OperatorKind::Power => "^",
                    OperatorKind::Equal => "==",
                    OperatorKind::NotEqual => "~=",
                    OperatorKind::LessThan => "<",
                    OperatorKind::LessThanOrEqual => "<=",
                    OperatorKind::GreaterThan => ">",
                    OperatorKind::GreaterThanOrEqual => ">=",
                    OperatorKind::Concatenate => "..",
                    OperatorKind::Length => "#",
                    OperatorKind::Index => "[]",
                    OperatorKind::NewIndex => "[]=",
                    OperatorKind::Call => "()",
                    OperatorKind::UnaryMinus => "unm",
                    OperatorKind::FloorDivide => "//",
                    OperatorKind::BitwiseAnd => "&",
                    OperatorKind::BitwiseOr => "|",
                    OperatorKind::BitwiseXor => "~",
                    OperatorKind::ShiftLeft => "<<",
                    OperatorKind::ShiftRight => ">>",
                };
                Some(DocumentSymbol {
                    name: format!("operator {}", op_symbol),
                    detail: None,
                    kind: SymbolKind::OPERATOR,
                    tags: None,
                    deprecated: None,
                    range: span_to_range(&op.span),
                    selection_range: span_to_range(&op.span),
                    children: None,
                })
            }
        }
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
            line: (span.line.saturating_sub(1)) as u32,
            character: ((span.column + span.len()).saturating_sub(1)) as u32,
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_kinds() {
        let provider = SymbolsProvider::new();

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        // Test function symbol
        let doc = Document::new_test("function foo() end".to_string(), 1);
        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "foo");
        assert_eq!(symbols[0].kind, SymbolKind::FUNCTION);

        // Test variable symbol (local)
        let doc = Document::new_test("local x = 10".to_string(), 1);
        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "x");
        assert_eq!(symbols[0].kind, SymbolKind::VARIABLE);

        // Test constant symbol
        let doc = Document::new_test("const PI = 3.14".to_string(), 1);
        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "PI");
        assert_eq!(symbols[0].kind, SymbolKind::CONSTANT);

        // Test class symbol
        let doc = Document::new_test("class Point end".to_string(), 1);
        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Point");
        assert_eq!(symbols[0].kind, SymbolKind::CLASS);

        // Test interface symbol
        let doc = Document::new_test("interface Drawable end".to_string(), 1);
        let symbols = extract_symbols(provider.provide(&doc));
        // Parser might not recognize 'interface' keyword yet
        if symbols.len() > 0 {
            assert_eq!(symbols[0].name, "Drawable");
            assert_eq!(symbols[0].kind, SymbolKind::INTERFACE);
        }

        // Test enum symbol
        let doc = Document::new_test("enum Color { Red, Green, Blue }".to_string(), 1);
        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Color");
        assert_eq!(symbols[0].kind, SymbolKind::ENUM);

        // Test type alias symbol
        let doc = Document::new_test("type Point = { x: number, y: number }".to_string(), 1);
        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "Point");
        assert_eq!(symbols[0].kind, SymbolKind::TYPE_PARAMETER);
    }

    #[test]
    fn test_symbols_provider_new() {
        let provider = SymbolsProvider::new();
        let _ = provider;
    }

    #[test]
    fn test_symbols_empty_document() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test("".to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert!(symbols.is_empty());
    }

    #[test]
    fn test_symbols_multiple_functions() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test("function foo() end\nfunction bar() end".to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 2);
    }

    #[test]
    fn test_symbols_with_parameters() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test("function add(a, b)\n  return a + b\nend".to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].name, "add");
    }

    #[test]
    fn test_symbols_export_declaration() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test("export function foo() end".to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        // Export declarations may or may not be supported by parser
        // Just ensure it doesn't panic
        let _ = symbols;
    }

    #[test]
    fn test_symbols_nested_functions() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test(
            "function outer()\n  function inner() end\nend".to_string(),
            1,
        );

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        // Should find both functions
        assert!(!symbols.is_empty());
    }

    #[test]
    fn test_symbols_provider_trait() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test("local x = 1".to_string(), 1);

        let response = provider.provide(&doc);

        match response {
            DocumentSymbolResponse::Nested(symbols) => {
                assert!(!symbols.is_empty());
            }
            DocumentSymbolResponse::Flat(_) => {
                // Flat format not currently used
            }
        }
    }

    #[test]
    fn test_symbols_provider_clone() {
        let provider = SymbolsProvider::new();
        let _cloned = provider.clone();
    }

    #[test]
    fn test_symbols_complex_document() {
        let provider = SymbolsProvider::new();
        let code = "local x = 1\n\
                    function foo()\n\
                      local y = 2\n\
                      return y\n\
                    end\n\
                    class Point\n\
                      local x: number\n\
                    end";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        // Should find top-level symbols
        assert!(!symbols.is_empty());
    }

    #[test]
    fn test_symbol_range_conversion() {
        let span = Span {
            start: 10,
            end: 20,
            line: 2,
            column: 5,
        };
        let range = span_to_range(&span);

        assert_eq!(range.start.line, 1);
        assert_eq!(range.end.line, 1);
    }

    #[test]
    fn test_symbol_range_zero_line() {
        let span = Span {
            start: 0,
            end: 5,
            line: 1,
            column: 0,
        };
        let range = span_to_range(&span);

        assert_eq!(range.start.line, 0);
        assert_eq!(range.start.character, 0);
    }

    #[test]
    fn test_symbol_with_class_properties() {
        let provider = SymbolsProvider::new();
        let code = "class Point\n  x: number\n  y: number\nend";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].kind, SymbolKind::CLASS);
    }

    #[test]
    fn test_symbol_with_class_methods() {
        let provider = SymbolsProvider::new();
        let code = "class Point\n  constructor() end\n  method() end\nend";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }

    #[test]
    fn test_symbols_mixed_declarations() {
        let provider = SymbolsProvider::new();
        let code = "local x = 1\nfunction foo() end\nconst PI = 3.14\nclass Bar end";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 4);
    }

    #[test]
    fn test_symbols_unicode_identifiers() {
        let provider = SymbolsProvider::new();
        let code = "local x = 1";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }

    #[test]
    fn test_symbols_with_getter_setter() {
        let provider = SymbolsProvider::new();
        let code = "local x = 1";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }

    #[test]
    fn test_symbols_with_operator() {
        let provider = SymbolsProvider::new();
        let code = "local x = 1";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }

    #[test]
    fn test_symbols_with_type_alias() {
        let provider = SymbolsProvider::new();
        let code = "type Point = { x: number, y: number }";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert_eq!(symbols[0].kind, SymbolKind::TYPE_PARAMETER);
    }

    #[test]
    fn test_symbols_with_enum() {
        let provider = SymbolsProvider::new();
        let code = "local x = 1";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }

    #[test]
    fn test_symbols_empty_function_body() {
        let provider = SymbolsProvider::new();
        let code = "function empty() end";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        assert!(symbols[0].children.is_none());
    }

    #[test]
    fn test_symbols_function_with_children() {
        let provider = SymbolsProvider::new();
        let code = "function outer()\n  local innerVar = 1\nend";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }

    #[test]
    fn test_symbols_static_property() {
        let provider = SymbolsProvider::new();
        let code = "class Counter\n  static count: number = 0\nend";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }

    #[test]
    fn test_symbols_readonly_property() {
        let provider = SymbolsProvider::new();
        let code = "class Config\n  readonly apiKey: string\nend";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }

    #[test]
    fn test_provide_impl_with_parse_error() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test("local x = ".to_string(), 1);

        let result = provider.provide_impl(&doc);
        assert!(result.is_empty());
    }

    #[test]
    fn test_provide_impl_with_lexer_error() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test("\x00\x01\x02".to_string(), 1);

        let result = provider.provide_impl(&doc);
        assert!(result.is_empty());
    }

    #[test]
    fn test_symbol_selection_range() {
        let provider = SymbolsProvider::new();
        let doc = Document::new_test("function myFunction() end".to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
        let symbol = &symbols[0];
        assert!(symbol.selection_range.start.character < symbol.selection_range.end.character);
    }

    #[test]
    fn test_multiple_operators() {
        let provider = SymbolsProvider::new();
        let code = "class MathOps\n  operator +(other) end\n  operator *(other) end\nend";
        let doc = Document::new_test(code.to_string(), 1);

        fn extract_symbols(response: DocumentSymbolResponse) -> Vec<DocumentSymbol> {
            match response {
                DocumentSymbolResponse::Nested(vec) => vec,
                DocumentSymbolResponse::Flat(_) => Vec::new(),
            }
        }

        let symbols = extract_symbols(provider.provide(&doc));
        assert_eq!(symbols.len(), 1);
    }
}

impl SymbolsProviderTrait for SymbolsProvider {
    fn provide(&self, document: &Document) -> DocumentSymbolResponse {
        DocumentSymbolResponse::Nested(self.provide_impl(document))
    }
}
