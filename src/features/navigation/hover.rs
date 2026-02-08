use crate::core::document::Document;
use crate::traits::HoverProviderTrait;
use lsp_types::*;
use std::sync::Arc;
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser};
use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use luanext_typechecker::{SymbolKind, TypeChecker};

/// Provides hover information (type info, documentation, signatures)
#[derive(Clone)]
pub struct HoverProvider;

impl HoverProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide hover information at a given position (internal method)
    pub fn provide_impl(&self, document: &Document, position: Position) -> Option<Hover> {
        // Get the word at the current position
        let word = self.get_word_at_position(document, position)?;

        // Check if it's a built-in keyword or type
        if let Some(hover) = self.hover_for_keyword(&word) {
            return Some(hover);
        }

        if let Some(hover) = self.hover_for_builtin_type(&word) {
            return Some(hover);
        }

        // Try to get type information from type checker
        if let Some(hover) = self.hover_for_symbol(document, &word) {
            return Some(hover);
        }

        None
    }

    /// Get hover information for a symbol using the type checker
    fn hover_for_symbol(&self, document: &Document, word: &str) -> Option<Hover> {
        // Parse and type check the document
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids);
        let mut ast = parser.parse().ok()?;

        let mut type_checker = TypeChecker::new(handler, &interner, &common_ids);
        type_checker.check_program(&mut ast).ok()?;

        // Look up the symbol
        let symbol = type_checker.lookup_symbol(word)?.clone();

        // Format the type information
        let type_str = Self::format_type(&symbol.typ, &interner);
        let kind_str = match symbol.kind {
            SymbolKind::Const => "const",
            SymbolKind::Variable => "let",
            SymbolKind::Function => "function",
            SymbolKind::Class => "class",
            SymbolKind::Interface => "interface",
            SymbolKind::TypeAlias => "type",
            SymbolKind::Enum => "enum",
            SymbolKind::Parameter => "parameter",
            SymbolKind::Namespace => "namespace",
        };

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```typedlua\n{} {}: {}\n```", kind_str, word, type_str),
            }),
            range: None,
        })
    }

    /// Format a type for display
    fn format_type(typ: &luanext_parser::ast::types::Type, interner: &StringInterner) -> String {
        use luanext_parser::ast::types::{PrimitiveType, TypeKind};

        match &typ.kind {
            TypeKind::Primitive(PrimitiveType::Nil) => "nil".to_string(),
            TypeKind::Primitive(PrimitiveType::Boolean) => "boolean".to_string(),
            TypeKind::Primitive(PrimitiveType::Number) => "number".to_string(),
            TypeKind::Primitive(PrimitiveType::Integer) => "integer".to_string(),
            TypeKind::Primitive(PrimitiveType::String) => "string".to_string(),
            TypeKind::Primitive(PrimitiveType::Unknown) => "unknown".to_string(),
            TypeKind::Primitive(PrimitiveType::Never) => "never".to_string(),
            TypeKind::Primitive(PrimitiveType::Void) => "void".to_string(),
            TypeKind::Primitive(PrimitiveType::Table) => "table".to_string(),
            TypeKind::Primitive(PrimitiveType::Coroutine) => "coroutine".to_string(),
            TypeKind::Primitive(PrimitiveType::Thread) => "thread".to_string(),
            TypeKind::Literal(_) => "literal".to_string(),
            TypeKind::Union(_) => "union type".to_string(),
            TypeKind::Intersection(_) => "intersection type".to_string(),
            TypeKind::Function(_) => "function".to_string(),
            TypeKind::Object(_) => "object".to_string(),
            TypeKind::Array(_) => "array".to_string(),
            TypeKind::Tuple(_) => "tuple".to_string(),
            TypeKind::TypeQuery(_) => "typeof".to_string(),
            TypeKind::Reference(type_ref) => interner.resolve(type_ref.name.node).to_string(),
            TypeKind::Nullable(_) => "nullable".to_string(),
            TypeKind::IndexAccess(_, _) => "indexed access".to_string(),
            TypeKind::Conditional(_) => "conditional type".to_string(),
            TypeKind::Infer(_) => "infer".to_string(),
            TypeKind::KeyOf(_) => "keyof".to_string(),
            TypeKind::Mapped(_) => "mapped type".to_string(),
            TypeKind::TemplateLiteral(_) => "template literal type".to_string(),
            TypeKind::Parenthesized(inner) => Self::format_type(inner, interner),
            TypeKind::TypePredicate(_) => "type predicate".to_string(),
            TypeKind::Variadic(_) => "variadic".to_string(),
            TypeKind::Namespace(_) => "namespace".to_string(),
        }
    }

    /// Get the word at the cursor position
    fn get_word_at_position(&self, document: &Document, position: Position) -> Option<String> {
        let lines: Vec<&str> = document.text.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }

        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        if char_pos > line.len() {
            return None;
        }

        // Find word boundaries
        let chars: Vec<char> = line.chars().collect();
        if char_pos >= chars.len() {
            return None;
        }

        // Check if we're on a word character
        if !chars[char_pos].is_alphanumeric() && chars[char_pos] != '_' {
            return None;
        }

        // Find start of word
        let mut start = char_pos;
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }

        // Find end of word
        let mut end = char_pos;
        while end < chars.len() && (chars[end].is_alphanumeric() || chars[end] == '_') {
            end += 1;
        }

        Some(chars[start..end].iter().collect())
    }

    /// Provide hover information for keywords
    fn hover_for_keyword(&self, word: &str) -> Option<Hover> {
        let (description, detail) = match word {
            "const" => (
                "Constant declaration",
                "Declares a constant value that cannot be reassigned.\n\n```typedlua\nconst PI: number = 3.14159\n```"
            ),
            "let" => (
                "Variable declaration",
                "Declares a mutable variable with block scope.\n\n```typedlua\nlet x: number = 10\nx = 20 -- reassignment allowed\n```"
            ),
            "local" => (
                "Local variable declaration",
                "Declares a local variable with block scope.\n\n```typedlua\nlocal x: number = 10\n```"
            ),
            "function" => (
                "Function declaration",
                "Declares a function.\n\n```typedlua\nfunction add(a: number, b: number): number\n    return a + b\nend\n```"
            ),
            "if" => ("Conditional statement", "Executes code based on a condition.\n\n```typedlua\nif condition then\n    -- code\nend\n```"),
            "then" => ("Then clause", "Marks the beginning of an if block."),
            "else" => ("Else clause", "Alternative branch in conditional statements."),
            "elseif" => ("Else-if clause", "Additional conditional branch."),
            "end" => ("End block", "Marks the end of a block (function, if, loop, etc)."),
            "while" => ("While loop", "Repeats code while a condition is true.\n\n```typedlua\nwhile condition do\n    -- code\nend\n```"),
            "for" => ("For loop", "Iteration statement.\n\n```typedlua\nfor i = 1, 10 do\n    -- code\nend\n```"),
            "in" => ("In operator", "Used in for-in loops to iterate over collections."),
            "do" => ("Do block", "Marks the beginning of a loop body."),
            "repeat" => ("Repeat-until loop", "Executes code at least once, then repeats until condition is true.\n\n```typedlua\nrepeat\n    -- code\nuntil condition\n```"),
            "until" => ("Until condition", "Termination condition for repeat loops."),
            "return" => ("Return statement", "Returns a value from a function."),
            "break" => ("Break statement", "Exits the current loop."),
            "continue" => ("Continue statement", "Skips to the next iteration of a loop."),
            "and" => ("Logical AND operator", "Returns true if both operands are true."),
            "or" => ("Logical OR operator", "Returns true if at least one operand is true."),
            "not" => ("Logical NOT operator", "Negates a boolean value."),
            "true" => ("Boolean true value", "Represents the boolean value true."),
            "false" => ("Boolean false value", "Represents the boolean value false."),
            "nil" => ("Nil value", "Represents the absence of a value."),
            "type" => ("Type alias declaration", "Declares a type alias.\n\n```typedlua\ntype Point = { x: number, y: number }\n```"),
            "interface" => ("Interface declaration", "Declares an interface for object shapes.\n\n```typedlua\ninterface Drawable {\n    draw(): void\n}\n```"),
            "enum" => ("Enum declaration", "Declares an enumeration.\n\n```typedlua\nenum Color {\n    Red,\n    Green,\n    Blue\n}\n```"),
            "class" => ("Class declaration", "Declares a class.\n\n```typedlua\nclass Point {\n    x: number\n    y: number\n}\n```"),
            "extends" => ("Extends clause", "Specifies class inheritance."),
            "implements" => ("Implements clause", "Specifies interface implementation."),
            "public" => ("Public access modifier", "Members are accessible from anywhere."),
            "private" => ("Private access modifier", "Members are only accessible within the class."),
            "protected" => ("Protected access modifier", "Members are accessible within the class and subclasses."),
            "static" => ("Static modifier", "Members belong to the class rather than instances."),
            "abstract" => ("Abstract modifier", "Declares abstract classes or methods."),
            "readonly" => ("Readonly modifier", "Prevents reassignment after initialization."),
            "match" => ("Match expression", "Pattern matching expression.\n\n```typedlua\nmatch value {\n    pattern1 => result1,\n    pattern2 => result2\n}\n```"),
            "when" => ("When guard", "Adds conditions to match patterns."),
            "where" => ("Where clause", "Type constraint clause for generics or type expressions."),
            "import" => ("Import statement", "Imports modules or specific exports.\n\n```typedlua\nimport { func } from \"module\"\n```"),
            "from" => ("From clause", "Specifies the module to import from."),
            "export" => ("Export statement", "Exports declarations from a module."),
            _ => return None,
        };

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("**{}**\n\n{}", description, detail),
            }),
            range: None,
        })
    }

    /// Provide hover information for built-in types
    fn hover_for_builtin_type(&self, word: &str) -> Option<Hover> {
        let detail = match word {
            "nil" => "The type of the nil value, representing absence of a value.",
            "boolean" => "Represents true or false values.",
            "number" => "Represents numeric values (integers and floats).",
            "string" => "Represents text/string values.",
            "unknown" => "Top type - all types are assignable to unknown.",
            "never" => "Bottom type - represents values that never occur.",
            "void" => "Represents the absence of a return value.",
            "any" => "Escape hatch - disables type checking for this value.",
            _ => return None,
        };

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!("```typedlua\n{}\n```\n\n{}", word, detail),
            }),
            range: None,
        })
    }
}

impl HoverProviderTrait for HoverProvider {
    fn provide(&self, document: &Document, position: Position) -> Option<Hover> {
        self.provide_impl(document, position)
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
    fn test_hover_for_keyword_function() {
        let _doc = create_test_document("function foo() end");
        let provider = HoverProvider::new();

        let result = provider.hover_for_keyword("function");

        assert!(result.is_some());
        let hover = result.unwrap();
        match hover.contents {
            HoverContents::Markup(markup) => {
                assert!(markup.value.contains("Function declaration"));
            }
            _ => panic!("Expected MarkupContent"),
        }
    }

    #[test]
    fn test_hover_for_keyword_class() {
        let _doc = create_test_document("class Foo end");
        let provider = HoverProvider::new();

        let result = provider.hover_for_keyword("class");

        assert!(result.is_some());
    }

    #[test]
    fn test_hover_for_keyword_not_found() {
        let _doc = create_test_document("local x = 1");
        let provider = HoverProvider::new();

        let result = provider.hover_for_keyword("notakeyword");

        assert!(result.is_none());
    }

    #[test]
    fn test_hover_for_builtin_type_number() {
        let _doc = create_test_document("local x: number = 1");
        let provider = HoverProvider::new();

        let result = provider.hover_for_builtin_type("number");

        assert!(result.is_some());
    }

    #[test]
    fn test_hover_for_builtin_type_nil() {
        let _doc = create_test_document("local x = nil");
        let provider = HoverProvider::new();

        let result = provider.hover_for_builtin_type("nil");

        assert!(result.is_some());
    }

    #[test]
    fn test_hover_for_builtin_type_unknown() {
        let _doc = create_test_document("let x: unknown = something");
        let provider = HoverProvider::new();

        let result = provider.hover_for_builtin_type("unknown");

        assert!(result.is_some());
    }

    #[test]
    fn test_hover_for_builtin_type_not_found() {
        let _doc = create_test_document("local x = 1");
        let provider = HoverProvider::new();

        let result = provider.hover_for_builtin_type("notatype");

        assert!(result.is_none());
    }

    #[test]
    fn test_empty_document() {
        let doc = create_test_document("");
        let provider = HoverProvider::new();

        let result = provider.provide(&doc, Position::new(0, 0));

        assert!(result.is_none());
    }

    #[test]
    fn test_out_of_bounds_position() {
        let doc = create_test_document("local x = 1");
        let provider = HoverProvider::new();

        let result = provider.provide(&doc, Position::new(10, 10));

        assert!(result.is_none());
    }

    #[test]
    fn test_hover_all_keywords() {
        let provider = HoverProvider::new();

        // Test that all keywords return hover info
        let keywords = vec![
            "local",
            "function",
            "return",
            "if",
            "then",
            "else",
            "elseif",
            "while",
            "do",
            "for",
            "in",
            "repeat",
            "until",
            "break",
            "class",
            "interface",
            "enum",
            "type",
            "import",
            "export",
            "const",
            "let",
            "match",
            "when",
            "where",
        ];

        for keyword in keywords {
            let result = provider.hover_for_keyword(keyword);
            assert!(
                result.is_some(),
                "Keyword '{}' should have hover info",
                keyword
            );
        }
    }

    #[test]
    fn test_hover_all_builtin_types() {
        let provider = HoverProvider::new();

        let types = vec![
            "nil", "boolean", "number", "string", "unknown", "never", "void", "any",
        ];

        for typ in types {
            let result = provider.hover_for_builtin_type(typ);
            assert!(result.is_some(), "Type '{}' should have hover info", typ);
        }
    }

    #[test]
    fn test_hover_on_variable() {
        let doc = create_test_document("local myVariable = 10");
        let provider = HoverProvider::new();

        // Hover on the variable name
        let result = provider.provide(&doc, Position::new(0, 8));

        // Should provide some hover info (may be None if not implemented)
        let _ = result;
    }

    #[test]
    fn test_hover_on_function_call() {
        let doc = create_test_document("print('hello')");
        let provider = HoverProvider::new();

        // Hover on function name
        let result = provider.provide(&doc, Position::new(0, 2));
        let _ = result;
    }

    #[test]
    fn test_hover_whitespace() {
        let doc = create_test_document("local x = 1");
        let provider = HoverProvider::new();

        // Hover on whitespace
        let result = provider.provide(&doc, Position::new(0, 5));
        assert!(result.is_none());
    }

    #[test]
    fn test_hover_multiline_document() {
        let doc = create_test_document("local x = 1\nlocal y = 2\nprint(x + y)");
        let provider = HoverProvider::new();

        // Hover on different lines
        let result1 = provider.provide(&doc, Position::new(0, 6));
        let result2 = provider.provide(&doc, Position::new(1, 6));
        let result3 = provider.provide(&doc, Position::new(2, 2));

        let _ = (result1, result2, result3);
    }

    #[test]
    fn test_hover_provider_trait() {
        let provider = HoverProvider::new();
        let doc = create_test_document("test");

        // Test through trait interface
        let result = provider.provide(&doc, Position::new(0, 0));
        let _ = result;
    }

    #[test]
    fn test_hover_keyword_content_structure() {
        let provider = HoverProvider::new();

        let result = provider.hover_for_keyword("function").unwrap();

        match result.contents {
            HoverContents::Markup(markup) => {
                assert_eq!(markup.kind, MarkupKind::Markdown);
                // Should have title and description
                assert!(!markup.value.is_empty());
                assert!(markup.value.contains("**")); // Bold title
            }
            _ => panic!("Expected MarkupContent"),
        }
    }

    #[test]
    fn test_hover_type_content_structure() {
        let provider = HoverProvider::new();

        let result = provider.hover_for_builtin_type("number").unwrap();

        match result.contents {
            HoverContents::Markup(markup) => {
                assert_eq!(markup.kind, MarkupKind::Markdown);
                // Should have code block
                assert!(markup.value.contains("```"));
            }
            _ => panic!("Expected MarkupContent"),
        }
    }

    #[test]
    fn test_hover_for_modifiers() {
        let provider = HoverProvider::new();

        let modifiers = vec!["public", "private", "protected", "static", "readonly"];

        for modifier in modifiers {
            let result = provider.hover_for_keyword(modifier);
            assert!(
                result.is_some(),
                "Modifier '{}' should have hover info",
                modifier
            );
        }
    }

    #[test]
    fn test_hover_provider_clone() {
        let provider = HoverProvider::new();
        let _cloned = provider.clone();
    }

    #[test]
    fn test_hover_format_type_primitives() {
        use luanext_parser::ast::types::{PrimitiveType, Type};
        use luanext_parser::string_interner::StringInterner;

        let interner = StringInterner::new();

        // Test primitive type formatting
        let nil_type = Type::new(
            luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Nil),
            luanext_parser::Span::new(0, 3, 1, 1),
        );
        let result = HoverProvider::format_type(&nil_type, &interner);
        assert_eq!(result, "nil");

        let bool_type = Type::new(
            luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Boolean),
            luanext_parser::Span::new(0, 7, 1, 1),
        );
        let result = HoverProvider::format_type(&bool_type, &interner);
        assert_eq!(result, "boolean");

        let num_type = Type::new(
            luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::Number),
            luanext_parser::Span::new(0, 6, 1, 1),
        );
        let result = HoverProvider::format_type(&num_type, &interner);
        assert_eq!(result, "number");

        let str_type = Type::new(
            luanext_parser::ast::types::TypeKind::Primitive(PrimitiveType::String),
            luanext_parser::Span::new(0, 6, 1, 1),
        );
        let result = HoverProvider::format_type(&str_type, &interner);
        assert_eq!(result, "string");
    }

    #[test]
    fn test_hover_format_type_function() {
        use luanext_parser::ast::types::{PrimitiveType, Type, TypeKind};
        use luanext_parser::string_interner::StringInterner;

        let interner = StringInterner::new();

        let func_type = Type::new(
            TypeKind::Function(luanext_parser::ast::types::FunctionType {
                type_parameters: None,
                parameters: vec![],
                return_type: Box::new(Type::new(
                    TypeKind::Primitive(PrimitiveType::Number),
                    luanext_parser::Span::new(0, 6, 1, 1),
                )),
                throws: None,
                span: luanext_parser::Span::new(0, 20, 1, 1),
            }),
            luanext_parser::Span::new(0, 20, 1, 1),
        );
        let result = HoverProvider::format_type(&func_type, &interner);
        assert_eq!(result, "function");
    }

    #[test]
    fn test_hover_format_type_array() {
        use luanext_parser::ast::types::{PrimitiveType, Type, TypeKind};
        use luanext_parser::string_interner::StringInterner;

        let interner = StringInterner::new();

        let array_type = Type::new(
            TypeKind::Array(Box::new(Type::new(
                TypeKind::Primitive(PrimitiveType::Number),
                luanext_parser::Span::new(0, 6, 1, 1),
            ))),
            luanext_parser::Span::new(0, 11, 1, 1),
        );
        let result = HoverProvider::format_type(&array_type, &interner);
        assert_eq!(result, "array");
    }

    #[test]
    fn test_hover_format_type_object() {
        use luanext_parser::ast::types::{PrimitiveType, Type, TypeKind};
        use luanext_parser::string_interner::StringInterner;

        let interner = StringInterner::new();

        let object_type = Type::new(
            TypeKind::Object(luanext_parser::ast::types::ObjectType {
                members: vec![],
                span: luanext_parser::Span::new(0, 6, 1, 1),
            }),
            luanext_parser::Span::new(0, 6, 1, 1),
        );
        let result = HoverProvider::format_type(&object_type, &interner);
        assert_eq!(result, "object");
    }

    #[test]
    fn test_hover_format_type_tuple() {
        use luanext_parser::ast::types::{Type, TypeKind};
        use luanext_parser::string_interner::StringInterner;

        let interner = StringInterner::new();

        let tuple_type = Type::new(
            TypeKind::Tuple(vec![]),
            luanext_parser::Span::new(0, 5, 1, 1),
        );
        let result = HoverProvider::format_type(&tuple_type, &interner);
        assert_eq!(result, "tuple");
    }

    #[test]
    fn test_hover_format_type_union() {
        use luanext_parser::ast::types::{Type, TypeKind};
        use luanext_parser::string_interner::StringInterner;

        let interner = StringInterner::new();

        let union_type = Type::new(
            TypeKind::Union(vec![]),
            luanext_parser::Span::new(0, 10, 1, 1),
        );
        let result = HoverProvider::format_type(&union_type, &interner);
        assert_eq!(result, "union type");
    }

    #[test]
    fn test_hover_format_type_intersection() {
        use luanext_parser::ast::types::{Type, TypeKind};
        use luanext_parser::string_interner::StringInterner;

        let interner = StringInterner::new();

        let intersection_type = Type::new(
            TypeKind::Intersection(vec![]),
            luanext_parser::Span::new(0, 12, 1, 1),
        );
        let result = HoverProvider::format_type(&intersection_type, &interner);
        assert_eq!(result, "intersection type");
    }

    #[test]
    fn test_hover_format_type_literal() {
        use luanext_parser::ast::types::{Type, TypeKind};
        use luanext_parser::string_interner::StringInterner;

        let interner = StringInterner::new();

        let literal_type = Type::new(
            TypeKind::Primitive(luanext_parser::ast::types::PrimitiveType::String),
            luanext_parser::Span::new(0, 5, 1, 1),
        );
        let result = HoverProvider::format_type(&literal_type, &interner);
        assert_eq!(result, "string");
    }

    #[test]
    fn test_hover_on_type_annotation() {
        let doc = create_test_document("local x: number = 1");
        let provider = HoverProvider::new();

        // Hover on the type annotation
        let result = provider.provide(&doc, Position::new(0, 9));

        let _ = result;
    }

    #[test]
    fn test_hover_on_class_keyword() {
        let _doc = create_test_document("class Foo end");
        let provider = HoverProvider::new();

        let result = provider.hover_for_keyword("class");

        assert!(result.is_some());
        if let Some(hover) = result {
            match hover.contents {
                HoverContents::Markup(markup) => {
                    assert!(markup.value.contains("Class"));
                }
                _ => panic!("Expected MarkupContent"),
            }
        }
    }

    #[test]
    fn test_hover_on_interface_keyword() {
        let _doc = create_test_document("interface Foo end");
        let provider = HoverProvider::new();

        let result = provider.hover_for_keyword("interface");

        assert!(result.is_some());
    }

    #[test]
    fn test_hover_on_enum_keyword() {
        let _doc = create_test_document("enum Color { Red } end");
        let provider = HoverProvider::new();

        let result = provider.hover_for_keyword("enum");

        assert!(result.is_some());
    }

    #[test]
    fn test_hover_on_type_keyword() {
        let _doc = create_test_document("type MyAlias = string");
        let provider = HoverProvider::new();

        let result = provider.hover_for_keyword("type");

        assert!(result.is_some());
    }

    #[test]
    fn test_hover_on_import_export() {
        let provider = HoverProvider::new();

        let import_result = provider.hover_for_keyword("import");
        let export_result = provider.hover_for_keyword("export");

        assert!(import_result.is_some());
        assert!(export_result.is_some());
    }

    #[test]
    fn test_hover_on_loop_keywords() {
        let provider = HoverProvider::new();

        let while_result = provider.hover_for_keyword("while");
        let for_result = provider.hover_for_keyword("for");
        let repeat_result = provider.hover_for_keyword("repeat");

        assert!(while_result.is_some());
        assert!(for_result.is_some());
        assert!(repeat_result.is_some());
    }

    #[test]
    fn test_hover_on_conditional_keywords() {
        let provider = HoverProvider::new();

        let if_result = provider.hover_for_keyword("if");
        let then_result = provider.hover_for_keyword("then");
        let elseif_result = provider.hover_for_keyword("elseif");
        let else_result = provider.hover_for_keyword("else");

        assert!(if_result.is_some());
        assert!(then_result.is_some());
        assert!(elseif_result.is_some());
        assert!(else_result.is_some());
    }

    #[test]
    fn test_hover_on_control_keywords() {
        let provider = HoverProvider::new();

        let return_result = provider.hover_for_keyword("return");
        let break_result = provider.hover_for_keyword("break");
        let continue_result = provider.hover_for_keyword("continue");

        assert!(return_result.is_some());
        assert!(break_result.is_some());
        assert!(continue_result.is_some());
    }

    #[test]
    fn test_hover_on_logical_operators() {
        let provider = HoverProvider::new();

        let and_result = provider.hover_for_keyword("and");
        let or_result = provider.hover_for_keyword("or");
        let not_result = provider.hover_for_keyword("not");

        assert!(and_result.is_some());
        assert!(or_result.is_some());
        assert!(not_result.is_some());
    }

    #[test]
    fn test_hover_on_boolean_values() {
        let provider = HoverProvider::new();

        let true_result = provider.hover_for_keyword("true");
        let false_result = provider.hover_for_keyword("false");

        assert!(true_result.is_some());
        assert!(false_result.is_some());
    }

    #[test]
    fn test_hover_on_builtin_types_extended() {
        let provider = HoverProvider::new();

        let void_result = provider.hover_for_builtin_type("void");
        let never_result = provider.hover_for_builtin_type("never");

        assert!(void_result.is_some());
        assert!(never_result.is_some());
    }

    #[test]
    fn test_hover_provide_impl_empty() {
        let doc = create_test_document("");
        let provider = HoverProvider::new();

        let result = provider.provide_impl(&doc, Position::new(0, 0));

        assert!(result.is_none());
    }

    #[test]
    fn test_hover_provide_impl_whitespace() {
        let doc = create_test_document("   ");
        let provider = HoverProvider::new();

        let result = provider.provide_impl(&doc, Position::new(0, 1));

        assert!(result.is_none());
    }
}
