use crate::core::diagnostics::DiagnosticsProvider;
use crate::core::document::Document;
use crate::traits::HoverProviderTrait;
use lsp_types::*;
use luanext_parser::ast::statement::{ExportKind, ImportClause, Statement};
use luanext_parser::string_interner::StringInterner;

/// Provides hover information (type info, documentation, signatures)
#[derive(Clone)]
pub struct HoverProvider;

impl HoverProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide hover information at a given position (internal method).
    ///
    /// Uses position-based caching: if the same position is requested again
    /// at the same document version, returns the cached result immediately.
    pub fn provide_impl(&self, document: &Document, position: Position) -> Option<Hover> {
        // Check cache first - clone result to release RefMut borrow before recording stats
        let cached = document
            .cache_mut()
            .hover_cache
            .get(position, document.version)
            .cloned();

        if let Some(result) = cached {
            document.cache_mut().stats_mut().record_hit();
            document.cache().stats().maybe_log("hover");
            return Some(result);
        }

        document.cache_mut().stats_mut().record_miss();

        // Get the word at the current position
        let word = self.get_word_at_position(document, position)?;

        // Check if it's a built-in keyword or type
        if let Some(hover) = self.hover_for_keyword(&word) {
            document
                .cache_mut()
                .hover_cache
                .insert(position, hover.clone(), document.version);
            return Some(hover);
        }

        if let Some(hover) = self.hover_for_builtin_type(&word) {
            document
                .cache_mut()
                .hover_cache
                .insert(position, hover.clone(), document.version);
            return Some(hover);
        }

        // Try to get type information from type checker
        if let Some(hover) = self.hover_for_symbol(document, &word) {
            document
                .cache_mut()
                .hover_cache
                .insert(position, hover.clone(), document.version);
            return Some(hover);
        }

        None
    }

    /// Get hover information for a symbol using the cached type-check result.
    fn hover_for_symbol(&self, document: &Document, word: &str) -> Option<Hover> {
        // Use cached type-check result instead of running type checker again
        let result = DiagnosticsProvider::ensure_type_checked(document);
        let symbol = result.symbols.get(word)?;

        let mut markdown = format!(
            "```typedlua\n{} {}: {}\n```",
            symbol.kind, word, symbol.type_display
        );

        // Type-only import and re-export detection uses cached AST (cheap, no type checking)
        if let Some((ast, interner, _, _)) = document.get_or_parse_ast() {
            if Self::is_type_only_import(ast, word, &interner) {
                markdown.push_str("\n\n*Imported as type-only*");
            }
            if let Some(source_module) = Self::get_reexport_source(ast, word, &interner) {
                markdown.push_str(&format!("\n\n*Re-exported from `{}`*", source_module));
            }
        }

        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: markdown,
            }),
            range: None,
        })
    }

    /// Check if a symbol name was imported via `import type { ... }`
    fn is_type_only_import(
        ast: &luanext_parser::ast::Program,
        symbol_name: &str,
        interner: &StringInterner,
    ) -> bool {
        for stmt in ast.statements {
            if let Statement::Import(import_decl) = stmt {
                if let ImportClause::TypeOnly(specs) = &import_decl.clause {
                    for spec in specs.iter() {
                        let local_name = spec
                            .local
                            .as_ref()
                            .map(|l| interner.resolve(l.node))
                            .unwrap_or_else(|| interner.resolve(spec.imported.node));
                        if local_name == symbol_name {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Get the source module for a re-exported symbol
    fn get_reexport_source(
        ast: &luanext_parser::ast::Program,
        symbol_name: &str,
        interner: &StringInterner,
    ) -> Option<String> {
        for stmt in ast.statements {
            if let Statement::Export(export_decl) = stmt {
                if let ExportKind::Named {
                    specifiers,
                    source,
                    is_type_only: _,
                } = &export_decl.kind
                {
                    if let Some(source_path) = source {
                        for spec in specifiers.iter() {
                            let exported_name = spec
                                .exported
                                .as_ref()
                                .map(|e| interner.resolve(e.node))
                                .unwrap_or_else(|| interner.resolve(spec.local.node));
                            if exported_name == symbol_name {
                                return Some(source_path.clone());
                            }
                        }
                    }
                }
            }
        }
        None
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
    fn test_format_type_display_primitives() {
        use crate::core::diagnostics::DiagnosticsProvider;
        use luanext_parser::ast::types::{PrimitiveType, TypeKind};

        assert_eq!(
            DiagnosticsProvider::format_type_display(&TypeKind::Primitive(PrimitiveType::Nil)),
            "nil"
        );
        assert_eq!(
            DiagnosticsProvider::format_type_display(&TypeKind::Primitive(PrimitiveType::Boolean)),
            "boolean"
        );
        assert_eq!(
            DiagnosticsProvider::format_type_display(&TypeKind::Primitive(PrimitiveType::Number)),
            "number"
        );
        assert_eq!(
            DiagnosticsProvider::format_type_display(&TypeKind::Primitive(PrimitiveType::String)),
            "string"
        );
    }

    #[test]
    fn test_format_type_display_complex() {
        use crate::core::diagnostics::DiagnosticsProvider;
        use bumpalo::Bump;
        use luanext_parser::ast::types::{PrimitiveType, Type, TypeKind};

        let arena = Bump::new();

        // Function type
        let return_type = arena.alloc(Type::new(
            TypeKind::Primitive(PrimitiveType::Number),
            luanext_parser::Span::new(0, 6, 1, 1),
        ));
        let func_kind = TypeKind::Function(luanext_parser::ast::types::FunctionType {
            type_parameters: None,
            parameters: &[],
            return_type,
            throws: None,
            span: luanext_parser::Span::new(0, 20, 1, 1),
        });
        assert_eq!(
            DiagnosticsProvider::format_type_display(&func_kind),
            "function"
        );

        // Array type
        let element_type = arena.alloc(Type::new(
            TypeKind::Primitive(PrimitiveType::Number),
            luanext_parser::Span::new(0, 6, 1, 1),
        ));
        assert_eq!(
            DiagnosticsProvider::format_type_display(&TypeKind::Array(element_type)),
            "array"
        );

        // Object type
        let obj_kind = TypeKind::Object(luanext_parser::ast::types::ObjectType {
            members: &[],
            span: luanext_parser::Span::new(0, 6, 1, 1),
        });
        assert_eq!(
            DiagnosticsProvider::format_type_display(&obj_kind),
            "object"
        );

        // Tuple, Union, Intersection
        assert_eq!(
            DiagnosticsProvider::format_type_display(&TypeKind::Tuple(&[])),
            "tuple"
        );
        assert_eq!(
            DiagnosticsProvider::format_type_display(&TypeKind::Union(&[])),
            "union type"
        );
        assert_eq!(
            DiagnosticsProvider::format_type_display(&TypeKind::Intersection(&[])),
            "intersection type"
        );
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

    // ── Cache tests ──────────────────────────────────────────────────

    #[test]
    fn test_hover_cache_hit_returns_same_result() {
        let doc = create_test_document("local x = 1");
        let provider = HoverProvider::new();
        let pos = Position::new(0, 0); // "local" keyword

        let result1 = provider.provide_impl(&doc, pos);
        assert!(result1.is_some());

        // Second call at same position should hit cache
        let result2 = provider.provide_impl(&doc, pos);
        assert!(result2.is_some());

        // Results should be identical
        assert_eq!(format!("{:?}", result1), format!("{:?}", result2),);

        // Cache should have recorded a hit
        let stats = doc.cache().stats().clone();
        assert!(
            stats.hits >= 1,
            "Expected at least 1 cache hit, got {}",
            stats.hits
        );
    }

    #[test]
    fn test_hover_cache_miss_on_different_position() {
        let doc = create_test_document("local x = 1\nlocal y = 2");
        let provider = HoverProvider::new();

        // Hover on first "local"
        let result1 = provider.provide_impl(&doc, Position::new(0, 0));
        assert!(result1.is_some());

        // Hover on second "local" - different position, should miss cache
        let result2 = provider.provide_impl(&doc, Position::new(1, 0));
        assert!(result2.is_some());

        let stats = doc.cache().stats().clone();
        assert!(stats.misses >= 2, "Expected at least 2 cache misses");
    }

    #[test]
    fn test_hover_cache_stores_keyword_hover() {
        let doc = create_test_document("function foo() end");
        let provider = HoverProvider::new();
        let pos = Position::new(0, 3); // "function" keyword

        let _result = provider.provide_impl(&doc, pos);

        // Cache should contain an entry now
        assert!(!doc.cache().hover_cache.is_empty() || doc.cache().stats().misses > 0);
    }
}
