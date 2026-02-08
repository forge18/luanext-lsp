use crate::document::Document;
use lsp_types::*;
use std::sync::Arc;
use typedlua_parser::string_interner::StringInterner;
use typedlua_parser::{Lexer, Parser};
use typedlua_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use typedlua_typechecker::TypeChecker;

/// Provides signature help (parameter info while typing function calls)
pub struct SignatureHelpProvider;

impl SignatureHelpProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide signature help at the given position
    pub fn provide(&self, document: &Document, position: Position) -> Option<SignatureHelp> {
        // Get the text before the cursor to analyze context
        let (function_name, active_parameter) = self.analyze_call_context(document, position)?;

        // Parse and type check the document to get function signature
        let handler = Arc::new(CollectingDiagnosticHandler::new());
        let (interner, common_ids) = StringInterner::new_with_common_identifiers();
        let mut lexer = Lexer::new(&document.text, handler.clone(), &interner);
        let tokens = lexer.tokenize().ok()?;

        let mut parser = Parser::new(tokens, handler.clone(), &interner, &common_ids, Box::leak(Box::new(bumpalo::Bump::new())));
        let mut ast = parser.parse().ok()?;

        let mut type_checker = TypeChecker::new(handler, &interner, &common_ids, Box::leak(Box::new(bumpalo::Bump::new())));
        type_checker.check_program(&mut ast).ok()?;

        // Look up the function symbol
        let symbol = type_checker.lookup_symbol(&function_name)?;

        // Format the signature
        let signature = self.format_signature(&function_name, symbol, &interner)?;

        Some(SignatureHelp {
            signatures: vec![signature],
            active_signature: Some(0),
            active_parameter: Some(active_parameter),
        })
    }

    /// Analyze the call context to determine function name and active parameter
    fn analyze_call_context(
        &self,
        document: &Document,
        position: Position,
    ) -> Option<(String, u32)> {
        let lines: Vec<&str> = document.text.lines().collect();
        if position.line as usize >= lines.len() {
            return None;
        }

        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        if char_pos > line.len() {
            return None;
        }

        // Get text before cursor
        let text_before = &line[..char_pos];

        // Find the opening paren going backwards
        let mut paren_count = 0;
        let mut call_start = None;

        for (i, ch) in text_before.char_indices().rev() {
            match ch {
                ')' => paren_count += 1,
                '(' => {
                    if paren_count == 0 {
                        call_start = Some(i);
                        break;
                    }
                    paren_count -= 1;
                }
                _ => {}
            }
        }

        let call_start = call_start?;

        // Extract function name before the '('
        let text_before_paren = &text_before[..call_start].trim_end();
        let function_name = text_before_paren
            .split(|c: char| !c.is_alphanumeric() && c != '_' && c != ':')
            .last()?
            .to_string();

        if function_name.is_empty() {
            return None;
        }

        // Count commas to determine active parameter (excluding nested calls)
        let text_in_call = &text_before[call_start + 1..];
        let active_parameter = self.count_parameters(text_in_call);

        Some((function_name, active_parameter))
    }

    /// Count the number of commas (parameters) considering nesting
    fn count_parameters(&self, text: &str) -> u32 {
        let mut count = 0;
        let mut paren_depth = 0;
        let mut bracket_depth = 0;
        let mut in_string = false;
        let mut string_char = '\0';

        for ch in text.chars() {
            match ch {
                '"' | '\'' if !in_string => {
                    in_string = true;
                    string_char = ch;
                }
                c if in_string && c == string_char => {
                    in_string = false;
                }
                '(' if !in_string => paren_depth += 1,
                ')' if !in_string => paren_depth -= 1,
                '[' if !in_string => bracket_depth += 1,
                ']' if !in_string => bracket_depth -= 1,
                ',' if !in_string && paren_depth == 0 && bracket_depth == 0 => count += 1,
                _ => {}
            }
        }

        count
    }

    /// Format a signature from a symbol
    fn format_signature(
        &self,
        name: &str,
        symbol: &typedlua_typechecker::Symbol,
        interner: &StringInterner,
    ) -> Option<SignatureInformation> {
        use typedlua_parser::ast::pattern::Pattern;
        use typedlua_parser::ast::types::TypeKind;

        // Check if the type is a function
        if let TypeKind::Function(func_type) = &symbol.typ.kind {
            let mut parameters = Vec::new();

            for (i, param) in func_type.parameters.iter().enumerate() {
                let param_name = if let Pattern::Identifier(ident) = &param.pattern {
                    interner.resolve(ident.node).to_string()
                } else {
                    format!("param{}", i)
                };

                let param_type = if let Some(ref type_ann) = param.type_annotation {
                    self.format_type_simple(type_ann, interner)
                } else {
                    "unknown".to_string()
                };

                parameters.push(ParameterInformation {
                    label: ParameterLabel::Simple(format!("{}: {}", param_name, param_type)),
                    documentation: None,
                });
            }

            let return_type = self.format_type_simple(&func_type.return_type, interner);

            let label = format!("function {}(...): {}", name, return_type);

            Some(SignatureInformation {
                label,
                documentation: None,
                parameters: Some(parameters),
                active_parameter: None,
            })
        } else {
            None
        }
    }

    /// Simple type formatting
    fn format_type_simple(
        &self,
        typ: &typedlua_parser::ast::types::Type,
        interner: &StringInterner,
    ) -> String {
        use typedlua_parser::ast::types::{PrimitiveType, TypeKind};

        match &typ.kind {
            TypeKind::Primitive(PrimitiveType::Nil) => "nil".to_string(),
            TypeKind::Primitive(PrimitiveType::Boolean) => "boolean".to_string(),
            TypeKind::Primitive(PrimitiveType::Number) => "number".to_string(),
            TypeKind::Primitive(PrimitiveType::Integer) => "integer".to_string(),
            TypeKind::Primitive(PrimitiveType::String) => "string".to_string(),
            TypeKind::Primitive(PrimitiveType::Unknown) => "unknown".to_string(),
            TypeKind::Primitive(PrimitiveType::Never) => "never".to_string(),
            TypeKind::Primitive(PrimitiveType::Void) => "void".to_string(),
            TypeKind::Reference(type_ref) => interner.resolve(type_ref.name.node).to_string(),
            TypeKind::Function(_) => "function".to_string(),
            _ => "unknown".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signature_help_scenarios() {
        let provider = SignatureHelpProvider::new();

        // Test simple function call
        let doc = Document::new_test("foo(".to_string(), 1);
        let result = provider.analyze_call_context(
            &doc,
            Position {
                line: 0,
                character: 4,
            },
        );
        assert!(result.is_some());
        let (func_name, param) = result.unwrap();
        assert_eq!(func_name, "foo");
        assert_eq!(param, 0);

        // Test multiple parameters with cursor after first comma
        let doc = Document::new_test("foo(a, ".to_string(), 1);
        let result = provider.analyze_call_context(
            &doc,
            Position {
                line: 0,
                character: 7,
            },
        );
        assert!(result.is_some());
        let (func_name, param) = result.unwrap();
        assert_eq!(func_name, "foo");
        assert_eq!(param, 1);

        // Test method calls
        let doc = Document::new_test("obj:method(".to_string(), 1);
        let result = provider.analyze_call_context(
            &doc,
            Position {
                line: 0,
                character: 11,
            },
        );
        assert!(result.is_some());
        let (func_name, param) = result.unwrap();
        // May extract full "obj:method" or just "method" depending on implementation
        assert!(func_name.contains("method"));
        assert_eq!(param, 0);
    }

    #[test]
    fn test_parameter_detection() {
        let provider = SignatureHelpProvider::new();

        // Test "foo(|)" -> parameter 0
        assert_eq!(provider.count_parameters(""), 0);

        // Test "foo(a, |)" -> parameter 1
        assert_eq!(provider.count_parameters("a, "), 1);

        // Test "foo(a, b, |)" -> parameter 2
        assert_eq!(provider.count_parameters("a, b, "), 2);

        // Test "foo(bar(x), |)" -> parameter 1 (not confused by nested call)
        assert_eq!(provider.count_parameters("bar(x), "), 1);

        // Test "foo('a, b', |)" -> parameter 1 (not confused by comma in string)
        assert_eq!(provider.count_parameters("'a, b', "), 1);

        // Test "foo(\"a, b\", |)" -> parameter 1 (double quotes)
        assert_eq!(provider.count_parameters("\"a, b\", "), 1);
    }
}
