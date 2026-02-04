use crate::document::Document;
use lsp_types::*;
use std::sync::Arc;
use typedlua_parser::string_interner::StringInterner;
use typedlua_parser::{Lexer, Parser};
use typedlua_typechecker::diagnostics::CollectingDiagnosticHandler;
use typedlua_typechecker::{SymbolKind, TypeChecker};

/// Provides hover information (type info, documentation, signatures)
#[derive(Clone)]
pub struct HoverProvider;

impl HoverProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide hover information at a given position
    pub fn provide(&self, document: &Document, position: Position) -> Option<Hover> {
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
    fn format_type(typ: &typedlua_parser::ast::types::Type, interner: &StringInterner) -> String {
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
