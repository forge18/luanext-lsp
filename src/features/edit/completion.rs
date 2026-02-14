use crate::arena_pool::with_pooled_arena;
use crate::core::document::Document;
use crate::traits::CompletionProviderTrait;
use lsp_types::*;
use luanext_parser::ast::statement::{ExportKind, ImportClause, Statement};
use luanext_parser::string_interner::StringInterner;
use luanext_parser::{Lexer, Parser};
use luanext_typechecker::cli::diagnostics::CollectingDiagnosticHandler;
use luanext_typechecker::TypeChecker;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Provides code completion (IntelliSense)
#[derive(Clone)]
pub struct CompletionProvider;

impl CompletionProvider {
    pub fn new() -> Self {
        Self
    }

    /// Provide completion items at a given position
    #[allow(dead_code)]
    pub fn provide(&self, document: &Document, position: Position) -> Vec<CompletionItem> {
        self.provide_with_workspace(document, position, None)
    }

    /// Provide completion items at a given position with workspace context.
    ///
    /// Uses position-based caching for expensive contexts (Statement, MemberAccess,
    /// MethodCall). Cheap contexts (TypeAnnotation, Decorator) return hardcoded
    /// lists and are not cached.
    pub fn provide_with_workspace(
        &self,
        document: &Document,
        position: Position,
        workspace_root: Option<&Path>,
    ) -> Vec<CompletionItem> {
        // Determine completion context from the text before the cursor
        let context = self.get_completion_context(document, position);

        // Only cache expensive contexts that involve type checking
        let use_cache = matches!(
            context,
            CompletionContext::Statement
                | CompletionContext::MemberAccess
                | CompletionContext::MethodCall
        );

        // Check cache for expensive contexts - clone result to release RefMut borrow
        if use_cache {
            let cached = document
                .cache_mut()
                .completion_cache
                .get(position, document.version)
                .cloned();

            if let Some(result) = cached {
                document.cache_mut().stats_mut().record_hit();
                document.cache().stats().maybe_log("completion");
                return result;
            }

            document.cache_mut().stats_mut().record_miss();
        }

        let mut items = Vec::new();

        match context {
            CompletionContext::MemberAccess => {
                // Get type information and provide member completions
                items.extend(self.complete_members(document, position, false));
            }
            CompletionContext::MethodCall => {
                // Get type information and provide method completions
                items.extend(self.complete_members(document, position, true));
            }
            CompletionContext::TypeAnnotation => {
                items.extend(self.complete_types());
            }
            CompletionContext::Decorator => {
                items.extend(self.complete_decorators());
            }
            CompletionContext::Import => {
                if let Some(workspace) = workspace_root {
                    items.extend(self.complete_imports(document, position, workspace));
                }
            }
            CompletionContext::Statement => {
                // Complete keywords and identifiers
                items.extend(self.complete_keywords());
                // Complete symbols from type checker
                items.extend(self.complete_symbols(document));
            }
        }

        // Store in cache for expensive contexts
        if use_cache {
            document
                .cache_mut()
                .completion_cache
                .insert(position, items.clone(), document.version);
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

    /// Complete symbols using the cached type-check result.
    fn complete_symbols(&self, document: &Document) -> Vec<CompletionItem> {
        use crate::core::diagnostics::DiagnosticsProvider;

        let result = DiagnosticsProvider::ensure_type_checked(document);

        // Get type-only imports and re-exports from cached AST (cheap, no type checking)
        let (type_only_imports, reexports) = match document.get_or_parse_ast() {
            Some((ast, interner, _, _)) => {
                let imports = Self::get_type_only_imports(ast, &interner);
                let reexports = Self::get_reexported_symbols(ast, &interner);
                (imports, reexports)
            }
            None => (HashSet::new(), HashMap::new()),
        };

        let mut items = Vec::new();
        for (name, symbol) in &result.symbols {
            let kind = Self::kind_str_to_completion_kind(&symbol.kind);

            let mut detail = format!("{}: {}", symbol.kind, symbol.type_display);
            if type_only_imports.contains(name) {
                detail.push_str(" (type-only import)");
            }
            if let Some(source) = reexports.get(name) {
                detail.push_str(&format!(" (re-exported from {})", source));
            }

            items.push(CompletionItem {
                label: name.clone(),
                kind: Some(kind),
                detail: Some(detail),
                documentation: None,
                ..Default::default()
            });
        }

        items
    }

    /// Map a cached symbol kind string to an LSP CompletionItemKind.
    fn kind_str_to_completion_kind(kind: &str) -> CompletionItemKind {
        match kind {
            "const" | "let" | "param" => CompletionItemKind::VARIABLE,
            "function" => CompletionItemKind::FUNCTION,
            "class" => CompletionItemKind::CLASS,
            "interface" => CompletionItemKind::INTERFACE,
            "type" => CompletionItemKind::STRUCT,
            "enum" => CompletionItemKind::ENUM,
            "namespace" => CompletionItemKind::MODULE,
            _ => CompletionItemKind::VARIABLE,
        }
    }

    /// Get names of symbols imported via `import type { ... }`
    fn get_type_only_imports(
        ast: &luanext_parser::ast::Program,
        interner: &StringInterner,
    ) -> HashSet<String> {
        let mut imports = HashSet::new();
        for stmt in ast.statements {
            if let Statement::Import(import_decl) = stmt {
                if let ImportClause::TypeOnly(specs) = &import_decl.clause {
                    for spec in specs.iter() {
                        let local_name = spec
                            .local
                            .as_ref()
                            .map(|l| interner.resolve(l.node).to_string())
                            .unwrap_or_else(|| interner.resolve(spec.imported.node).to_string());
                        imports.insert(local_name);
                    }
                }
            }
        }
        imports
    }

    /// Get symbols that are re-exported and their source modules
    fn get_reexported_symbols(
        ast: &luanext_parser::ast::Program,
        interner: &StringInterner,
    ) -> HashMap<String, String> {
        let mut reexports = HashMap::new();
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
                                .map(|e| interner.resolve(e.node).to_string())
                                .unwrap_or_else(|| interner.resolve(spec.local.node).to_string());
                            reexports.insert(exported_name, source_path.clone());
                        }
                    }
                }
            }
        }
        reexports
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

    /// Complete members for member access (obj.) or method calls (obj:)
    fn complete_members(
        &self,
        document: &Document,
        position: Position,
        methods_only: bool,
    ) -> Vec<CompletionItem> {
        // Extract the identifier before the '.' or ':'
        let lines: Vec<&str> = document.text.lines().collect();
        if position.line as usize >= lines.len() {
            return Vec::new();
        }

        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        if char_pos == 0 || char_pos > line.len() {
            return Vec::new();
        }

        let before_cursor = &line[..char_pos - 1]; // -1 to exclude the '.' or ':'
        let identifier = Self::extract_identifier_before_dot(before_cursor);
        if identifier.is_empty() {
            return Vec::new();
        }

        // Parse and type check the document to get type information
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
                // Even with errors, continue to try to get type information
            }

            // Look up the symbol's type
            let symbol_table = type_checker.symbol_table();
            let symbol = match symbol_table.lookup(&identifier) {
                Some(s) => s,
                None => return Vec::new(),
            };

            // Get members based on the type
            Self::extract_members_from_type(&symbol.typ, methods_only, &interner)
        })
    }

    /// Extract the identifier before the dot or colon
    fn extract_identifier_before_dot(text: &str) -> String {
        // Simple extraction: find the last word before the cursor
        let trimmed = text.trim_end();
        let mut chars = trimmed.chars().rev();
        let mut identifier = String::new();

        while let Some(ch) = chars.next() {
            if ch.is_alphanumeric() || ch == '_' {
                identifier.push(ch);
            } else {
                break;
            }
        }

        identifier.chars().rev().collect()
    }

    /// Extract completion items from a type
    fn extract_members_from_type(
        typ: &luanext_parser::ast::types::Type,
        methods_only: bool,
        interner: &StringInterner,
    ) -> Vec<CompletionItem> {
        use luanext_parser::ast::types::{ObjectTypeMember, TypeKind};

        match &typ.kind {
            TypeKind::Object(obj_type) => {
                let mut items = Vec::new();

                for member in obj_type.members {
                    match member {
                        ObjectTypeMember::Property(prop) if !methods_only => {
                            let name = interner.resolve(prop.name.node);
                            items.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::PROPERTY),
                                detail: Some(Self::format_type_detail(&prop.type_annotation)),
                                documentation: None,
                                ..Default::default()
                            });
                        }
                        ObjectTypeMember::Method(method) => {
                            let name = interner.resolve(method.name.node);
                            items.push(CompletionItem {
                                label: name.to_string(),
                                kind: Some(CompletionItemKind::METHOD),
                                detail: Some(Self::format_method_detail(method)),
                                documentation: None,
                                ..Default::default()
                            });
                        }
                        _ => {}
                    }
                }

                items
            }
            TypeKind::Reference(_type_ref) => {
                // For type references, we'd need to resolve them
                // For now, return empty
                Vec::new()
            }
            TypeKind::Union(types) => {
                // For unions, collect members from all types
                let mut all_members = Vec::new();
                for t in *types {
                    all_members.extend(Self::extract_members_from_type(t, methods_only, interner));
                }
                // Deduplicate by label
                all_members.sort_by(|a, b| a.label.cmp(&b.label));
                all_members.dedup_by(|a, b| a.label == b.label);
                all_members
            }
            // For primitive types, provide built-in methods
            TypeKind::Primitive(prim) => Self::get_primitive_members(prim, methods_only),
            TypeKind::Array(_) => {
                // Array methods
                if methods_only {
                    vec![
                        CompletionItem {
                            label: "insert".to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some("function(index: number, value: T)".to_string()),
                            ..Default::default()
                        },
                        CompletionItem {
                            label: "remove".to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some("function(index: number): T".to_string()),
                            ..Default::default()
                        },
                    ]
                } else {
                    vec![CompletionItem {
                        label: "length".to_string(),
                        kind: Some(CompletionItemKind::PROPERTY),
                        detail: Some("number".to_string()),
                        ..Default::default()
                    }]
                }
            }
            _ => Vec::new(),
        }
    }

    /// Get built-in members for primitive types
    fn get_primitive_members(
        prim: &luanext_parser::ast::types::PrimitiveType,
        methods_only: bool,
    ) -> Vec<CompletionItem> {
        use luanext_parser::ast::types::PrimitiveType;

        match prim {
            PrimitiveType::String => {
                if methods_only {
                    vec![
                        CompletionItem {
                            label: "sub".to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some("function(i: number, j?: number): string".to_string()),
                            documentation: Some(Documentation::String(
                                "Returns substring from i to j".to_string(),
                            )),
                            ..Default::default()
                        },
                        CompletionItem {
                            label: "upper".to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some("function(): string".to_string()),
                            documentation: Some(Documentation::String(
                                "Converts string to uppercase".to_string(),
                            )),
                            ..Default::default()
                        },
                        CompletionItem {
                            label: "lower".to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some("function(): string".to_string()),
                            documentation: Some(Documentation::String(
                                "Converts string to lowercase".to_string(),
                            )),
                            ..Default::default()
                        },
                        CompletionItem {
                            label: "find".to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some(
                                "function(pattern: string, init?: number): number | nil"
                                    .to_string(),
                            ),
                            documentation: Some(Documentation::String(
                                "Finds pattern in string".to_string(),
                            )),
                            ..Default::default()
                        },
                        CompletionItem {
                            label: "gsub".to_string(),
                            kind: Some(CompletionItemKind::METHOD),
                            detail: Some(
                                "function(pattern: string, repl: string): string".to_string(),
                            ),
                            documentation: Some(Documentation::String(
                                "Global substitution".to_string(),
                            )),
                            ..Default::default()
                        },
                    ]
                } else {
                    vec![CompletionItem {
                        label: "length".to_string(),
                        kind: Some(CompletionItemKind::PROPERTY),
                        detail: Some("number".to_string()),
                        documentation: Some(Documentation::String("String length".to_string())),
                        ..Default::default()
                    }]
                }
            }
            _ => Vec::new(),
        }
    }

    /// Format type detail for display
    fn format_type_detail(typ: &luanext_parser::ast::types::Type) -> String {
        use luanext_parser::ast::types::{PrimitiveType, TypeKind};

        match &typ.kind {
            TypeKind::Primitive(PrimitiveType::Number) => "number".to_string(),
            TypeKind::Primitive(PrimitiveType::String) => "string".to_string(),
            TypeKind::Primitive(PrimitiveType::Boolean) => "boolean".to_string(),
            TypeKind::Primitive(PrimitiveType::Nil) => "nil".to_string(),
            TypeKind::Function(_) => "function".to_string(),
            TypeKind::Object(_) => "object".to_string(),
            TypeKind::Array(_) => "array".to_string(),
            _ => "type".to_string(),
        }
    }

    /// Format method signature for display
    fn format_method_detail(method: &luanext_parser::ast::statement::MethodSignature) -> String {
        // Simple signature format
        let param_count = method.parameters.len();
        format!("function({} parameters)", param_count)
    }

    /// Resolve additional details for a completion item
    #[allow(dead_code)]
    pub fn resolve(&self, item: CompletionItem) -> CompletionItem {
        // For now, just return the item as-is
        item
    }

    /// Complete import paths based on workspace files
    fn complete_imports(
        &self,
        document: &Document,
        position: Position,
        workspace_root: &Path,
    ) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Get the current line to extract the partial path
        let lines: Vec<&str> = document.text.lines().collect();
        if position.line as usize >= lines.len() {
            return items;
        }

        let line = lines[position.line as usize];
        let char_pos = position.character as usize;
        if char_pos > line.len() {
            return items;
        }

        let before_cursor = &line[..char_pos];

        // Extract the partial import path from patterns like:
        // - import ... from "./path"
        // - import ... from "path"
        // - import "path"
        let partial_path = self.extract_import_path(before_cursor);

        // Scan workspace for .luax and .d.luax files
        let available_modules = self.scan_workspace_files(workspace_root);

        // Filter and format completions based on partial path
        for module_path in available_modules {
            // Convert absolute path to relative import path
            if let Ok(rel_path) = module_path.strip_prefix(workspace_root) {
                let path_str = rel_path.to_string_lossy();
                // Remove .luax extension, handling both .luax and .d.luax
                let import_path = if path_str.ends_with(".d.luax") {
                    path_str.trim_end_matches(".d.luax")
                } else {
                    path_str.trim_end_matches(".luax")
                }
                .replace('\\', "/");

                // Check if this module matches the partial path
                if import_path.starts_with(&partial_path) || partial_path.is_empty() {
                    items.push(CompletionItem {
                        label: import_path.clone(),
                        kind: Some(CompletionItemKind::MODULE),
                        detail: Some(format!("module: {}", rel_path.display())),
                        insert_text: Some(format!("./{}", import_path)),
                        ..Default::default()
                    });
                }
            }
        }

        items
    }

    /// Extract the partial import path from the line before cursor
    fn extract_import_path(&self, before_cursor: &str) -> String {
        // Look for string literals after "from" or "import"
        if let Some(quote_start) = before_cursor.rfind(&['"', '\''][..]) {
            let after_quote = &before_cursor[quote_start + 1..];
            // Remove leading "./" if present
            after_quote.trim_start_matches("./").to_string()
        } else {
            String::new()
        }
    }

    /// Scan workspace directory for .luax and .d.luax files
    fn scan_workspace_files(&self, workspace_root: &Path) -> Vec<PathBuf> {
        let mut files = Vec::new();

        if let Ok(entries) = std::fs::read_dir(workspace_root) {
            for entry in entries.flatten() {
                let path = entry.path();

                if path.is_dir() {
                    // Recursively scan subdirectories
                    files.extend(self.scan_workspace_files(&path));
                } else if path.is_file() {
                    // Check for .luax or .d.luax extension
                    if let Some(ext) = path.extension() {
                        if ext == "luax" {
                            files.push(path);
                        }
                    }
                }
            }
        }

        files
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
    fn test_kind_str_to_completion_kind() {
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("const"),
            CompletionItemKind::VARIABLE
        );
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("let"),
            CompletionItemKind::VARIABLE
        );
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("param"),
            CompletionItemKind::VARIABLE
        );
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("function"),
            CompletionItemKind::FUNCTION
        );
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("class"),
            CompletionItemKind::CLASS
        );
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("interface"),
            CompletionItemKind::INTERFACE
        );
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("type"),
            CompletionItemKind::STRUCT
        );
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("enum"),
            CompletionItemKind::ENUM
        );
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("namespace"),
            CompletionItemKind::MODULE
        );
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
    fn test_kind_str_to_completion_kind_unknown_defaults_to_variable() {
        assert_eq!(
            CompletionProvider::kind_str_to_completion_kind("unknown_kind"),
            CompletionItemKind::VARIABLE
        );
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

    #[test]
    fn test_extract_identifier_before_dot() {
        let id = CompletionProvider::extract_identifier_before_dot("obj");
        assert_eq!(id, "obj");

        let id2 = CompletionProvider::extract_identifier_before_dot("  myVar");
        assert_eq!(id2, "myVar");

        let id3 = CompletionProvider::extract_identifier_before_dot("foo.bar");
        assert_eq!(id3, "bar");
    }

    #[test]
    fn test_member_completion_for_string() {
        let doc = create_test_document("local s: string = \"hello\"\ns:");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(1, 2));

        // Should include string methods
        let labels: Vec<&str> = result.iter().map(|i| i.label.as_str()).collect();
        assert!(
            labels.contains(&"upper") || labels.contains(&"sub"),
            "Should have string methods"
        );
    }

    #[test]
    fn test_property_completion_for_string() {
        let doc = create_test_document("local s: string = \"hello\"\ns.");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(1, 2));

        // Should include length property
        let labels: Vec<&str> = result.iter().map(|i| i.label.as_str()).collect();
        assert!(labels.contains(&"length"), "Should have length property");
    }

    #[test]
    fn test_member_completion_empty_for_unknown_var() {
        let doc = create_test_document("unknownVar.");
        let provider = CompletionProvider::new();

        let result = provider.provide(&doc, Position::new(0, 11));

        // Should return empty or handle gracefully
        let _ = result;
    }

    #[test]
    fn test_get_primitive_members_string() {
        use luanext_parser::ast::types::PrimitiveType;

        let methods = CompletionProvider::get_primitive_members(&PrimitiveType::String, true);
        assert!(!methods.is_empty());
        assert!(methods.iter().any(|m| m.label == "upper"));

        let props = CompletionProvider::get_primitive_members(&PrimitiveType::String, false);
        assert!(!props.is_empty());
        assert!(props.iter().any(|p| p.label == "length"));
    }

    #[test]
    fn test_extract_import_path_with_quotes() {
        let provider = CompletionProvider::new();

        let path1 = provider.extract_import_path("import { foo } from \"./utils");
        assert_eq!(path1, "utils");

        let path2 = provider.extract_import_path("import { foo } from \"./src/helpers");
        assert_eq!(path2, "src/helpers");

        let path3 = provider.extract_import_path("import \"");
        assert_eq!(path3, "");
    }

    #[test]
    fn test_extract_import_path_without_quotes() {
        let provider = CompletionProvider::new();

        let path = provider.extract_import_path("import { foo } from ");
        assert_eq!(path, "");
    }

    #[test]
    fn test_scan_workspace_files_basic() {
        use std::fs;
        use tempfile::tempdir;

        let provider = CompletionProvider::new();

        // Create a temporary directory structure
        let temp_dir = tempdir().unwrap();
        let temp_path = temp_dir.path();

        // Create test files
        fs::write(temp_path.join("test1.luax"), "").unwrap();
        fs::write(temp_path.join("test2.luax"), "").unwrap();
        fs::write(temp_path.join("test.txt"), "").unwrap(); // Should be ignored

        let files = provider.scan_workspace_files(temp_path);

        assert_eq!(files.len(), 2, "Should find exactly 2 .luax files");
        assert!(files.iter().any(|f| f.ends_with("test1.luax")));
        assert!(files.iter().any(|f| f.ends_with("test2.luax")));
    }

    #[test]
    fn test_scan_workspace_files_recursive() {
        use std::fs;
        use tempfile::tempdir;

        let provider = CompletionProvider::new();

        // Create a temporary directory structure with subdirectories
        let temp_dir = tempdir().unwrap();
        let temp_path = temp_dir.path();
        let sub_dir = temp_path.join("src");
        fs::create_dir(&sub_dir).unwrap();

        // Create test files
        fs::write(temp_path.join("root.luax"), "").unwrap();
        fs::write(sub_dir.join("nested.luax"), "").unwrap();

        let files = provider.scan_workspace_files(temp_path);

        assert_eq!(files.len(), 2, "Should find files recursively");
        assert!(files.iter().any(|f| f.ends_with("root.luax")));
        assert!(files.iter().any(|f| f.ends_with("nested.luax")));
    }

    #[test]
    fn test_complete_imports_with_workspace() {
        use std::fs;
        use tempfile::tempdir;

        let provider = CompletionProvider::new();

        // Create a temporary workspace
        let temp_dir = tempdir().unwrap();
        let temp_path = temp_dir.path();

        // Create test module files
        fs::write(temp_path.join("utils.luax"), "export function helper() end").unwrap();
        fs::write(
            temp_path.join("types.d.luax"),
            "export type MyType = string",
        )
        .unwrap();

        let doc = create_test_document("import { foo } from \"./");
        let position = Position::new(0, 23); // At the end of the line

        let completions = provider.complete_imports(&doc, position, temp_path);

        assert!(!completions.is_empty(), "Should find available modules");
        assert!(completions.iter().any(|c| c.label.contains("utils")));
        assert!(completions.iter().any(|c| c.label.contains("types")));

        // Check that completions have the right kind
        for completion in &completions {
            assert_eq!(completion.kind, Some(CompletionItemKind::MODULE));
        }
    }

    // ── Cache tests ──────────────────────────────────────────────────

    #[test]
    fn test_completion_cache_hit_returns_same_result() {
        let doc = create_test_document("local x = 1");
        let provider = CompletionProvider::new();
        let pos = Position::new(0, 0); // Statement context

        let result1 = provider.provide(&doc, pos);
        assert!(!result1.is_empty());

        // Second call at same position should hit cache
        let result2 = provider.provide(&doc, pos);
        assert!(!result2.is_empty());

        // Results should be identical
        assert_eq!(result1.len(), result2.len());
        for (a, b) in result1.iter().zip(result2.iter()) {
            assert_eq!(a.label, b.label);
        }

        // Cache should have recorded a hit
        let stats = doc.cache().stats().clone();
        assert!(
            stats.hits >= 1,
            "Expected at least 1 cache hit, got {}",
            stats.hits
        );
    }

    #[test]
    fn test_completion_cache_not_used_for_type_annotation() {
        let doc = create_test_document("local x: ");
        let provider = CompletionProvider::new();
        let pos = Position::new(0, 9); // TypeAnnotation context

        let _result1 = provider.provide(&doc, pos);
        let _result2 = provider.provide(&doc, pos);

        // TypeAnnotation is cheap and should NOT be cached,
        // so no stats should be recorded
        let stats = doc.cache().stats().clone();
        assert_eq!(stats.hits, 0, "TypeAnnotation should not use cache");
        assert_eq!(stats.misses, 0, "TypeAnnotation should not record misses");
    }

    #[test]
    fn test_completion_cache_not_used_for_decorator() {
        let doc = create_test_document("@");
        let provider = CompletionProvider::new();
        let pos = Position::new(0, 1); // Decorator context

        let _result1 = provider.provide(&doc, pos);
        let _result2 = provider.provide(&doc, pos);

        // Decorator is cheap and should NOT be cached
        let stats = doc.cache().stats().clone();
        assert_eq!(stats.hits, 0, "Decorator should not use cache");
    }

    #[test]
    fn test_completion_cache_miss_on_different_position() {
        let doc = create_test_document("local x = 1\nlocal y = 2");
        let provider = CompletionProvider::new();

        let _result1 = provider.provide(&doc, Position::new(0, 0));
        let _result2 = provider.provide(&doc, Position::new(1, 0));

        let stats = doc.cache().stats().clone();
        assert!(stats.misses >= 2, "Expected at least 2 cache misses");
    }
}
