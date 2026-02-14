use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use lsp_types::Position;
use luanext_lsp::core::document::Document;
use luanext_lsp::features::edit::completion::CompletionProvider;
use luanext_lsp::features::navigation::hover::HoverProvider;
use luanext_lsp::traits::HoverProviderTrait;

/// Generate a realistic LuaNext module with types and cross-file references
fn generate_realistic_module(module_id: usize, import_count: usize) -> String {
    let mut code = format!("-- Module {}\n\n", module_id);

    // Add imports from other modules
    for i in 0..import_count {
        code.push_str(&format!(
            "import type {{ DataType{}, ProcessorType{} }} from \"./module_{}\"\n",
            i, i, i
        ));
    }
    code.push('\n');

    // Add interfaces
    code.push_str(&format!(
        r#"interface IService{} {{
    id: number
    name: string
    process(input: number): number
    transform(data: string): string
}}

"#,
        module_id
    ));

    // Add type aliases
    code.push_str(&format!(
        r#"type ServiceConfig{} = {{
    enabled: boolean
    timeout: number
    retries: number
}}

"#,
        module_id
    ));

    // Add a class with methods
    code.push_str(&format!(
        r#"class ServiceImpl{0} implements IService{0} {{
    private config: ServiceConfig{0}
    private cache: Map<string, number>

    constructor(cfg: ServiceConfig{0})
        self.config = cfg
        self.cache = Map.new()
    end

    process(input: number): number
        const result = input * 2 + self.config.timeout
        return result
    end

    transform(data: string): string
        const processed = data.upper()
        return processed
    end

    private validate(value: number): boolean
        return value > 0 && value < self.config.retries
    end
}}

"#,
        module_id
    ));

    // Add functions
    code.push_str(&format!(
        r#"function createService{}(config: ServiceConfig{}): ServiceImpl{}
    const service = ServiceImpl{}(config)
    return service
end

function processData{}(service: IService{}, value: number): number
    const result = service.process(value)
    return result
end

"#,
        module_id, module_id, module_id, module_id, module_id, module_id
    ));

    // Export types
    code.push_str("export type {\n");
    code.push_str(&format!("    IService{},\n", module_id));
    code.push_str(&format!("    ServiceConfig{},\n", module_id));
    code.push_str(&format!("    ServiceImpl{},\n", module_id));
    code.push_str("}\n\n");

    // Export functions
    code.push_str("export {\n");
    code.push_str(&format!("    createService{},\n", module_id));
    code.push_str(&format!("    processData{},\n", module_id));
    code.push_str("}\n");

    code
}

/// Generate a document with many cross-file references
fn generate_document_with_many_references(reference_count: usize) -> String {
    let mut code = String::from("-- Main module with many references\n\n");

    // Import from many modules
    for i in 0..reference_count {
        code.push_str(&format!(
            "import {{ createService{}, processData{} }} from \"./module_{}\"\n",
            i, i, i
        ));
        code.push_str(&format!(
            "import type {{ ServiceConfig{} }} from \"./module_{}\"\n",
            i, i
        ));
    }

    code.push('\n');

    // Create a function that uses all imports
    code.push_str("function orchestrate(): void\n");
    for i in 0..reference_count {
        code.push_str(&format!(
            "    const config{}: ServiceConfig{} = {{ enabled: true, timeout: 100, retries: 3 }}\n",
            i, i
        ));
        code.push_str(&format!(
            "    const service{} = createService{}(config{})\n",
            i, i, i
        ));
        code.push_str(&format!(
            "    const result{} = processData{}(service{}, {})\n",
            i,
            i,
            i,
            i * 10
        ));
    }
    code.push_str("end\n");

    code
}

/// Benchmark: Hover performance with cross-file type resolution
/// Target: <100ms for hover requests
fn benchmark_hover_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("lsp_hover");
    group.sample_size(50);

    let hover_provider = HoverProvider::new();

    // Test with varying numbers of cross-file references
    for ref_count in [10, 25, 50, 100] {
        let content = generate_document_with_many_references(ref_count);
        let document = Document::new_test(content.clone(), 1);

        // Position on a variable that requires type resolution
        let position = Position {
            line: (ref_count as u32 + 3), // Point to first 'service' variable
            character: 15,
        };

        group.bench_with_input(
            BenchmarkId::new("cross_file_refs", ref_count),
            &(document, position),
            |b, (doc, pos)| {
                b.iter(|| {
                    // Note: In real implementation, this would resolve types across files
                    // For now, we benchmark the hover provider itself
                    hover_provider.provide(doc, *pos)
                })
            },
        );
    }

    group.finish();
}

/// Benchmark: Completion performance with many imported symbols
/// Target: <100ms for completion requests
fn benchmark_completion_performance(c: &mut Criterion) {
    let mut group = c.benchmark_group("lsp_completion");
    group.sample_size(50);

    let completion_provider = CompletionProvider::new();

    // Test with varying numbers of imported symbols
    for import_count in [10, 25, 50, 100] {
        let content = generate_realistic_module(1, import_count);
        let document = Document::new_test(content.clone(), 1);

        // Position after typing "create" to trigger completion
        let position = Position {
            line: (import_count as u32 + 20),
            character: 10,
        };

        group.bench_with_input(
            BenchmarkId::new("imported_symbols", import_count),
            &(document, position),
            |b, (doc, pos)| b.iter(|| completion_provider.provide(doc, *pos)),
        );
    }

    group.finish();
}

/// Benchmark: Document parsing performance (AST generation)
fn benchmark_document_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("lsp_parsing");
    group.sample_size(50);

    // Test with varying module complexity
    for import_count in [5, 10, 20, 50] {
        let content = generate_realistic_module(1, import_count);

        group.bench_with_input(
            BenchmarkId::new("module_complexity", import_count),
            &content,
            |b, content| {
                b.iter(|| {
                    // Parse and create new document (measures full parse cost)
                    Document::new_test(content.clone(), 1)
                })
            },
        );
    }

    group.finish();
}

/// Benchmark: Symbol table generation
fn benchmark_symbol_table_generation(c: &mut Criterion) {
    let mut group = c.benchmark_group("lsp_symbol_table");
    group.sample_size(50);

    for import_count in [5, 10, 20, 50] {
        let content = generate_realistic_module(1, import_count);
        let document = Document::new_test(content.clone(), 1);

        group.bench_with_input(
            BenchmarkId::new("symbols", import_count),
            &document,
            |b, doc| {
                b.iter(|| {
                    // Force symbol table regeneration
                    doc.get_or_parse_ast()
                })
            },
        );
    }

    group.finish();
}

/// Benchmark: Re-export chain traversal in LSP
fn benchmark_reexport_resolution(c: &mut Criterion) {
    let mut group = c.benchmark_group("lsp_reexport_resolution");
    group.sample_size(50);

    let hover_provider = HoverProvider::new();

    // Create a document with re-exports
    let content = r#"
-- Module with re-exports
export type * from "./base"
export { processData } from "./utils"

import type { ServiceConfig } from "./base"
import { createService } from "./utils"

function main(): void
    const config: ServiceConfig = { enabled: true, timeout: 100, retries: 3 }
    const service = createService(config)
end
"#;

    let document = Document::new_test(content.to_string(), 1);
    let position = Position {
        line: 8,
        character: 15,
    };

    group.bench_function("resolve_through_reexport", |b| {
        b.iter(|| hover_provider.provide(&document, position))
    });

    group.finish();
}

/// Benchmark: Incremental document updates
fn benchmark_incremental_updates(c: &mut Criterion) {
    let mut group = c.benchmark_group("lsp_incremental_updates");
    group.sample_size(50);

    let base_content = generate_realistic_module(1, 10);

    group.bench_function("small_edit", |b| {
        let mut document = Document::new_test(base_content.clone(), 1);

        b.iter(|| {
            // Simulate small edit (add a comment)
            let new_content = base_content.clone() + "\n-- New comment\n";
            document = Document::new_test(new_content, 2);
        })
    });

    group.bench_function("large_edit", |b| {
        let mut document = Document::new_test(base_content.clone(), 1);

        b.iter(|| {
            // Simulate large edit (add new function)
            let new_content = base_content.clone()
                + r#"
function newFunction(x: number): number
    return x * 2
end
"#;
            document = Document::new_test(new_content, 2);
        })
    });

    group.finish();
}

criterion_group!(
    benches,
    benchmark_hover_performance,
    benchmark_completion_performance,
    benchmark_document_parsing,
    benchmark_symbol_table_generation,
    benchmark_reexport_resolution,
    benchmark_incremental_updates
);
criterion_main!(benches);
