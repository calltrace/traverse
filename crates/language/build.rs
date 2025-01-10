extern crate cc;

use std::path::Path;

use cc::Build;

// Compile the Vendored Tree-sitter grammars
fn main() {
    let grammars = vec![
        ("solidity", "vendor/tree-sitter-solidity/src"),
        ("mermaid", "vendor/tree-sitter-mermaid/src"),
    ];

    for (name, grammar_dir) in grammars {
        compile_grammar(name, grammar_dir);
    }

    println!("cargo:rerun-if-changed=vendor/tree-sitter-solidity/src/");
    println!("cargo:rerun-if-changed=vendor/tree-sitter-mermaid/src/");
}

fn compile_grammar(name: &str, grammar_dir: &str) {
    let parser_path = Path::new(grammar_dir).join("parser.c");
    if !parser_path.exists() {
        panic!("Missing parser.c in {}", parser_path.display());
    }

    let mut build = Build::new();
    build.file(parser_path);

    let scanner_c_path = Path::new(grammar_dir).join("scanner.c");
    let scanner_cc_path = Path::new(grammar_dir).join("scanner.cc");
    if scanner_c_path.exists() {
        build.file(scanner_c_path);
    } else if scanner_cc_path.exists() {
        build.file(scanner_cc_path);
    }

    build.compile(&format!("tree-sitter-{}", name));
}

