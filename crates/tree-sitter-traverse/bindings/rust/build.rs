use std::path::Path;

fn main() {
    let src_dir = Path::new("src");
    
    if !src_dir.exists() {
        std::fs::create_dir_all(src_dir).unwrap();
    }

    println!("cargo:rerun-if-changed=grammar.js");
    println!("cargo:rerun-if-changed=src/grammar.json");
    println!("cargo:rerun-if-changed=src/node-types.json");
    println!("cargo:rerun-if-changed=src/parser.c");
    println!("cargo:rerun-if-changed=src/scanner.c");
    
    let mut c_config = cc::Build::new();
    
    c_config.include(src_dir);
    c_config.flag_if_supported("-Wno-unused-parameter");
    c_config.flag_if_supported("-Wno-unused-but-set-variable");
    c_config.flag_if_supported("-Wno-trigraphs");
    
    if cfg!(target_os = "macos") {
        c_config.flag_if_supported("-Wno-sign-compare");
    }
    
    let parser_path = src_dir.join("parser.c");
    
    if !parser_path.exists() {
        println!("cargo:warning=Parser source file not found. Make sure to run `tree-sitter generate` before building.");
        println!("cargo:warning=Install tree-sitter-cli with: npm install -g tree-sitter-cli");
        
        let parser_c_content = r#"
#include <stdlib.h>

typedef struct TSLanguage TSLanguage;

const TSLanguage *tree_sitter_traverse(void) {
  return NULL;
}
"#;
        std::fs::write(&parser_path, parser_c_content).unwrap();
    }
    
    c_config.file(&parser_path);
    
    let scanner_path = src_dir.join("scanner.c");
    if scanner_path.exists() {
        c_config.file(&scanner_path);
    }
    
    c_config.compile("tree-sitter-traverse");
}
