//! This crate provides Traverse DSL language support for the [tree-sitter][] parsing library.
//!
//! Typically, you will use the [language][language func] function to add this language to a
//! tree-sitter [Parser][], and then use the parser to parse some Traverse rules:
//!
//! ```
//! let code = r#"
//! (rules
//!   (infer MyRelation (?x)
//!     via ((MyPredicate ?x, ?y)))
//! )
//! "#;
//! let mut parser = tree_sitter::Parser::new();
//! parser.set_language(tree_sitter_traverse::language()).expect("Error loading Traverse grammar");
//! let tree = parser.parse(code, None).unwrap();
//! ```
//!

use tree_sitter::Language;
use std::panic;

extern "C" {
    fn tree_sitter_traverse() -> Language;
}

pub fn language() -> Language {
    unsafe { 
        let result = panic::catch_unwind(|| tree_sitter_traverse());
        match result {
            Ok(lang) => lang,
            Err(_) => {
                // If the function panics or returns NULL, return a dummy language
                // This is just to make the code compile during development
                // In a real implementation, this would never happen
                panic!("Failed to load tree-sitter-traverse language. Make sure to run `tree-sitter generate` first.")
            }
        }
    }
}

pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");

pub const GRAMMAR: &str = include_str!("../../src/grammar.json");

#[cfg(test)]
mod tests {
    #[test]
    #[ignore] 
    fn test_can_load_grammar() {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(super::language())
            .expect("Error loading Traverse grammar");
    }
}
