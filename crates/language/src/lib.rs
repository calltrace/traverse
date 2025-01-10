
use std::path::Path;
use tree_sitter::{Language as TreeSitterLanguage, Parser, Tree};

/// Language trait to define parsing behavior for multiple languages
pub trait Language {
    /// Name of the language (e.g., "Solidity", "Mermaid").
    fn name(&self) -> &str;

    /// Supported file extensions for this language.
    fn extensions(&self) -> &[&str];

    /// Returns the Tree-sitter language instance for this language.
    fn get_tree_sitter_language(&self) -> TreeSitterLanguage;

    /// Parses the provided source code into a Tree-sitter `Tree`.
    fn parse(&self, source_code: &str) -> Option<Tree> {
        let mut parser = Parser::new();
        parser
            .set_language(&self.get_tree_sitter_language())
            .expect("Failed to set Tree-sitter language");
        parser.parse(source_code, None)
    }
}

/// Solidity language implementation
pub struct Solidity;

extern "C" {
    fn tree_sitter_solidity() -> TreeSitterLanguage;
}

impl Language for Solidity {

    fn name(&self) -> &str {
        "Solidity"
    }

    fn extensions(&self) -> &[&str] {
        &[".sol"]
    }

    fn get_tree_sitter_language(&self) -> TreeSitterLanguage {
        unsafe { tree_sitter_solidity() }
    }
}

/// Mermaid language implementation
pub struct Mermaid;

extern "C" {
    fn tree_sitter_mermaid() -> TreeSitterLanguage;
}

impl Language for Mermaid {
    fn name(&self) -> &str {
        "Mermaid"
    }

    fn extensions(&self) -> &[&str] {
        &[".mmd"]
    }

    fn get_tree_sitter_language(&self) -> TreeSitterLanguage {
        unsafe { tree_sitter_mermaid() }
    }
}

/// Factory function to get a language implementation by file extension
pub fn get_language_by_extension(ext: &str) -> Option<Box<dyn Language>> {
    match ext {
        "sol" => Some(Box::new(Solidity)),
        "mmd" => Some(Box::new(Mermaid)),
        _ => None,
    }
}

/// Factory function to get a language implementation for a file
pub fn get_language_for_file(file_path: &Path) -> Option<Box<dyn Language>> {
    file_path
        .extension()
        .and_then(|ext| ext.to_str())
        .and_then(get_language_by_extension)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_solidity_language() {
        let solidity = Solidity;
        assert_eq!(solidity.name(), "Solidity");
        assert_eq!(solidity.extensions(), &[".sol"]);
    }

    #[test]
    fn test_mermaid_language() {
        let mermaid = Mermaid;
        assert_eq!(mermaid.name(), "Mermaid");
        assert_eq!(mermaid.extensions(), &[".mmd"]);
    }

    #[test]
    fn test_get_language_by_extension() {
        let lang = get_language_by_extension("sol");
        assert!(lang.is_some());
        assert_eq!(lang.unwrap().name(), "Solidity");

        let lang = get_language_by_extension("mmd");
        assert!(lang.is_some());
        assert_eq!(lang.unwrap().name(), "Mermaid");

        let lang = get_language_by_extension("unknown");
        assert!(lang.is_none());
    }

    #[test]
    fn test_get_language_for_file() {
        let solidity_file = Path::new("example.sol");
        let mermaid_file = Path::new("example.mmd");
        let unknown_file = Path::new("example.txt");

        let lang = get_language_for_file(solidity_file);
        assert!(lang.is_some());
        assert_eq!(lang.unwrap().name(), "Solidity");

        let lang = get_language_for_file(mermaid_file);
        assert!(lang.is_some());
        assert_eq!(lang.unwrap().name(), "Mermaid");

        let lang = get_language_for_file(unknown_file);
        assert!(lang.is_none());
    }

    #[test]
    fn test_language_parsing() {
        let solidity = Solidity;
        let mermaid = Mermaid;

        // Mock source code for testing
        let solidity_code = "pragma solidity ^0.8.0; contract Test { }";
        let mermaid_code = "graph TD; A-->B;";

        // Parse Solidity
        let solidity_ast = solidity.parse(solidity_code);
        assert!(solidity_ast.is_some(), "Failed to parse Solidity code");

        // Parse Mermaid
        let mermaid_ast = mermaid.parse(mermaid_code);
        assert!(mermaid_ast.is_some(), "Failed to parse Mermaid code");
    }
}
