use anyhow::{Context, Result};
use tree_sitter::{Language, Parser, Tree};

/// Get the Solidity tree-sitter language
pub fn get_solidity_language() -> Language {
    tree_sitter_solidity::LANGUAGE.into()
}

/// Represents a parsed Solidity AST
#[derive(Debug)] // Add Debug for easier testing/logging if needed
pub struct SolidityAST {
    pub tree: Tree,
    pub source: String,
}

/// Parse Solidity source code into an AST
pub fn parse_solidity(source: &str) -> Result<SolidityAST> {
    let mut parser = Parser::new();
    parser
        .set_language(&get_solidity_language())
        .context("Failed to set language for Solidity parser")?;

    // Parse the source code
    let tree = parser
        .parse(source, None)
        .context("Failed to parse Solidity source")?;

    Ok(SolidityAST {
        tree,
        source: source.to_string(),
    })
}

pub fn get_node_text<'a>(node: &tree_sitter::Node, source: &'a str) -> &'a str {
    let start = node.start_byte();
    let end = node.end_byte();
    &source[start..end]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_solidity() {
        let source = r#"
        pragma solidity ^0.8.0;
        
        contract SimpleStorage {
            uint256 private value;
            
            function setValue(uint256 _value) public {
                value = _value;
            }
            
            function getValue() public view returns (uint256) {
                return value;
            }
        }
        "#;

        let ast = parse_solidity(source).unwrap();
        let root_node = ast.tree.root_node();

        assert_eq!(root_node.kind(), "source_file");
        assert!(root_node.child_count() > 0);
    }
}
