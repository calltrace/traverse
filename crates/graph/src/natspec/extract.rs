/*
    This module focuses on extracting comments from Solidity source code files
    and associating them with the relevant code items (like contracts, functions,
    state variables, etc.). It utilizes `tree-sitter` to parse the Solidity
    Abstract Syntax Tree (AST).

    Key functionalities include:
    - Defining `SourceItemKind` to categorize Solidity code elements (e.g.,
      Contract, Function, StateVariable).
    - Defining `SourceComment` to store the extracted comment text, its source
      span, the kind and name of the associated code item, the item's span,
      and a flag indicating if it's a NatSpec comment.
    - Using a `tree-sitter` query to identify comments (both block and line
      comments) that are positioned immediately before or adjacent to
      recognizable Solidity constructs.
    - Extracting details for each matched comment and its corresponding code
      item, including the item's kind and name. For certain items like state
      variables, the name is derived by inspecting the item node's children,
      as it might not be directly captured by a simple query name field.
    - The main function `extract_source_comments` takes Solidity source code
      as input and returns a vector of `SourceComment` structs.

    This module acts as a bridge between raw Solidity code and structured
    comment information, which can then be further processed, for instance,
    by parsing the `text` field of `SourceComment` using the `natspec/mod.rs`
    parsers if `is_natspec` is true.
*/
use crate::parser::get_node_text;
use anyhow::{Context, Result};
use language::{Language, Solidity};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Node, Parser, Query, QueryCursor};
use serde::{Serialize, Deserialize};

use super::{TextIndex, TextRange};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SourceItemKind {
    Contract,
    Interface,
    Library,
    Struct,
    Enum,
    Function,
    Modifier,
    Event,
    Error,
    StateVariable,
    UsingDirective,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceComment {
    pub text: String,
    pub raw_comment_span: TextRange,
    pub item_kind: SourceItemKind,
    pub item_name: Option<String>,
    pub item_span: TextRange,
    pub is_natspec: bool,
}

fn node_to_text_range(node: &tree_sitter::Node) -> TextRange {
    TextRange {
        start: TextIndex {
            utf8: node.start_byte(),
            line: node.start_position().row,
            column: node.start_position().column,
        },
        end: TextIndex {
            utf8: node.end_byte(),
            line: node.end_position().row,
            column: node.end_position().column,
        },
    }
}

const SOURCE_ITEM_COMMENT_QUERY: &str = r#"
(
  (comment) @comment
  .
  [
    (contract_declaration name: (identifier) @item_name)
    (interface_declaration name: (identifier) @item_name)
    (library_declaration name: (identifier) @item_name)
    (struct_declaration name: (identifier) @item_name)
    (enum_declaration name: (identifier) @item_name)
    (function_definition name: (identifier) @item_name)
    (modifier_definition name: (identifier) @item_name)
    (event_definition name: (identifier) @item_name)
    (error_declaration name: (identifier) @item_name)
    (state_variable_declaration name: (identifier) @item_name)
    (using_directive)
  ] @item
)
"#;

pub fn extract_source_comments(source: &str) -> Result<Vec<SourceComment>> {
    let solidity_lang = Solidity.get_tree_sitter_language();
    let mut parser = Parser::new();
    parser
        .set_language(&solidity_lang)
        .context("Failed to set language for Solidity parser")?;

    let tree = parser
        .parse(source, None)
        .context("Failed to parse Solidity source")?;

    let query = Query::new(&solidity_lang, SOURCE_ITEM_COMMENT_QUERY)
        .context("Failed to create source item comment query")?;

    let mut query_cursor = QueryCursor::new();
    let mut matches = query_cursor.matches(&query, tree.root_node(), source.as_bytes());

    let mut source_comments = Vec::new();

    matches.advance();
    while let Some(mat) = matches.get() {
        let mut comment_node: Option<Node> = None;
        let mut item_node: Option<Node> = None;
        let mut item_name_node: Option<Node> = None;

        for capture in mat.captures {
            let capture_name = &query.capture_names()[capture.index as usize];
            match *capture_name {
                "comment" => comment_node = Some(capture.node),
                "item" => item_node = Some(capture.node),
                "item_name" => item_name_node = Some(capture.node),
                _ => {}
            }
        }

        if let (Some(comment_n), Some(item_n)) = (comment_node, item_node) {
            let comment_text_str = get_node_text(&comment_n, source);
            let is_natspec =
                comment_text_str.starts_with("///") || comment_text_str.starts_with("/**");

            let item_kind_str = item_n.kind();
            let (item_kind, extracted_name) = match item_kind_str {
                "contract_declaration" => (
                    SourceItemKind::Contract,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "interface_declaration" => (
                    SourceItemKind::Interface,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "library_declaration" => (
                    SourceItemKind::Library,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "struct_declaration" => (
                    SourceItemKind::Struct,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "enum_declaration" => (
                    SourceItemKind::Enum,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "function_definition" => (
                    SourceItemKind::Function,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "modifier_definition" => (
                    SourceItemKind::Modifier,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "event_definition" => (
                    SourceItemKind::Event,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "error_declaration" => (
                    SourceItemKind::Error,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "state_variable_declaration" => (
                    SourceItemKind::StateVariable,
                    item_name_node.map(|n| get_node_text(&n, source).to_string()),
                ),
                "using_directive" => (
                    SourceItemKind::UsingDirective,
                    Some(get_node_text(&item_n, source).to_string()), // Name for using_directive is the full text
                ),
                _ => (SourceItemKind::Unknown, None),
            };

            source_comments.push(SourceComment {
                text: comment_text_str.to_string(),
                raw_comment_span: node_to_text_range(&comment_n),
                item_kind,
                item_name: extracted_name,
                item_span: node_to_text_range(&item_n),
                is_natspec,
            });
        }
        matches.advance();
    }

    Ok(source_comments)
}

#[cfg(test)]
mod source_comment_extraction_tests {
    use super::*;
    

    #[test]
    fn test_extract_simple_contract_comment() {
        let source = r#"
        /// This is a contract
        contract MyContract {}
        "#;
        let comments = extract_source_comments(source).unwrap();
        assert_eq!(comments.len(), 1);
        let comment = &comments[0];
        assert_eq!(comment.text, "/// This is a contract");
        assert!(comment.is_natspec);
        assert_eq!(comment.item_kind, SourceItemKind::Contract);
        assert_eq!(comment.item_name, Some("MyContract".to_string()));
    }

    #[test]
    fn test_extract_function_comment() {
        let source = r#"
        /**
         * This is a function.
         * @param x an integer
         */
        function myFunction(uint x) public {}
        "#;
        let comments = extract_source_comments(source).unwrap();
        assert_eq!(comments.len(), 1);
        let comment = &comments[0];
        assert_eq!(
            comment.text,
            "/**\n         * This is a function.\n         * @param x an integer\n         */"
        );
        assert!(comment.is_natspec);
        assert_eq!(comment.item_kind, SourceItemKind::Function);
        assert_eq!(comment.item_name, Some("myFunction".to_string()));
    }

    #[test]
    fn test_extract_state_variable_comment() {
        let source = r#"
        contract TestContract {
            /// The counter value
            uint256 public count;
        }
        "#;
        let comments = extract_source_comments(source).unwrap();
        assert_eq!(comments.len(), 1);
        let comment = &comments[0];
        assert_eq!(comment.text, "/// The counter value");
        assert!(comment.is_natspec);
        assert_eq!(comment.item_kind, SourceItemKind::StateVariable);
        assert_eq!(comment.item_name, Some("count".to_string()));
    }

    #[test]
    fn test_extract_multiple_comments() {
        let source = r#"
        /// Contract C
        contract C {
            /// Var V
            uint public v;

            /** Func F */
            function f() public {}
        }
        "#;
        let comments = extract_source_comments(source).unwrap();
        assert_eq!(comments.len(), 3);

        let contract_comment = comments
            .iter()
            .find(|c| c.item_name == Some("C".to_string()))
            .unwrap();
        assert_eq!(contract_comment.text, "/// Contract C");
        assert_eq!(contract_comment.item_kind, SourceItemKind::Contract);

        let var_comment = comments
            .iter()
            .find(|c| c.item_name == Some("v".to_string()))
            .unwrap();
        assert_eq!(var_comment.text, "/// Var V");
        assert_eq!(var_comment.item_kind, SourceItemKind::StateVariable);

        let func_comment = comments
            .iter()
            .find(|c| c.item_name == Some("f".to_string()))
            .unwrap();
        assert_eq!(func_comment.text, "/** Func F */");
        assert_eq!(func_comment.item_kind, SourceItemKind::Function);
    }

    #[test]
    fn test_no_comment() {
        let source = "contract NoComment {}";
        let comments = extract_source_comments(source).unwrap();
        assert!(comments.is_empty());
    }

    #[test]
    fn test_regular_comment_not_natspec() {
        let source = r#"
        // A regular comment
        function test() public {}
        "#;
        let comments = extract_source_comments(source).unwrap();
        assert_eq!(comments.len(), 1);
        assert_eq!(comments[0].text, "// A regular comment");
        assert!(!comments[0].is_natspec);
        assert_eq!(comments[0].item_kind, SourceItemKind::Function);
        assert_eq!(comments[0].item_name, Some("test".to_string()));
    }

    #[test]
    fn test_using_directive_comment() {
        let source = r#"
        contract TestContract {
            /// @title Using SafeMath for uint256
            using SafeMath for uint256;
        }
        "#;
        let comments = extract_source_comments(source).unwrap();
        assert_eq!(comments.len(), 1);
        let comment = &comments[0];
        assert_eq!(comment.text, "/// @title Using SafeMath for uint256");
        assert!(comment.is_natspec);
        assert_eq!(comment.item_kind, SourceItemKind::UsingDirective);
        assert_eq!(
            comment.item_name,
            Some("using SafeMath for uint256;".to_string())
        );
    }

    #[test]
    fn test_state_variable_complex_declaration() {
        let source = r#"
        contract TestContract {
            /// Stores the owner of the contract
            address payable public owner;
        }
        "#;
        let comments = extract_source_comments(source).unwrap();
        assert_eq!(comments.len(), 1);
        let comment = &comments[0];
        assert_eq!(comment.text, "/// Stores the owner of the contract");
        assert_eq!(comment.item_kind, SourceItemKind::StateVariable);
        assert_eq!(comment.item_name, Some("owner".to_string()));
    }

    #[test]
    fn test_state_variable_no_name_found() {
        let source = r#"
        contract Test {
            /// This is a mapping
            mapping(address => uint) public balances;
        }
        "#;
        let comments = extract_source_comments(source).unwrap();
        let mapping_comment = comments
            .iter()
            .find(|c| c.text == "/// This is a mapping")
            .unwrap();
        assert_eq!(mapping_comment.item_kind, SourceItemKind::StateVariable);
        assert_eq!(mapping_comment.item_name, Some("balances".to_string()));
    }

    #[test]
    fn test_extract_struct_and_event_comments() {
        let source = r#"
        /// Defines a new proposal.
        struct Proposal {
            address proposer;
            string description;
            uint voteCount;
        }

        /** @dev Emitted when a new proposal is created.
          * @param proposalId The ID of the new proposal.
          */
        event ProposalCreated(uint proposalId);
        "#;
        let comments = extract_source_comments(source).unwrap();
        assert_eq!(comments.len(), 2);

        let struct_comment = comments
            .iter()
            .find(|c| c.item_name == Some("Proposal".to_string()))
            .unwrap();
        assert_eq!(struct_comment.text, "/// Defines a new proposal.");
        assert_eq!(struct_comment.item_kind, SourceItemKind::Struct);

        let event_comment = comments
            .iter()
            .find(|c| c.item_name == Some("ProposalCreated".to_string()))
            .unwrap();
        assert_eq!(event_comment.text, "/** @dev Emitted when a new proposal is created.\n          * @param proposalId The ID of the new proposal.\n          */");
        assert_eq!(event_comment.item_kind, SourceItemKind::Event);
    }
}
