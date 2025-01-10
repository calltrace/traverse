use std::collections::HashSet;
use std::fs::File;
use std::io::Write;
use tree_sitter::{Language, Node, Parser, Tree};

#[derive(Debug, PartialEq)]
pub enum DDLogCommand {
    Start,
    Insert {
        fact_type: String,
        id: usize,
        parent_id: usize,
        field_name: String,
    },
    Commit,
}

impl DDLogCommand {
    pub fn to_string(&self) -> String {
        match self {
            DDLogCommand::Start => "start;".to_string(),
            DDLogCommand::Commit => "commit;".to_string(),
            DDLogCommand::Insert {
                fact_type,
                id,
                parent_id,
                field_name,
            } => format!(
                "insert {}({}, {}, \"{}\");",
                fact_type, id, parent_id, field_name
            ),
        }
    }
}

pub struct TreeSitterToDDLog<'a> {
    source_code: &'a str,
    language: Language,
    valid_types: HashSet<String>,
}

impl<'a> TreeSitterToDDLog<'a> {
    /// Constructor
    pub fn new(source_code: &'a str, language: Language, valid_types: HashSet<String>) -> Self {
        Self {
            source_code,
            language,
            valid_types,
        }
    }

    pub fn extract_commands(&self) -> Vec<DDLogCommand> {
        let mut parser = Parser::new();
        parser.set_language(&self.language).unwrap();

        let tree = parser
            .parse(self.source_code, None)
            .expect("Failed to parse source code");

        let mut commands = vec![DDLogCommand::Start]; // Start batch
        let mut next_id = 1;

        fn traverse(
            node: Node,
            parent_id: Option<usize>,
            facts: &mut Vec<DDLogCommand>,
            valid_types: &HashSet<String>,
            next_id: &mut usize,
        ) {
            let kind = node.kind().to_string();

            if !valid_types.contains(&kind) {
                eprintln!("Invalid node kind '{}'. Skipping...", kind);
                return;
            }

            let current_id = *next_id;
            *next_id += 1;

            facts.push(DDLogCommand::Insert {
                fact_type: kind,
                id: current_id,
                parent_id: parent_id.unwrap_or(0),
                field_name: "null".to_string(),
            });

            for i in 0..node.child_count() {
                if let Some(child) = node.child(i) {
                    traverse(child, Some(current_id), facts, valid_types, next_id);
                }
            }
        }

        traverse(
            tree.root_node(),
            None,
            &mut commands,
            &self.valid_types,
            &mut next_id,
        );

        commands.push(DDLogCommand::Commit);
        commands
    }

    /// Save commands to a plain text file
    pub fn save_to_file(&self, commands: &[DDLogCommand], file_path: &str) {
        let mut file = File::create(file_path).expect("Failed to create file");
        for command in commands {
            let line = command.to_string();
            file.write_all(line.as_bytes())
                .expect("Failed to write to file");
            file.write_all(b"\n").expect("Failed to write newline");
        }
        println!("Commands saved to '{}'", file_path);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use language::Solidity;
    use language::Language;

    fn mock_valid_types() -> HashSet<String> {
        vec![
            "source_unit".to_string(),
            "contract_definition".to_string(),
            "function_definition".to_string(),
            "variable_declaration".to_string(),
            "block".to_string(),
            "return_statement".to_string(),
            "expression_statement".to_string(),
            "literal".to_string(),
        ]
        .into_iter()
        .collect()
    }

    #[test]
    fn test_extract_commands_solidity() {
        let source_code = r#"
        pragma solidity ^0.8.0;

        contract SimpleStorage {
            uint256 storedData;

            function set(uint256 x) public {
                storedData = x;
            }

            function get() public view returns (uint256) {
                return storedData;
            }
        }
        "#;

        let valid_types = mock_valid_types();
        let language = Solidity.get_tree_sitter_language();


        let converter = TreeSitterToDDLog::new(source_code, language, valid_types);
        let commands = converter.extract_commands();

        let expected = vec![
            DDLogCommand::Start,
            DDLogCommand::Insert {
                fact_type: "source_unit".to_string(),
                id: 1,
                parent_id: 0,
                field_name: "null".to_string(),
            },
            DDLogCommand::Insert {
                fact_type: "contract_definition".to_string(),
                id: 2,
                parent_id: 1,
                field_name: "null".to_string(),
            },
            DDLogCommand::Insert {
                fact_type: "variable_declaration".to_string(),
                id: 3,
                parent_id: 2,
                field_name: "null".to_string(),
            },
            DDLogCommand::Insert {
                fact_type: "function_definition".to_string(),
                id: 4,
                parent_id: 2,
                field_name: "null".to_string(),
            },
            DDLogCommand::Insert {
                fact_type: "block".to_string(),
                id: 5,
                parent_id: 4,
                field_name: "null".to_string(),
            },
            DDLogCommand::Commit,
        ];

        assert_eq!(commands, expected);
    }
}
