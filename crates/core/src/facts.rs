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
        field_name: Option<String>,
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
                field_name: Some(field_name),
            } => format!(
                "insert {}({}, {}, \"{}\");",
                fact_type, id, parent_id, field_name
            ),
            DDLogCommand::Insert {
                fact_type,
                id,
                parent_id,
                field_name: None,
            } => format!(
                "insert {}({}, {});",
                fact_type, id, parent_id)
        }
    }
}

pub struct TreeSitterToDDLog<'a, T: language::Language> {
    source_code: &'a str,
    language: &'a T,
}

impl<'a, T: language::Language> TreeSitterToDDLog<'a, T> {
    pub fn new(source_code: &'a str, language: &'a T) -> Self {
        Self {
            source_code,
            language,
        }
    }

    pub fn extract_commands<F>(&self, create_insert_command: F) -> Vec<DDLogCommand>
    where
        F: Fn(&str, &Node, usize, Option<usize>) -> Option<DDLogCommand>,
    {
        let tree = self
            .language
            .parse(self.source_code)
            .expect("Failed to parse source code");

        let mut commands = vec![DDLogCommand::Start];
        let mut next_id = 1;

        fn traverse<F>(
            source_code: &str,
            node: Node,
            parent_id: Option<usize>,
            facts: &mut Vec<DDLogCommand>,
            next_id: &mut usize,
            create_insert_command: &F,
        ) where
            F: Fn(&str, &Node, usize, Option<usize>) -> Option<DDLogCommand>,
        {
            let kind = node.kind().to_string();

            let content = source_code.get(node.start_byte()..node.end_byte()).unwrap();
            if node.child_count() == 0 {
                println!("content: {}", content);
            }

            let current_id = *next_id;
            *next_id += 1;

            let insert_command = create_insert_command(source_code, &node, current_id, parent_id);
            if let Some(insert_command) = insert_command {
                facts.push(insert_command);
            }

            for i in 0..node.child_count() {
                if let Some(child) = node.named_child(i) {
                    traverse(source_code, child, Some(current_id), facts, next_id, create_insert_command);
                }
            }
        }

        traverse(
            self.source_code,
            tree.root_node(),
            None,
            &mut commands,
            &mut next_id,
            &create_insert_command,
        );

        commands.push(DDLogCommand::Commit);
        commands
    }

    pub fn save_to_file(&self, commands: &[DDLogCommand], file_path: &str) {
        let mut file = File::create(file_path).expect("Failed to create file");
        for command in commands {
            let line = command.to_string();
            file.write_all(line.as_bytes()).expect("Failed to write");
            file.write_all(b"\n").expect("Failed newline");
        }
    }
}

pub fn to_pascal_case(s: &str) -> String {
    s.split('_')
        .filter(|part| !part.is_empty())
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                None => String::new(),
                Some(c) => c.to_ascii_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use language::{Solidity, Mermaid, Language};

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

        let solidity = Solidity;
        let parser = TreeSitterToDDLog::new(source_code, &solidity);

        let create_insert_command = |source: &str, node: &Node, id: usize, parent_id: Option<usize>| {
            let kind = to_pascal_case(node.kind());
            let field_name = if node.child_count() == 0 {
                source.get(node.start_byte()..node.end_byte()).map(|s| s.to_string())
            } else {
                None
            };

            Some(DDLogCommand::Insert {
                fact_type: kind,
                id,
                parent_id: parent_id.unwrap_or(0),
                field_name,
            })
        };

        let commands = parser.extract_commands(create_insert_command);
        assert_eq!(commands[0], DDLogCommand::Start);
        assert_eq!(commands.last().unwrap(), &DDLogCommand::Commit);

        let found_contract = commands.iter().any(|cmd| match cmd {
            DDLogCommand::Insert { fact_type, .. } if fact_type == "ContractDefinition" => true,
            _ => false,
        });
        assert!(found_contract);
    }

    #[test]
    fn test_extract_commands_mermaid() {
        let source_code = r#"
        sequenceDiagram
        participant Alice
        participant Bob
        Alice->>Bob: Hello Bob, how are you?
        Bob->>Alice: I am good thanks!
        "#;

        let mermaid = Mermaid;
        let parser = TreeSitterToDDLog::new(source_code, &mermaid);

        let create_insert_command = |source: &str, node: &Node, id: usize, parent_id: Option<usize>| {
            let kind = to_pascal_case(node.kind());
            let field_name = if node.child_count() == 0 {
                source.get(node.start_byte()..node.end_byte()).map(|s| s.to_string())
            } else {
                None
            };

            Some(DDLogCommand::Insert {
                fact_type: kind,
                id,
                parent_id: parent_id.unwrap_or(0),
                field_name,
            })
        };

        let commands = parser.extract_commands(create_insert_command);
        assert_eq!(commands[0], DDLogCommand::Start);
        assert_eq!(commands.last().unwrap(), &DDLogCommand::Commit);

        let found_sequence = commands.iter().any(|cmd| match cmd {
            DDLogCommand::Insert { fact_type, .. } if fact_type == "SequenceStmt" => true,
            _ => false,
        });
        assert!(found_sequence);
    }
}
