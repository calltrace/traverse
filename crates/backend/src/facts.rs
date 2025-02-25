use crate::{
    ddlog_lang::{Atom, Delay, Expr, ModuleName, Pos, Rule, RuleLHS},
    gen_ddlog,
};
use std::fs::File;
use std::io::Write;
use std::{
    collections::HashSet,
    fmt::{self, Display, Formatter},
};
use tree_sitter::{Language, Node, Parser, Tree};

#[derive(Debug, PartialEq)]
pub enum DDLogCommand {
    Start,
    Insert(Rule),
    Commit,
    Dump(Option<String>),
    CommitDumpChanges,
}

impl DDLogCommand {
    // facts can be considered a degenerate version of a rule with an empty body.
    pub fn create_fact(
        relation: String,
        id: usize,
        parent_id: usize,
        field_name: Option<String>,
    ) -> Self {
        let atom = Atom {
            pos: Pos::nopos(),
            relation,
            delay: Delay::zero(),
            diff: false,
            value: match field_name {
                Some(field) => Expr::tuple(vec![
                    Expr::int(id as i64),
                    Expr::int(parent_id as i64),
                    Expr::string_lit(&field),
                ]),
                None => Expr::tuple(vec![Expr::int(id as i64), Expr::int(parent_id as i64)]),
            },
        };

        DDLogCommand::Insert(Rule {
            pos: Pos::nopos(),
            module: ModuleName { path: vec![] },
            lhs: vec![RuleLHS {
                pos: Pos::nopos(),
                atom,
                location: None,
            }],
            rhs: vec![], // Empty rhs makes this a fact
        })
    }
}

pub struct TreeSitterToDDLog<'a, T: language::Language> {
    source_code: &'a str,
    language: &'a T,
    excluded_relations: Option<HashSet<String>>,
}

pub type InsertCommandFn = fn(&str, &Node, usize, Option<usize>) -> Option<DDLogCommand>;

impl<'a, T: language::Language> TreeSitterToDDLog<'a, T> {
    pub fn new(source_code: &'a str, language: &'a T) -> Self {
        Self {
            source_code,
            language,
            excluded_relations: None,
        }
    }

    pub fn with_excluded_relations(mut self, excluded_relations: HashSet<String>) -> Self {
        self.excluded_relations = Some(excluded_relations);
        self
    }

    pub fn extract_commands(
        &self,
        create_insert_command: Option<&InsertCommandFn>,
    ) -> Vec<DDLogCommand> {
        let tree = self
            .language
            .parse(self.source_code)
            .expect("Failed to parse source code");

        let mut commands = vec![DDLogCommand::Start];
        let mut next_id = 1;

        fn traverse(
            source_code: &str,
            node: Node,
            parent_id: Option<usize>,
            facts: &mut Vec<DDLogCommand>,
            next_id: &mut usize,
            create_insert_command: Option<&InsertCommandFn>,
            excluded_relations: &Option<HashSet<String>>,
        ) {
            let kind = node.kind().to_string();
            let relation = to_pascal_case(&kind);

            // Skip if relation is in exclusion list
            if let Some(excluded) = excluded_relations {
                if excluded.contains(&relation) {
                    for i in 0..node.child_count() {
                        if let Some(child) = node.named_child(i) {
                            traverse(
                                source_code,
                                child,
                                parent_id,
                                facts,
                                next_id,
                                create_insert_command,
                                excluded_relations,
                            );
                        }
                    }
                    return;
                }
            }

            let content = source_code.get(node.start_byte()..node.end_byte()).unwrap();

            let current_id = *next_id;
            *next_id += 1;

            if let Some(cmd_fn) = create_insert_command {
                if let Some(fact) = cmd_fn(source_code, &node, current_id, parent_id) {
                    facts.push(fact);
                }
            } else {
                let fact = DDLogCommand::create_fact(
                    to_pascal_case(&kind),
                    current_id,
                    parent_id.unwrap_or(0),
                    Some(content.to_string()),
                );
                facts.push(fact);
            }

            for i in 0..node.child_count() {
                if let Some(child) = node.named_child(i) {
                    traverse(
                        source_code,
                        child,
                        Some(current_id),
                        facts,
                        next_id,
                        create_insert_command,
                        excluded_relations,
                    );
                }
            }
        }

        traverse(
            self.source_code,
            tree.root_node(),
            None,
            &mut commands,
            &mut next_id,
            create_insert_command,
            &self.excluded_relations,
        );

        commands.push(DDLogCommand::CommitDumpChanges);
        commands
    }

    pub fn save_to_writer(&self, commands: &[DDLogCommand], mut writer: Box<dyn Write>) {
        for command in commands {
            let line = command.to_string();
            writer.write_all(line.as_bytes()).expect("Failed to write");
            writer.write_all(b"\n").expect("Failed newline");
        }
    }

    pub fn save_to_file(&self, commands: &[DDLogCommand], file_path: &str) {
        let file = File::create(file_path).expect("Failed to create file");
        self.save_to_writer(commands, Box::new(file))
    }

    pub fn save_to_stdout(&self, commands: &[DDLogCommand]) {
        self.save_to_writer(commands, Box::new(std::io::stdout()))
    }
}

impl Display for DDLogCommand {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            DDLogCommand::Start => write!(f, "start;"),
            DDLogCommand::Insert(rule) => {
                let mut insert = format!("insert {}", rule);
                insert.pop();
                insert.push(';');
                write!(f, "{}", insert) // Uses Rule's Display impl
            }
            DDLogCommand::Commit => write!(f, "commit;"),
            DDLogCommand::Dump(rel) => {
                if rel.is_none() {
                    write!(f, "dump;")
                } else {
                    write!(f, "dump {};", rel.as_ref().unwrap())
                }
            }
            DDLogCommand::CommitDumpChanges => write!(f, "commit dump_changes;"),
        }
    }
}

fn to_pascal_case(s: &str) -> String {
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
    use language::{Language, Mermaid, Solidity};

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

        let create_insert_command: InsertCommandFn =
            |source: &str, node: &Node, id: usize, parent_id: Option<usize>| {
                Some(DDLogCommand::create_fact(
                    to_pascal_case(node.kind()),
                    id,
                    parent_id.unwrap_or(0),
                    if node.child_count() == 0 {
                        source
                            .get(node.start_byte()..node.end_byte())
                            .map(|s| s.to_string())
                    } else {
                        None
                    },
                ))
            };

        let commands = parser.extract_commands(Some(&create_insert_command));
        assert_eq!(commands[0], DDLogCommand::Start);
        assert_eq!(commands.last().unwrap(), &DDLogCommand::CommitDumpChanges);

        let found_contract = commands.iter().any(|cmd| match cmd {
            DDLogCommand::Insert(rule) => rule
                .lhs
                .iter()
                .any(|lhs| lhs.atom.relation == "ContractDefinition"),
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

        let create_insert_command: InsertCommandFn =
            |source: &str, node: &Node, id: usize, parent_id: Option<usize>| {
                Some(DDLogCommand::create_fact(
                    to_pascal_case(node.kind()),
                    id,
                    parent_id.unwrap_or(0),
                    if node.child_count() == 0 {
                        source
                            .get(node.start_byte()..node.end_byte())
                            .map(|s| s.to_string())
                    } else {
                        None
                    },
                ))
            };

        let commands = parser.extract_commands(Some(&create_insert_command));
        assert_eq!(commands[0], DDLogCommand::Start);
        assert_eq!(commands.last().unwrap(), &DDLogCommand::CommitDumpChanges);

        let found_sequence = commands.iter().any(|cmd| match cmd {
            DDLogCommand::Insert(rule) => rule
                .lhs
                .iter()
                .any(|lhs| lhs.atom.relation == "SequenceStmt"),
            _ => false,
        });
        assert!(found_sequence);
    }
}
