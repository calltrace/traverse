use crate::{
    ddlog_lang::{Atom, Delay, Expr, ModuleName, Pos, Rule, RuleLHS},
    gen_ddlog,
};
use std::fs::File;
use std::io::Write;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Display, Formatter},
};
use tree_sitter::{Language, Node, Parser, Tree};

/// Represents a Tree-sitter node with all the attributes needed for fact generation
#[derive(Debug, Clone, PartialEq)]
pub struct FactNode {
    pub id: usize,
    pub parent_id: usize,
    pub kind: String,
    pub relation: String,
    pub value: Option<String>,
}

impl FactNode {
    pub fn new(source_code: &str, node: &Node, id: usize, parent_id: Option<usize>) -> Self {
        let kind = node.kind().to_string();
        let relation = to_pascal_case(&kind);
        let value = source_code
            .get(node.start_byte()..node.end_byte())
            .map(|s| s.to_string());

        // escape special characters in the value 
        let value = value.map(|v| {
            v.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('\n', "\\n")
        });

        Self {
            id,
            parent_id: parent_id.unwrap_or(0),
            kind,
            relation,
            value,
        }
    }

    pub fn to_ddlog_command(&self) -> DDLogCommand {
        DDLogCommand::create_fact(
            self.relation.clone(),
            self.id,
            self.parent_id,
            self.value.clone(),
        )
    }

    /// Returns a string representation of this node for tree display
    fn to_display_string(&self) -> String {
        let mut result = self.kind.clone();
        result.push_str(&format!(" (id: {})", self.id));

        if let Some(value) = &self.value {
            // Escape quotes and special characters for display
            let escaped_value = value.replace('\\', "\\\\").replace('"', "\\\"");
            result.push_str(&format!(" \"{}\"", escaped_value));
        }

        result
    }
}

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

/// Utility for creating ASCII tree representations of FactNode trees
pub struct FactNodeTreeDumper {
    nodes: Vec<FactNode>,
    node_map: HashMap<usize, usize>, // Maps node id to index in nodes vector
    children_map: HashMap<usize, Vec<usize>>, // Maps parent id to child ids
}

impl FactNodeTreeDumper {
    /// Create a new tree dumper from a list of FactNodes
    pub fn new(nodes: Vec<FactNode>) -> Self {
        let mut node_map = HashMap::new();
        let mut children_map: HashMap<usize, Vec<usize>> = HashMap::new();

        // Build node map for quick lookups
        for (idx, node) in nodes.iter().enumerate() {
            node_map.insert(node.id, idx);

            // Add this node to its parent's children list
            if node.parent_id != 0 {
                // Skip root's parent
                children_map
                    .entry(node.parent_id)
                    .or_insert_with(Vec::new)
                    .push(node.id);
            }
        }

        Self {
            nodes,
            node_map,
            children_map,
        }
    }

    /// Find the root node (node with no parent or parent_id = 0)
    fn find_root(&self) -> Option<usize> {
        self.nodes
            .iter()
            .find(|node| node.parent_id == 0)
            .map(|node| node.id)
    }

    /// Get children of a node by its ID
    fn get_children(&self, node_id: usize) -> Vec<usize> {
        self.children_map.get(&node_id).cloned().unwrap_or_default()
    }

    /// Get a node by its ID
    fn get_node(&self, node_id: usize) -> Option<&FactNode> {
        self.node_map.get(&node_id).map(|&idx| &self.nodes[idx])
    }

    /// Generate the ASCII tree representation
    pub fn dump_tree(&self) -> String {
        let mut result = String::new();

        if let Some(root_id) = self.find_root() {
            if let Some(root) = self.get_node(root_id) {
                result.push_str(&root.to_display_string());
                result.push('\n');

                self.dump_children(root_id, &mut result, "", true);
            }
        }

        result
    }

    /// Recursively dump children of a node
    fn dump_children(&self, node_id: usize, result: &mut String, prefix: &str, is_last: bool) {
        let children = self.get_children(node_id);

        for (i, &child_id) in children.iter().enumerate() {
            let is_last_child = i == children.len() - 1;

            if is_last_child {
                result.push_str(&format!("{}└── ", prefix));
            } else {
                result.push_str(&format!("{}├── ", prefix));
            }

            // Add the node content
            if let Some(child) = self.get_node(child_id) {
                result.push_str(&child.to_display_string());
                result.push('\n');
            }

            let new_prefix = if is_last_child {
                format!("{}    ", prefix)
            } else {
                format!("{}│   ", prefix)
            };

            self.dump_children(child_id, result, &new_prefix, is_last_child);
        }
    }

    /// Write the tree to a writer
    pub fn write_tree<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        let tree = self.dump_tree();
        writer.write_all(tree.as_bytes())
    }

    /// Write the tree to a file
    pub fn write_tree_to_file(&self, file_path: &str) -> std::io::Result<()> {
        let mut file = File::create(file_path)?;
        self.write_tree(&mut file)
    }
}

pub type InsertCommandFn = fn(&str, &Node, usize, Option<usize>) -> Option<DDLogCommand>;
pub type FactNodeFilterFn = fn(&FactNode) -> bool;

impl<'a, T: language::Language> TreeSitterToDDLog<'a, T> {
    pub fn new(source_code: &'a str, language: &'a T) -> Self {
        Self {
            source_code,
            language,
            excluded_relations: None,
        }
    }

    /// Create a tree dumper for the extracted fact nodes
    pub fn create_tree_dumper(&self) -> FactNodeTreeDumper {
        let fact_nodes = self.extract_fact_nodes();
        FactNodeTreeDumper::new(fact_nodes)
    }

    /// Dump the tree representation to a string
    pub fn dump_tree(&self) -> String {
        self.create_tree_dumper().dump_tree()
    }

    /// Write the tree representation to a writer
    pub fn write_tree<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        self.create_tree_dumper().write_tree(writer)
    }

    /// Write the tree representation to a file
    pub fn write_tree_to_file(&self, file_path: &str) -> std::io::Result<()> {
        self.create_tree_dumper().write_tree_to_file(file_path)
    }

    pub fn with_excluded_relations(mut self, excluded_relations: HashSet<String>) -> Self {
        self.excluded_relations = Some(excluded_relations);
        self
    }

    pub fn extract_fact_nodes(&self) -> Vec<FactNode> {
        let tree = self
            .language
            .parse(self.source_code)
            .expect("Failed to parse source code");

        let mut fact_nodes = Vec::new();
        let mut next_id = 1;

        fn traverse(
            source_code: &str,
            node: Node,
            parent_id: Option<usize>,
            fact_nodes: &mut Vec<FactNode>,
            next_id: &mut usize,
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
                                fact_nodes,
                                next_id,
                                excluded_relations,
                            );
                        }
                    }
                    return;
                }
            }

            let current_id = *next_id;
            *next_id += 1;

            let fact_node = FactNode::new(source_code, &node, current_id, parent_id);
            fact_nodes.push(fact_node);

            for i in 0..node.child_count() {
                if let Some(child) = node.named_child(i) {
                    traverse(
                        source_code,
                        child,
                        Some(current_id),
                        fact_nodes,
                        next_id,
                        excluded_relations,
                    );
                }
            }
        }

        traverse(
            self.source_code,
            tree.root_node(),
            None,
            &mut fact_nodes,
            &mut next_id,
            &self.excluded_relations,
        );

        fact_nodes
    }

    /// Filter fact nodes using a predicate function
    pub fn filter_fact_nodes(
        &self,
        fact_nodes: &[FactNode],
        filter: &FactNodeFilterFn,
    ) -> Vec<FactNode> {
        fact_nodes
            .iter()
            .filter(|node| filter(node))
            .cloned()
            .collect()
    }

    /// Convert fact nodes to DDLogCommands
    pub fn fact_nodes_to_commands(&self, fact_nodes: &[FactNode]) -> Vec<DDLogCommand> {
        let mut commands = vec![DDLogCommand::Start];

        for fact_node in fact_nodes {
            commands.push(fact_node.to_ddlog_command());
        }

        commands.push(DDLogCommand::CommitDumpChanges);
        commands
    }

    /// Extract commands using the new FactNode approach
    pub fn extract_commands_with_fact_nodes(
        &self,
        filter: Option<&FactNodeFilterFn>,
    ) -> Vec<DDLogCommand> {
        let fact_nodes = self.extract_fact_nodes();

        let filtered_nodes = if let Some(filter_fn) = filter {
            self.filter_fact_nodes(&fact_nodes, filter_fn)
        } else {
            fact_nodes
        };

        self.fact_nodes_to_commands(&filtered_nodes)
    }

    /// TODO: Deprecate
    /// Legacy method for backward compatibility
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
    fn test_fact_node() {
        let source_code = "function test() {}";
        let solidity = Solidity;
        let mut parser = Parser::new();
        parser.set_language(solidity.language()).unwrap();
        let tree = parser.parse(source_code, None).unwrap();

        let fact_node = FactNode::new(source_code, &tree.root_node(), 1, None);

        assert_eq!(fact_node.id, 1);
        assert_eq!(fact_node.parent_id, 0);
        assert_eq!(fact_node.kind, "source_file");
        assert_eq!(fact_node.relation, "SourceFile");
        assert_eq!(fact_node.value, None);

        let command = fact_node.to_ddlog_command();
        match command {
            DDLogCommand::Insert(rule) => {
                assert_eq!(rule.lhs[0].atom.relation, "SourceFile");
            }
            _ => panic!("Expected Insert command"),
        }
    }

    #[test]
    fn test_tree_dumper() {
        // Create a simple tree structure for testing
        let nodes = vec![
            FactNode {
                id: 1,
                parent_id: 0,
                kind: "source_file".to_string(),
                relation: "SourceFile".to_string(),
                value: None,
            },
            FactNode {
                id: 2,
                parent_id: 1,
                kind: "function_definition".to_string(),
                relation: "FunctionDefinition".to_string(),
                value: None,
            },
            FactNode {
                id: 3,
                parent_id: 2,
                kind: "identifier".to_string(),
                relation: "Identifier".to_string(),
                value: Some("test".to_string()),
            },
            FactNode {
                id: 4,
                parent_id: 2,
                kind: "parameter_list".to_string(),
                relation: "ParameterList".to_string(),
                value: None,
            },
            FactNode {
                id: 5,
                parent_id: 2,
                kind: "block".to_string(),
                relation: "Block".to_string(),
                value: None,
            },
        ];

        let tree_dumper = FactNodeTreeDumper::new(nodes);
        let tree_output = tree_dumper.dump_tree();

        // Verify the tree structure
        assert!(tree_output.contains("source_file (id: 1)"));
        assert!(tree_output.contains("└── function_definition (id: 2)"));
        assert!(tree_output.contains("    ├── identifier (id: 3) \"test\""));
        assert!(tree_output.contains("    ├── parameter_list (id: 4)"));
        assert!(tree_output.contains("    └── block (id: 5)"));
    }

    #[test]
    fn test_extract_fact_nodes() {
        let source_code = r#"
        function test() {
            return 42;
        }
        "#;

        let solidity = Solidity;
        let parser = TreeSitterToDDLog::new(source_code, &solidity);

        let fact_nodes = parser.extract_fact_nodes();
        assert!(!fact_nodes.is_empty());

        // Check that we have a function_definition node
        let has_function = fact_nodes
            .iter()
            .any(|node| node.kind == "function_definition");
        assert!(has_function);

        // Test filtering
        let filter: FactNodeFilterFn = |node| node.kind == "function_definition";
        let filtered = parser.filter_fact_nodes(&fact_nodes, &filter);
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].kind, "function_definition");

        // Test conversion to commands
        let commands = parser.fact_nodes_to_commands(&filtered);
        assert_eq!(commands.len(), 3); // Start + 1 fact + CommitDumpChanges

        // Test extract_commands_with_fact_nodes
        let commands = parser.extract_commands_with_fact_nodes(Some(&filter));
        assert_eq!(commands.len(), 3); // Start + 1 fact + CommitDumpChanges
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

    #[test]
    fn test_tree_dump_solidity() {
        let source_code = r#"
        pragma solidity ^0.8.0;
        contract SimpleStorage {
            uint256 storedData;
        }
        "#;

        let solidity = Solidity;
        let parser = TreeSitterToDDLog::new(source_code, &solidity);

        // Get the tree dump
        let tree_dump = parser.dump_tree();

        // Verify key elements are in the tree
        assert!(tree_dump.contains("source_file"));
        assert!(tree_dump.contains("pragma_directive"));
        assert!(tree_dump.contains("contract_definition"));
        assert!(tree_dump.contains("SimpleStorage"));

        // Verify the structure with indentation
        assert!(tree_dump.contains("└──") || tree_dump.contains("├──"));
        assert!(tree_dump.contains("    └──") || tree_dump.contains("    ├──"));
    }
}
