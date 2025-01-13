/*!
# Simplified Datalog Program Representation

This module provides a lightweight and simplified representation of a Datalog program. It is designed as a convenience layer to help generate Datalog rules and manage dependencies,
while staying decoupled from specific backends such as DDLog or Soufflé.

The primary rationale for this module is to simplify the process of defining and working with Datalog rules programmatically. Writing Datalog directly can be cumbersome when managing multiple
relations and dependencies, especially in larger systems. This module offers an intermediate representation (IR) that abstracts away backend-specific complexities.
By using this abstraction, consumers can define rules, analyze dependencies, and export the resulting program to any Datalog backend.

This module is designed to be backend-agnostic, ensuring flexibility for integration with various Datalog implementations. It supports graph traversal using customizable closures, making it
easier to process and manipulate rules dynamically. Dependencies between relations and rules are automatically tracked, reducing the burden of managing complex rule graphs.
To ensure unique identification of nodes and rules, the module uses an iterator-based ID generation mechanism.

Consumers can programmatically define a Datalog program using this IR, analyze the relationships between rules and relations, and export the results to their preferred Datalog backend.
The structure is lightweight, adaptable, and emphasizes ease of use.
*/
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug)]
struct IdGenerator {
    current: u64,
}

impl IdGenerator {
    fn new() -> Self {
        IdGenerator { current: 0 }
    }
}

impl Iterator for IdGenerator {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.current;
        self.current += 1;
        Some(id)
    }
}

/// A node on the RHS of a Datalog rule.
/// NOTE: `parents` is a `Vec<u64>` so we can have multiple parents.
#[derive(Debug, Clone)]
pub (crate) struct RHSNode {
    pub(crate) id: u64,
    pub(crate) relation_name: String,
    pub(crate) variables: HashSet<String>,
    pub(crate) parents: Vec<u64>, // <---- The parents field is defined here
    pub(crate) children: Vec<u64>,
}

impl RHSNode {
    fn new(id: u64, relation_name: &str, variables: HashSet<String>) -> Self {
        RHSNode {
            id,
            relation_name: relation_name.to_string(),
            variables,
            parents: Vec::new(),
            children: Vec::new(),
        }
    }

    /// Add a parent ID to the parents list
    fn add_parent(&mut self, parent_id: u64) {
        self.parents.push(parent_id);
    }

    /// Add a child ID to the children list
    fn add_child(&mut self, child_id: u64) {
        self.children.push(child_id);
    }
}

/// A node on the LHS of a Datalog rule.
#[derive(Debug, Clone)]
pub(crate) struct LHSNode {
    id: u64,
    pub(crate) relation_name: String,
    pub(crate) referenced_variables: HashSet<String>,
}

impl LHSNode {
    fn new(id: u64, relation_name: &str, referenced_variables: HashSet<String>) -> Self {
        LHSNode {
            id,
            relation_name: relation_name.to_string(),
            referenced_variables,
        }
    }
}

/// A fully defined Datalog rule node: LHS, RHS, and expression string.
#[derive(Debug)]
pub(crate) struct RuleNode {
    id: u64,
    pub(crate) lhs: LHSNode,
    pub(crate) rhs: RHSNode,
    pub(crate) expr: String,
}

impl RuleNode {
    fn new(id: u64, lhs: LHSNode, rhs: RHSNode, expr: &str) -> Self {
        RuleNode {
            id,
            lhs,
            rhs,
            expr: expr.to_string(),
        }
    }
}

/// The main IR graph that ties everything together.
#[derive(Debug)]
pub(crate) struct IRGraph {
    id_generator: IdGenerator,
    pub(crate) rhs_nodes: HashMap<u64, RHSNode>,
    pub(crate) lhs_nodes: HashMap<u64, LHSNode>,
    pub(crate) rules: HashMap<u64, RuleNode>,
}

impl IRGraph {
    pub(crate) fn new() -> Self {
        IRGraph {
            id_generator: IdGenerator::new(),
            rhs_nodes: HashMap::new(),
            lhs_nodes: HashMap::new(),
            rules: HashMap::new(),
        }
    }

    /// Obtain the next unique ID from the generator.
    pub(crate) fn next_id(&mut self) -> u64 {
        self.id_generator.next().unwrap()
    }

    /// Create and insert an LHS node.
    pub(crate) fn add_lhs_node(&mut self, relation_name: &str, referenced_variables: HashSet<String>) -> u64 {
        let id = self.next_id();
        let node = LHSNode::new(id, relation_name, referenced_variables);
        self.lhs_nodes.insert(id, node);
        id
    }

    /// Create and insert an RHS node with multiple parents.
    pub(crate) fn add_rhs_node(
        &mut self,
        relation_name: &str,
        variables: HashSet<String>,
        parents: Vec<u64>,
    ) -> u64 {
        let id = self.next_id();
        let mut node = RHSNode::new(id, relation_name, variables);

        // For each parent, link this node as a child of that parent
        for parent_id in parents {
            node.add_parent(parent_id);
            if let Some(parent_node) = self.rhs_nodes.get_mut(&parent_id) {
                parent_node.add_child(id);
            }
        }

        self.rhs_nodes.insert(id, node);
        id
    }

    /// Create and insert a RuleNode tying an LHS and an RHS together.
    pub(crate) fn add_rule(&mut self, lhs_id: u64, rhs_id: u64, expr: &str) -> u64 {
        let id = self.next_id();
        let lhs = self
            .lhs_nodes
            .get(&lhs_id)
            .expect("LHS node not found")
            .clone();
        let rhs = self
            .rhs_nodes
            .get(&rhs_id)
            .expect("RHS node not found")
            .clone();
        let rule = RuleNode::new(id, lhs, rhs, expr);
        self.rules.insert(id, rule);
        id
    }

    /// Perform a BFS-like traversal from a given RHS node, visiting children along the way.
    pub(crate) fn traverse_from_rhs<F>(&self, start_id: u64, mut visit: F)
    where
        F: FnMut(&RHSNode),
    {
        let mut visited = HashSet::new();
        let mut queue = VecDeque::new();
        queue.push_back(start_id);

        while let Some(current_id) = queue.pop_front() {
            if visited.contains(&current_id) {
                continue;
            }
            visited.insert(current_id);

            if let Some(node) = self.rhs_nodes.get(&current_id) {
                visit(node);
                for &child_id in &node.children {
                    queue.push_back(child_id);
                }
            }
        }
    }
}
