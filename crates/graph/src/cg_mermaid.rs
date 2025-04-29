use crate::cg::{CallGraph, Edge, EdgeType, Node, NodeType};
use mermaid::sequence_diagram_ast::{ActivationMarker, Arrow, Message, SequenceDiagram, Statement};
use mermaid::sequence_diagram_builder::SequenceDiagramBuilder;
use std::collections::{HashMap, HashSet}; // Import HashMap

/// Trait for converting a CallGraph into a Mermaid Sequence Diagram AST.
pub trait ToSequenceDiagram {
    /// Converts the given CallGraph into a Mermaid SequenceDiagram.
    ///
    /// # Arguments
    /// * `graph` - The CallGraph to convert.
    ///
    /// # Returns
    /// A `SequenceDiagram` representing the call flow.
    fn to_sequence_diagram(&self, graph: &CallGraph) -> SequenceDiagram;
}

/// Generates Mermaid Sequence Diagrams from CallGraphs.
#[derive(Default)]
pub struct MermaidGenerator;

impl MermaidGenerator {
    pub fn new() -> Self {
        Default::default()
    }

    // Define constants for participant IDs and Aliases
    const GLOBAL_SCOPE_ID: &str = "_GlobalScope_";
    const GLOBAL_SCOPE_ALIAS: &str = "Global Scope";
    const USER_ID: &str = "User";
    const USER_ALIAS: &str = "User";
    // Use constants from cg.rs for synthetic nodes
    const EVM_ID: &str = crate::cg::EVM_NODE_NAME; // "EVM"
    const EVM_ALIAS: &str = "EVM";
    const LISTENER_ID: &str = crate::cg::EVENT_LISTENER_NODE_NAME; // "EventListener"
    const LISTENER_ALIAS: &str = "EventListener";

    /// Generates a unique and Mermaid-compatible participant ID.
    /// Handles contracts, global scope, EVM, and EventListener based on node name and contract scope.
    /// Replaces potentially problematic characters.
    fn get_participant_id(node_name: &str, contract_name: Option<&String>) -> String {
        let name = match node_name {
            // Use predefined IDs for synthetic nodes
            crate::cg::EVM_NODE_NAME => MermaidGenerator::EVM_ID.to_string(),
            crate::cg::EVENT_LISTENER_NODE_NAME => MermaidGenerator::LISTENER_ID.to_string(),
            // Otherwise, use contract name or global scope ID
            _ => match contract_name {
                Some(contract) => contract.clone(),
                None => MermaidGenerator::GLOBAL_SCOPE_ID.to_string(),
            },
        };
        // Mermaid IDs might have restrictions, replace common problematic chars
        // Allow underscores, alphanumeric. Replace others.
        name.chars()
            .map(|c| {
                if c.is_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect()
    }

    /// Generates a display alias for a participant.
    /// Handles contracts, global scope, EVM, and EventListener based on node name and contract scope.
    fn get_participant_alias(node_name: &str, contract_name: Option<&String>) -> String {
        match node_name {
            // Use predefined aliases for synthetic nodes
            crate::cg::EVM_NODE_NAME => MermaidGenerator::EVM_ALIAS.to_string(),
            crate::cg::EVENT_LISTENER_NODE_NAME => MermaidGenerator::LISTENER_ALIAS.to_string(),
            // Otherwise, use contract name or global scope alias
            _ => match contract_name {
                Some(contract) => contract.clone(),
                None => MermaidGenerator::GLOBAL_SCOPE_ALIAS.to_string(),
            },
        }
    }

    /// Recursive function to process the call flow starting from a node.
    fn process_flow(
        &self,
        current_node_id: usize,
        graph: &CallGraph,
        processed_edges: &mut HashSet<usize>,
        builder: &mut SequenceDiagramBuilder,
        return_edge_lookup: &HashMap<(usize, usize, usize), usize>,
        visiting: &mut HashSet<usize>, // For cycle detection
    ) {
        // Cycle detection
        if !visiting.insert(current_node_id) {
            return; // Already visiting this node in the current path, stop recursion
        }

        // Find *all* outgoing edges (calls, returns, storage ops)
        let outgoing_edges: Vec<(usize, &Edge)> = graph // Store index along with edge ref
            .edges
            .iter()
            .enumerate()
            .filter(|(_, edge)| edge.source_node_id == current_node_id)
            .collect();

        // Sort edges primarily by sequence number (for calls/returns), then by type for stable ordering
        let mut sorted_edges = outgoing_edges;
        sorted_edges.sort_by_key(|(_, edge)| (edge.sequence_number, edge.edge_type.clone())); // Clone edge_type for sorting

        for (edge_index, edge) in sorted_edges {
            // --- Skip if this edge was already processed ---
            if processed_edges.contains(&edge_index) {
                continue;
            }

            let target_node_id = edge.target_node_id;

            if let (Some(source_node), Some(target_node)) = (
                graph.nodes.get(current_node_id),
                graph.nodes.get(target_node_id),
            ) {
                match edge.edge_type {
                    EdgeType::Call => {
                        // --- Handle Emit (Caller -> EVM) ---
                        if target_node.name == crate::cg::EVM_NODE_NAME {
                            // Find the corresponding EVM -> EventListener edge
                            let listener_edge_opt = graph.edges.iter().enumerate().find(|(_, le)| {
                                le.source_node_id == target_node_id // Source is EVM
                                && le.edge_type == EdgeType::Call
                                && le.sequence_number == edge.sequence_number // Match sequence
                                && graph.nodes.get(le.target_node_id).map_or(false, |n| n.name == crate::cg::EVENT_LISTENER_NODE_NAME) // Target is Listener
                            });

                            // Process Caller -> EVM edge
                            if processed_edges.insert(edge_index) {
                                let source_participant_id = Self::get_participant_id(
                                    &source_node.name,
                                    source_node.contract_name.as_ref(),
                                );
                                let evm_participant_id =
                                    Self::get_participant_id(crate::cg::EVM_NODE_NAME, None);
                                let event_name = edge.event_name.as_deref().unwrap_or("UnknownEvent");
                                let args_str = edge
                                    .argument_names
                                    .as_ref()
                                    .map(|args| args.join(", "))
                                    .unwrap_or_default();
                                let message_content = format!("emit {}({})", event_name, args_str);
                                builder.signal(
                                    source_participant_id,
                                    evm_participant_id,
                                    "->>",
                                    Some(message_content),
                                );
                            }

                            // Process EVM -> Listener edge
                            if let Some((listener_edge_index, _)) = listener_edge_opt {
                                if processed_edges.insert(listener_edge_index) {
                                    let evm_participant_id =
                                        Self::get_participant_id(crate::cg::EVM_NODE_NAME, None);
                                    let listener_participant_id = Self::get_participant_id(
                                        crate::cg::EVENT_LISTENER_NODE_NAME,
                                        None,
                                    );
                                    let event_name =
                                        edge.event_name.as_deref().unwrap_or("UnknownEvent");
                                    let args_str = edge
                                        .argument_names
                                        .as_ref()
                                        .map(|args| args.join(", "))
                                        .unwrap_or_default();
                                    let message_content =
                                        format!("Event: {}({})", event_name, args_str);
                                    builder.signal(
                                        evm_participant_id,
                                        listener_participant_id,
                                        "->>",
                                        Some(message_content),
                                    );
                                }
                            }
                            // Skip recursion for emit path
                            continue; // Move to the next edge
                        } else {
                            // --- Handle Regular Call ---
                            if processed_edges.insert(edge_index) {
                                let source_participant_id = Self::get_participant_id(
                                    &source_node.name,
                                    source_node.contract_name.as_ref(),
                                );
                                let target_participant_id = Self::get_participant_id(
                                    &target_node.name,
                                    target_node.contract_name.as_ref(),
                                );
                                let args_str = edge
                                    .argument_names
                                    .as_ref()
                                    .map(|args| args.join(", "))
                                    .unwrap_or_default();
                                let message_content = format!("{}({})", target_node.name, args_str);
                                builder.signal(
                                    source_participant_id,
                                    target_participant_id,
                                    "->>",
                                    Some(message_content),
                                );
                            }

                            // Recurse into the target node
                            self.process_flow(
                                target_node_id,
                                graph,
                                processed_edges,
                                builder,
                                return_edge_lookup,
                                visiting,
                            );

                            // After recursion, process the corresponding return edge
                            let return_lookup_key =
                                (target_node_id, current_node_id, edge.sequence_number);
                            if let Some(return_edge_index) =
                                return_edge_lookup.get(&return_lookup_key)
                            {
                                if processed_edges.insert(*return_edge_index) {
                                    if let Some(return_edge) = graph.edges.get(*return_edge_index)
                                    {
                                        if let (Some(ret_source_node), Some(ret_target_node)) = (
                                            graph.nodes.get(return_edge.source_node_id),
                                            graph.nodes.get(return_edge.target_node_id),
                                        ) {
                                            let ret_source_participant_id =
                                                Self::get_participant_id(
                                                    &ret_source_node.name,
                                                    ret_source_node.contract_name.as_ref(),
                                                );
                                            let ret_target_participant_id =
                                                Self::get_participant_id(
                                                    &ret_target_node.name,
                                                    ret_target_node.contract_name.as_ref(),
                                                );
                                            let returned_value_str = return_edge
                                                .returned_value
                                                .as_ref()
                                                .map(|v| {
                                                    let sanitized_v = v
                                                        .replace('\n', " ")
                                                        .split_whitespace()
                                                        .collect::<Vec<&str>>()
                                                        .join(" ");
                                                    format!(" {}", sanitized_v)
                                                })
                                                .unwrap_or_default();
                                            let message_content = format!(
                                                "ret{} from {}",
                                                returned_value_str, ret_source_node.name
                                            );
                                            builder.signal(
                                                ret_source_participant_id,
                                                ret_target_participant_id,
                                                "-->>",
                                                Some(message_content),
                                            );
                                        }
                                    }
                                }
                            } // End return edge processing
                        } // End else (regular call)
                    } // End EdgeType::Call
                    EdgeType::Return => {
                        // Return edges are handled implicitly after their corresponding Call edge recursion returns.
                        // We mark them processed there. So, if we encounter one here directly,
                        // it means it wasn't associated with a processed call, which might indicate an issue
                        // or it's a return from an entry point handled separately.
                        // We can optionally log this, but don't add a signal here.
                        // processed_edges.insert(edge_index); // Mark as processed if needed, though likely already done.
                    }
                    EdgeType::StorageRead | EdgeType::StorageWrite => {
                        // --- Handle Storage Read/Write ---
                        if processed_edges.insert(edge_index) {
                            // Source node is the function performing the action
                            let source_participant_id = Self::get_participant_id(
                                &source_node.name,
                                source_node.contract_name.as_ref(),
                            );
                            // Target node is the storage variable
                            let var_name = &target_node.name;
                            // Use the target node's contract name for the variable's scope
                            let var_contract_name = target_node
                                .contract_name
                                .as_deref()
                                .unwrap_or("<Global>"); // Should ideally always have a contract

                            let action = if edge.edge_type == EdgeType::StorageRead {
                                "Read"
                            } else {
                                "Write"
                            };
                            let note_text = format!("{} {}.{}", action, var_contract_name, var_name);

                            // Add note over the participant performing the action
                            builder.note_over(vec![source_participant_id], note_text);
                        }
                        // Do not recurse into storage variable nodes
                    }
                } // End match edge.edge_type
            } // End if let Some(source/target_node)
        } // End for loop over sorted_edges

        // Backtrack: remove from visiting set
        visiting.remove(&current_node_id);
    }
}

impl ToSequenceDiagram for MermaidGenerator {
    fn to_sequence_diagram(&self, graph: &CallGraph) -> SequenceDiagram {
        let mut builder = SequenceDiagramBuilder::new();
        let mut declared_participants: HashSet<String> = HashSet::new();
        let mut processed_edges: HashSet<usize> = HashSet::new(); // Track processed edge indices

        // 1. Add Title
        builder.title("Solidity Call Graph Sequence Diagram (Contract Level)");

        // 2. Declare User Participant
        builder.participant_as(Self::USER_ID.to_string(), Self::USER_ALIAS.to_string());
        declared_participants.insert(Self::USER_ID.to_string());

        // 3. Declare Participants (Contracts, Interfaces, Global Scope)
        for node in graph.iter_nodes() {
            // Get the participant ID based on the node's name and contract/interface name (or global scope)
            let participant_id = Self::get_participant_id(&node.name, node.contract_name.as_ref());

            // Only declare if not already declared
            if declared_participants.insert(participant_id.clone()) {
                // Alias is based on contract name or synthetic node name
                let alias = Self::get_participant_alias(&node.name, node.contract_name.as_ref());
                builder.participant_as(participant_id, alias);
            }
        }

        // 4. Precompute Return Edge Lookup Map
        // Key: (source_node_id, target_node_id, sequence_number) -> Value: edge_index
        let mut return_edge_lookup: HashMap<(usize, usize, usize), usize> = HashMap::new();
        for (index, edge) in graph.edges.iter().enumerate() {
            if edge.edge_type == EdgeType::Return {
                return_edge_lookup.insert(
                    (
                        edge.source_node_id,
                        edge.target_node_id,
                        edge.sequence_number,
                    ),
                    index,
                );
            }
        }

        // 5. Find Public/External Entry Points and Process Flow
        let entry_points: Vec<&Node> = graph
            .iter_nodes()
            .filter(|node| {
                // Entry points must be public/external functions AND NOT part of an interface definition
                (node.visibility == crate::cg::Visibility::Public
                    || node.visibility == crate::cg::Visibility::External)
                    && node.node_type == NodeType::Function
                    // Check if the node's contract_name corresponds to a known interface
                    && !node.contract_name.as_ref().map_or(false, |c_name| {
                        // Check if the graph contains an interface node with this name
                        graph.nodes.iter().any(|n| n.node_type == NodeType::Interface && n.name == *c_name)
                    })
            })
            .collect();
        for entry_node in entry_points {
            // Emit initial User call signal
            let target_contract_id =
                Self::get_participant_id(&entry_node.name, entry_node.contract_name.as_ref());
            let message_content = format!("call {}()", entry_node.name);
            builder.signal(
                Self::USER_ID.to_string(),
                target_contract_id,
                "->>", // Solid line for call
                Some(message_content),
            );

            // Start recursive processing for this entry point's flow
            let mut visiting = HashSet::new(); // Initialize cycle detection set for each entry point
            self.process_flow(
                entry_node.id,
                graph,
                &mut processed_edges,
                &mut builder,
                &return_edge_lookup,
                &mut visiting,
            );

            // --- Add synthetic return edge from entry point back to User ---
            // Check the pre-calculated flag on the entry node
            if entry_node.has_explicit_return {
                let source_contract_id =
                    Self::get_participant_id(&entry_node.name, entry_node.contract_name.as_ref());
                let message_content = format!("ret from {}()", entry_node.name); // Simple return message
                builder.signal(
                    source_contract_id,
                    Self::USER_ID.to_string(),
                    "-->>", // Dashed line for return
                    Some(message_content),
                );
            }
            // --- End synthetic return edge ---
        }

        // Note: Any edges not reachable from a public/external function called by the User
        // will not be included in the diagram due to the traversal starting from entry points.

        builder.build()
    }
}

// --- Tests ---
#[cfg(test)]
mod tests {
    use super::*;
    use crate::cg::{CallGraph, EdgeType, NodeType, Visibility};
    use mermaid::sequence_diagram_ast::*; // Import AST elements for assertions

    // Helper to create a simple graph for testing
    fn create_test_graph() -> CallGraph {
        let mut graph = CallGraph::new();
        // ContractA.funcA
        let node_a_id = graph.add_node(
            "funcA".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Public,
            (0, 10),
        );
        // ContractA.funcB
        let node_b_id = graph.add_node(
            "funcB".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Private,
            (20, 30),
        );
        // ContractB.funcC
        let node_c_id = graph.add_node(
            "funcC".to_string(),
            NodeType::Function,
            Some("ContractB".to_string()),
            Visibility::Public,
            (40, 50),
        );

        // Call: A -> B (seq 1)
        graph.add_edge(
            node_a_id,
            node_b_id,
            EdgeType::Call,
            (5, 8),
            None,
            1,
            None,
            None,
            None,
        );
        // Call: B -> C (seq 2)
        graph.add_edge(
            node_b_id,
            node_c_id,
            EdgeType::Call,
            (25, 28),
            None,
            2,
            None,
            None,
            None,
        );
        // Return: C -> B (seq 2) with value "result"
        graph.add_edge(
            node_c_id,
            node_b_id,
            EdgeType::Return,
            (40, 50),       // func def span
            Some((48, 49)), // return statement span
            2,              // Corresponds to call seq 2
            Some("result".to_string()),
            None,
            None,
        );
        // Return: B -> A (seq 1)
        graph.add_edge(
            node_b_id,
            node_a_id,
            EdgeType::Return,
            (20, 30),       // func def span
            Some((29, 29)), // return statement span (implicit/end of func)
            1,              // Corresponds to call seq 1
            None,
            None,
            None,
        );

        graph
    }

    #[test]
    fn test_get_participant_id_and_alias() {
        let node1 = Node {
            id: 0,
            name: "myFunc".to_string(),
            node_type: NodeType::Function,
            contract_name: Some("MyContract.Sol".to_string()), // Contains '.'
            visibility: Visibility::Public,
            span: (0, 0),
            has_explicit_return: true,
        };
        let node2 = Node {
            id: 1,
            name: "freeFunc".to_string(),
            node_type: NodeType::Function,
            contract_name: None,
            visibility: Visibility::Public,
            span: (0, 0),
            has_explicit_return: true,
        };

        // Test with a node that has a contract name
        assert_eq!(
            MermaidGenerator::get_participant_id(&node1.name, node1.contract_name.as_ref()),
            "MyContract_Sol" // ID should be based on contract only, replacing '.' with '_'
        );
        assert_eq!(
            MermaidGenerator::get_participant_alias(&node1.name, node1.contract_name.as_ref()),
            "MyContract.Sol" // Alias should be based on contract only
        );

        // Test with a node that has no contract name (global scope)
        assert_eq!(
            MermaidGenerator::get_participant_id(&node2.name, node2.contract_name.as_ref()),
            MermaidGenerator::GLOBAL_SCOPE_ID // Use global scope ID
        );
        assert_eq!(
            MermaidGenerator::get_participant_alias(&node2.name, node2.contract_name.as_ref()),
            MermaidGenerator::GLOBAL_SCOPE_ALIAS // Use global scope alias
        );

        // Re-asserting the contract-only ID/Alias for clarity (already tested above)
        assert_eq!(
            MermaidGenerator::get_participant_id(&node1.name, node1.contract_name.as_ref()),
            "MyContract_Sol" // ID should be based on contract only
        );
        assert_eq!(
            MermaidGenerator::get_participant_alias(&node1.name, node1.contract_name.as_ref()),
            "MyContract.Sol" // Alias should be based on contract only
        );

        assert_eq!(
            MermaidGenerator::get_participant_id(&node2.name, node2.contract_name.as_ref()),
            MermaidGenerator::GLOBAL_SCOPE_ID // Use global scope ID
        );
        assert_eq!(
            MermaidGenerator::get_participant_alias(&node2.name, node2.contract_name.as_ref()),
            MermaidGenerator::GLOBAL_SCOPE_ALIAS // Use global scope alias
        );
    }

    #[test]
    fn test_to_sequence_diagram_conversion() {
        let graph = create_test_graph(); // funcA (Pub), funcB (Priv), funcC (Pub)
        let generator = MermaidGenerator::new();
        let diagram = generator.to_sequence_diagram(&graph);

        // Expected participant IDs and Aliases (Contract Level)
        let id_a = "ContractA";
        let alias_a = "ContractA";
        let id_b = "ContractB";
        let alias_b = "ContractB";
        let user_id = MermaidGenerator::USER_ID;
        let user_alias = MermaidGenerator::USER_ALIAS;

        // Check Title
        assert!(
            matches!(&diagram.statements[0], Statement::Title(t) if t.title == "Solidity Call Graph Sequence Diagram (Contract Level)")
        );

        // Check Participants (User, ContractA, ContractB) - Order should be deterministic now
        assert!(
            matches!(&diagram.statements[1], Statement::Participant(p) if p.id == user_id && p.alias.as_deref() == Some(user_alias)),
            "User participant missing or out of order"
        );
        // Order of A and B might vary based on node iteration order in graph, check presence
        let mut found_a = false;
        let mut found_b = false;
        for stmt in diagram.statements.iter().skip(2) {
            // Skip title and user
            if let Statement::Participant(p) = stmt {
                if p.id == id_a && p.alias.as_deref() == Some(alias_a) {
                    found_a = true;
                }
                if p.id == id_b && p.alias.as_deref() == Some(alias_b) {
                    found_b = true;
                }
            }
        }
        assert!(found_a, "ContractA participant missing");
        assert!(found_b, "ContractB participant missing");

        // Check Signals (Order based on traversal from public entry points)
        // Entry points: funcA (ContractA), funcC (ContractB)
        // Expected Flow 1 (from funcA):
        // 1. User -> ContractA (call funcA)
        // 2. ContractA -> ContractA (call funcB) - Internal call within A
        // 3. ContractA -> ContractB (call funcC) - Call from A's funcB to B's funcC
        // 4. ContractB -> ContractA (ret from funcC) - Return from C to B (within A)
        // 5. ContractA -> ContractA (ret from funcB) - Return from B to A (within A)
        // Expected Flow 2 (from funcC):
        // 6. User -> ContractB (call funcC)
        // Note: The internal calls/returns (2-5) might be processed during Flow 1 and marked,
        // so they won't be repeated when processing Flow 2 if funcC is called directly by user.
        // The exact order depends on which entry point (funcA or funcC) is processed first.
        // Let's assume funcA is processed first based on node order in create_test_graph.

        let signal_statements: Vec<&SignalStatement> = diagram
            .statements
            .iter()
            .filter_map(|stmt| {
                if let Statement::Signal(s) = stmt {
                    Some(s)
                } else {
                    None
                }
            })
            .collect();

        // Expected signals: User->A, A->A(B), A->B(C), B->A(ret C), A->A(ret B), User->C
        assert_eq!(
            signal_statements.len(),
            6,
            "Should have 6 signal statements (2 user calls + 2 internal calls + 2 returns)"
        );

        // --- Assertions based on funcA being the first entry point processed ---

        // 1. User -> ContractA (call funcA)
        let sig1 = &signal_statements[0];
        assert_eq!(sig1.from, user_id, "Signal 1 From (User)");
        assert_eq!(sig1.to, id_a, "Signal 1 To (ContractA)");
        assert_eq!(sig1.arrow.sequence, "->>", "Signal 1 Arrow");
        assert_eq!(
            sig1.message.as_ref().map(|m| m.content.as_str()),
            Some("call funcA()"),
            "Signal 1 Message"
        );

        // 2. ContractA -> ContractA (call funcB) - Triggered by funcA flow
        let sig2 = &signal_statements[1];
        assert_eq!(sig2.from, id_a, "Signal 2 From (ContractA)");
        assert_eq!(sig2.to, id_a, "Signal 2 To (ContractA)"); // Internal call
        assert_eq!(sig2.arrow.sequence, "->>", "Signal 2 Arrow");
        assert_eq!(
            sig2.message.as_ref().map(|m| m.content.as_str()),
            Some("funcB()"), // Call to funcB
            "Signal 2 Message"
        );

        // 3. ContractA -> ContractB (call funcC) - Triggered by funcB flow
        let sig3 = &signal_statements[2];
        assert_eq!(sig3.from, id_a, "Signal 3 From (ContractA)"); // From funcB in ContractA
        assert_eq!(sig3.to, id_b, "Signal 3 To (ContractB)");
        assert_eq!(sig3.arrow.sequence, "->>", "Signal 3 Arrow");
        assert_eq!(
            sig3.message.as_ref().map(|m| m.content.as_str()),
            Some("funcC()"), // Call to funcC
            "Signal 3 Message"
        );

        // 4. ContractB -> ContractA (ret from funcC) - Return corresponding to call in sig3
        let sig4 = &signal_statements[3];
        assert_eq!(sig4.from, id_b, "Signal 4 From (ContractB)"); // Return from funcC in B
        assert_eq!(sig4.to, id_a, "Signal 4 To (ContractA)"); // Return to funcB in A
        assert_eq!(sig4.arrow.sequence, "-->>", "Signal 4 Arrow");
        assert_eq!(
            sig4.message.as_ref().map(|m| m.content.as_str()),
            Some("ret result from funcC"),
            "Signal 4 Message"
        );

        // 5. ContractA -> ContractA (ret from funcB) - Return corresponding to call in sig2
        let sig5 = &signal_statements[4];
        assert_eq!(sig5.from, id_a, "Signal 5 From (ContractA)"); // Return from funcB in A
        assert_eq!(sig5.to, id_a, "Signal 5 To (ContractA)"); // Return to funcA in A
        assert_eq!(sig5.arrow.sequence, "-->>", "Signal 5 Arrow");
        assert_eq!(
            sig5.message.as_ref().map(|m| m.content.as_str()),
            Some("ret from funcB"),
            "Signal 5 Message"
        );

        // 6. User -> ContractB (call funcC) - Processing the second entry point
        let sig6 = &signal_statements[5];
        assert_eq!(sig6.from, user_id, "Signal 6 From (User)");
        assert_eq!(sig6.to, id_b, "Signal 6 To (ContractB)");
        assert_eq!(sig6.arrow.sequence, "->>", "Signal 6 Arrow");
        assert_eq!(
            sig6.message.as_ref().map(|m| m.content.as_str()),
            Some("call funcC()"),
            "Signal 6 Message"
        );
        // Note: No further signals after sig6 because the internal calls/returns
        // originating from funcC (if any) would have been processed and marked
        // during the funcA flow traversal (specifically steps 3 & 4).
    }

    #[test]
    fn test_empty_graph() {
        let graph = CallGraph::new();
        let generator = MermaidGenerator::new();
        let diagram = generator.to_sequence_diagram(&graph);

        // Should have title and user participant only
        assert_eq!(
            diagram.statements.len(),
            2,
            "Expected Title and User Participant"
        );
        assert!(matches!(&diagram.statements[0], Statement::Title(_)));
        assert!(
            matches!(&diagram.statements[1], Statement::Participant(p) if p.id == MermaidGenerator::USER_ID)
        );
    }
}
