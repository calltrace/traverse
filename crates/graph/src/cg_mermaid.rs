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

    /// Emits the sequence of a call, its recursive processing, and its return.
    fn emit_call_and_return_sequence(
        &self,
        current_builder: &mut SequenceDiagramBuilder,
        source_node: &Node,
        target_node: &Node,
        call_edge: &Edge, // This is the call edge
        graph: &CallGraph,
        processed_return_edges: &mut HashSet<usize>,
        return_edge_lookup: &HashMap<(usize, usize, usize), usize>,
        visiting: &mut HashSet<usize>,
    ) {
        let source_participant_id =
            Self::get_participant_id(&source_node.name, source_node.contract_name.as_ref());
        let target_participant_id =
            Self::get_participant_id(&target_node.name, target_node.contract_name.as_ref()); // Callee's participant ID

        let args_str = call_edge
            .argument_names
            .as_ref()
            .map(|args| args.join(", "))
            .unwrap_or_default();
        let function_display_name = target_node.contract_name.as_ref().map_or_else(
            || target_node.name.clone(),
            |c| format!("{}.{}", c, target_node.name),
        );
        let message_content = format!("{}({})", function_display_name, args_str);

        // Activate the target participant (callee)
        current_builder.activate(target_participant_id.clone());

        current_builder.signal(
            source_participant_id,
            target_participant_id.clone(),
            "->>", // Solid arrow for call
            Some(message_content),
        );

        self.process_flow(
            target_node.id,
            graph,
            processed_return_edges,
            current_builder,
            return_edge_lookup,
            visiting,
        );

        // After recursion, process the corresponding return edge
        let return_lookup_key = (target_node.id, source_node.id, call_edge.sequence_number);
        if let Some(return_edge_index) = return_edge_lookup.get(&return_lookup_key) {
            if processed_return_edges.insert(*return_edge_index) {
                // Ensure this specific return instance hasn't been drawn yet
                if let Some(return_edge) = graph.edges.get(*return_edge_index) {
                    if let (Some(ret_source_node), Some(ret_target_node)) = (
                        // ret_source_node is the original target_node (callee)
                        graph.nodes.get(return_edge.source_node_id),
                        graph.nodes.get(return_edge.target_node_id), // ret_target_node is the original source_node (caller)
                    ) {
                        let ret_source_participant_id_for_signal = Self::get_participant_id(
                            &ret_source_node.name,
                            ret_source_node.contract_name.as_ref(),
                        );
                        let ret_target_participant_id_for_signal = Self::get_participant_id(
                            &ret_target_node.name,
                            ret_target_node.contract_name.as_ref(),
                        );
                        let returned_value_text = return_edge
                            .returned_value
                            .as_ref()
                            .map(|v| {
                                v.replace('\n', " ")
                                    .split_whitespace()
                                    .collect::<Vec<&str>>()
                                    .join(" ")
                            })
                            .filter(|s| !s.is_empty()); // Filter out empty strings after sanitizing

                        let mut value_and_type_display = String::new();
                        if let Some(val_text) = returned_value_text {
                            value_and_type_display.push_str(&val_text);
                        }

                        if let Some(type_text) = &return_edge.declared_return_type {
                            if !value_and_type_display.is_empty() {
                                value_and_type_display.push_str(": ");
                            } else {
                                // If no value text, but type exists, prefix with ":" to distinguish
                                value_and_type_display.push_str(": ");
                            }
                            value_and_type_display.push_str(type_text);
                        }

                        let message_content_ret = if value_and_type_display.is_empty() {
                            format!("ret from {}", ret_source_node.name)
                        } else {
                            format!(
                                "ret {} from {}",
                                value_and_type_display, ret_source_node.name
                            )
                        };

                        current_builder.signal(
                            ret_source_participant_id_for_signal,
                            ret_target_participant_id_for_signal,
                            "-->>", // Dashed arrow for return
                            Some(message_content_ret),
                        );
                        // Deactivation of the callee (target_participant_id) is handled at the end of this function.
                    }
                }
            }
        }
        // Else: No explicit return edge was found for this call sequence in the lookup,
        // or it was already processed (e.g., multiple calls to the same returning function).
        // The deactivation of target_participant_id will still happen below, balancing the initial activation.

        // Deactivate the target participant (callee)
        current_builder.deactivate(target_participant_id);
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
    /// Handles contracts and global scope based on node name and contract scope.
    fn get_participant_alias(node_name: &str, contract_name: Option<&String>) -> String {
        // Removed EVM/Listener specific handling
        match contract_name {
            Some(contract) => contract.clone(),
            None => MermaidGenerator::GLOBAL_SCOPE_ALIAS.to_string(),
        }
    }

    /// Recursive function to process the call flow starting from a node.
    fn process_flow(
        &self,
        current_node_id: usize,
        graph: &CallGraph,
        processed_return_edges: &mut HashSet<usize>, // Tracks processed *return* edges to avoid duplicates
        builder: &mut SequenceDiagramBuilder,
        return_edge_lookup: &HashMap<(usize, usize, usize), usize>,
        visiting: &mut HashSet<usize>, // For cycle detection within a single call stack path
    ) {
        // Cycle detection for the current path
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
            // Note: The check for already processed non-Call edges has been removed
            // to allow internal actions (storage, require, emit) to be shown on every call.
            // The `processed_return_edges` set is now used *only* to prevent duplicate return signals.

            let target_node_id = edge.target_node_id;

            if let (Some(source_node), Some(target_node)) = (
                graph.nodes.get(current_node_id),
                graph.nodes.get(target_node_id),
            ) {
                match edge.edge_type {
                    EdgeType::Call => {
                        if target_node.name == crate::cg::EVM_NODE_NAME {
                            // Process the original Caller -> EVM edge as a self-signal on the caller.
                            let source_participant_id = Self::get_participant_id(
                                &source_node.name,
                                source_node.contract_name.as_ref(),
                            );
                            let event_name = edge.event_name.as_deref().unwrap_or("UnknownEvent");
                            let args_str = edge
                                .argument_names
                                .as_ref()
                                .map(|args| args.join(", "))
                                .unwrap_or_default();
                            let message_content = format!("emit {}({})", event_name, args_str);
                            builder.signal(
                                source_participant_id.clone(),
                                source_participant_id,
                                "->>",
                                Some(message_content),
                            );
                            continue; // Move to the next edge for emits
                        }

                        let opt_label: Option<String>;
                        let source_is_user = source_node.name == Self::USER_ID;

                        if source_is_user {
                            opt_label = None; // Don't wrap calls from User
                        } else {
                            let source_contract_name_opt = source_node.contract_name.as_deref();
                            let target_contract_name_opt = target_node.contract_name.as_deref();

                            if source_contract_name_opt.is_some()
                                && target_contract_name_opt.is_some()
                                && source_contract_name_opt == target_contract_name_opt
                            {
                                // Call within the same contract
                                if target_node.node_type == NodeType::Function
                                    || target_node.node_type == NodeType::Modifier
                                {
                                    opt_label = Some(format!(
                                        "Internal: {}.{}",
                                        source_contract_name_opt.unwrap_or("?"),
                                        target_node.name
                                    ));
                                } else {
                                    opt_label = None;
                                }
                            } else if target_contract_name_opt.is_some()
                                && (target_node.node_type == NodeType::Function
                                    || target_node.node_type == NodeType::Constructor
                                    || target_node.node_type == NodeType::Modifier)
                            {
                                // Call to a function/constructor/modifier in a different contract (or from global to contract)
                                opt_label = Some(format!(
                                    "External: {}.{}",
                                    target_contract_name_opt.unwrap_or("?"),
                                    target_node.name
                                ));
                            } else if target_node.node_type == NodeType::Interface
                                || (target_contract_name_opt.is_some()
                                    && graph.nodes.iter().any(|n| {
                                        n.node_type == NodeType::Interface
                                            && Some(n.name.as_str()) == target_contract_name_opt
                                    })
                                    && target_node.node_type == NodeType::Function)
                            {
                                // Call to an interface method
                                opt_label = Some(format!(
                                    "Interface: {}.{}",
                                    target_contract_name_opt
                                        .unwrap_or_else(|| target_node.name.as_str()),
                                    target_node.name
                                ));
                            } else {
                                opt_label = None; // Global functions, or other unclassified calls
                            }
                        }

                        if let Some(label) = opt_label {
                            builder.opt_block(Some(label), |inner_builder| {
                                self.emit_call_and_return_sequence(
                                    inner_builder,
                                    source_node,
                                    target_node,
                                    edge,
                                    graph,
                                    processed_return_edges,
                                    return_edge_lookup,
                                    visiting,
                                );
                            });
                        } else {
                            self.emit_call_and_return_sequence(
                                builder, // Use the main builder if no opt block
                                source_node,
                                target_node,
                                edge,
                                graph,
                                processed_return_edges,
                                return_edge_lookup,
                                visiting,
                            );
                        }
                    } // End EdgeType::Call
                    EdgeType::Require => {
                        // --- Handle Require Statement as Note ---
                        let source_participant_id = Self::get_participant_id(
                            &source_node.name,
                            source_node.contract_name.as_ref(),
                        );
                        let condition = edge
                            .argument_names
                            .as_ref()
                            .and_then(|args| args.get(0)) // Get the first argument (condition)
                            .map(|s| s.as_str())
                            .unwrap_or("?");
                        let message = edge
                            .argument_names
                            .as_ref()
                            .and_then(|args| args.get(1)) // Get the second argument (message)
                            .map(|s| format!(" ({})", s)) // Add parentheses if message exists
                            .unwrap_or_default();
                        let condition_text = format!("require({}){}", condition, message);

                        // Represent the require check as an alt block
                        builder.alt_start(condition_text);
                        // Add the success note
                        builder.note_over(
                            vec![source_participant_id.clone()], // Note is over the target participant
                            "Continue processing".to_string(),
                        );
                        builder.alt_else("");
                        builder.note_over(
                            vec![source_participant_id], // Note is over the target participant
                            "Revert transaction".to_string(),
                        );
                        builder.alt_end();
                        // Do not recurse for require path.
                    }
                    EdgeType::Return => {
                        // Return edges are handled implicitly after their corresponding Call edge recursion returns.
                        // We mark them processed there using `processed_return_edges.insert(*return_edge_index)`.
                        // If we encounter one here directly, it means its Call was not processed in this path
                        // (e.g., skipped due to cycle detection, or it's a return from an entry point).
                        // We do nothing here.
                    }
                    EdgeType::IfConditionBranch => {
                        // The target_node_id of this edge is the synthetic IfStatementNode
                        let if_statement_node_id = edge.target_node_id;

                        // Assume condition is the first argument of the IfConditionBranch edge
                        let condition_text = edge
                            .argument_names
                            .as_ref()
                            .and_then(|args| args.get(0))
                            .map(|s| s.as_str())
                            .unwrap_or("condition") // Default text if not found
                            .to_string();

                        // Participant where the if condition is evaluated
                        let containing_participant_id = Self::get_participant_id(
                            &source_node.name, // source_node is the function/block containing the if
                            source_node.contract_name.as_ref(),
                        );

                        builder.alt_start(condition_text);

                        // Process "then" branch
                        let then_branch_edge_opt = graph.edges.iter().find(|e| {
                            e.source_node_id == if_statement_node_id
                                && e.edge_type == EdgeType::ThenBranch
                        });

                        if let Some(then_edge) = then_branch_edge_opt {
                            let stmts_before_then = builder.statement_count();
                            // Recursively process the flow starting from the ThenBlockNode
                            self.process_flow(
                                then_edge.target_node_id,
                                graph,
                                processed_return_edges,
                                builder,
                                return_edge_lookup,
                                visiting,
                            );
                            let stmts_after_then = builder.statement_count();
                            if stmts_after_then == stmts_before_then {
                                builder.note_over(
                                    vec![containing_participant_id.clone()],
                                    "No operations in 'then' branch".to_string(),
                                );
                            }
                        } else {
                            // No 'then' branch edge found, add a default note
                            builder.note_over(
                                vec![containing_participant_id.clone()],
                                "No operations in 'then' branch".to_string(),
                            );
                        }

                        // Process "else" branch
                        let else_branch_edge_opt = graph.edges.iter().find(|e| {
                            e.source_node_id == if_statement_node_id
                                && e.edge_type == EdgeType::ElseBranch
                        });

                        if let Some(else_edge) = else_branch_edge_opt {
                            builder.alt_else("else".to_string()); // Default "else" label
                            let stmts_before_else = builder.statement_count();
                            // Recursively process the flow starting from the ElseBlockNode
                            self.process_flow(
                                else_edge.target_node_id,
                                graph,
                                processed_return_edges,
                                builder,
                                return_edge_lookup,
                                visiting,
                            );
                            let stmts_after_else = builder.statement_count();
                            if stmts_after_else == stmts_before_else {
                                builder.note_over(
                                    vec![containing_participant_id.clone()],
                                    "No operations in 'else' branch".to_string(),
                                );
                            }
                        }
                        // If else_branch_edge_opt is None, no 'else' part is added to the alt block.
                        builder.alt_end();
                        // This edge guides control flow structure; actual operations are within branches.
                        // No direct recursion on if_statement_node_id from here in the main loop for this edge.
                    }
                    EdgeType::ThenBranch | EdgeType::ElseBranch => {
                        // These edges are handled by the IfConditionBranch logic when it
                        // processes the contents of the then/else blocks.
                        // If encountered directly here, they are part of that recursive processing
                        // or indicate a graph structure not originating from an IfConditionBranch.
                        // We skip them to avoid duplicate processing or incorrect diagram structure.
                        continue;
                    }
                    EdgeType::StorageRead | EdgeType::StorageWrite => {
                        // --- Handle Storage Read/Write ---
                        // Only add the note if this specific storage edge hasn't been processed.
                        // Source node is the function performing the action
                        let source_participant_id = Self::get_participant_id(
                            &source_node.name,
                            source_node.contract_name.as_ref(),
                        );
                        // Target node is the storage variable
                        let var_name = &target_node.name;
                        // Use the target node's contract name for the variable's scope
                        let var_contract_name =
                            target_node.contract_name.as_deref().unwrap_or("<Global>"); // Should ideally always have a contract

                        let action = if edge.edge_type == EdgeType::StorageRead {
                            "Read"
                        } else {
                            "Write"
                        };
                        let note_text = format!("{} {}.{}", action, var_contract_name, var_name);

                        // Add note over the participant performing the action
                        builder.note_over(vec![source_participant_id], note_text);
                        // Do not recurse into storage variable nodes
                    }
                    EdgeType::WhileConditionBranch => {
                        // The target_node_id of this edge is the synthetic WhileConditionNode
                        let while_condition_node_id = edge.target_node_id;

                        let condition_text = edge
                            .argument_names
                            .as_ref()
                            .and_then(|args| args.first())
                            .map(|s| s.as_str())
                            .unwrap_or("loop condition") // Default text
                            .to_string();

                        // Participant where the while loop is evaluated
                        let containing_participant_id = Self::get_participant_id(
                            &source_node.name, // source_node is the function/block containing the while
                            source_node.contract_name.as_ref(),
                        );

                        builder.loop_block(Some(condition_text), |inner_builder| {
                            let while_body_edge_opt = graph.edges.iter().find(|e| {
                                e.source_node_id == while_condition_node_id
                                    && e.edge_type == EdgeType::WhileBodyBranch
                            });

                            if let Some(body_edge) = while_body_edge_opt {
                                let stmts_before_body = inner_builder.statement_count();
                                self.process_flow(
                                    body_edge.target_node_id, // This is the WhileBlockNode
                                    graph,
                                    processed_return_edges,
                                    inner_builder, // Use the inner_builder for the loop content
                                    return_edge_lookup,
                                    visiting,
                                );
                                let stmts_after_body = inner_builder.statement_count();
                                if stmts_after_body == stmts_before_body {
                                    inner_builder.note_over(
                                        vec![containing_participant_id.clone()], // Note over the containing participant
                                        "Loop body has no operations".to_string(),
                                    );
                                }
                            } else {
                                // No body branch found, add a default note inside the loop
                                inner_builder.note_over(
                                    vec![containing_participant_id.clone()],
                                    "Loop body has no operations".to_string(),
                                );
                            }
                        });
                        // This edge guides control flow structure; actual operations are within the loop body.
                    }
                    EdgeType::WhileBodyBranch => {
                        // These edges are handled by the WhileConditionBranch logic.
                        // Skip them if encountered directly to avoid duplicate processing.
                        continue;
                    }
                    EdgeType::ForConditionBranch => {
                        let for_condition_node_id = edge.target_node_id;
                        let condition_text = edge
                            .argument_names
                            .as_ref()
                            .and_then(|args| args.first())
                            .map(|s| s.as_str())
                            .unwrap_or("for loop") // Default text
                            .to_string();

                        let containing_participant_id = Self::get_participant_id(
                            &source_node.name,
                            source_node.contract_name.as_ref(),
                        );

                        builder.loop_block(Some(condition_text), |inner_builder| {
                            let for_body_edge_opt = graph.edges.iter().find(|e| {
                                e.source_node_id == for_condition_node_id
                                    && e.edge_type == EdgeType::ForBodyBranch
                            });

                            if let Some(body_edge) = for_body_edge_opt {
                                let stmts_before_body = inner_builder.statement_count();
                                self.process_flow(
                                    body_edge.target_node_id, // This is the ForBlockNode
                                    graph,
                                    processed_return_edges,
                                    inner_builder,
                                    return_edge_lookup,
                                    visiting,
                                );
                                let stmts_after_body = inner_builder.statement_count();
                                if stmts_after_body == stmts_before_body {
                                    inner_builder.note_over(
                                        vec![containing_participant_id.clone()],
                                        "Loop body has no operations".to_string(),
                                    );
                                }
                            } else {
                                inner_builder.note_over(
                                    vec![containing_participant_id.clone()],
                                    "Loop body has no operations".to_string(),
                                );
                            }
                        });
                    }
                    EdgeType::ForBodyBranch => {
                        // Handled by ForConditionBranch logic.
                        continue;
                    }
                } // End match edge.edge_type
            } // End if let Some(source/target_node)
        } // End for loop over sorted_edges

        // Backtrack: remove from visiting set for this specific path
        visiting.remove(&current_node_id);
    }
}

impl ToSequenceDiagram for MermaidGenerator {
    fn to_sequence_diagram(&self, graph: &CallGraph) -> SequenceDiagram {
        let mut builder = SequenceDiagramBuilder::new();
        let mut declared_participants: HashSet<String> = HashSet::new();
        // This set now *only* tracks Return edges to prevent their duplication.
        let mut processed_return_edges: HashSet<usize> = HashSet::new();

        // 1. Add Title
        builder.title("Solidity Call Graph Sequence Diagram (Contract Level)");

        // 2. Declare User Participant
        builder.participant_as(Self::USER_ID.to_string(), Self::USER_ALIAS.to_string());
        declared_participants.insert(Self::USER_ID.to_string());

        // 3. Declare Participants (Contracts, Interfaces, Global Scope - excluding EVM/Listener)
        for node in graph.iter_nodes() {
            // Skip synthetic EVM and EventListener nodes
            if node.name == crate::cg::EVM_NODE_NAME
                || node.name == crate::cg::EVENT_LISTENER_NODE_NAME
            {
                continue;
            }

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

        // Sort entry points by name for deterministic output order
        let mut sorted_entry_points = entry_points;
        sorted_entry_points.sort_by_key(|node| &node.name);

        for entry_node in sorted_entry_points {
            // Emit initial User call signal
            let target_contract_id =
                Self::get_participant_id(&entry_node.name, entry_node.contract_name.as_ref());
            let message_content = format!("call {}()", entry_node.name);
            builder.signal(
                Self::USER_ID.to_string(),
                target_contract_id.clone(), // Clone here for potential use in return
                "->>",                      // Solid line for call
                Some(message_content),
            );

            // Start recursive processing for this entry point's flow
            let mut visiting = HashSet::new(); // Initialize cycle detection set for each entry point
            self.process_flow(
                entry_node.id,
                graph,
                &mut processed_return_edges,
                &mut builder,
                &return_edge_lookup,
                &mut visiting,
            );

            // --- Add synthetic return edge from entry point back to User ---
            // All public/external entry points return control to the user.
            let mut return_display_parts: Vec<String> = Vec::new();
            // Access the declared_return_type from the Node struct
            if let Some(return_type) = &entry_node.declared_return_type {
                if !return_type.is_empty() {
                    // Ensure type is not empty string
                    return_display_parts.push(format!(": {}", return_type));
                }
            }

            let message_content = format!(
                "ret{} from {}()",
                return_display_parts.join(""),
                entry_node.name
            );
            builder.signal(
                target_contract_id, // Source is the contract participant
                Self::USER_ID.to_string(),
                "-->>", // Dashed line for return
                Some(message_content),
            );
        }

        // Note: Any edges not reachable from a public/external function called by the User
        // will not be included in the diagram due to the traversal starting from entry points.

        builder.build()
    }
}

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
            1, // Sequence number 1
            None,
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
            2, // Sequence number 2
            None,
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
            None,
        );

        // Add a second call site: A -> C (seq 3)
        graph.add_edge(
            node_a_id,
            node_c_id,
            EdgeType::Call,
            (9, 9), // Different location in A
            None,
            3, // New sequence number 3
            None,
            None,
            None,
            None,
        );
        // Return: C -> A (seq 3)
        graph.add_edge(
            node_c_id,
            node_a_id,
            EdgeType::Return,
            (40, 50),                   // func def span
            Some((48, 49)),             // return statement span (same return as before)
            3,                          // Corresponds to call seq 3
            Some("result".to_string()), // Same return value for simplicity
            None,
            None,
            None,
        );

        // Mark funcA and funcC as having explicit returns for testing synthetic returns
        graph.nodes[node_a_id].has_explicit_return = true;
        graph.nodes[node_c_id].has_explicit_return = true;

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
            declared_return_type: None,
        };
        let node2 = Node {
            id: 1,
            name: "freeFunc".to_string(),
            node_type: NodeType::Function,
            contract_name: None,
            visibility: Visibility::Public,
            span: (0, 0),
            has_explicit_return: true,
            declared_return_type: None,
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
    fn test_to_sequence_diagram_conversion_multiple_calls() {
        let graph = create_test_graph(); // funcA (Pub), funcB (Priv), funcC (Pub)
                                         // funcA calls funcB, funcB calls funcC
                                         // funcA also calls funcC directly
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
        let mut participant_count = 0;
        for stmt in diagram.statements.iter().skip(1) {
            // Skip title
            if let Statement::Participant(p) = stmt {
                participant_count += 1;
                if p.id == id_a && p.alias.as_deref() == Some(alias_a) {
                    found_a = true;
                }
                if p.id == id_b && p.alias.as_deref() == Some(alias_b) {
                    found_b = true;
                }
            } else {
                // Stop checking participants once we hit other statements
                break;
            }
        }
        assert!(found_a, "ContractA participant missing");
        assert!(found_b, "ContractB participant missing");
        assert_eq!(participant_count, 3, "Expected 3 participants (User, A, B)");

        // Check Signals (Order based on traversal from public entry points, sorted: funcA, funcC)
        // Entry points: funcA (ContractA), funcC (ContractB)
        // Expected Flow 1 (from funcA):
        // 1. User -> ContractA (call funcA)
        // --- Inside funcA (sorted by seq num) ---
        // 2. ContractA -> ContractA (call funcB) [seq 1]
        // --- Inside funcB ---
        // 3. ContractA -> ContractB (call funcC) [seq 2]
        // --- Inside funcC (returns) ---
        // 4. ContractB -> ContractA (ret from funcC) [seq 2]
        // --- Back in funcB (returns) ---
        // 5. ContractA -> ContractA (ret from funcB) [seq 1]
        // --- Back in funcA ---
        // 6. ContractA -> ContractB (call funcC) [seq 3]
        // --- Inside funcC (returns) ---
        // 7. ContractB -> ContractA (ret from funcC) [seq 3]
        // --- Back in funcA (returns to user) ---
        // 8. ContractA -> User (ret from funcA) [synthetic]
        // Expected Flow 2 (from funcC):
        // 9. User -> ContractB (call funcC)
        // --- Inside funcC (returns) ---
        // 10. ContractB -> User (ret from funcC) [synthetic]

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

        // Expected signals: User->A, A->A(B), A->B(C), B->A(ret C), A->A(ret B), A->B(C), B->A(ret C), A->User(ret A), User->B(C), B->User(ret C)
        assert_eq!(
            signal_statements.len(),
            10, // Updated count
            "Should have 10 signal statements"
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

        // 2. ContractA -> ContractA (call funcB) [seq 1]
        let sig2 = &signal_statements[1];
        assert_eq!(sig2.from, id_a, "Signal 2 From (ContractA)");
        assert_eq!(sig2.to, id_a, "Signal 2 To (ContractA)"); // Internal call
        assert_eq!(sig2.arrow.sequence, "->>", "Signal 2 Arrow");
        assert_eq!(
            sig2.message.as_ref().map(|m| m.content.as_str()),
            Some("funcB()"), // Call to funcB
            "Signal 2 Message"
        );

        // 3. ContractA -> ContractB (call funcC) [seq 2] - Triggered by funcB flow
        let sig3 = &signal_statements[2];
        assert_eq!(sig3.from, id_a, "Signal 3 From (ContractA)"); // From funcB in ContractA
        assert_eq!(sig3.to, id_b, "Signal 3 To (ContractB)");
        assert_eq!(sig3.arrow.sequence, "->>", "Signal 3 Arrow");
        assert_eq!(
            sig3.message.as_ref().map(|m| m.content.as_str()),
            Some("funcC()"), // Call to funcC
            "Signal 3 Message"
        );

        // 4. ContractB -> ContractA (ret from funcC) [seq 2] - Return corresponding to call in sig3
        let sig4 = &signal_statements[3];
        assert_eq!(sig4.from, id_b, "Signal 4 From (ContractB)"); // Return from funcC in B
        assert_eq!(sig4.to, id_a, "Signal 4 To (ContractA)"); // Return to funcB in A
        assert_eq!(sig4.arrow.sequence, "-->>", "Signal 4 Arrow");
        assert_eq!(
            sig4.message.as_ref().map(|m| m.content.as_str()),
            Some("ret result from funcC"),
            "Signal 4 Message"
        );

        // 5. ContractA -> ContractA (ret from funcB) [seq 1] - Return corresponding to call in sig2
        let sig5 = &signal_statements[4];
        assert_eq!(sig5.from, id_a, "Signal 5 From (ContractA)"); // Return from funcB in A
        assert_eq!(sig5.to, id_a, "Signal 5 To (ContractA)"); // Return to funcA in A
        assert_eq!(sig5.arrow.sequence, "-->>", "Signal 5 Arrow");
        assert_eq!(
            sig5.message.as_ref().map(|m| m.content.as_str()),
            Some("ret from funcB"),
            "Signal 5 Message"
        );

        // 6. ContractA -> ContractB (call funcC) [seq 3] - Second call site in funcA
        let sig6 = &signal_statements[5];
        assert_eq!(sig6.from, id_a, "Signal 6 From (ContractA)"); // From funcA in ContractA
        assert_eq!(sig6.to, id_b, "Signal 6 To (ContractB)");
        assert_eq!(sig6.arrow.sequence, "->>", "Signal 6 Arrow");
        assert_eq!(
            sig6.message.as_ref().map(|m| m.content.as_str()),
            Some("funcC()"), // Call to funcC
            "Signal 6 Message"
        );

        // 7. ContractB -> ContractA (ret from funcC) [seq 3] - Return corresponding to call in sig6
        let sig7 = &signal_statements[6];
        assert_eq!(sig7.from, id_b, "Signal 7 From (ContractB)"); // Return from funcC in B
        assert_eq!(sig7.to, id_a, "Signal 7 To (ContractA)"); // Return to funcA in A
        assert_eq!(sig7.arrow.sequence, "-->>", "Signal 7 Arrow");
        assert_eq!(
            sig7.message.as_ref().map(|m| m.content.as_str()),
            Some("ret result from funcC"),
            "Signal 7 Message"
        );

        // 8. ContractA -> User (ret from funcA) [synthetic]
        let sig8 = &signal_statements[7];
        assert_eq!(sig8.from, id_a, "Signal 8 From (ContractA)");
        assert_eq!(sig8.to, user_id, "Signal 8 To (User)");
        assert_eq!(sig8.arrow.sequence, "-->>", "Signal 8 Arrow");
        assert_eq!(
            sig8.message.as_ref().map(|m| m.content.as_str()),
            Some("ret from funcA()"),
            "Signal 8 Message"
        );

        // --- Assertions for funcC being the second entry point processed ---

        // 9. User -> ContractB (call funcC)
        let sig9 = &signal_statements[8];
        assert_eq!(sig9.from, user_id, "Signal 9 From (User)");
        assert_eq!(sig9.to, id_b, "Signal 9 To (ContractB)");
        assert_eq!(sig9.arrow.sequence, "->>", "Signal 9 Arrow");
        assert_eq!(
            sig9.message.as_ref().map(|m| m.content.as_str()),
            Some("call funcC()"),
            "Signal 9 Message"
        );

        // 10. ContractB -> User (ret from funcC) [synthetic]
        let sig10 = &signal_statements[9];
        assert_eq!(sig10.from, id_b, "Signal 10 From (ContractB)");
        assert_eq!(sig10.to, user_id, "Signal 10 To (User)");
        assert_eq!(sig10.arrow.sequence, "-->>", "Signal 10 Arrow");
        assert_eq!(
            sig10.message.as_ref().map(|m| m.content.as_str()),
            Some("ret from funcC()"),
            "Signal 10 Message"
        );

        // Verify that the internal call B->C and its return C->B were NOT repeated
        // when processing the User->C entry point, because those specific *return* edges
        // (associated with seq 2) were marked processed during the funcA flow.
        // The count of 10 signals confirms this.
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

    // Test for simple recursion A -> A
    #[test]
    fn test_simple_recursion() {
        let mut graph = CallGraph::new();
        let node_a_id = graph.add_node(
            "recursiveFunc".to_string(),
            NodeType::Function,
            Some("RecurContract".to_string()),
            Visibility::Public,
            (0, 10),
        );
        graph.nodes[node_a_id].has_explicit_return = true; // Add synthetic return

        // Call: A -> A (seq 1)
        graph.add_edge(
            node_a_id,
            node_a_id,
            EdgeType::Call,
            (5, 8),
            None,
            1,
            None,
            None,
            None,
            None,
        );
        // Return: A -> A (seq 1)
        graph.add_edge(
            node_a_id,
            node_a_id,
            EdgeType::Return,
            (0, 10),
            Some((9, 9)),
            1,
            None,
            None,
            None,
            None,
        );

        let generator = MermaidGenerator::new();
        let diagram = generator.to_sequence_diagram(&graph);

        let id_a = "RecurContract";
        let user_id = MermaidGenerator::USER_ID;

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

        // Expected: User->A, A->A (call), A->A (return), A->User (synthetic return)
        assert_eq!(
            signal_statements.len(),
            4,
            "Expected 4 signals for simple recursion"
        );

        // 1. User -> A
        assert_eq!(signal_statements[0].from, user_id);
        assert_eq!(signal_statements[0].to, id_a);
        assert_eq!(signal_statements[0].arrow.sequence, "->>");
        assert!(signal_statements[0]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("call recursiveFunc()"));

        // 2. A -> A (call)
        assert_eq!(signal_statements[1].from, id_a);
        assert_eq!(signal_statements[1].to, id_a);
        assert_eq!(signal_statements[1].arrow.sequence, "->>");
        assert!(signal_statements[1]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("recursiveFunc()"));

        // 3. A -> A (return) - This return corresponds to the recursive call A->A
        // Because the recursive call is stopped by the `visiting` set, the return edge
        // associated with sequence 1 (A->A) might not be added by the standard mechanism
        // which relies on the recursion returning. Let's check if it's present.
        // *Correction*: The cycle detection prevents *further* recursion, but the return
        // for the *first* recursive call should still be processed when the outer call returns.
        assert_eq!(signal_statements[2].from, id_a);
        assert_eq!(signal_statements[2].to, id_a);
        assert_eq!(signal_statements[2].arrow.sequence, "-->>");
        assert!(signal_statements[2]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("ret from recursiveFunc"));

        // 4. A -> User (synthetic return)
        assert_eq!(signal_statements[3].from, id_a);
        assert_eq!(signal_statements[3].to, user_id);
        assert_eq!(signal_statements[3].arrow.sequence, "-->>");
        assert!(signal_statements[3]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("ret from recursiveFunc()"));
    }

    // Test for mutual recursion A -> B -> A
    #[test]
    fn test_mutual_recursion() {
        let mut graph = CallGraph::new();
        let node_a_id = graph.add_node(
            "funcA".to_string(),
            NodeType::Function,
            Some("ContractM".to_string()),
            Visibility::Public,
            (0, 10),
        );
        let node_b_id = graph.add_node(
            "funcB".to_string(),
            NodeType::Function,
            Some("ContractM".to_string()), // Same contract
            Visibility::Private,
            (20, 30),
        );
        graph.nodes[node_a_id].has_explicit_return = true; // Add synthetic return for A

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
            None,
        );
        // Call: B -> A (seq 2) - Recursive step
        graph.add_edge(
            node_b_id,
            node_a_id,
            EdgeType::Call,
            (25, 28),
            None,
            2,
            None,
            None,
            None,
            None,
        );
        // Return: A -> B (seq 2)
        graph.add_edge(
            node_a_id,
            node_b_id,
            EdgeType::Return,
            (0, 10),
            Some((9, 9)),
            2,
            None,
            None,
            None,
            None,
        );
        // Return: B -> A (seq 1)
        graph.add_edge(
            node_b_id,
            node_a_id,
            EdgeType::Return,
            (20, 30),
            Some((29, 29)),
            1,
            None,
            None,
            None,
            None,
        );

        let generator = MermaidGenerator::new();
        let diagram = generator.to_sequence_diagram(&graph);

        let id_m = "ContractM";
        let user_id = MermaidGenerator::USER_ID;

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

        // Expected: User->M(A), M(A)->M(B), M(B)->M(A) [call, stopped by cycle detection], M(A)->M(B) [return], M(B)->M(A) [return], M(A)->User [synthetic return]
        assert_eq!(
            signal_statements.len(),
            6,
            "Expected 6 signals for mutual recursion"
        );

        // 1. User -> M (call funcA)
        assert_eq!(signal_statements[0].from, user_id);
        assert_eq!(signal_statements[0].to, id_m);
        assert_eq!(signal_statements[0].arrow.sequence, "->>");
        // 2. M -> M (call funcB from funcA)
        assert_eq!(signal_statements[1].from, id_m);
        assert_eq!(signal_statements[1].to, id_m);
        assert_eq!(signal_statements[1].arrow.sequence, "->>");
        assert!(signal_statements[1]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("funcB()"));
        // 3. M -> M (call funcA from funcB) - This call happens, but recursion stops here
        assert_eq!(signal_statements[2].from, id_m);
        assert_eq!(signal_statements[2].to, id_m);
        assert_eq!(signal_statements[2].arrow.sequence, "->>");
        assert!(signal_statements[2]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("funcA()"));
        // 4. M -> M (ret from funcA to funcB) - Corresponds to seq 2 call
        assert_eq!(signal_statements[3].from, id_m);
        assert_eq!(signal_statements[3].to, id_m);
        assert_eq!(signal_statements[3].arrow.sequence, "-->>");
        assert!(signal_statements[3]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("ret from funcA"));
        // 5. M -> M (ret from funcB to funcA) - Corresponds to seq 1 call
        assert_eq!(signal_statements[4].from, id_m);
        assert_eq!(signal_statements[4].to, id_m);
        assert_eq!(signal_statements[4].arrow.sequence, "-->>");
        assert!(signal_statements[4]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("ret from funcB"));
        // 6. M -> User (ret from funcA) - Synthetic return for initial user call
        assert_eq!(signal_statements[5].from, id_m);
        assert_eq!(signal_statements[5].to, user_id);
        assert_eq!(signal_statements[5].arrow.sequence, "-->>");
        assert!(signal_statements[5]
            .message
            .as_ref()
            .unwrap()
            .content
            .contains("ret from funcA()"));
    }
}
