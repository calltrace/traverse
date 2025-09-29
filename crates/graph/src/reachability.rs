use crate::cg::{CallGraph, EdgeType, Node, NodeType, Visibility}; // Assuming NodeId is usize internally
use std::collections::{HashMap, HashSet};
use tracing::debug;

pub type NodeId = usize; // Ensure this is pub if storage_access needs it directly from here.

pub struct ReachabilityAnalyzer;

impl ReachabilityAnalyzer {
    pub fn new() -> Self {
        Self
    }

    /// Performs reachability analysis starting from public/external functions.
    ///
    /// Traverses the call graph via `Call` edges. For each function node visited
    /// during this traversal, if `is_node_of_interest` returns true for that function node,
    /// `process_node_of_interest` is called.
    ///
    /// The `process_node_of_interest` function is responsible for inspecting the
    /// current function node (e.g., its direct edges like `StorageRead`/`StorageWrite`)
    /// and updating the state `S`.
    ///
    /// # Type Parameters
    ///
    /// * `S`: The type of the state to be accumulated for each entry point.
    /// * `FNodeOfInterest`: A predicate `Fn(&Node) -> bool` that determines if a visited
    ///   function/modifier/constructor node in the call graph is of interest.
    /// * `FProcessNode`: An action `Fn(&Node, &mut S, &CallGraph)` called when a node of interest
    ///   is visited. It receives the node of interest, the mutable state for the current
    ///   entry point, and a reference to the full graph for context (e.g., to inspect edges).
    ///
    /// # Arguments
    ///
    /// * `graph`: The `CallGraph` to analyze.
    /// * `is_node_of_interest`: Predicate to identify interesting function/modifier/constructor nodes.
    /// * `process_node_of_interest`: Action to perform on interesting nodes.
    /// * `initial_state_factory`: A function that creates an initial state `S` for each entry point.
    ///
    /// # Returns
    ///
    /// A `HashMap` mapping the `NodeId` of each public/external entry point to the
    /// accumulated state `S` for that entry point.
    pub fn analyze_entry_points<S, FNodeOfInterest, FProcessNode>(
        &self,
        graph: &CallGraph,
        is_node_of_interest: &FNodeOfInterest,
        process_node_of_interest: &FProcessNode,
        initial_state_factory: impl Fn() -> S,
    ) -> HashMap<NodeId, S>
    where
        FNodeOfInterest: Fn(&Node) -> bool,
        FProcessNode: Fn(&Node, &mut S, &CallGraph),
    {
        let mut results: HashMap<NodeId, S> = HashMap::new();

        let entry_point_nodes: Vec<&Node> = graph.iter_nodes()
            .filter(|node|
                node.node_type == NodeType::Function &&
                (node.visibility == Visibility::Public || node.visibility == Visibility::External) &&
                // Exclude interface function declarations:
                // A function is an interface declaration if its contract_name (e.g., "IMyInterface")
                // matches the name of an actual Interface node in the graph.
                !node.contract_name.as_ref().map_or(false, |func_contract_name| {
                    graph.nodes.iter().any(|n| {
                        n.node_type == NodeType::Interface &&
                        n.name == *func_contract_name && // The Interface node's name
                        n.contract_name.as_deref() == Some(func_contract_name) // Interface node's scope is itself
                    })
                })
            )
            .collect();

        for entry_node in entry_point_nodes {
            let mut current_state = initial_state_factory();
            // This set tracks visited functions *within the traversal for a single entry point*.
            let mut visited_functions_for_this_entry_point: HashSet<NodeId> = HashSet::new();

            self.dfs_traverse(
                entry_node.id,
                graph,
                is_node_of_interest,
                process_node_of_interest,
                &mut current_state,
                &mut visited_functions_for_this_entry_point,
            );
            results.insert(entry_node.id, current_state);
        }
        results
    }

    pub fn dfs_traverse<S, FNodeOfInterest, FProcessNode>(
        &self,
        current_node_id: NodeId,
        graph: &CallGraph,
        is_node_of_interest: &FNodeOfInterest,
        process_node_of_interest: &FProcessNode,
        state: &mut S,
        visited_functions_for_this_entry_point: &mut HashSet<NodeId>,
    ) where
        FNodeOfInterest: Fn(&Node) -> bool,
        FProcessNode: Fn(&Node, &mut S, &CallGraph),
    {
        let current_node = match graph.nodes.get(current_node_id) {
            Some(node) => node,
            None => {
                debug!(
                    "[Reachability DFS] Error: Node ID {} not found in graph.",
                    current_node_id
                );
                return;
            }
        };

        // Check if this function/modifier/constructor node has already been processed
        // for this particular entry point's traversal to avoid redundant work and cycles.
        if matches!(
            current_node.node_type,
            NodeType::Function | NodeType::Modifier | NodeType::Constructor
        ) {
            if !visited_functions_for_this_entry_point.insert(current_node_id) {
                return;
            }
        }

        // Process the current node (function/modifier/constructor) if it's of interest.
        // The `process_node_of_interest` function will then typically look at this
        // node's direct interactions (e.g., StorageRead/Write edges).
        if is_node_of_interest(current_node) {
            process_node_of_interest(current_node, state, graph);
        }

        // Traverse outgoing 'Call' edges to explore the call graph further.
        for edge in &graph.edges {
            if edge.source_node_id == current_node_id && edge.edge_type == EdgeType::Call {
                // The target of this edge is the callee.
                self.dfs_traverse(
                    edge.target_node_id,
                    graph,
                    is_node_of_interest,
                    process_node_of_interest,
                    state,
                    visited_functions_for_this_entry_point, // Pass the same set
                );
            }
        }
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use crate::cg::{CallGraph, EdgeParams, EdgeType, NodeType, Visibility};
    use std::collections::HashSet;

    pub fn create_test_graph_for_reachability() -> CallGraph {
        let mut graph = CallGraph::new();

        let a_pub_func_id = graph.add_node(
            "a_pub_func".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Public,
            (0, 0),
        );
        let a_priv_func_id = graph.add_node(
            "a_priv_func".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Private,
            (0, 0),
        );
        let b_pub_func_id = graph.add_node(
            "b_pub_func".to_string(),
            NodeType::Function,
            Some("ContractB".to_string()),
            Visibility::External,
            (0, 0),
        );
        let b_internal_func_id = graph.add_node(
            "b_internal_func".to_string(),
            NodeType::Function,
            Some("ContractB".to_string()),
            Visibility::Internal,
            (0, 0),
        );
        let c_internal_func_id = graph.add_node(
            "c_internal_func".to_string(),
            NodeType::Function,
            Some("ContractC".to_string()),
            Visibility::Internal,
            (0, 0),
        );
        let _itest_iface_id = graph.add_node(
            "ITest".to_string(),
            NodeType::Interface,
            Some("ITest".to_string()),
            Visibility::Default,
            (0, 0),
        );
        let itest_func_decl_id = graph.add_node(
            "interface_func".to_string(),
            NodeType::Function,
            Some("ITest".to_string()),
            Visibility::External,
            (0, 0),
        );

        let storage_var1_id = graph.add_node(
            "var1".to_string(),
            NodeType::StorageVariable,
            Some("ContractA".to_string()),
            Visibility::Default,
            (0, 0),
        );
        let storage_var2_id = graph.add_node(
            "var2".to_string(),
            NodeType::StorageVariable,
            Some("ContractB".to_string()),
            Visibility::Default,
            (0, 0),
        );
        let storage_var3_id = graph.add_node(
            "var3".to_string(),
            NodeType::StorageVariable,
            Some("ContractC".to_string()),
            Visibility::Default,
            (0, 0),
        );

        graph.add_edge(EdgeParams {
            source_node_id: a_pub_func_id,

            target_node_id: a_priv_func_id,

            edge_type: EdgeType::Call,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 1,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });
        graph.add_edge(EdgeParams {
            source_node_id: a_priv_func_id,

            target_node_id: b_internal_func_id,

            edge_type: EdgeType::Call,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 1,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });
        graph.add_edge(EdgeParams {
            source_node_id: b_pub_func_id,

            target_node_id: b_internal_func_id,

            edge_type: EdgeType::Call,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 1,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });
        graph.add_edge(EdgeParams {
            source_node_id: b_internal_func_id,

            target_node_id: c_internal_func_id,

            edge_type: EdgeType::Call,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 1,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });

        graph.add_edge(EdgeParams {
            source_node_id: a_pub_func_id,

            target_node_id: storage_var1_id,

            edge_type: EdgeType::StorageRead,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 2,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });
        graph.add_edge(EdgeParams {
            source_node_id: a_priv_func_id,

            target_node_id: storage_var1_id,

            edge_type: EdgeType::StorageWrite,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 2,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });
        graph.add_edge(EdgeParams {
            source_node_id: b_internal_func_id,

            target_node_id: storage_var2_id,

            edge_type: EdgeType::StorageRead,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 2,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });
        graph.add_edge(EdgeParams {
            source_node_id: c_internal_func_id,

            target_node_id: storage_var3_id,

            edge_type: EdgeType::StorageWrite,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 1,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });
        graph.add_edge(EdgeParams {
            source_node_id: b_pub_func_id,

            target_node_id: storage_var2_id,

            edge_type: EdgeType::StorageWrite,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 2,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });

        assert_eq!(graph.nodes[a_pub_func_id].name, "a_pub_func");
        assert_eq!(graph.nodes[itest_func_decl_id].name, "interface_func");
        assert_eq!(graph.nodes[storage_var1_id].name, "var1");

        graph
    }

    #[test]
    fn test_analyze_entry_points_no_entry_points() {
        let mut graph = CallGraph::new();
        graph.add_node(
            "internal_func".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Internal,
            (0, 0),
        );
        let analyzer = ReachabilityAnalyzer::new();
        let results = analyzer.analyze_entry_points(
            &graph,
            &|_| true,     // Interested in all nodes for processing
            &|_, _, _| {}, // No-op process
            || (),         // Dummy state
        );
        assert!(
            results.is_empty(),
            "Expected no results for a graph with no public/external entry points"
        );
    }

    #[test]
    fn test_analyze_entry_points_with_cycle() {
        let mut graph = CallGraph::new();
        let func1_id = graph.add_node(
            "func1".to_string(),
            NodeType::Function,
            Some("CycleContract".to_string()),
            Visibility::Public,
            (0, 0),
        );
        let func2_id = graph.add_node(
            "func2".to_string(),
            NodeType::Function,
            Some("CycleContract".to_string()),
            Visibility::Private,
            (0, 0),
        );
        graph.add_edge(EdgeParams {
            source_node_id: func1_id,

            target_node_id: func2_id,

            edge_type: EdgeType::Call,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 1,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });
        graph.add_edge(EdgeParams {
            source_node_id: func2_id,

            target_node_id: func1_id,

            edge_type: EdgeType::Call,

            call_site_span: (0, 0),

            return_site_span: None,

            sequence_number: 1,

            returned_value: None,

            argument_names: None,

            event_name: None,

            declared_return_type: None,
        });

        let analyzer = ReachabilityAnalyzer::new();
        let _processed_nodes: HashSet<NodeId> = HashSet::new();

        let results = analyzer.analyze_entry_points(
            &graph,
            &|node| {
                matches!(
                    node.node_type,
                    NodeType::Function | NodeType::Modifier | NodeType::Constructor
                )
            },
            &|node, state: &mut HashSet<NodeId>, _graph_ref| {
                state.insert(node.id);
            },
            HashSet::new,
        );

        assert_eq!(results.len(), 1, "Expected one entry point result");
        let summary = results.get(&func1_id).unwrap();
        // Both func1 and func2 should have been processed once due to the cycle detection
        // within the scope of a single entry point's traversal.
        let expected_processed: HashSet<NodeId> = [func1_id, func2_id].iter().cloned().collect();
        assert_eq!(
            *summary, expected_processed,
            "Both functions in cycle should be processed once for the entry point"
        );
    }

    #[test]
    fn test_interface_function_declarations_are_not_entry_points() {
        let mut graph = CallGraph::new();

        let iface_node_id = graph.add_node(
            "IMyInterface".to_string(),
            NodeType::Interface,
            Some("IMyInterface".to_string()),
            Visibility::Default,
            (0, 0),
        );
        let iface_func_id = graph.add_node(
            "doSomething".to_string(),
            NodeType::Function,
            Some("IMyInterface".to_string()),
            Visibility::External,
            (0, 0),
        );

        let regular_pub_func_id = graph.add_node(
            "regularPublic".to_string(),
            NodeType::Function,
            Some("MyContract".to_string()),
            Visibility::Public,
            (0, 0),
        );

        let analyzer = ReachabilityAnalyzer::new();
        let results = analyzer.analyze_entry_points(&graph, &|_| true, &|_, _, _| {}, || ());

        assert_eq!(
            results.len(),
            1,
            "Only regularPublic should be an entry point"
        );
        assert!(
            results.contains_key(&regular_pub_func_id),
            "regularPublic should be an entry point"
        );
        assert!(
            !results.contains_key(&iface_func_id),
            "Interface function declaration should not be an entry point"
        );
        assert!(
            !results.contains_key(&iface_node_id),
            "Interface node itself should not be an entry point"
        );
    }
}
