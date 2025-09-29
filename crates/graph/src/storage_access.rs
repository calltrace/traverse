//! # Storage Access Analysis Module
//!
//! This module provides functionality to analyze a `CallGraph` and determine
//! which storage variables are read or written by each public or external
//! entry point function within the analyzed contracts.
//!
//! ## Core Functionality
//!
//! The main entry point is the `analyze_storage_access` function. It performs
//! the following steps:
//!
//! 1.  **Identifies Entry Points**: It uses `ReachabilityAnalyzer` to find all
//!     functions marked as `public` or `external` that are not part of an
//!     interface definition. These serve as the starting points for the analysis.
//!
//! 2.  **Call Graph Traversal**: For each identified entry point, it traverses
//!     the call graph using a Depth-First Search (DFS) approach, following
//!     `Call` edges.
//!
//! 3.  **Storage Interaction Processing**: When a function, modifier, or
//!     constructor node is visited during the DFS traversal, this module
//!     inspects its direct outgoing edges. If these edges are of type
//!     `StorageRead` or `StorageWrite` and target a `StorageVariable` node,
//!     the ID of the storage variable is recorded.
//!
//! 4.  **Aggregation**: The read and write accesses are aggregated into a
//!     `StorageAccessSummary` for each entry point. This summary contains
//!     two `HashSet`s: one for the NodeIds of storage variables read, and
//!     one for those written.
//!
//! ## Output
//!
//! The `analyze_storage_access` function returns a `HashMap` where the keys are
//! the `NodeId`s of the entry point functions, and the values are their
//! corresponding `StorageAccessSummary` objects.
//!
//! This information is crucial for understanding the storage footprint and
//! side effects of different functions in a smart contract system.
//!
use crate::cg::{CallGraph, EdgeType, Node, NodeType};
use crate::reachability::{NodeId, ReachabilityAnalyzer};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct StorageAccessSummary {
    pub reads: HashSet<NodeId>,
    pub writes: HashSet<NodeId>,
}

pub fn analyze_storage_access(graph: &CallGraph) -> HashMap<NodeId, StorageAccessSummary> {
    let analyzer = ReachabilityAnalyzer::new();

    // For storage analysis, every function/modifier/constructor encountered in the
    // call tree is "of interest" because we need to check its direct storage interactions.
    let is_function_like_node = |node: &Node| -> bool {
        matches!(
            node.node_type,
            NodeType::Function | NodeType::Modifier | NodeType::Constructor
        )
    };

    // This function is called for each `func_node` (a function, modifier, or constructor)
    // that is visited during the DFS traversal of the call graph.
    // It inspects `func_node`'s direct outgoing edges to find StorageRead/StorageWrite
    // interactions and updates the `StorageAccessSummary` state.
    let process_function_for_storage_interactions =
        |func_node: &Node, // The function/modifier/constructor currently being processed
         state: &mut StorageAccessSummary,
         graph: &CallGraph| {
            for edge in &graph.edges {
                // Check if the edge originates from the current function-like node
                if edge.source_node_id == func_node.id {
                    // Check if the target of the edge is a storage variable
                    if let Some(target_node) = graph.nodes.get(edge.target_node_id) {
                        if target_node.node_type == NodeType::StorageVariable {
                            match edge.edge_type {
                                EdgeType::StorageRead => {
                                    state.reads.insert(target_node.id);
                                }
                                EdgeType::StorageWrite => {
                                    state.writes.insert(target_node.id);
                                }
                                _ => {} // Other edge types to storage vars are not relevant for this summary
                            }
                        }
                    }
                }
            }
        };

    analyzer.analyze_entry_points(
        graph,
        &is_function_like_node,
        &process_function_for_storage_interactions,
        StorageAccessSummary::default,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::reachability::tests::create_test_graph_for_reachability;
    use std::collections::HashSet;

    #[test]
    fn test_analyze_storage_access_basic() {
        let graph = create_test_graph_for_reachability();
        let results = analyze_storage_access(&graph);

        assert_eq!(
            results.len(),
            2,
            "Expected 2 entry points (a_pub_func, b_pub_func)"
        );

        let a_pub_func_id = graph
            .iter_nodes()
            .find(|n| n.name == "a_pub_func")
            .unwrap()
            .id;
        let b_pub_func_id = graph
            .iter_nodes()
            .find(|n| n.name == "b_pub_func")
            .unwrap()
            .id;

        let storage_var1_id = graph.iter_nodes().find(|n| n.name == "var1").unwrap().id;
        let storage_var2_id = graph.iter_nodes().find(|n| n.name == "var2").unwrap().id;
        let storage_var3_id = graph.iter_nodes().find(|n| n.name == "var3").unwrap().id;

        let summary_a = results
            .get(&a_pub_func_id)
            .expect("Summary for a_pub_func missing");
        let expected_reads_a: HashSet<NodeId> =
            [storage_var1_id, storage_var2_id].iter().cloned().collect();
        let expected_writes_a: HashSet<NodeId> =
            [storage_var1_id, storage_var3_id].iter().cloned().collect();
        assert_eq!(
            summary_a.reads, expected_reads_a,
            "Mismatch in reads for a_pub_func"
        );
        assert_eq!(
            summary_a.writes, expected_writes_a,
            "Mismatch in writes for a_pub_func"
        );

        let summary_b = results
            .get(&b_pub_func_id)
            .expect("Summary for b_pub_func missing");
        let expected_reads_b: HashSet<NodeId> = [storage_var2_id].iter().cloned().collect();
        let expected_writes_b: HashSet<NodeId> =
            [storage_var2_id, storage_var3_id].iter().cloned().collect();
        assert_eq!(
            summary_b.reads, expected_reads_b,
            "Mismatch in reads for b_pub_func"
        );
        assert_eq!(
            summary_b.writes, expected_writes_b,
            "Mismatch in writes for b_pub_func"
        );
    }
}
