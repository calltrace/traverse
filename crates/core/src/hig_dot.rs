//! DOT format export for Hierarchical Interval Graph
//!
//! This module provides functionality to export a Hierarchical Interval Graph (HIG)
//! to DOT format, which can be visualized using tools like Graphviz.

use std::fmt::{self, Write};

use crate::hig::{EdgeIndex, HierarchicalIntervalGraph, VertexIndex};

pub trait ToDotLabel {
    fn to_dot_label(&self) -> String;
}

impl<T: fmt::Display> ToDotLabel for T {
    fn to_dot_label(&self) -> String {
        self.to_string()
    }
}

pub trait ToDotAttributes {
    fn to_dot_attributes(&self) -> Vec<(String, String)>;
}

impl ToDotAttributes for &str {
    fn to_dot_attributes(&self) -> Vec<(String, String)> {
        Vec::new()
    }
}

pub trait HigToDot<V, E> {
    /// Export the graph to DOT format
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the graph
    ///
    /// # Returns
    ///
    /// A string containing the DOT representation of the graph
    fn to_dot(&self, name: &str, sort: bool) -> String;

    /// Export the graph to DOT format with custom node and edge formatters
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the graph
    /// * `node_formatter` - A function that formats a vertex index into node attributes
    /// * `edge_formatter` - A function that formats an edge index into edge attributes
    ///
    /// # Returns
    ///
    /// A string containing the DOT representation of the graph
    fn to_dot_with_formatters<NF, EF>(
        &self,
        name: &str,
        node_formatter: NF,
        edge_formatter: EF,
        sort: bool,
    ) -> String
    where
        NF: Fn(VertexIndex, &Self) -> Vec<(String, String)>,
        EF: Fn(EdgeIndex, &Self) -> Vec<(String, String)>;
}

impl<V, E> HigToDot<V, E> for HierarchicalIntervalGraph<V, E>
where
    V: ToDotLabel + ToDotAttributes,
    E: ToDotLabel + ToDotAttributes,
{
    fn to_dot(&self, name: &str, sort: bool) -> String {
        self.to_dot_with_formatters(
            name,
            |idx, graph| {
                let mut attrs = Vec::new();
                let vertex = graph.get_vertex(idx).unwrap();

                attrs.push(("label".to_string(), format!("\"{}\"", vertex.id())));

                if let Some(value) = vertex.value() {
                    attrs.push((
                        "tooltip".to_string(),
                        format!("\"{}\"", value.to_dot_label()),
                    ));

                    attrs.extend(value.to_dot_attributes());
                }

                attrs
            },
            |idx, graph| {
                let mut attrs = Vec::new();
                let edge = graph.get_edge(idx).unwrap();

                if let Some(value) = edge.value() {
                    attrs.push(("label".to_string(), format!("\"{}\"", value.to_dot_label())));
                }

                if let Some(metadata) = edge.metadata() {
                    attrs.push((
                        "tooltip".to_string(),
                        format!("\"{}\"", metadata.to_dot_label()),
                    ));

                    attrs.extend(metadata.to_dot_attributes());
                }

                attrs
            },
            sort,
        )
    }

    fn to_dot_with_formatters<NF, EF>(
        &self,
        name: &str,
        node_formatter: NF,
        edge_formatter: EF,
        sort: bool,
    ) -> String
    where
        NF: Fn(VertexIndex, &Self) -> Vec<(String, String)>,
        EF: Fn(EdgeIndex, &Self) -> Vec<(String, String)>,
    {
        // Estimate capacity to avoid reallocations
        let vertex_count = self.vertices().count();
        let edge_count = self.edges().count();
        let estimated_capacity = 500 + (vertex_count * 100) + (edge_count * 100);
        
        let mut dot_output = String::with_capacity(estimated_capacity);
        
        // Start the digraph
        dot_output.push_str(&format!("digraph {} {{\n", name));
        
        // Add graph attributes
        dot_output.push_str("    // Graph attributes\n");
        dot_output.push_str("    graph [rankdir=LR, fontname=\"Arial\", splines=true];\n");
        dot_output.push_str("    node [shape=box, style=\"rounded,filled\", fillcolor=lightblue, fontname=\"Arial\"];\n");
        dot_output.push_str("    edge [fontname=\"Arial\"];\n");
        
        // Add nodes
        dot_output.push_str("    // Nodes\n");
        for (idx, _vertex) in self.vertices().enumerate() {
            let attrs = node_formatter(idx, self);
            let attrs_str = attrs
                .iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .collect::<Vec<_>>()
                .join(", ");
                
            dot_output.push_str(&format!("    n{} [{}];\n", idx, attrs_str));
        }
        
        // Add edges
        dot_output.push_str("    // Edges\n");
        let edges = if sort {
            if let Ok(edges) = self.topological_sort_edges() {
                edges
                    .into_iter()
                    .map(|e| self.get_edge(e).unwrap())
                    .collect::<Vec<_>>()
            } else {
                self.edges().collect::<Vec<_>>()
            }
        } else {
            self.edges().collect::<Vec<_>>()
        };
        
        for (idx, edge) in edges.iter().enumerate() {
            let attrs = edge_formatter(idx, self);
            let attrs_str = attrs
                .iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .collect::<Vec<_>>()
                .join(", ");
            
            // Remove debug print statement
            dot_output.push_str(&format!(
                "    n{} -> n{} [{}];\n",
                edge.source(),
                edge.target(),
                attrs_str
            ));
        }
        
        // End the digraph
        dot_output.push_str("}\n");
        
        dot_output
    }
}

pub fn escape_dot_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hig::{HierarchicalId, HierarchicalIntervalGraph};

    #[test]
    fn test_simple_graph_to_dot() {
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        graph
            .add_vertex(HierarchicalId::new("1"), Some("Root"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.1"), Some("Child 1"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.2"), Some("Child 2"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.1.1"), Some("Grandchild 1"))
            .unwrap();

        graph
            .add_edge(
                &HierarchicalId::new("1"),
                &HierarchicalId::new("1.1"),
                Some("Edge 1"),
            )
            .unwrap();
        graph
            .add_edge(
                &HierarchicalId::new("1"),
                &HierarchicalId::new("1.2"),
                Some("Edge 2"),
            )
            .unwrap();
        graph
            .add_edge(
                &HierarchicalId::new("1.1"),
                &HierarchicalId::new("1.1.1"),
                Some("Edge 3"),
            )
            .unwrap();

        let dot_output = graph.to_dot("TestGraph", true);

        // Basic validation
        assert!(dot_output.starts_with("digraph TestGraph {"));
        assert!(dot_output.contains("n0 [label=\"1\", tooltip=\"Root\"];"));
        assert!(dot_output.contains("n0 -> n1"));
        assert!(dot_output.ends_with(
            "}
"
        ));
    }

    #[test]
    fn test_custom_formatters() {
        let mut graph = HierarchicalIntervalGraph::<&str, &str>::new();

        graph
            .add_vertex(HierarchicalId::new("1"), Some("Root"))
            .unwrap();
        graph
            .add_vertex(HierarchicalId::new("1.1"), Some("Child 1"))
            .unwrap();

        graph
            .add_edge(
                &HierarchicalId::new("1"),
                &HierarchicalId::new("1.1"),
                Some("Edge 1"),
            )
            .unwrap();

        let dot_output = graph.to_dot_with_formatters(
            "CustomGraph",
            |idx, graph| {
                let vertex = graph.get_vertex(idx).unwrap();
                vec![
                    ("label".to_string(), format!("\"Node {}\"", vertex.id())),
                    ("color".to_string(), "\"red\"".to_string()),
                ]
            },
            |idx, graph| {
                let edge = graph.get_edge(idx).unwrap();
                vec![
                    ("label".to_string(), "\"Custom Edge\"".to_string()),
                    ("style".to_string(), "\"dashed\"".to_string()),
                ]
            },
            true,
        );

        assert!(dot_output.contains("n0 [label=\"Node 1\", color=\"red\"];"));
        assert!(dot_output.contains("n0 -> n1 [label=\"Custom Edge\", style=\"dashed\"];"));
    }

    #[derive(Debug, Clone)]
    struct CustomNode {
        name: String,
        color: String,
    }

    impl ToDotLabel for CustomNode {
        fn to_dot_label(&self) -> String {
            self.name.clone()
        }
    }

    impl ToDotAttributes for CustomNode {
        fn to_dot_attributes(&self) -> Vec<(String, String)> {
            vec![
                ("color".to_string(), format!("\"{}\"", self.color)),
                ("style".to_string(), "\"filled\"".to_string()),
            ]
        }
    }

    #[test]
    fn test_custom_attributes() {
        let mut graph = HierarchicalIntervalGraph::<CustomNode, &str>::new();

        graph
            .add_vertex(
                HierarchicalId::new("1"),
                Some(CustomNode {
                    name: "Root".to_string(),
                    color: "red".to_string(),
                }),
            )
            .unwrap();

        graph
            .add_vertex(
                HierarchicalId::new("1.1"),
                Some(CustomNode {
                    name: "Child".to_string(),
                    color: "blue".to_string(),
                }),
            )
            .unwrap();

        graph
            .add_edge(
                &HierarchicalId::new("1"),
                &HierarchicalId::new("1.1"),
                Some("Edge"),
            )
            .unwrap();

        let dot_output = graph.to_dot("AttributeGraph", true);

        // Validate custom attributes
        assert!(dot_output.contains("color=\"red\""));
        assert!(dot_output.contains("color=\"blue\""));
        assert!(dot_output.contains("style=\"filled\""));
    }
}
