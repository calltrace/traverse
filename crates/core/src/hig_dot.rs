//! DOT format export for Hierarchical Interval Graph
//!
//! This module provides functionality to export a Hierarchical Interval Graph (HIG)
//! to DOT format, which can be visualized using tools like Graphviz.

use std::fmt::{self, Write};

use crate::hig::{EdgeIndex, HierarchicalIntervalGraph, VertexIndex};

/// Trait for converting a value to a DOT label
pub trait ToDotLabel {
    /// Convert the value to a DOT label string
    fn to_dot_label(&self) -> String;
}

/// Default implementation for types that implement Display
impl<T: fmt::Display> ToDotLabel for T {
    fn to_dot_label(&self) -> String {
        self.to_string()
    }
}

/// Trait for converting a value to DOT attributes
pub trait ToDotAttributes {
    /// Convert the value to DOT attributes
    fn to_dot_attributes(&self) -> Vec<(String, String)>;
}

/// Default implementation that provides no attributes
impl ToDotAttributes for &str
{
    fn to_dot_attributes(&self) -> Vec<(String, String)> {
        Vec::new()
    }
}

/// Extension trait for HierarchicalIntervalGraph to export to DOT format
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
    fn to_dot(&self, name: &str) -> String;

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
    fn to_dot(&self, name: &str) -> String {
        self.to_dot_with_formatters(
            name,
            |idx, graph| {
                let mut attrs = Vec::new();
                let vertex = graph.get_vertex(idx).unwrap();

                // Add label with hierarchical ID
                attrs.push(("label".to_string(), format!("\"{}\"", vertex.id())));

                // Add value as tooltip if available
                if let Some(value) = vertex.value() {
                    attrs.push((
                        "tooltip".to_string(),
                        format!("\"{}\"", value.to_dot_label()),
                    ));

                    // Add custom attributes from the value
                    attrs.extend(value.to_dot_attributes());
                }

                attrs
            },
            |idx, graph| {
                let mut attrs = Vec::new();
                let edge = graph.get_edge(idx).unwrap();

                // Add label if edge has a value
                if let Some(value) = edge.value() {
                    attrs.push(("label".to_string(), format!("\"{}\"", value.to_dot_label())));
                }

                // Add metadata as tooltip if available
                if let Some(metadata) = edge.metadata() {
                    attrs.push((
                        "tooltip".to_string(),
                        format!("\"{}\"", metadata.to_dot_label()),
                    ));

                    // Add custom attributes from the metadata
                    attrs.extend(metadata.to_dot_attributes());
                }

                attrs
            },
        )
    }

    fn to_dot_with_formatters<NF, EF>(
        &self,
        name: &str,
        node_formatter: NF,
        edge_formatter: EF,
    ) -> String
    where
        NF: Fn(VertexIndex, &Self) -> Vec<(String, String)>,
        EF: Fn(EdgeIndex, &Self) -> Vec<(String, String)>,
    {
        let mut dot_output = String::new();

        // Start the digraph
        writeln!(&mut dot_output, "digraph {} {{", name).unwrap();

        // Add graph attributes
        writeln!(&mut dot_output, "    // Graph attributes").unwrap();
        writeln!(
            &mut dot_output,
            "    graph [rankdir=LR, fontname=\"Arial\", splines=true];"
        )
        .unwrap();
        writeln!(&mut dot_output, "    node [shape=box, style=\"rounded,filled\", fillcolor=lightblue, fontname=\"Arial\"];").unwrap();
        writeln!(&mut dot_output, "    edge [fontname=\"Arial\"];").unwrap();

        // Add nodes
        writeln!(&mut dot_output, "    // Nodes").unwrap();
        for (idx, _vertex) in self.vertices().enumerate() {
            let attrs = node_formatter(idx, self);
            let attrs_str = attrs
                .iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .collect::<Vec<_>>()
                .join(", ");

            writeln!(&mut dot_output, "    n{} [{}];", idx, attrs_str).unwrap();
        }

        // Add edges
        writeln!(&mut dot_output, "    // Edges").unwrap();
        for (idx, edge) in self.edges().enumerate() {
            let attrs = edge_formatter(idx, self);
            let attrs_str = attrs
                .iter()
                .map(|(k, v)| format!("{}={}", k, v))
                .collect::<Vec<_>>()
                .join(", ");

            writeln!(
                &mut dot_output,
                "    n{} -> n{} [{}];",
                edge.source(),
                edge.target(),
                attrs_str
            )
            .unwrap();
        }

        // End the digraph
        writeln!(&mut dot_output, "}}").unwrap();

        dot_output
    }
}

/// Utility function to escape a string for DOT format
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

        let dot_output = graph.to_dot("TestGraph");

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
        );

        // Validate custom formatting
        assert!(dot_output.contains("n0 [label=\"Node 1\", color=\"red\"];"));
        assert!(dot_output.contains("n0 -> n1 [label=\"Custom Edge\", style=\"dashed\"];"));
    }

    // Custom type with ToDotAttributes implementation
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

        let dot_output = graph.to_dot("AttributeGraph");

        // Validate custom attributes
        assert!(dot_output.contains("color=\"red\""));
        assert!(dot_output.contains("color=\"blue\""));
        assert!(dot_output.contains("style=\"filled\""));
    }
}
