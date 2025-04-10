//! DOT (Graphviz) format export for Call Graphs.
//!
//! Provides functionality to convert a `CallGraph` into a DOT language string,
//! suitable for visualization with tools like Graphviz. Includes default formatting
//! and allows customization via closures.

use crate::cg::{CallGraph, Edge, EdgeType, Node, NodeType};
use std::fmt::Write;

pub trait ToDotLabel {
    fn to_dot_label(&self) -> String;
}

pub trait ToDotAttributes {
    fn to_dot_attributes(&self) -> Vec<(String, String)>;
}

impl ToDotLabel for &str {
    fn to_dot_label(&self) -> String {
        escape_dot_string(self)
    }
}

impl ToDotLabel for String {
    fn to_dot_label(&self) -> String {
        escape_dot_string(self)
    }
}

impl<T> ToDotAttributes for T {
    fn to_dot_attributes(&self) -> Vec<(String, String)> {
        Vec::new()
    }
}

pub trait CgToDot {
    /// Exports the graph to DOT format using default label/attribute generation.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the graph in the DOT output.
    ///
    /// # Returns
    ///
    /// A string containing the DOT representation of the graph.
    fn to_dot(&self, name: &str) -> String;

    /// Exports the graph to DOT format with custom node and edge formatters.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the graph.
    /// * `node_formatter` - A closure that takes a `&Node` and returns its DOT attributes.
    /// * `edge_formatter` - A closure that takes an `&Edge` and returns its DOT attributes.
    ///
    /// # Returns
    ///
    /// A string containing the DOT representation of the graph.
    fn to_dot_with_formatters<NF, EF>(&self, name: &str, node_formatter: NF, edge_formatter: EF)
        -> String
    where
        NF: Fn(&Node) -> Vec<(String, String)>,
        EF: Fn(&Edge) -> Vec<(String, String)>;
}

impl CgToDot for CallGraph {
    /// Default DOT export using `ToDotLabel` and `ToDotAttributes` implementations
    /// for `Node` and `Edge`.
    fn to_dot(&self, name: &str) -> String {
        self.to_dot_with_formatters(
            name,

            |node| {
                let mut attrs = vec![
                    ("label".to_string(), escape_dot_string(&node.to_dot_label())),
                    (
                        "tooltip".to_string(),
                        escape_dot_string(&format!(
                            "Type: {:?}\\nVisibility: {:?}\\nSpan: {:?}",
                            node.node_type, node.visibility, node.span
                        )),
                    ),
                    (
                        "fillcolor".to_string(),
                        match node.node_type {
                            NodeType::Function => "lightblue".to_string(),
                            NodeType::Constructor => "lightgoldenrodyellow".to_string(),
                            NodeType::Modifier => "lightcoral".to_string(),
                        },
                    ),
                ];
                attrs.extend(node.to_dot_attributes());
                attrs
            },
            |edge| {
                let mut attrs = Vec::new();
                match edge.edge_type {
                    EdgeType::Call => {
                        attrs.push((
                            "tooltip".to_string(),
                            escape_dot_string(&format!(
                                "Call Site Span: {:?}\\nSequence: {}",
                                edge.call_site_span, edge.sequence_number
                            )),
                        ));
                        // Use sequence number as the label for call edges
                        attrs.push(("label".to_string(), edge.sequence_number.to_string()));
                    }
                    EdgeType::Return => {
                        attrs.push((
                            "tooltip".to_string(),
                            escape_dot_string(&format!(
                                "Return from function defined at {:?}\\nReturn Statement Span: {:?}\\nSequence: {}", // Added sequence to tooltip
                                edge.call_site_span, // Span of the function definition
                                edge.return_site_span.unwrap_or((0, 0)), // Span of the return statement
                                edge.sequence_number // Sequence number (matches call)
                            )),
                        ));
                        // Add returned value to tooltip if present
                        if let Some(ref value) = edge.returned_value {
                             if let Some((_, tooltip_val)) = attrs.last_mut() { // Find the tooltip attribute
                                write!(tooltip_val, "\\nReturns: {}", escape_dot_string(value)).unwrap(); // Append to existing tooltip
                             }
                        }
                        // Set label based on returned value
                        let label = match &edge.returned_value {
                            Some(value) => format!("ret {}", escape_dot_string(value)),
                            None => "ret".to_string(),
                        };
                        attrs.push(("label".to_string(), label.clone()));
                        // Style return edges differently
                        attrs.push(("style".to_string(), "dashed".to_string()));
                        attrs.push(("color".to_string(), "grey".to_string()));
                        attrs.push(("arrowhead".to_string(), "empty".to_string()));

                    }
                }
                // Allow overriding attributes from Edge::to_dot_attributes if needed
                attrs.extend(edge.to_dot_attributes());
                attrs
            },
        )
    }

    /// Generic DOT export allowing full customization via closures.
    fn to_dot_with_formatters<NF, EF>(
        &self,
        name: &str,
        node_formatter: NF,
        edge_formatter: EF,
    ) -> String
    where
        NF: Fn(&Node) -> Vec<(String, String)>,
        EF: Fn(&Edge) -> Vec<(String, String)>,
    {
        let mut dot_output = String::new();
        let _ = writeln!(dot_output, "digraph \"{}\" {{", escape_dot_string(name));


        let _ = writeln!(
            dot_output,
            "    graph [rankdir=LR, fontname=\"Arial\", splines=true];"
        );
        let _ = writeln!(
            dot_output,
            "    node [shape=box, style=\"rounded,filled\", fontname=\"Arial\"];"
        );
        let _ = writeln!(dot_output, "    edge [fontname=\"Arial\"];");
        let _ = writeln!(dot_output);

        for node in self.iter_nodes() {
            let attrs = node_formatter(node);
            let attrs_str = attrs
                .iter()
                .map(|(k, v)| format!("{}=\"{}\"", k, v))
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(dot_output, "    n{} [{}];", node.id, attrs_str);
        }
        let _ = writeln!(dot_output);

        for edge in self.iter_edges() {
            let attrs = edge_formatter(edge);
            let attrs_str = attrs
                .iter()
                .map(|(k, v)| format!("{}=\"{}\"", k, v))
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(
                dot_output,
                "    n{} -> n{} [{}];",
                edge.source_node_id, edge.target_node_id, attrs_str
            );
        }

        let _ = writeln!(dot_output, "}}");
        dot_output
    }
}

/// Escapes characters in a string to be valid within a DOT label, tooltip, or attribute value.
/// Ensures the output is enclosed in double quotes if needed (e.g., for labels/tooltips).
pub fn escape_dot_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "")
        .replace('\t', "\\t")
        .replace('{', "\\{")
        .replace('}', "\\}")
        .replace('<', "\\<")
        .replace('>', "\\>")
}

// --- Tests for cg_dot ---
#[cfg(test)]
mod tests {
    use super::*;
    use crate::cg::{CallGraph, NodeType, Visibility};

    fn create_test_graph() -> CallGraph {
        let mut graph = CallGraph::new();
        let n0 = graph.add_node(
            "foo".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Public,
            (10, 20),
        );
        let n1 = graph.add_node(
            "bar".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Private,
            (30, 40),
        );
        // Updated add_edge call to match the new signature
        graph.add_edge(
            n1,
            n0,
            EdgeType::Call, // edge_type
            (35, 38),       // call_site_span
            None,           // return_site_span
            1,              // sequence_number
            None,           // returned_value
        );
        graph
    }

    #[test]
    fn test_default_dot_export() {
        let graph = create_test_graph();
        let dot = graph.to_dot("TestDefault");

        assert!(dot.starts_with("digraph \"TestDefault\" {"));
        assert!(dot.contains("n0"), "Node n0 definition missing");
        assert!(dot.contains("label=\"ContractA.foo\\n(function)\""), "Node n0 label incorrect");
        assert!(dot.contains("tooltip=\"Type: Function\\\\nVisibility: Public\\\\nSpan: (10, 20)\""), "Node n0 tooltip incorrect");
        assert!(dot.contains("fillcolor=\"lightblue\""), "Node n0 fillcolor incorrect");
        assert!(dot.contains("n1"), "Node n1 definition missing");
        assert!(dot.contains("label=\"ContractA.bar\\n(function)\""), "Node n1 label incorrect");
        assert!(dot.contains("tooltip=\"Type: Function\\\\nVisibility: Private\\\\nSpan: (30, 40)\""), "Node n1 tooltip incorrect");
        assert!(dot.contains("fillcolor=\"lightblue\""), "Node n1 fillcolor incorrect");
        assert!(dot.contains("n1 -> n0"), "Edge n1 -> n0 missing");
        assert!(dot.contains("tooltip=\"Call Site Span: (35, 38)\\\\nSequence: 1\""), "Edge tooltip incorrect or missing sequence");
        assert!(dot.ends_with("}\n"));
    }

    #[test]
    fn test_custom_formatter_dot_export() {
        let graph = create_test_graph();
        let dot = graph.to_dot_with_formatters(
            "TestCustom",
            |node| {
                vec![
                    ("label".to_string(), format!("N_{}", node.name)),
                    ("color".to_string(), escape_dot_string("\"green\"")),
                ]
            },
            |_edge| { // Prefix edge with underscore as it's unused in this custom formatter
                vec![
                    ("label".to_string(), escape_dot_string("\"CustomCall\"")),
                    ("style".to_string(), escape_dot_string("\"dashed\"")),
                ]
            },
        );

        assert!(dot.starts_with("digraph \"TestCustom\" {"));
        assert!(dot.contains("n0"), "Node n0 definition missing");
        assert!(dot.contains("label=\"N_foo\""), "Node n0 label incorrect");
        assert!(dot.contains("color=\"\\\"green\\\"\""), "Node n0 color incorrect");

        assert!(dot.contains("n1"), "Node n1 definition missing");
        assert!(dot.contains("label=\"N_bar\""), "Node n1 label incorrect");
        assert!(dot.contains("color=\"\\\"green\\\"\""), "Node n1 color incorrect");

        assert!(dot.contains("n1 -> n0"), "Edge n1 -> n0 missing");
        assert!(dot.contains("label=\"\\\"CustomCall\\\"\""), "Edge label incorrect");
        assert!(dot.contains("style=\"\\\"dashed\\\"\""), "Edge style incorrect");
        assert!(dot.ends_with("}\n"));
    }

     #[test]
    fn test_dot_escape_string_internal() {
        assert_eq!(escape_dot_string(""), "");
        assert_eq!(escape_dot_string("simple"), "simple");
        assert_eq!(escape_dot_string("with \"quotes\""), "with \\\"quotes\\\"");
        assert_eq!(escape_dot_string("new\nline"), "new\\nline");
        assert_eq!(escape_dot_string("back\\slash"), "back\\\\slash");
        assert_eq!(escape_dot_string("<html>"), "\\<html\\>");
        assert_eq!(escape_dot_string("{record}"), "\\{record\\}");
    }
}

