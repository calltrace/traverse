//! DOT (Graphviz) format export for Call Graphs.
//!
//! Provides functionality to convert a `CallGraph` into a DOT language string,
//! suitable for visualization with tools like Graphviz. Includes default formatting
//! and allows customization via closures.

use crate::cg::{CallGraph, Edge, EdgeType, Node, NodeType};
use std::collections::HashSet; // Import HashSet
use std::fmt::Write;

/// Configuration options for DOT export.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DotExportConfig {
    /// If true, nodes with no incoming or outgoing edges will be excluded from the output.
    pub exclude_isolated_nodes: bool,
}

impl Default for DotExportConfig {
    fn default() -> Self {
        Self {
            exclude_isolated_nodes: false, // Default is to include isolated nodes
        }
    }
}

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
    fn to_dot(&self, name: &str, config: &DotExportConfig) -> String;

    /// Exports the graph to DOT format with custom node and edge formatters.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the graph.
    /// * `config` - Configuration options for the export.
    /// * `node_formatter` - A closure that takes a `&Node` and returns its DOT attributes.
    /// * `edge_formatter` - A closure that takes an `&Edge` and returns its DOT attributes.
    ///
    /// # Returns
    ///
    /// A string containing the DOT representation of the graph.
    fn to_dot_with_formatters<NF, EF>(
        &self,
        name: &str,
        config: &DotExportConfig,
        node_formatter: NF,
        edge_formatter: EF,
    ) -> String
    where
        NF: Fn(&Node) -> Vec<(String, String)>,
        EF: Fn(&Edge) -> Vec<(String, String)>;
}

impl CgToDot for CallGraph {
    /// Default DOT export using `ToDotLabel` and `ToDotAttributes` implementations
    /// for `Node` and `Edge`.
    fn to_dot(&self, name: &str, config: &DotExportConfig) -> String {
        self.to_dot_with_formatters(
            name,
            config, // Pass config

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
                            NodeType::Library => "lightgrey".to_string(), // Keep lightgrey for Library
                            NodeType::Interface => "lightpink".to_string(), // Use lightpink for Interface
                            NodeType::StorageVariable => "khaki".to_string(), // Added color for StorageVariable
                            NodeType::Evm => "gray".to_string(), // Added color for EVM
                            NodeType::EventListener => "lightcyan".to_string(), // Added color for EventListener
                            NodeType::RequireCondition => "orange".to_string(), // Added color for RequireCondition
                            NodeType::IfStatement => "mediumpurple1".to_string(), // Added color for IfStatement
                            NodeType::ThenBlock => "palegreen".to_string(),    // Added color for ThenBlock
                            NodeType::ElseBlock => "lightsalmon".to_string(),  // Added color for ElseBlock
                            NodeType::WhileStatement => "lightsteelblue".to_string(), // Added color for WhileStatement
                            NodeType::WhileBlock => "lightseagreen".to_string(), // Added color for WhileBlock
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
                        // Construct base tooltip
                        let mut tooltip = format!("Call Site Span: {:?}", edge.call_site_span);

                        // Construct the argument string
                        let args_str = edge.argument_names.as_ref()
                            .map(|args| {
                                if args.is_empty() {
                                    "".to_string() // No arguments, empty string inside parens
                                } else {
                                    // Escape each argument individually before joining
                                    args.iter().map(|arg| escape_dot_string(arg)).collect::<Vec<_>>().join(", ")
                                }
                            })
                            .unwrap_or_default(); // Default to empty string if None

                        // Construct label and potentially add event info to tooltip
                        let raw_label = if let Some(event_name) = &edge.event_name {
                            // It's an emit-related edge
                            write!(tooltip, "\\nEvent: {}", escape_dot_string(event_name)).unwrap();
                            format!("emit {}({})\nSeq: {}", escape_dot_string(event_name), args_str, edge.sequence_number)
                        } else {
                            // It's a regular function call
                            format!("({})\n{}", args_str, edge.sequence_number)
                        };

                        // Add sequence number to tooltip regardless
                        write!(tooltip, "\\nSequence: {}", edge.sequence_number).unwrap();

                        // Add final tooltip and label attributes
                        attrs.push(("tooltip".to_string(), escape_dot_string(&tooltip)));
                        attrs.push(("label".to_string(), escape_dot_string(&raw_label)));

                        // Style emit edges differently? (Optional)
                        if edge.event_name.is_some() {
                            attrs.push(("color".to_string(), "blue".to_string()));
                            attrs.push(("fontcolor".to_string(), "blue".to_string()));
                        }
                    }
                    EdgeType::Return => {
                        eprintln!("[DEBUG cg_dot] Formatting Return edge: {} -> {}", edge.source_node_id, edge.target_node_id);
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
                    EdgeType::StorageRead => {
                        let tooltip = format!("Read Span: {:?}", edge.call_site_span);
                        attrs.push(("label".to_string(), "read".to_string()));
                        attrs.push(("tooltip".to_string(), escape_dot_string(&tooltip)));
                        attrs.push(("color".to_string(), "darkgreen".to_string()));
                        attrs.push(("fontcolor".to_string(), "darkgreen".to_string()));
                        attrs.push(("style".to_string(), "dotted".to_string()));
                    }
                    EdgeType::StorageWrite => {
                        let tooltip = format!("Write Span: {:?}", edge.call_site_span);
                        attrs.push(("label".to_string(), "write".to_string()));
                        attrs.push(("tooltip".to_string(), escape_dot_string(&tooltip)));
                        attrs.push(("color".to_string(), "darkred".to_string()));
                        attrs.push(("fontcolor".to_string(), "darkred".to_string()));
                        attrs.push(("style".to_string(), "bold".to_string()));
                    }
                    EdgeType::Require => {
                        eprintln!("[DOT Require DEBUG] Formatting Require edge: {} -> {}", edge.source_node_id, edge.target_node_id);
                        let tooltip = format!("Require Check Span: {:?}", edge.call_site_span);
                        let args_str = edge.argument_names.as_ref()
                            .map(|args| {
                                if args.is_empty() {
                                    "".to_string()
                                } else {
                                    args.iter().map(|arg| escape_dot_string(arg)).collect::<Vec<_>>().join(", ")
                                }
                            })
                            .unwrap_or_default();
                        let label = format!("require({})", args_str);
                        attrs.push(("label".to_string(), escape_dot_string(&label)));
                        attrs.push(("tooltip".to_string(), escape_dot_string(&tooltip)));
                        attrs.push(("color".to_string(), "orange".to_string()));
                        attrs.push(("fontcolor".to_string(), "orange".to_string()));
                        attrs.push(("style".to_string(), "dashed".to_string()));
                    }
                    EdgeType::IfConditionBranch => {
                        let condition = edge.argument_names.as_ref()
                            .and_then(|args| args.first())
                            .map(|arg| escape_dot_string(arg))
                            .unwrap_or_else(|| "condition".to_string());
                        let tooltip = format!("If Condition: {}\\nSpan: {:?}", condition, edge.call_site_span);
                        attrs.push(("label".to_string(), format!("if ({})", condition)));
                        attrs.push(("tooltip".to_string(), escape_dot_string(&tooltip)));
                        attrs.push(("color".to_string(), "mediumpurple4".to_string()));
                        attrs.push(("fontcolor".to_string(), "mediumpurple4".to_string()));
                    }
                    EdgeType::ThenBranch => {
                        let tooltip = format!("Then branch taken\\nSpan: {:?}", edge.call_site_span);
                        attrs.push(("label".to_string(), "then".to_string()));
                        attrs.push(("tooltip".to_string(), escape_dot_string(&tooltip)));
                        attrs.push(("color".to_string(), "green4".to_string()));
                        attrs.push(("fontcolor".to_string(), "green4".to_string()));
                    }
                    EdgeType::ElseBranch => {
                        let tooltip = format!("Else branch taken\\nSpan: {:?}", edge.call_site_span);
                        attrs.push(("label".to_string(), "else".to_string()));
                        attrs.push(("tooltip".to_string(), escape_dot_string(&tooltip)));
                        attrs.push(("color".to_string(), "salmon4".to_string()));
                        attrs.push(("fontcolor".to_string(), "salmon4".to_string()));
                    }
                    EdgeType::WhileConditionBranch | EdgeType::WhileBodyBranch => {
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
        config: &DotExportConfig, // Add config parameter
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

        // --- Node Filtering Logic ---
        let connected_node_ids: Option<HashSet<usize>> = if config.exclude_isolated_nodes {
            let mut ids = HashSet::new();
            for edge in self.iter_edges() {
                ids.insert(edge.source_node_id);
                ids.insert(edge.target_node_id);
            }
            Some(ids)
        } else {
            None // No filtering needed
        };

        if config.exclude_isolated_nodes {
            if let Some(ref connected_ids) = connected_node_ids {
                eprintln!("[DEBUG cg_dot Filter] Connected Node IDs: {:?}", connected_ids);
            } else {
                 eprintln!("[DEBUG cg_dot Filter] Filtering active, but connected_node_ids is None (unexpected).");
            }
        }

        for node in self.iter_nodes() {
            // --- Apply Filtering ---
            let mut is_isolated = false; // DEBUG flag
            if let Some(ref connected_ids) = connected_node_ids {
                if !connected_ids.contains(&node.id) {
                     is_isolated = true; // DEBUG Mark as isolated
                    eprintln!(
                        "[DEBUG cg_dot Filter] Skipping isolated node: ID={}, Name='{}', Contract='{:?}'",
                        node.id, node.name, node.contract_name
                    );
                    continue; // Skip isolated node
                }
            }

            if config.exclude_isolated_nodes { // Only log if filtering is active
                 eprintln!(
                     "[DEBUG cg_dot Filter] Including {}node: ID={}, Name='{}', Contract='{:?}'",
                     if is_isolated { "ISOLATED (ERROR?) " } else { "" }, // Highlight if it was marked isolated but not skipped
                     node.id, node.name, node.contract_name
                 );
            }


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
            None,           // argument_names
            None,
        );
        graph
    }

    #[test]
    fn test_default_dot_export() {
        let graph = create_test_graph();
        let config = DotExportConfig::default(); // Use default config
        let dot = graph.to_dot("TestDefault", &config); // Pass config
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
        // Check tooltip format (Call Site Span + Sequence)
        assert!(dot.contains("tooltip=\"Call Site Span: (35, 38)\\\\nSequence: 1\""), "Edge tooltip incorrect"); // Updated to include sequence
        // Check label format (Args + Sequence) - No args in this test case
        assert!(dot.contains("label=\"()\\n1\""), "Edge label incorrect or missing sequence"); // Label still includes sequence
        assert!(dot.ends_with("}\n"));
    }

    #[test]
    fn test_custom_formatter_dot_export() {
        let graph = create_test_graph();
        let config = DotExportConfig::default(); // Use default config
        let dot = graph.to_dot_with_formatters(
            "TestCustom",
            &config, // Pass config
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

    #[test]
    fn test_exclude_isolated_nodes() {
        let mut graph = CallGraph::new();
        let n0 = graph.add_node(
            "connected1".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Public,
            (10, 20),
        );
        let n1 = graph.add_node(
            "connected2".to_string(),
            NodeType::Function,
            Some("ContractA".to_string()),
            Visibility::Private,
            (30, 40),
        );
        let n2 = graph.add_node(
            "isolated".to_string(),
            NodeType::Function,
            Some("ContractB".to_string()),
            Visibility::Public,
            (50, 60),
        );
        // Add an edge to connect n0 and n1
        graph.add_edge(
            n0, n1, EdgeType::Call, (15, 18), None, 1, None, None, None,
        );

        // Test 1: Exclude isolated nodes
        let config_exclude = DotExportConfig {
            exclude_isolated_nodes: true,
        };
        let dot_excluded = graph.to_dot("TestExcludeIsolated", &config_exclude);

        assert!(dot_excluded.contains("n0"), "Node n0 (connected) should be present when excluding");
        assert!(dot_excluded.contains("n1"), "Node n1 (connected) should be present when excluding");
        assert!(!dot_excluded.contains("n2"), "Node n2 (isolated) should NOT be present when excluding");
        assert!(dot_excluded.contains("n0 -> n1"), "Edge n0 -> n1 should be present when excluding");

        // Test 2: Include isolated nodes (default)
        let config_include = DotExportConfig {
            exclude_isolated_nodes: false, // Or use DotExportConfig::default()
        };
        let dot_included = graph.to_dot("TestIncludeIsolated", &config_include);

        assert!(dot_included.contains("n0"), "Node n0 (connected) should be present when including");
        assert!(dot_included.contains("n1"), "Node n1 (connected) should be present when including");
        assert!(dot_included.contains("n2"), "Node n2 (isolated) should be present when including");
        assert!(dot_included.contains("n0 -> n1"), "Edge n0 -> n1 should be present when including");
    }
}

