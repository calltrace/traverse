//! JSON format export for Call Graphs.
//!
//! Provides functionality to convert a `CallGraph` into JSON format,
//! suitable for programmatic consumption and tooling integration.

use crate::cg::{CallGraph, Edge, Node};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashSet;

/// Configuration options for JSON export.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct JsonExportConfig {
    /// If true, nodes with no incoming or outgoing edges will be excluded from the output.
    pub exclude_isolated_nodes: bool,
    /// If true, the JSON output will be pretty-printed with indentation.
    pub pretty_print: bool,
    /// If true, additional metadata will be included in the output.
    pub include_metadata: bool,
}

impl Default for JsonExportConfig {
    fn default() -> Self {
        Self {
            exclude_isolated_nodes: false,
            pretty_print: true,
            include_metadata: true,
        }
    }
}

/// Represents the complete graph structure in JSON format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonGraph {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<JsonMetadata>,
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
}

/// Metadata about the graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonMetadata {
    pub node_count: usize,
    pub edge_count: usize,
    pub isolated_node_count: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub generated_at: Option<String>,
}

pub trait CgToJson {
    /// Exports the graph to JSON format using default serialization.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the graph in the JSON output.
    /// * `config` - Configuration options for the export.
    ///
    /// # Returns
    ///
    /// A string containing the JSON representation of the graph.
    fn to_json(&self, name: &str, config: &JsonExportConfig) -> String;

    /// Exports the graph to JSON format as a serde_json::Value with custom formatters.
    ///
    /// # Arguments
    ///
    /// * `name` - The name of the graph.
    /// * `config` - Configuration options for the export.
    /// * `node_formatter` - A closure that takes a `&Node` and returns a custom JSON value.
    /// * `edge_formatter` - A closure that takes an `&Edge` and returns a custom JSON value.
    ///
    /// # Returns
    ///
    /// A serde_json::Value containing the JSON representation of the graph.
    fn to_json_with_formatters<NF, EF>(
        &self,
        name: &str,
        config: &JsonExportConfig,
        node_formatter: NF,
        edge_formatter: EF,
    ) -> Value
    where
        NF: Fn(&Node) -> Value,
        EF: Fn(&Edge) -> Value;
}

impl CgToJson for CallGraph {
    /// Default JSON export using direct serialization of Node and Edge structures.
    fn to_json(&self, name: &str, config: &JsonExportConfig) -> String {
        let json_value = self.to_json_with_formatters(
            name,
            config,
            |node| serde_json::to_value(node).unwrap_or(Value::Null),
            |edge| serde_json::to_value(edge).unwrap_or(Value::Null),
        );

        if config.pretty_print {
            serde_json::to_string_pretty(&json_value).unwrap_or_else(|e| {
                format!("{{\"error\": \"Failed to serialize to JSON: {}\"}}", e)
            })
        } else {
            serde_json::to_string(&json_value).unwrap_or_else(|e| {
                format!("{{\"error\": \"Failed to serialize to JSON: {}\"}}", e)
            })
        }
    }

    /// Generic JSON export allowing full customization via closures.
    fn to_json_with_formatters<NF, EF>(
        &self,
        name: &str,
        config: &JsonExportConfig,
        node_formatter: NF,
        edge_formatter: EF,
    ) -> Value
    where
        NF: Fn(&Node) -> Value,
        EF: Fn(&Edge) -> Value,
    {
        let connected_node_ids: Option<HashSet<usize>> = if config.exclude_isolated_nodes {
            let mut ids = HashSet::new();
            for edge in self.iter_edges() {
                ids.insert(edge.source_node_id);
                ids.insert(edge.target_node_id);
            }
            Some(ids)
        } else {
            None
        };

        let nodes: Vec<Value> = self
            .iter_nodes()
            .filter(|node| {
                if let Some(ref connected_ids) = connected_node_ids {
                    connected_ids.contains(&node.id)
                } else {
                    true
                }
            })
            .map(&node_formatter)
            .collect();

        let edges: Vec<Value> = self.iter_edges().map(&edge_formatter).collect();

        let mut graph = serde_json::json!({
            "name": name,
            "nodes": nodes,
            "edges": edges,
        });

        if config.include_metadata {
            let isolated_count = if config.exclude_isolated_nodes {
                0
            } else {
                let connected_ids = {
                    let mut ids = HashSet::new();
                    for edge in self.iter_edges() {
                        ids.insert(edge.source_node_id);
                        ids.insert(edge.target_node_id);
                    }
                    ids
                };
                self.iter_nodes()
                    .filter(|node| !connected_ids.contains(&node.id))
                    .count()
            };

            let metadata = JsonMetadata {
                node_count: nodes.len(),
                edge_count: edges.len(),
                isolated_node_count: isolated_count,
                generated_at: Some(chrono::Utc::now().to_rfc3339()),
            };

            graph["metadata"] = serde_json::to_value(metadata).unwrap_or(Value::Null);
        }

        graph
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cg::{CallGraph, EdgeParams, EdgeType, NodeType, Visibility};
    use serde_json::json;

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
        graph.add_edge(EdgeParams {
            source_node_id: n1,
            target_node_id: n0,
            edge_type: EdgeType::Call,
            call_site_span: (35, 38),
            return_site_span: None,
            sequence_number: 1,
            returned_value: None,
            argument_names: None,
            event_name: None,
            declared_return_type: None,
        });
        graph
    }

    #[test]
    fn test_default_json_export() {
        let graph = create_test_graph();
        let config = JsonExportConfig::default();
        let json_str = graph.to_json("Test Graph", &config);

        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert_eq!(json["name"], "Test Graph");
        assert!(json["metadata"].is_object());
        assert_eq!(json["metadata"]["node_count"], 2);
        assert_eq!(json["metadata"]["edge_count"], 1);

        assert!(json["nodes"].is_array());
        assert_eq!(json["nodes"].as_array().unwrap().len(), 2);

        assert!(json["edges"].is_array());
        assert_eq!(json["edges"].as_array().unwrap().len(), 1);
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
        let _n2 = graph.add_node(
            "isolated".to_string(),
            NodeType::Function,
            Some("ContractB".to_string()),
            Visibility::Public,
            (50, 60),
        );
        graph.add_edge(EdgeParams {
            source_node_id: n0,
            target_node_id: n1,
            edge_type: EdgeType::Call,
            call_site_span: (15, 18),
            return_site_span: None,
            sequence_number: 1,
            returned_value: None,
            argument_names: None,
            event_name: None,
            declared_return_type: None,
        });

        let config_exclude = JsonExportConfig {
            exclude_isolated_nodes: true,
            pretty_print: false,
            include_metadata: true,
        };
        let json_str = graph.to_json("Test", &config_exclude);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert_eq!(json["nodes"].as_array().unwrap().len(), 2);
        assert_eq!(json["metadata"]["node_count"], 2);
        assert_eq!(json["metadata"]["isolated_node_count"], 0);

        let config_include = JsonExportConfig {
            exclude_isolated_nodes: false,
            pretty_print: false,
            include_metadata: true,
        };
        let json_str = graph.to_json("Test", &config_include);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert_eq!(json["nodes"].as_array().unwrap().len(), 3);
        assert_eq!(json["metadata"]["node_count"], 3);
        assert_eq!(json["metadata"]["isolated_node_count"], 1);
    }

    #[test]
    fn test_custom_formatter_json_export() {
        let graph = create_test_graph();
        let config = JsonExportConfig {
            exclude_isolated_nodes: false,
            pretty_print: false,
            include_metadata: false,
        };

        let json_value = graph.to_json_with_formatters(
            "Custom Test",
            &config,
            |node| {
                json!({
                    "id": node.id,
                    "label": format!("{}.{}",
                        node.contract_name.as_deref().unwrap_or(""),
                        node.name
                    ),
                    "custom_field": "custom_value"
                })
            },
            |edge| {
                json!({
                    "from": edge.source_node_id,
                    "to": edge.target_node_id,
                    "label": "custom_edge"
                })
            },
        );

        assert_eq!(json_value["name"], "Custom Test");
        assert!(json_value["metadata"].is_null());

        let nodes = json_value["nodes"].as_array().unwrap();
        assert_eq!(nodes.len(), 2);
        assert_eq!(nodes[0]["custom_field"], "custom_value");

        let edges = json_value["edges"].as_array().unwrap();
        assert_eq!(edges.len(), 1);
        assert_eq!(edges[0]["label"], "custom_edge");
    }
}
