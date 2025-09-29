//! Comprehensive unit tests for JSON serialization functionality

#[cfg(test)]
mod tests {
    use crate::cg::{CallGraph, Edge, EdgeType, Node, NodeType, Visibility, EdgeParams};
    use crate::cg_json::{CgToJson, JsonExportConfig};
    use serde_json::{json, Value};
    use std::collections::HashSet;

    /// Create a simple test graph
    fn create_simple_graph() -> CallGraph {
        let mut graph = CallGraph::new();

        let n0 = graph.add_node(
            "constructor".to_string(),
            NodeType::Constructor,
            Some("SimpleContract".to_string()),
            Visibility::Public,
            (0, 50),
        );

        let n1 = graph.add_node(
            "setValue".to_string(),
            NodeType::Function,
            Some("SimpleContract".to_string()),
            Visibility::Public,
            (60, 120),
        );

        let n2 = graph.add_node(
            "_internal".to_string(),
            NodeType::Function,
            Some("SimpleContract".to_string()),
            Visibility::Internal,
            (130, 180),
        );

        graph.add_edge(EdgeParams {


            source_node_id: n0,


            target_node_id: n1,


            edge_type: EdgeType::Call,


            call_site_span: (45, 48),


            return_site_span: None,


            sequence_number: 1,


            returned_value: None,


            argument_names: Some(vec!["42".to_string()]),


            event_name: None,


            declared_return_type: None,


        });

        graph.add_edge(EdgeParams {


            source_node_id: n1,


            target_node_id: n2,


            edge_type: EdgeType::Call,


            call_site_span: (100, 110),


            return_site_span: None,


            sequence_number: 1,


            returned_value: None,


            argument_names: None,


            event_name: None,


            declared_return_type: None,


        });

        graph
    }

    /// Create a complex graph with various node and edge types
    fn create_complex_graph() -> CallGraph {
        let mut graph = CallGraph::new();

        let storage = graph.add_node(
            "balance".to_string(),
            NodeType::StorageVariable,
            Some("ComplexContract".to_string()),
            Visibility::Private,
            (0, 20),
        );

        let func = graph.add_node(
            "transfer".to_string(),
            NodeType::Function,
            Some("ComplexContract".to_string()),
            Visibility::External,
            (30, 100),
        );

        let modifier = graph.add_node(
            "onlyOwner".to_string(),
            NodeType::Modifier,
            Some("ComplexContract".to_string()),
            Visibility::Private,
            (110, 150),
        );

        let interface = graph.add_node(
            "IERC20".to_string(),
            NodeType::Interface,
            None,
            Visibility::Default,
            (160, 200),
        );

        let library = graph.add_node(
            "SafeMath".to_string(),
            NodeType::Library,
            None,
            Visibility::Default,
            (210, 250),
        );

        let event_listener = graph.add_node(
            "EventListener".to_string(),
            NodeType::EventListener,
            None,
            Visibility::Default,
            (260, 280),
        );

        let _isolated = graph.add_node(
            "isolatedFunction".to_string(),
            NodeType::Function,
            Some("ComplexContract".to_string()),
            Visibility::Private,
            (290, 320),
        );

        graph.add_edge(EdgeParams {


            source_node_id: func,


            target_node_id: storage,


            edge_type: EdgeType::StorageRead,


            call_site_span: (40, 45),


            return_site_span: None,


            sequence_number: 1,


            returned_value: None,


            argument_names: None,


            event_name: None,


            declared_return_type: None,


        });

        graph.add_edge(EdgeParams {


            source_node_id: func,


            target_node_id: storage,


            edge_type: EdgeType::StorageWrite,


            call_site_span: (50, 55),


            return_site_span: None,


            sequence_number: 2,


            returned_value: None,


            argument_names: None,


            event_name: None,


            declared_return_type: None,


        });

        graph.add_edge(EdgeParams {


            source_node_id: func,


            target_node_id: modifier,


            edge_type: EdgeType::Call,


            call_site_span: (35, 38),


            return_site_span: None,


            sequence_number: 3,


            returned_value: None,


            argument_names: None,


            event_name: None,


            declared_return_type: None,


        });

        graph.add_edge(EdgeParams {


            source_node_id: func,


            target_node_id: interface,


            edge_type: EdgeType::Call,


            call_site_span: (60, 65),


            return_site_span: None,


            sequence_number: 4,


            returned_value: None,


            argument_names: Some(vec!["recipient".to_string(), "amount".to_string()]),


            event_name: None,


            declared_return_type: None,


        });

        graph.add_edge(EdgeParams {


            source_node_id: func,


            target_node_id: library,


            edge_type: EdgeType::Call,


            call_site_span: (70, 75),


            return_site_span: None,


            sequence_number: 5,


            returned_value: None,


            argument_names: Some(vec!["a".to_string(), "b".to_string()]),


            event_name: None,


            declared_return_type: None,


        });

        graph.add_edge(EdgeParams {


            source_node_id: func,


            target_node_id: event_listener,


            edge_type: EdgeType::Call,


            call_site_span: (80, 85),


            return_site_span: None,


            sequence_number: 6,


            returned_value: None,


            argument_names: Some(vec![
                "from".to_string(),
                "to".to_string(),
                "value".to_string(),
            ]),


            event_name: Some("Transfer".to_string()),


            declared_return_type: None,


        });

        graph.add_edge(EdgeParams {


            source_node_id: modifier,


            target_node_id: func,


            edge_type: EdgeType::Return,


            call_site_span: (145, 148),


            return_site_span: Some((147, 148)),


            sequence_number: 3,


            returned_value: Some("true".to_string()),


            argument_names: None,


            event_name: None,


            declared_return_type: Some("bool".to_string()),


        });

        graph
    }

    #[test]
    fn test_basic_json_serialization() {
        let graph = create_simple_graph();
        let config = JsonExportConfig {
            exclude_isolated_nodes: false,
            pretty_print: false,
            include_metadata: false,
        };

        let json_str = graph.to_json("Test Graph", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert_eq!(json["name"], "Test Graph");
        assert_eq!(json["nodes"].as_array().unwrap().len(), 3);
        assert_eq!(json["edges"].as_array().unwrap().len(), 2);
        assert!(json["metadata"].is_null());
    }

    #[test]
    fn test_json_with_metadata() {
        let graph = create_simple_graph();
        let config = JsonExportConfig {
            exclude_isolated_nodes: false,
            pretty_print: false,
            include_metadata: true,
        };

        let json_str = graph.to_json("Test Graph", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert!(json["metadata"].is_object());
        assert_eq!(json["metadata"]["node_count"], 3);
        assert_eq!(json["metadata"]["edge_count"], 2);
        assert_eq!(json["metadata"]["isolated_node_count"], 0);
        assert!(json["metadata"]["generated_at"].is_string());
    }

    #[test]
    fn test_node_serialization_fields() {
        let graph = create_simple_graph();
        let config = JsonExportConfig::default();

        let json_str = graph.to_json("Test", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        let nodes = json["nodes"].as_array().unwrap();
        let first_node = &nodes[0];

        assert!(first_node["id"].is_number());
        assert!(first_node["name"].is_string());
        assert!(first_node["node_type"].is_string());
        assert!(first_node["contract_name"].is_string());
        assert!(first_node["visibility"].is_string());
        assert!(first_node["span"].is_array());
        assert!(first_node["has_explicit_return"].is_boolean());
        assert!(first_node["parameters"].is_array());

        assert_eq!(first_node["name"], "constructor");
        assert_eq!(first_node["node_type"], "Constructor");
        assert_eq!(first_node["contract_name"], "SimpleContract");
        assert_eq!(first_node["visibility"], "Public");

        let span = first_node["span"].as_array().unwrap();
        assert_eq!(span[0], 0);
        assert_eq!(span[1], 50);
    }

    #[test]
    fn test_edge_serialization_fields() {
        let graph = create_simple_graph();
        let config = JsonExportConfig::default();

        let json_str = graph.to_json("Test", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        let edges = json["edges"].as_array().unwrap();
        let first_edge = &edges[0];

        assert!(first_edge["source_node_id"].is_number());
        assert!(first_edge["target_node_id"].is_number());
        assert!(first_edge["edge_type"].is_string());
        assert!(first_edge["call_site_span"].is_array());
        assert!(first_edge["sequence_number"].is_number());

        assert_eq!(first_edge["edge_type"], "Call");
        assert_eq!(first_edge["source_node_id"], 0);
        assert_eq!(first_edge["target_node_id"], 1);
        assert_eq!(first_edge["sequence_number"], 1);

        let arguments = first_edge["argument_names"].as_array().unwrap();
        assert_eq!(arguments.len(), 1);
        assert_eq!(arguments[0], "42");
    }

    #[test]
    fn test_isolated_nodes_exclusion() {
        let graph = create_complex_graph();

        let config_include = JsonExportConfig {
            exclude_isolated_nodes: false,
            pretty_print: false,
            include_metadata: true,
        };

        let json_str = graph.to_json("Test", &config_include);
        let json_include: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert_eq!(json_include["nodes"].as_array().unwrap().len(), 7); // All nodes
        assert_eq!(json_include["metadata"]["isolated_node_count"], 1);

        let config_exclude = JsonExportConfig {
            exclude_isolated_nodes: true,
            pretty_print: false,
            include_metadata: true,
        };

        let json_str = graph.to_json("Test", &config_exclude);
        let json_exclude: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert_eq!(json_exclude["nodes"].as_array().unwrap().len(), 6); // Without isolated
        assert_eq!(json_exclude["metadata"]["isolated_node_count"], 0);

        let node_names: Vec<String> = json_exclude["nodes"]
            .as_array()
            .unwrap()
            .iter()
            .map(|n| n["name"].as_str().unwrap().to_string())
            .collect();
        assert!(!node_names.contains(&"isolatedFunction".to_string()));
    }

    #[test]
    fn test_all_node_types_serialization() {
        let graph = create_complex_graph();
        let config = JsonExportConfig::default();

        let json_str = graph.to_json("Test", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        let nodes = json["nodes"].as_array().unwrap();
        let node_types: HashSet<String> = nodes
            .iter()
            .map(|n| n["node_type"].as_str().unwrap().to_string())
            .collect();

        assert!(node_types.contains("StorageVariable"));
        assert!(node_types.contains("Function"));
        assert!(node_types.contains("Modifier"));
        assert!(node_types.contains("Interface"));
        assert!(node_types.contains("Library"));
        assert!(node_types.contains("EventListener"));
    }

    #[test]
    fn test_all_edge_types_serialization() {
        let graph = create_complex_graph();
        let config = JsonExportConfig::default();

        let json_str = graph.to_json("Test", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        let edges = json["edges"].as_array().unwrap();
        let edge_types: HashSet<String> = edges
            .iter()
            .map(|e| e["edge_type"].as_str().unwrap().to_string())
            .collect();

        assert!(edge_types.contains("Call"));
        assert!(edge_types.contains("Return"));
        assert!(edge_types.contains("StorageRead"));
        assert!(edge_types.contains("StorageWrite"));
    }

    #[test]
    fn test_event_edge_serialization() {
        let graph = create_complex_graph();
        let config = JsonExportConfig::default();

        let json_str = graph.to_json("Test", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        let edges = json["edges"].as_array().unwrap();
        let event_edge = edges.iter().find(|e| e["event_name"].is_string()).unwrap();

        assert_eq!(event_edge["event_name"], "Transfer");
        assert_eq!(event_edge["edge_type"], "Call");

        let args = event_edge["argument_names"].as_array().unwrap();
        assert_eq!(args.len(), 3);
        assert_eq!(args[0], "from");
        assert_eq!(args[1], "to");
        assert_eq!(args[2], "value");
    }

    #[test]
    fn test_return_edge_serialization() {
        let graph = create_complex_graph();
        let config = JsonExportConfig::default();

        let json_str = graph.to_json("Test", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        let edges = json["edges"].as_array().unwrap();
        let return_edge = edges
            .iter()
            .find(|e| e["edge_type"] == "Return")
            .expect("Return edge not found");

        assert_eq!(return_edge["returned_value"], "true");
        assert_eq!(return_edge["declared_return_type"], "bool");
        assert!(return_edge["return_site_span"].is_array());

        let return_span = return_edge["return_site_span"].as_array().unwrap();
        assert_eq!(return_span[0], 147);
        assert_eq!(return_span[1], 148);
    }

    #[test]
    fn test_custom_formatters() {
        let graph = create_simple_graph();
        let config = JsonExportConfig {
            exclude_isolated_nodes: false,
            pretty_print: false,
            include_metadata: false,
        };

        let json_value = graph.to_json_with_formatters(
            "Custom Graph",
            &config,
            |node| {
                json!({
                    "custom_id": format!("node_{}", node.id),
                    "custom_name": node.name.to_uppercase(),
                    "is_public": node.visibility == Visibility::Public
                })
            },
            |edge| {
                json!({
                    "from": format!("node_{}", edge.source_node_id),
                    "to": format!("node_{}", edge.target_node_id),
                    "type": format!("{:?}", edge.edge_type).to_lowercase()
                })
            },
        );

        assert_eq!(json_value["name"], "Custom Graph");

        let nodes = json_value["nodes"].as_array().unwrap();
        assert_eq!(nodes[0]["custom_id"], "node_0");
        assert_eq!(nodes[0]["custom_name"], "CONSTRUCTOR");
        assert_eq!(nodes[0]["is_public"], true);

        let edges = json_value["edges"].as_array().unwrap();
        assert_eq!(edges[0]["from"], "node_0");
        assert_eq!(edges[0]["to"], "node_1");
        assert_eq!(edges[0]["type"], "call");
    }

    #[test]
    fn test_empty_graph_serialization() {
        let graph = CallGraph::new();
        let config = JsonExportConfig::default();

        let json_str = graph.to_json("Empty Graph", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert_eq!(json["name"], "Empty Graph");
        assert_eq!(json["nodes"].as_array().unwrap().len(), 0);
        assert_eq!(json["edges"].as_array().unwrap().len(), 0);
        assert_eq!(json["metadata"]["node_count"], 0);
        assert_eq!(json["metadata"]["edge_count"], 0);
        assert_eq!(json["metadata"]["isolated_node_count"], 0);
    }

    #[test]
    fn test_pretty_print_option() {
        let graph = create_simple_graph();

        let config_compact = JsonExportConfig {
            exclude_isolated_nodes: false,
            pretty_print: false,
            include_metadata: false,
        };

        let compact = graph.to_json("Test", &config_compact);
        assert!(!compact.contains("\n  ")); // No indentation

        let config_pretty = JsonExportConfig {
            exclude_isolated_nodes: false,
            pretty_print: true,
            include_metadata: false,
        };

        let pretty = graph.to_json("Test", &config_pretty);
        assert!(pretty.contains("\n  ")); // Has indentation
        assert!(pretty.len() > compact.len()); // Pretty is longer
    }

    #[test]
    fn test_round_trip_node_serialization() {
        let node = Node {
            id: 42,
            name: "testFunction".to_string(),
            node_type: NodeType::Function,
            contract_name: Some("TestContract".to_string()),
            visibility: Visibility::External,
            span: (100, 200),
            has_explicit_return: true,
            declared_return_type: Some("uint256".to_string()),
            parameters: vec![crate::cg::ParameterInfo {
                name: "amount".to_string(),
                param_type: "uint256".to_string(),
                description: Some("The amount to transfer".to_string()),
            }],
            revert_message: Some("Insufficient balance".to_string()),
            condition_expression: Some("balance >= amount".to_string()),
        };

        let json_value = serde_json::to_value(&node).expect("Failed to serialize node");

        let deserialized: Node =
            serde_json::from_value(json_value).expect("Failed to deserialize node");

        assert_eq!(deserialized.id, node.id);
        assert_eq!(deserialized.name, node.name);
        assert_eq!(deserialized.node_type, node.node_type);
        assert_eq!(deserialized.contract_name, node.contract_name);
        assert_eq!(deserialized.visibility, node.visibility);
        assert_eq!(deserialized.span, node.span);
        assert_eq!(deserialized.has_explicit_return, node.has_explicit_return);
        assert_eq!(deserialized.declared_return_type, node.declared_return_type);
        assert_eq!(deserialized.parameters.len(), node.parameters.len());
        assert_eq!(deserialized.revert_message, node.revert_message);
        assert_eq!(deserialized.condition_expression, node.condition_expression);
    }

    #[test]
    fn test_round_trip_edge_serialization() {
        let edge = Edge {
            source_node_id: 10,
            target_node_id: 20,
            edge_type: EdgeType::Call,
            call_site_span: (150, 175),
            return_site_span: Some((170, 172)),
            sequence_number: 5,
            returned_value: Some("result".to_string()),
            argument_names: Some(vec!["arg1".to_string(), "arg2".to_string()]),
            event_name: Some("TransferComplete".to_string()),
            declared_return_type: Some("bool".to_string()),
        };

        let json_value = serde_json::to_value(&edge).expect("Failed to serialize edge");

        let deserialized: Edge =
            serde_json::from_value(json_value).expect("Failed to deserialize edge");

        assert_eq!(deserialized.source_node_id, edge.source_node_id);
        assert_eq!(deserialized.target_node_id, edge.target_node_id);
        assert_eq!(deserialized.edge_type, edge.edge_type);
        assert_eq!(deserialized.call_site_span, edge.call_site_span);
        assert_eq!(deserialized.return_site_span, edge.return_site_span);
        assert_eq!(deserialized.sequence_number, edge.sequence_number);
        assert_eq!(deserialized.returned_value, edge.returned_value);
        assert_eq!(deserialized.argument_names, edge.argument_names);
        assert_eq!(deserialized.event_name, edge.event_name);
        assert_eq!(deserialized.declared_return_type, edge.declared_return_type);
    }

    #[test]
    fn test_enum_serialization() {
        let node_types = vec![
            NodeType::Function,
            NodeType::Interface,
            NodeType::Constructor,
            NodeType::Modifier,
            NodeType::Library,
            NodeType::StorageVariable,
            NodeType::Evm,
            NodeType::EventListener,
            NodeType::RequireCondition,
            NodeType::IfStatement,
            NodeType::ThenBlock,
            NodeType::ElseBlock,
            NodeType::WhileStatement,
            NodeType::WhileBlock,
            NodeType::ForCondition,
            NodeType::ForBlock,
        ];

        for node_type in node_types {
            let json = serde_json::to_value(&node_type).unwrap();
            let deserialized: NodeType = serde_json::from_value(json).unwrap();
            assert_eq!(deserialized, node_type);
        }

        let edge_types = vec![
            EdgeType::Call,
            EdgeType::Return,
            EdgeType::StorageRead,
            EdgeType::StorageWrite,
            EdgeType::Require,
            EdgeType::IfConditionBranch,
            EdgeType::ThenBranch,
            EdgeType::ElseBranch,
            EdgeType::WhileConditionBranch,
            EdgeType::WhileBodyBranch,
            EdgeType::ForConditionBranch,
            EdgeType::ForBodyBranch,
        ];

        for edge_type in edge_types {
            let json = serde_json::to_value(&edge_type).unwrap();
            let deserialized: EdgeType = serde_json::from_value(json).unwrap();
            assert_eq!(deserialized, edge_type);
        }

        let visibilities = vec![
            Visibility::Public,
            Visibility::Private,
            Visibility::Internal,
            Visibility::External,
            Visibility::Default,
        ];

        for visibility in visibilities {
            let json = serde_json::to_value(&visibility).unwrap();
            let deserialized: Visibility = serde_json::from_value(json).unwrap();
            assert_eq!(deserialized, visibility);
        }
    }

    #[test]
    fn test_large_graph_performance() {
        let mut graph = CallGraph::new();

        let node_count = 100;
        let mut node_ids = Vec::new();

        for i in 0..node_count {
            let node_id = graph.add_node(
                format!("function_{}", i),
                NodeType::Function,
                Some("LargeContract".to_string()),
                Visibility::Public,
                (i * 10, i * 10 + 5),
            );
            node_ids.push(node_id);
        }

        for i in 0..node_count - 1 {
            graph.add_edge(EdgeParams {

                source_node_id: node_ids[i],

                target_node_id: node_ids[i + 1],

                edge_type: EdgeType::Call,

                call_site_span: (i * 10 + 2, i * 10 + 3),

                return_site_span: None,

                sequence_number: i,

                returned_value: None,

                argument_names: None,

                event_name: None,

                declared_return_type: None,

            });
        }

        let config = JsonExportConfig::default();
        let json_str = graph.to_json("Large Graph", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        assert_eq!(json["nodes"].as_array().unwrap().len(), node_count);
        assert_eq!(json["edges"].as_array().unwrap().len(), node_count - 1);
    }

    #[test]
    fn test_special_characters_in_names() {
        let mut graph = CallGraph::new();

        let n0 = graph.add_node(
            "test\"quotes\"".to_string(),
            NodeType::Function,
            Some("Contract\\With\\Backslash".to_string()),
            Visibility::Public,
            (0, 10),
        );

        let n1 = graph.add_node(
            "new\nline\ttab".to_string(),
            NodeType::Function,
            Some("Contract'With'Apostrophe".to_string()),
            Visibility::Public,
            (20, 30),
        );

        graph.add_edge(EdgeParams {


            source_node_id: n0,


            target_node_id: n1,


            edge_type: EdgeType::Call,


            call_site_span: (5, 8),


            return_site_span: None,


            sequence_number: 1,


            returned_value: Some("return\nvalue".to_string()),


            argument_names: Some(vec!["arg\"with\"quotes".to_string()]),


            event_name: Some("Event\"Name".to_string()),


            declared_return_type: None,


        });

        let config = JsonExportConfig::default();
        let json_str = graph.to_json("Special Chars", &config);

        let json: Value =
            serde_json::from_str(&json_str).expect("Failed to parse JSON with special chars");

        let nodes = json["nodes"].as_array().unwrap();
        assert_eq!(nodes[0]["name"], "test\"quotes\"");
        assert_eq!(nodes[0]["contract_name"], "Contract\\With\\Backslash");
        assert_eq!(nodes[1]["name"], "new\nline\ttab");

        let edges = json["edges"].as_array().unwrap();
        assert_eq!(edges[0]["returned_value"], "return\nvalue");
        assert_eq!(edges[0]["event_name"], "Event\"Name");
    }

    #[test]
    fn test_null_optional_fields() {
        let mut graph = CallGraph::new();

        let n0 = graph.add_node(
            "minimal".to_string(),
            NodeType::Function,
            None, // No contract name
            Visibility::Default,
            (0, 10),
        );

        let n1 = graph.add_node(
            "target".to_string(),
            NodeType::Function,
            None,
            Visibility::Default,
            (20, 30),
        );

        graph.add_edge(EdgeParams {


            source_node_id: n0,


            target_node_id: n1,


            edge_type: EdgeType::Call,


            call_site_span: (5, 8),


            return_site_span: None,


            sequence_number: // No return site
            1,


            returned_value: None,


            argument_names: // No returned value
            None,


            event_name: // No arguments
            None,


            declared_return_type: // No event name
            None,


        });

        let config = JsonExportConfig::default();
        let json_str = graph.to_json("Minimal", &config);
        let json: Value = serde_json::from_str(&json_str).expect("Failed to parse JSON");

        let nodes = json["nodes"].as_array().unwrap();
        assert!(nodes[0]["contract_name"].is_null());
        assert!(nodes[0]["declared_return_type"].is_null());
        assert!(nodes[0]["revert_message"].is_null());
        assert!(nodes[0]["condition_expression"].is_null());

        let edges = json["edges"].as_array().unwrap();
        assert!(edges[0]["return_site_span"].is_null());
        assert!(edges[0]["returned_value"].is_null());
        assert!(edges[0]["argument_names"].is_null());
        assert!(edges[0]["event_name"].is_null());
        assert!(edges[0]["declared_return_type"].is_null());
    }
}
