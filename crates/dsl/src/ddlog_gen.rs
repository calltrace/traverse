//! ddlog_gen.rs
//! Example module that parses a Tree-Sitter `node-types.json` into custom
//! data structures, then generates a Differential Datalog (.dl) file.
//!
//! Usage:
//!     let parsed = parse_node_types(json_str)?;
//!     let ddlog_output = generate_ddlog_file(&parsed);

use serde::Deserialize;
use indexmap::IndexSet;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;

use crate::node_types::{ContextFreeNodeType, NodeTypeKind};
///
/// =========================
/// Generating the DDlog File
/// =========================

/// Generates a `.dl` file as a `String`, containing:
/// 1. `typedef NodeId`
/// 2. One `input relation` for each distinct node-type encountered in the input
/// 3. **Fact lines** for each `ContextFreeNodeType` instance
///
/// Each node-type relation is named after the original Tree-Sitter `"type"`, 
/// converted to a valid Rust/DDL identifier (e.g. "function_definition" -> "FunctionDefinition").
/// 
/// Each node is flattened into multiple lines: 
///   - (node_id, is_named, field_name, child_id)
/// 
/// where `field_name` is either `"fields:<field>"`, `"children"`, or `"subtypes"`.
/// If a node has no children, we output exactly one fact with an empty `field_name` and `child_id`.
pub fn generate_ddlog_file(nodes: &[ContextFreeNodeType]) -> String {
    let mut output = String::new();

    // 1) Write a single type definition for NodeId
    writeln!(output, "// Auto-generated DDlog from node-types.json\n").unwrap();
    writeln!(output, "typedef NodeId = string\n").unwrap();

    // Collect distinct node-types (Tree-Sitter "type" strings) to produce input relations
    let mut distinct_node_types = BTreeSet::new();
    for node in nodes {
        distinct_node_types.insert(node.name.sexp_name.clone());
        // Also consider any child node types that appear in fields/children/subtypes
        match &node.kind {
            NodeTypeKind::Supertype { subtypes } => {
                for st in subtypes {
                    distinct_node_types.insert(st.sexp_name.clone());
                }
            }
            NodeTypeKind::Regular { fields, children } => {
                for (_, childspec) in fields {
                    for ctype in &childspec.types {
                        distinct_node_types.insert(ctype.sexp_name.clone());
                    }
                }
                for ctype in &children.types {
                    distinct_node_types.insert(ctype.sexp_name.clone());
                }
            }
        }
    }

    // 2) Generate an input relation for each distinct node type
    //    Named identically to the type (converted to CamelCase).
    for ntype in &distinct_node_types {
        let ddlog_rel_name = to_ddlog_relation(ntype);
        writeln!(
            output,
            "input relation {}(node_id: NodeId, is_named: bool, field_name: string, child_id: NodeId)",
            ddlog_rel_name
        ).unwrap();
    }
    writeln!(output).unwrap();

    // 3) Generate DDlog facts (lines) for each node
    for (i, node) in nodes.iter().enumerate() {
        let node_id = format!("{}_{}", node.name.sexp_name, i); // simplistic unique ID

        let ddlog_rel_name = to_ddlog_relation(&node.name.sexp_name);
        let is_named = node.name.is_named;

        match &node.kind {
            NodeTypeKind::Supertype { subtypes } => {
                if subtypes.is_empty() {
                    // Output exactly one row if no subtypes
                    writeln!(
                        output,
                        "{}(\"{}\", {}, \"\", \"\").",
                        ddlog_rel_name, node_id, is_named
                    ).unwrap();
                } else {
                    for (idx, st) in subtypes.iter().enumerate() {
                        let child_id = format!("{}_sub_{}", node_id, idx);
                        // field_name = "subtypes"
                        writeln!(
                            output,
                            "{}(\"{}\", {}, \"subtypes\", \"{}\").",
                            ddlog_rel_name, node_id, is_named, child_id
                        ).unwrap();
                    }
                }
            }
            NodeTypeKind::Regular { fields, children } => {
                let mut wrote_any = false;

                // Flatten fields
                for (field_key, child_spec) in fields {
                    // If child_spec is empty, we could still output one row
                    if child_spec.types.is_empty() {
                        let child_id = "";
                        writeln!(
                            output,
                            "{}(\"{}\", {}, \"fields:{}\", \"{}\").",
                            ddlog_rel_name, node_id, is_named, field_key, child_id
                        ).unwrap();
                        wrote_any = true;
                    } else {
                        // If child_spec has multiple possible child types,
                        // you might produce multiple lines referencing each possibility.
                        let mut idx = 0;
                        for ctype in &child_spec.types {
                            let child_id = format!("{}_{}_{}", node_id, field_key, idx);
                            writeln!(
                                output,
                                "{}(\"{}\", {}, \"fields:{}\", \"{}\").",
                                ddlog_rel_name, node_id, is_named, field_key, child_id
                            ).unwrap();
                            idx += 1;
                            wrote_any = true;
                        }
                    }
                }

                // Flatten children
                if !children.is_empty() {
                    let mut idx = 0;
                    for ctype in &children.types {
                        let child_id = format!("{}_children_{}", node_id, idx);
                        writeln!(
                            output,
                            "{}(\"{}\", {}, \"children\", \"{}\").",
                            ddlog_rel_name, node_id, is_named, child_id
                        ).unwrap();
                        idx += 1;
                        wrote_any = true;
                    }
                } else {
                    // if no children, optionally output a single line
                    // to indicate the node has no children
                }

                // If no fields/children, output a single row
                if !wrote_any && fields.is_empty() && children.is_empty() {
                    writeln!(
                        output,
                        "{}(\"{}\", {}, \"\", \"\").",
                        ddlog_rel_name, node_id, is_named
                    ).unwrap();
                }
            }
        }
    }

    output
}

/// Convert a Tree-Sitter node-type (e.g. "function_definition") to a valid DDlog
/// relation name (e.g. "FunctionDefinition"). Adjust as needed.
fn to_ddlog_relation(type_name: &str) -> String {
    // Convert snake_case to PascalCase
    // e.g. "function_definition" -> "FunctionDefinition"
    type_name
        .split('_')
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                None => String::new(),
                Some(c) => c.to_ascii_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::node_types::{parse_node_types, Children, NodeName};

    use super::*;
    use indexmap::indexset;
    use std::collections::{BTreeMap, BTreeSet};

    #[test]
    fn test_parse_and_generate_ddlog_supertype() {
        // Build a supertype node by hand
        let node = ContextFreeNodeType {
            name: NodeName {
                sexp_name: "expression".to_owned(),
                is_named: true,
            },
            kind: NodeTypeKind::Supertype {
                subtypes: {
                    let mut s = BTreeSet::new();
                    s.insert(NodeName {
                        sexp_name: "binary_expression".to_owned(),
                        is_named: true,
                    });
                    s.insert(NodeName {
                        sexp_name: "unary_expression".to_owned(),
                        is_named: true,
                    });
                    s
                },
            },
        };

        let ddlog = generate_ddlog_file(&[node]);
        // Distinct types: "expression", "binary_expression", "unary_expression"
        // Expect input relations for Expression, BinaryExpression, UnaryExpression
        assert!(ddlog.contains("input relation Expression("));
        assert!(ddlog.contains("input relation BinaryExpression("));
        assert!(ddlog.contains("input relation UnaryExpression("));

        // Expect 2 lines referencing subtypes
        // Expression("expression_0", true, "subtypes", "expression_0_sub_0").
        // Expression("expression_0", true, "subtypes", "expression_0_sub_1").
        assert!(ddlog.contains("Expression(\"expression_0\", true, \"subtypes\", \"expression_0_sub_"));
    }

    #[test]
    fn test_parse_and_generate_ddlog_regular() {
        // A "function_definition" with fields + children
        let node = ContextFreeNodeType {
            name: NodeName {
                sexp_name: "function_definition".to_owned(),
                is_named: true,
            },
            kind: NodeTypeKind::Regular {
                fields: {
                    let mut fm = BTreeMap::new();
                    let mut field_children = Children {
                        multiple: false,
                        required: true,
                        types: indexset! {
                            NodeName {
                                sexp_name: "identifier".to_string(),
                                is_named: true
                            }
                        },
                    };
                    fm.insert("name".to_string(), field_children);
                    fm
                },
                children: Children {
                    multiple: false,
                    required: true,
                    types: indexset! {
                        NodeName {
                            sexp_name: "block".to_string(),
                            is_named: true
                        }
                    },
                },
            },
        };

        let ddlog = generate_ddlog_file(&[node]);
        // We expect one relation definition: "FunctionDefinition".
        // Also "Identifier" and "Block" from child references
        assert!(ddlog.contains("input relation FunctionDefinition("));
        assert!(ddlog.contains("input relation Identifier("));
        assert!(ddlog.contains("input relation Block("));

        // Fact lines:
        // FunctionDefinition("function_definition_0", true, "fields:name", "...").
        // FunctionDefinition("function_definition_0", true, "children", "...").
        assert!(ddlog.contains("FunctionDefinition(\"function_definition_0\", true, \"fields:name\", \"function_definition_0_name_0\")."));
        assert!(ddlog.contains("FunctionDefinition(\"function_definition_0\", true, \"children\", \"function_definition_0_children_0\")."));
    }

    #[test]
    fn test_parse_from_json_and_generate_ddlog() {
        // This test verifies we can parse JSON into ContextFreeNodeType, then produce .dl output
        let json_str = r#"
        [
            {
                "type": "variable_declaration",
                "named": true,
                "fields": {
                    "name": {
                        "multiple": false,
                        "required": true,
                        "types": [
                            { "type": "identifier", "named": true }
                        ]
                    }
                }
            }
        ]
        "#;

        let parsed = parse_node_types(json_str).expect("Failed to parse");
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].name.sexp_name, "variable_declaration");

        let ddlog = generate_ddlog_file(&parsed);
        // We expect "VariableDeclaration" and "Identifier" input relation definitions:
        assert!(ddlog.contains("input relation VariableDeclaration("));
        assert!(ddlog.contains("input relation Identifier("));

        // We expect one fact line: 
        // VariableDeclaration("variable_declaration_0", true, "fields:name", "variable_declaration_0_name_0").
        assert!(ddlog.contains("VariableDeclaration(\"variable_declaration_0\", true, \"fields:name\", \"variable_declaration_0_name_0\")."));
    }
}
