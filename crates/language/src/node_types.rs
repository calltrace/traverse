// import serde derive
use indexmap::{indexset, IndexSet};
use serde_derive::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::ops::BitOrAssign;
/// Parse a Tree-Sitter `node-types.json` into our custom data structure.
///
pub fn parse_node_types(json_str: &str) -> Result<Vec<ContextFreeNodeType>, serde_json::Error> {
    serde_json::from_str(json_str)
}


#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct NodeName {
    #[serde(rename = "type")]
    pub sexp_name: String,
    #[serde(rename = "named")]
    pub is_named: bool,
}

#[derive(Clone, Debug, Deserialize)]
pub struct ContextFreeNodeType {
    #[serde(flatten)]
    pub name: NodeName,
    #[serde(flatten)]
    pub kind: NodeTypeKind,
}

#[derive(Clone, Debug, Deserialize)]
#[serde(untagged)]
pub enum NodeTypeKind {
    Supertype {
        subtypes: BTreeSet<NodeName>,
    },
    Regular {
        #[serde(default)]
        fields: BTreeMap<String, Children>,
        #[serde(default)]
        children: Children,
    },
}

/// Describes a node's named children: their types and quantity.
///
/// "Quantity" here means:
/// - Zero
/// - Zero to one
/// - Zero to many
/// - Exactly one
/// - One to many
///
/// **Unchecked invariant:** if `types` is empty, `multiple` and `required` must be `false`.
#[derive(Clone, Debug, Deserialize, PartialEq, Eq)]
pub struct Children {
    /// If `true`, there can be more than one child.
    pub multiple: bool,
    /// If `false`, there can be zero children.
    pub required: bool,
    /// Possible types of children.
    ///
    /// Additionally, if this is empty, that means there are no children.
    pub types: IndexSet<NodeName>,
}

impl Children {
    pub fn empty() -> Self {
        Self {
            multiple: false,
            required: false,
            types: indexset! {},
        }
    }

    /// Whether there are no children.
    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}

impl Default for Children {
    fn default() -> Self {
        Self::empty().clone()
    }
}

impl BitOrAssign for Children {
    fn bitor_assign(&mut self, other: Self) {
        if other.is_empty() {
            return;
        } else if self.is_empty() {
            *self = other;
            return;
        }

        // If either original `Children` has at least 1 element, then this `Children` will.
        self.required |= other.required;

        // If either original `Children` may have multiple children, then this `Children` may.
        self.multiple |= other.multiple;

        // Add other child types, but no duplicates.
        self.types.extend(other.types);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn parse_node_types() {
        let input = r#"
        {
            "type": "FunctionDefinition",
            "named": false,
            "fields": {
            "name": {
                "multiple": false,
                "required": true,
                "types": [
                    {
                        "type": "Identifier",
                        "named": true
                    }
                ]
            },
            "parameters": {
                "multiple": false,
                "required": true,
                "types": [
                    {
                        "type": "ParameterList",
                        "named": false
                    }
                ]
            },
            "body": {
                "multiple": false,
                "required": true,
                "types": [
                    {
                        "type": "Block",
                        "named": false
                    }
                ]
            }
            },
            "children": {
            "multiple": false,
            "required": false,
            "types": []
            }
        }"#;
        let _node_type: ContextFreeNodeType = serde_json::from_str(input).expect("failed to parse");
    }

    /// 1) **Minimal Regular Node**:
    ///    A single node with no children or fields (both defaulted to empty).
    #[test]
    fn test_minimal_regular_node() {
        let json_data = r#"
        [
            {
                "type": "source_file",
                "named": true
            }
        ]
        "#;

        let parsed: Vec<ContextFreeNodeType> =
            serde_json::from_str(json_data).expect("failed to parse");
        assert_eq!(parsed.len(), 1);

        let node = &parsed[0];
        // `name` fields:
        assert_eq!(node.name.sexp_name, "source_file");
        assert!(node.name.is_named);

        // `kind` should parse as a Regular node by default (since 'subtypes' isn't present).
        match &node.kind {
            NodeTypeKind::Regular { fields, children } => {
                // Both `fields` and `children` default to empty
                assert!(fields.is_empty());
                assert!(!children.multiple);
                assert!(!children.required);
                assert!(children.types.is_empty());
            }
            NodeTypeKind::Supertype { .. } => panic!("Expected a Regular node, found a Supertype."),
        }
    }

    /// 2) **Supertype with Multiple Subtypes**:
    ///    One node is a "supertype" referencing other node names;
    ///    the other node is a minimal regular node.
    #[test]
    fn test_supertype_with_subtypes() {
        let json_data = r#"
        [
            {
                "type": "expression",
                "named": true,
                "subtypes": [
                    { "type": "binary_expression", "named": true },
                    { "type": "unary_expression",  "named": true  }
                ]
            },
            {
                "type": "comment",
                "named": false
            }
        ]
        "#;

        let parsed: Vec<ContextFreeNodeType> =
            serde_json::from_str(json_data).expect("failed to parse");
        assert_eq!(parsed.len(), 2);

        // First: Supertype node
        let expr_node = &parsed[0];
        assert_eq!(expr_node.name.sexp_name, "expression");
        assert!(expr_node.name.is_named);

        match &expr_node.kind {
            NodeTypeKind::Supertype { subtypes } => {
                // subtypes should have 2 NodeName entries
                assert_eq!(subtypes.len(), 2);
                let expected: BTreeSet<NodeName> = [
                    NodeName {
                        sexp_name: "binary_expression".to_string(),
                        is_named: true,
                    },
                    NodeName {
                        sexp_name: "unary_expression".to_string(),
                        is_named: true,
                    },
                ]
                .into_iter()
                .collect();
                assert_eq!(subtypes, &expected);
            }
            NodeTypeKind::Regular { .. } => panic!("Expected a Supertype node, found a Regular."),
        }

        // Second: Minimal Regular node
        let comment_node = &parsed[1];
        assert_eq!(comment_node.name.sexp_name, "comment");
        assert!(!comment_node.name.is_named);
        match &comment_node.kind {
            NodeTypeKind::Regular { fields, children } => {
                assert!(fields.is_empty());
                assert!(!children.multiple);
                assert!(!children.required);
                assert!(children.types.is_empty());
            }
            NodeTypeKind::Supertype { .. } => panic!("Expected a Regular node, found a Supertype."),
        }
    }

    /// 3) **Complex Regular Node**:
    ///    Demonstrates fields with `Children` structures of varying multiplicities,
    ///    along with a top-level children definition.
    #[test]
    fn test_complex_regular_node() {
        let json_data = r#"
        [
            {
                "type": "if_statement",
                "named": true,
                "fields": {
                    "condition": {
                        "multiple": false,
                        "required": true,
                        "types": [
                            { "type": "expression", "named": true }
                        ]
                    },
                    "body": {
                        "multiple": false,
                        "required": false,
                        "types": [
                            { "type": "block", "named": true }
                        ]
                    }
                },
                "children": {
                    "multiple": true,
                    "required": false,
                    "types": [
                        { "type": "comment", "named": false }
                    ]
                }
            }
        ]
        "#;

        let parsed: Vec<ContextFreeNodeType> =
            serde_json::from_str(json_data).expect("failed to parse");
        assert_eq!(parsed.len(), 1);

        let if_stmt = &parsed[0];
        assert_eq!(if_stmt.name.sexp_name, "if_statement");
        assert!(if_stmt.name.is_named);

        match &if_stmt.kind {
            NodeTypeKind::Regular { fields, children } => {
                // Check `fields`
                assert_eq!(fields.len(), 2);
                // 1) condition
                let condition = fields.get("condition").unwrap();
                assert!(!condition.multiple);
                assert!(condition.required);
                assert_eq!(condition.types.len(), 1);
                let cond_name = condition.types.iter().next().unwrap();
                assert_eq!(cond_name.sexp_name, "expression");
                assert!(cond_name.is_named);

                // 2) body
                let body = fields.get("body").unwrap();
                assert!(!body.multiple);
                assert!(!body.required);
                assert_eq!(body.types.len(), 1);
                let body_name = body.types.iter().next().unwrap();
                assert_eq!(body_name.sexp_name, "block");
                assert!(body_name.is_named);

                // Check top-level children
                assert!(children.multiple);
                assert!(!children.required);
                assert_eq!(children.types.len(), 1);
                let child_name = children.types.iter().next().unwrap();
                assert_eq!(child_name.sexp_name, "comment");
                assert!(!child_name.is_named);
            }
            NodeTypeKind::Supertype { .. } => {
                panic!("Expected a Regular node, found a Supertype.");
            }
        }
    }

    #[test]
    fn test_solidity_grammar() {
        let json_data = r#"
        [
            {
                "type": "contract_definition",
                "named": true,
                "fields": {
                    "name": {
                        "multiple": false,
                        "required": true,
                        "types": [
                            { "type": "identifier", "named": true }
                        ]
                    },
                    "body": {
                        "multiple": false,
                        "required": true,
                        "types": [
                            { "type": "function_definition", "named": true },
                            { "type": "variable_declaration", "named": true }
                        ]
                    }
                }
            },
            {
                "type": "function_definition",
                "named": true,
                "fields": {
                    "name": {
                        "multiple": false,
                        "required": true,
                        "types": [
                            { "type": "identifier", "named": true }
                        ]
                    },
                    "parameters": {
                        "multiple": true,
                        "required": false,
                        "types": [
                            { "type": "parameter", "named": true }
                        ]
                    }
                },
                "children": {
                    "multiple": false,
                    "required": true,
                    "types": [
                        { "type": "block", "named": true }
                    ]
                }
            },
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
                    },
                    "type": {
                        "multiple": false,
                        "required": true,
                        "types": [
                            { "type": "type_name", "named": true }
                        ]
                    }
                }
            }
        ]
        "#;

        let parsed: Vec<ContextFreeNodeType> = serde_json::from_str(json_data).expect("failed to parse");
        assert_eq!(parsed.len(), 3);

        // 1. Validate contract_definition node
        let contract_def = &parsed[0];
        assert_eq!(contract_def.name.sexp_name, "contract_definition");
        assert!(contract_def.name.is_named);
        match &contract_def.kind {
            NodeTypeKind::Regular { fields, children } => {
                assert_eq!(fields.len(), 2);

                // Validate `name` field
                let name_field = fields.get("name").unwrap();
                assert!(!name_field.multiple);
                assert!(name_field.required);
                assert_eq!(name_field.types.len(), 1);
                let name_type = name_field.types.iter().next().unwrap();
                assert_eq!(name_type.sexp_name, "identifier");
                assert!(name_type.is_named);

                // Validate `body` field
                let body_field = fields.get("body").unwrap();
                assert!(!body_field.multiple);
                assert!(body_field.required);
                assert_eq!(body_field.types.len(), 2);
                let body_types: IndexSet<_> =
                    body_field.types.iter().map(|t| t.sexp_name.clone()).collect();
                assert!(body_types.contains("function_definition"));
                assert!(body_types.contains("variable_declaration"));
            }
            _ => panic!("Expected contract_definition to be a Regular node"),
        }

        // 2. Validate function_definition node
        let func_def = &parsed[1];
        assert_eq!(func_def.name.sexp_name, "function_definition");
        assert!(func_def.name.is_named);
        match &func_def.kind {
            NodeTypeKind::Regular { fields, children } => {
                // Validate fields
                assert_eq!(fields.len(), 2);

                let name_field = fields.get("name").unwrap();
                assert!(!name_field.multiple);
                assert!(name_field.required);
                assert_eq!(name_field.types.len(), 1);
                let name_type = name_field.types.iter().next().unwrap();
                assert_eq!(name_type.sexp_name, "identifier");
                assert!(name_type.is_named);

                let params_field = fields.get("parameters").unwrap();
                assert!(params_field.multiple);
                assert!(!params_field.required);
                assert_eq!(params_field.types.len(), 1);
                let param_type = params_field.types.iter().next().unwrap();
                assert_eq!(param_type.sexp_name, "parameter");
                assert!(param_type.is_named);

                // Validate children
                assert!(children.required);
                assert_eq!(children.types.len(), 1);
                let child_type = children.types.iter().next().unwrap();
                assert_eq!(child_type.sexp_name, "block");
                assert!(child_type.is_named);
            }
            _ => panic!("Expected function_definition to be a Regular node"),
        }

        // 3. Validate variable_declaration node
        let var_decl = &parsed[2];
        assert_eq!(var_decl.name.sexp_name, "variable_declaration");
        assert!(var_decl.name.is_named);
        match &var_decl.kind {
            NodeTypeKind::Regular { fields, children } => {
                assert!(children.is_empty());
                assert_eq!(fields.len(), 2);

                let name_field = fields.get("name").unwrap();
                assert!(!name_field.multiple);
                assert!(name_field.required);
                assert_eq!(name_field.types.len(), 1);
                let name_type = name_field.types.iter().next().unwrap();
                assert_eq!(name_type.sexp_name, "identifier");
                assert!(name_type.is_named);

                let type_field = fields.get("type").unwrap();
                assert!(!type_field.multiple);
                assert!(type_field.required);
                assert_eq!(type_field.types.len(), 1);
                let type_name = type_field.types.iter().next().unwrap();
                assert_eq!(type_name.sexp_name, "type_name");
                assert!(type_name.is_named);
            }
            _ => panic!("Expected variable_declaration to be a Regular node"),
        }

    }

    #[test]
    fn children_or_assign() {
        let mut c1 = Children {
            multiple: false,
            required: false,
            types: indexset! {NodeName { sexp_name: "foo".to_string(), is_named: false }},
        };
        let c2 = Children {
            multiple: true,
            required: true,
            types: indexset! {NodeName { sexp_name: "bar".to_string(), is_named: false }},
        };
        c1 |= c2;
        assert_eq!(
            c1,
            Children {
                multiple: true,
                required: true,
                types: indexset! {
                    NodeName { sexp_name: "foo".to_string(), is_named: false },
                    NodeName { sexp_name: "bar".to_string(), is_named: false }
                }
            }
        );
    }
    #[test]
    fn children_or_assign_empty() {
        let mut c1 = Children {
            multiple: false,
            required: false,
            types: indexset! {NodeName { sexp_name: "foo".to_string(), is_named: false }},
        };
        let c2 = Children::empty();
        c1 |= c2;
        assert_eq!(
            c1,
            Children {
                multiple: false,
                required: false,
                types: indexset! {NodeName { sexp_name: "foo".to_string(), is_named: false }}
            }
        );
    }
    #[test]
    fn children_or_assign_empty_self() {
        let mut c1 = Children::empty();
        let c2 = Children {
            multiple: true,
            required: true,
            types: indexset! {NodeName { sexp_name: "bar".to_string(), is_named: false }},
        };
        c1 |= c2;
        assert_eq!(
            c1,
            Children {
                multiple: true,
                required: true,
                types: indexset! {NodeName { sexp_name: "bar".to_string(), is_named: false }}
            }
        );
    }
    #[test]
    fn children_or_assign_both_empty() {
        let mut c1 = Children::empty();
        let c2 = Children::empty();
        c1 |= c2;
        assert_eq!(c1, Children::empty());
    }
}
