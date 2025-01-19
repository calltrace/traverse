use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use crate::node_types::{ContextFreeNodeType, NodeTypeKind};

const RESERVED_WORDS: &[&str] = &["else","function","type","match","var"];

pub(crate) fn to_pascal_case(s: &str) -> String {
    s.split('_')
        .filter(|part| !part.is_empty())
        .map(|part| {
            let mut chars = part.chars();
            match chars.next() {
                None => String::new(),
                Some(c) => c.to_ascii_uppercase().to_string() + chars.as_str()
            }
        })
        .collect()
}

pub(crate) fn sanitize_reserved(name: &str) -> String {
    let lower = name.to_lowercase();
    if RESERVED_WORDS.contains(&lower.as_str()) {
        format!("_{}", name)
    } else {
        name.to_string()
    }
}

fn is_leaf_node(node: &ContextFreeNodeType) -> bool {
    match &node.kind {
        NodeTypeKind::Regular { fields, children } => fields.is_empty() && children.is_empty(),
        NodeTypeKind::Supertype { subtypes } => subtypes.is_empty(),
    }
}

pub fn generate_ddlog_file(nodes: &[ContextFreeNodeType]) -> String {
    let mut output = String::new();
    let mut distinct_types = BTreeSet::new();
    for node in nodes {
        distinct_types.insert(node.name.sexp_name.clone());
        match &node.kind {
            NodeTypeKind::Supertype { subtypes } => {
                for st in subtypes {
                    distinct_types.insert(st.sexp_name.clone());
                }
            }
            NodeTypeKind::Regular { fields, children } => {
                for spec in fields.values() {
                    for ctype in &spec.types {
                        distinct_types.insert(ctype.sexp_name.clone());
                    }
                }
                for ctype in &children.types {
                    distinct_types.insert(ctype.sexp_name.clone());
                }
            }
        }
    }
    let mut is_leaf_map = BTreeMap::<String, bool>::new();
    for node in nodes {
        let leaf = is_leaf_node(node);
        is_leaf_map.insert(node.name.sexp_name.clone(), leaf);
    }
    let mut main_relation_schemas = BTreeMap::<String, Vec<String>>::new();
    let mut link_relation_fields = BTreeMap::<String, BTreeSet<String>>::new();
    for tname in &distinct_types {
        let is_leaf = is_leaf_map.get(tname).copied().unwrap_or(false);
        if !is_leaf {
            let pascal = to_pascal_case(tname);
            let rel_name = sanitize_reserved(&pascal);
            main_relation_schemas.insert(rel_name.clone(), vec!["node_id: bigint".to_string()]);
            link_relation_fields.insert(rel_name, BTreeSet::new());
        }
    }
    for node in nodes {
        let parent_tname = &node.name.sexp_name;
        let parent_is_leaf = is_leaf_map.get(parent_tname).copied().unwrap_or(false);
        if parent_is_leaf {
            continue;
        }
        let parent_pascal = to_pascal_case(parent_tname);
        let parent_rel = sanitize_reserved(&parent_pascal);
        match &node.kind {
            NodeTypeKind::Supertype { subtypes } => {
                if !subtypes.is_empty() {
                    link_relation_fields
                        .get_mut(&parent_rel)
                        .unwrap()
                        .insert("subtypes".to_string());
                }
            }
            NodeTypeKind::Regular { fields, children } => {
                for (field_name, child_spec) in fields {
                    let all_leaf = child_spec.types.iter().all(|ty| {
                        is_leaf_map.get(&ty.sexp_name).copied().unwrap_or(false)
                    });
                    if child_spec.multiple {
                        link_relation_fields
                            .get_mut(&parent_rel)
                            .unwrap()
                            .insert(field_name.clone());
                    } else {
                        let col_type = if all_leaf { "string" } else { "bigint" };
                        let safe_field_name = sanitize_reserved(field_name);
                        let col_def = format!("{}: {}", safe_field_name, col_type);
                        main_relation_schemas
                            .get_mut(&parent_rel)
                            .unwrap()
                            .push(col_def);
                    }
                }
                if !children.types.is_empty() {
                    let all_leaf = children.types.iter().all(|ty| {
                        is_leaf_map.get(&ty.sexp_name).copied().unwrap_or(false)
                    });
                    if children.multiple {
                        link_relation_fields
                            .get_mut(&parent_rel)
                            .unwrap()
                            .insert("children".to_string());
                    } else {
                        let col_type = if all_leaf { "string" } else { "bigint" };
                        let col_def = format!("children: {}", col_type);
                        main_relation_schemas
                            .get_mut(&parent_rel)
                            .unwrap()
                            .push(col_def);
                    }
                }
            }
        }
    }
    for (rel_name, columns) in &main_relation_schemas {
        writeln!(output, "input relation {}(", rel_name).unwrap();
        for (i, col) in columns.iter().enumerate() {
            if i < columns.len() - 1 {
                writeln!(output, "    {},", col).unwrap();
            } else {
                writeln!(output, "    {}", col).unwrap();
            }
        }
        writeln!(output, ")\n").unwrap();
    }
    for (rel_name, fields) in &link_relation_fields {
        for field_name in fields {
            let mut all_leaf = true;
            'outer: for node in nodes {
                let node_parent_pascal = to_pascal_case(&node.name.sexp_name);
                let node_parent_rel = sanitize_reserved(&node_parent_pascal);
                if node_parent_rel == *rel_name {
                    match &node.kind {
                        NodeTypeKind::Supertype { subtypes } => {
                            if field_name == "subtypes" {
                                for st in subtypes {
                                    if !is_leaf_map.get(&st.sexp_name).copied().unwrap_or(false) {
                                        all_leaf = false;
                                        break 'outer;
                                    }
                                }
                            }
                        }
                        NodeTypeKind::Regular { fields, children } => {
                            if let Some(ch_spec) = fields.get(field_name) {
                                let cstr = ch_spec.types.iter().all(|ty| {
                                    is_leaf_map.get(&ty.sexp_name).copied().unwrap_or(false)
                                });
                                if !cstr {
                                    all_leaf = false;
                                    break 'outer;
                                }
                            }
                            if field_name == "children" {
                                let cstr = children.types.iter().all(|ty| {
                                    is_leaf_map.get(&ty.sexp_name).copied().unwrap_or(false)
                                });
                                if !cstr {
                                    all_leaf = false;
                                    break 'outer;
                                }
                            }
                        }
                    }
                }
            }
            let sanitized_field = sanitize_reserved(field_name);
            let link_name = format!("{}_{}", rel_name, sanitized_field);
            writeln!(output, "input relation {}(", link_name).unwrap();
            writeln!(output, "    parent_id: bigint,").unwrap();
            if all_leaf {
                writeln!(output, "    value: string").unwrap();
            } else {
                writeln!(output, "    child_id: bigint").unwrap();
            }
            writeln!(output, ")\n").unwrap();
        }
    }
    output
}
