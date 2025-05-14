use std::{collections::HashMap, iter};

use crate::{
    cg::{
        CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorStep,
        NodeInfo, NodeType, Visibility,
    },
    parser::get_node_text,
};
use anyhow::{anyhow, Context, Result};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Node as TsNode, Query, QueryCursor};

#[derive(Default)] // Add Default derive
pub struct ContractHandling {
    // Add field to store config
    config: HashMap<String, String>,
}

// Remove 'a lifetime
impl CallGraphGeneratorStep for ContractHandling {
    fn name(&self) -> &'static str {
        "Contract-Handling"
    }

    fn config(&mut self, config: &HashMap<String, String>) {
        self.config = config.clone(); // Store the configuration
    }

    fn generate(
        &self,
        input: CallGraphGeneratorInput,
        ctx: &mut CallGraphGeneratorContext, // Remove 'a
        graph: &mut CallGraph,
    ) -> Result<()> {
        // Access stored config via self.config if needed later
        let _config = &self.config; // Example access (currently unused)

        let mut definition_cursor = QueryCursor::new();

        let definition_query_str = r#"
            ; Contract identifier (captures name node for ALL contracts)
            (contract_declaration
              name: (identifier) @contract_identifier_node @contract_name_for_map
            ) @contract_def_item

            ; Interface definition
            (interface_declaration
              name: (identifier) @interface_name
            ) @interface_def_item

            ; Library definition
            (library_declaration
                name: (identifier) @library_name
            ) @library_def_item

            ; Contract inheritance
            (contract_declaration
              name: (identifier) @contract_name_for_inheritance
              (inheritance_specifier
                ancestor: (user_defined_type
                  (identifier) @inherited_name_for_contract
                )
              )
            ) @contract_inheritance_item

            ; Interface inheritance
            (interface_declaration
              name: (identifier) @interface_name_for_inheritance
              (inheritance_specifier
                ancestor: (user_defined_type
                  (identifier) @inherited_name_for_interface
                )
              )
            ) @interface_inheritance_item

            ; Function within an interface
            (interface_declaration
                (identifier) @interface_scope_for_func
                (contract_body
                    (function_definition
                        (identifier) @function_name
                        (_)?
                        [(visibility) @visibility_node]?
                    ) @function_def_item
                )
            )

            ; Function within a library
            (library_declaration
                (identifier) @library_scope_for_func
                (contract_body
                    (function_definition
                        (identifier) @function_name
                        (_)?
                        [(visibility) @visibility_node]?
                    ) @function_def_item
                )
            )

            ; Function within a contract
            (contract_declaration
                (identifier) @contract_scope_for_func
                (contract_body
                    (function_definition
                        (identifier) @function_name
                        (_)?
                        [(visibility) @visibility_node]?
                    ) @function_def_item
                )
            )

            ; Modifier within a contract
            (contract_declaration
                (identifier) @contract_scope_for_modifier
                (contract_body
                    (modifier_definition
                        (identifier) @modifier_name
                        (_)?
                        [(visibility) @visibility_node]?
                    ) @modifier_def_item
                )
            )

            ; Constructor within a contract
            (contract_declaration
                (identifier) @contract_scope_for_constructor
                (contract_body
                    (constructor_definition
                        (_)?
                        [(visibility) @visibility_node]?
                    ) @constructor_def_item
                )
            )

            ; Top-level function
            (source_file
                (function_definition
                    (identifier) @function_name
                    (_)?
                    [(visibility) @visibility_node]?
                ) @function_def_item
            )

            ; State variable within a contract
            (contract_declaration
                name: (identifier) @contract_scope_for_var
                (contract_body
                    (state_variable_declaration
                        (type_name) @var_type
                        (identifier) @var_name
                    ) @state_var_def_item
                )
            )

            ; Using directive within a contract
            (contract_declaration
              name: (identifier) @contract_scope_for_using
              (contract_body
                (using_directive
                  (type_alias (identifier) @using_library_name)
                  source: (_) @using_type_or_wildcard_node
                ) @using_directive_item
              )
            )
        "#;
        let definition_query = Query::new(&input.solidity_lang, definition_query_str)
            .context("Failed to create definition query")?;

        let root_node = input.tree.root_node();
        let source_bytes = input.source.as_bytes();

        // --- Pass 1: Identify all contract, interface, and library names and add their primary nodes ---
        eprintln!("[ContractHandling] Pass 1: Identifying Contracts, Interfaces, Libraries...");
        let mut matches_pass1 =
            definition_cursor.matches(&definition_query, root_node, |node: TsNode| {
                iter::once(&source_bytes[node.byte_range()])
            });
        matches_pass1.advance();
        while let Some(match_) = matches_pass1.get() {
            for capture in match_.captures {
                let capture_name = &definition_query.capture_names()[capture.index as usize];
                let captured_ts_node = capture.node;
                let text = get_node_text(&captured_ts_node, &input.source);

                match *capture_name {
                    "contract_def_item" => {
                        let name_node = captured_ts_node.child_by_field_name("name").unwrap();
                        let contract_name = get_node_text(&name_node, &input.source).to_string();
                        if !ctx.all_contracts.contains_key(&contract_name) {
                            let node_info = NodeInfo {
                                span: (name_node.start_byte(), name_node.end_byte()),
                                kind: name_node.kind().to_string(),
                            };
                            ctx.all_contracts.insert(contract_name.clone(), node_info);
                            // Add graph node for the contract itself (as a conceptual entity)
                            // This node isn't directly callable but represents the scope.
                            // Constructors will be separate nodes.
                        }
                    }
                    "interface_def_item" => {
                        let name_node = captured_ts_node.child_by_field_name("name").unwrap();
                        let interface_name = get_node_text(&name_node, &input.source).to_string();
                        if !ctx.all_interfaces.contains_key(&interface_name) {
                            let node_info = NodeInfo {
                                span: (name_node.start_byte(), name_node.end_byte()),
                                kind: name_node.kind().to_string(),
                            };
                            ctx.all_interfaces.insert(interface_name.clone(), node_info.clone());
                            let node_id = graph.add_node(
                                interface_name.clone(),
                                NodeType::Interface,
                                Some(interface_name.clone()),
                                Visibility::Default,
                                node_info.span,
                            );
                            ctx.definition_nodes_info.push((node_id, node_info, Some(interface_name)));
                        }
                    }
                    "library_def_item" => {
                        let name_node = captured_ts_node.child_by_field_name("name").unwrap();
                        let library_name = get_node_text(&name_node, &input.source).to_string();
                        if !ctx.all_libraries.contains_key(&library_name) {
                            let node_info = NodeInfo {
                                span: (name_node.start_byte(), name_node.end_byte()),
                                kind: name_node.kind().to_string(),
                            };
                            ctx.all_libraries.insert(library_name.clone(), node_info.clone());
                             let node_id = graph.add_node(
                                library_name.clone(),
                                NodeType::Library,
                                Some(library_name.clone()),
                                Visibility::Default,
                                node_info.span,
                            );
                            ctx.definition_nodes_info.push((node_id, node_info, Some(library_name)));
                        }
                    }
                    _ => {}
                }
            }
            matches_pass1.advance();
        }
        eprintln!("[ContractHandling] Pass 1: Found {} contracts, {} interfaces, {} libraries.", ctx.all_contracts.len(), ctx.all_interfaces.len(), ctx.all_libraries.len());

        // --- Pass 2: Process members, inheritance, and other relationships ---
        eprintln!("[ContractHandling] Pass 2: Processing members and relationships...");
        let mut matches_pass2 =
            definition_cursor.matches(&definition_query, root_node, |node: TsNode| {
                iter::once(&source_bytes[node.byte_range()])
            });
        matches_pass2.advance();
        while let Some(match_) = matches_pass2.get() {
            let mut item_node_kind_opt: Option<&str> = None;
            if let Some(item_capture) = match_.captures.iter().find(|cap| {
                let cap_name = &definition_query.capture_names()[cap.index as usize];
                cap_name.ends_with("_item") // e.g., contract_def_item, function_def_item
            }) {
                item_node_kind_opt = Some(definition_query.capture_names()[item_capture.index as usize]);
            }

            let mut captures_map: HashMap<String, TsNode> = HashMap::new();
            for capture in match_.captures {
                captures_map.insert(definition_query.capture_names()[capture.index as usize].to_string(), capture.node);
            }

            if let Some(item_kind_name) = item_node_kind_opt {
                match item_kind_name {
                    "contract_inheritance_item" => {
                        if let (Some(contract_name_node), Some(inherited_name_node)) = (
                            captures_map.get("contract_name_for_inheritance"),
                            captures_map.get("inherited_name_for_contract"),
                        ) {
                            let contract_name = get_node_text(contract_name_node, &input.source).to_string();
                            let inherited_name = get_node_text(inherited_name_node, &input.source).to_string();
                            if ctx.all_interfaces.contains_key(&inherited_name) {
                                ctx.contract_implements.entry(contract_name.clone()).or_default().push(inherited_name.clone());
                                eprintln!("[ContractHandling] Contract '{}' implements interface '{}'", contract_name, inherited_name);
                            }
                            ctx.contract_inherits.entry(contract_name).or_default().push(inherited_name);
                        }
                    }
                    "interface_inheritance_item" => {
                         if let (Some(iface_name_node), Some(inherited_name_node)) = (
                            captures_map.get("interface_name_for_inheritance"),
                            captures_map.get("inherited_name_for_interface"),
                        ) {
                            let iface_name = get_node_text(iface_name_node, &input.source).to_string();
                            let inherited_name = get_node_text(inherited_name_node, &input.source).to_string();
                            ctx.interface_inherits.entry(iface_name).or_default().push(inherited_name);
                        }
                    }
                    "function_def_item" | "modifier_def_item" | "constructor_def_item" => {
                        let def_node = captures_map.get(item_kind_name).unwrap(); // Item node itself
                        let node_type = match item_kind_name {
                            "function_def_item" => NodeType::Function,
                            "modifier_def_item" => NodeType::Modifier,
                            "constructor_def_item" => NodeType::Constructor,
                            _ => unreachable!(),
                        };

                        let name_opt = captures_map.get("function_name")
                            .or_else(|| captures_map.get("modifier_name"))
                            .map(|n| get_node_text(n, &input.source).to_string());

                        let scope_name_opt = captures_map.get("contract_scope_for_func")
                            .or_else(|| captures_map.get("library_scope_for_func"))
                            .or_else(|| captures_map.get("interface_scope_for_func"))
                            .or_else(|| captures_map.get("contract_scope_for_modifier"))
                            .or_else(|| captures_map.get("contract_scope_for_constructor"))
                            .map(|n| get_node_text(n, &input.source).to_string());

                        let final_name = match node_type {
                            NodeType::Constructor => scope_name_opt.clone().unwrap_or_default(),
                            _ => name_opt.unwrap_or_default(),
                        };

                        if final_name.is_empty() && node_type != NodeType::Constructor { // Constructors can have scope as name
                            eprintln!("Warning: Empty name for {:?} at span {:?}", node_type, def_node.byte_range());
                            matches_pass2.advance();
                            continue;
                        }
                        
                        if node_type == NodeType::Constructor {
                             if let Some(c_name) = &scope_name_opt {
                                ctx.contracts_with_explicit_constructors.insert(c_name.clone());
                            }
                        }

                        let visibility = captures_map.get("visibility_node").map_or_else(
                            || match node_type { // Default visibilities
                                NodeType::Constructor => Visibility::Public,
                                _ => Visibility::Internal,
                            },
                            |vn| match get_node_text(vn, &input.source) {
                                "public" => Visibility::Public,
                                "private" => Visibility::Private,
                                "internal" => Visibility::Internal,
                                "external" => Visibility::External,
                                _ => Visibility::Internal, // Default if text not recognized
                            },
                        );

                        let node_id = graph.add_node(
                            final_name.clone(),
                            node_type.clone(),
                            scope_name_opt.clone(),
                            visibility,
                            (def_node.start_byte(), def_node.end_byte()),
                        );
                        let node_info = NodeInfo {
                            span: (def_node.start_byte(), def_node.end_byte()),
                            kind: def_node.kind().to_string(),
                        };
                        ctx.definition_nodes_info.push((node_id, node_info, scope_name_opt.clone()));
                        
                        if node_type == NodeType::Function {
                            if let Some(scope) = &scope_name_opt {
                                if ctx.all_interfaces.contains_key(scope) {
                                     ctx.interface_functions.entry(scope.clone()).or_default().push(final_name);
                                }
                            }
                        }
                    }
                    "state_var_def_item" => {
                        if let (Some(scope_node), Some(type_node), Some(name_node)) = (
                            captures_map.get("contract_scope_for_var"),
                            captures_map.get("var_type"),
                            captures_map.get("var_name"),
                        ) {
                            let contract_name = get_node_text(scope_node, &input.source).to_string();
                            let var_name = get_node_text(name_node, &input.source).to_string();
                            let var_type = get_node_text(type_node, &input.source).to_string();
                            let def_node = captures_map.get(item_kind_name).unwrap();

                            ctx.state_var_types.insert((contract_name.clone(), var_name.clone()), var_type);
                            let node_id = graph.add_node(
                                var_name.clone(),
                                NodeType::StorageVariable,
                                Some(contract_name.clone()),
                                Visibility::Default,
                                (def_node.start_byte(), def_node.end_byte()),
                            );
                            ctx.storage_var_nodes.insert((Some(contract_name), var_name), node_id);
                        }
                    }
                    "using_directive_item" => {
                        if let (Some(scope_node), Some(lib_name_node), Some(type_node)) = (
                            captures_map.get("contract_scope_for_using"),
                            captures_map.get("using_library_name"),
                            captures_map.get("using_type_or_wildcard_node"),
                        ) {
                            let contract_name = get_node_text(scope_node, &input.source).to_string();
                            let library_name = get_node_text(lib_name_node, &input.source).to_string();
                            let type_text = get_node_text(type_node, &input.source).to_string();
                            ctx.using_for_directives.entry((Some(contract_name), type_text)).or_default().push(library_name);
                        }
                    }
                    // contract_def_item, interface_def_item, library_def_item were handled in Pass 1
                    "contract_def_item" | "interface_def_item" | "library_def_item" => { /* Already handled */ }
                    _ => {
                        eprintln!("Warning: Unhandled item kind in Pass 2: {}", item_kind_name);
                    }
                }
            }
            matches_pass2.advance();
        }

        // --- Add Default Constructor Nodes ---
        // Iterate through all identified contracts from Pass 1
        for (contract_name, identifier_node_info) in &ctx.all_contracts {
            if !ctx.contracts_with_explicit_constructors.contains(contract_name) {
                let span = identifier_node_info.span;
                let constructor_name = contract_name.clone();
                let _node_id = graph.add_node( // Mark unused
                    constructor_name.clone(),
                    NodeType::Constructor,
                    Some(contract_name.clone()),
                    Visibility::Public,
                    span,
                );
                // No need to add to definition_nodes_info for default constructors as they have no TsNode
            }
        }
        eprintln!("[ContractHandling] Pass 2: Processing complete.");
        Ok(())
    }
}
