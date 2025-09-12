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
use tracing::debug;

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
                    ; Capture the whole state_variable_declaration node
                    ; Type, name, and visibility will be extracted from its children
                    (state_variable_declaration) @state_var_node_capture
                )
            ) @state_var_item

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
        debug!("[ContractHandling] Pass 1: Identifying Contracts, Interfaces, Libraries...");
        let mut matches_pass1 =
            definition_cursor.matches(&definition_query, root_node, |node: TsNode| {
                iter::once(&source_bytes[node.byte_range()])
            });
        matches_pass1.advance();
        while let Some(match_) = matches_pass1.get() {
            for capture in match_.captures {
                let capture_name = &definition_query.capture_names()[capture.index as usize];
                let captured_ts_node = capture.node;
                let _text = get_node_text(&captured_ts_node, &input.source);

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
        debug!("[ContractHandling] Pass 1: Found {} contracts, {} interfaces, {} libraries.", ctx.all_contracts.len(), ctx.all_interfaces.len(), ctx.all_libraries.len());

        // --- Pass 2: Process members, inheritance, and other relationships ---
        debug!("[ContractHandling] Pass 2: Processing members and relationships...");
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
                                debug!("[ContractHandling] Contract '{}' implements interface '{}'", contract_name, inherited_name);
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
                            debug!("Warning: Empty name for {:?} at span {:?}", node_type, def_node.byte_range());
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
                        // Extract and store parameters for this node
                        let params = crate::cg::extract_function_parameters(*def_node, &input.source);
                        if let Some(graph_node_mut) = graph.nodes.get_mut(node_id) {
                            graph_node_mut.parameters = params;
                        }

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
                    "state_var_item" => {
                        let state_var_decl_node = captures_map.get("state_var_node_capture")
                            .ok_or_else(|| anyhow!("state_var_item missing state_var_node_capture capture"))?;
                        let contract_name_node = captures_map.get("contract_scope_for_var")
                            .ok_or_else(|| anyhow!("state_var_item missing contract_scope_for_var capture"))?;
                        let contract_name = get_node_text(contract_name_node, &input.source).to_string();

                        // Extract type, name, and visibility from children of state_var_decl_node
                        let type_node_opt = state_var_decl_node.child_by_field_name("type");
                        let name_node_opt = state_var_decl_node.child_by_field_name("name");
                        let mut visibility_text_opt: Option<String> = None;

                        let mut child_cursor = state_var_decl_node.walk();
                        for child in state_var_decl_node.children(&mut child_cursor) {
                            if child.kind() == "visibility" {
                                visibility_text_opt = Some(get_node_text(&child, &input.source).to_string());
                                break;
                            }
                        }

                        if let (Some(var_type_node), Some(var_name_node)) = (type_node_opt, name_node_opt) {
                            let var_name_str = get_node_text(&var_name_node, &input.source).to_string();
                            let visibility = match visibility_text_opt.as_deref() {
                                Some("public") => Visibility::Public,
                                Some("internal") => Visibility::Internal,
                                Some("private") => Visibility::Private,
                                _ => Visibility::Internal, // Default visibility for state variables
                            };


                                let mut extracted_key_types = Vec::new();
                                // Call parse_mapping_recursive. It will return the final value type,
                                // the full type string, and a boolean indicating if it was a mapping.
                                match parse_mapping_recursive(var_type_node, &input.source, &mut extracted_key_types) {
                                    Ok((final_value_type, full_type_str, is_mapping)) => {
                                        // Always store the full type string in state_var_types
                                        ctx.state_var_types.insert((contract_name.clone(), var_name_str.clone()), full_type_str.clone());
                                        debug!("[ContractHandling DEBUG] Adding to state_var_types (any type): Key=({}, {}), Value={}", contract_name, var_name_str, full_type_str);

                                        if is_mapping {
                                            // If it was a mapping, populate contract_mappings
                                            let mapping_info = crate::cg::MappingInfo {
                                                name: var_name_str.clone(),
                                                visibility: visibility.clone(),
                                                key_types: extracted_key_types, // These are filled by parse_mapping_recursive
                                                value_type: final_value_type, // This is the ultimate value type from parse_mapping_recursive
                                                span: (state_var_decl_node.start_byte(), state_var_decl_node.end_byte()),
                                                full_type_str: full_type_str.clone(), // This is the full "mapping(...)" string
                                            };
                                            ctx.contract_mappings.insert((contract_name.clone(), var_name_str.clone()), mapping_info.clone());
                                            debug!("[ContractHandling] Added mapping info for {}.{}: Name='{}', Visibility='{:?}', Keys='{:?}', ValueType='{}', FullType='{}'",
                                                contract_name, var_name_str,
                                                mapping_info.name, mapping_info.visibility, mapping_info.key_types, mapping_info.value_type, mapping_info.full_type_str);
                                        } else {
                                            // If it wasn't a mapping, full_type_str is just the simple type string.
                                            // state_var_types is already populated above. No need to populate contract_mappings.
                                            debug!("[ContractHandling] State variable {}.{} is not a mapping. Type: {}", contract_name, var_name_str, full_type_str);
                                        }
                                    }
                                    Err(e) => {
                                        debug!("Error parsing type for {}.{}: {}", contract_name, var_name_str, e);
                                        // Fallback: store the raw text if parsing fails, though parse_mapping_recursive should handle non-mappings gracefully.
                                        let raw_type_str = get_node_text(&var_type_node, &input.source).to_string();
                                        ctx.state_var_types.insert((contract_name.clone(), var_name_str.clone()), raw_type_str);

                                    }
                                }
                                // The `else` block for non-mapping_type kinds is removed because
                                // parse_mapping_recursive now handles all type_name nodes.

                            // Add node to graph for all state variables (mapping or not)
                            let node_id = graph.add_node(
                                var_name_str.clone(),
                                NodeType::StorageVariable,
                                Some(contract_name.clone()),
                                visibility, // Use parsed visibility
                                (state_var_decl_node.start_byte(), state_var_decl_node.end_byte()),
                            );
                            ctx.storage_var_nodes.insert((Some(contract_name), var_name_str), node_id);

                        } else {
                             debug!("Warning: Could not extract type or name for state variable in contract '{}' at span {:?}. Type found: {}, Name found: {}", 
                                contract_name, 
                                state_var_decl_node.byte_range(),
                                type_node_opt.is_some(),
                                name_node_opt.is_some()
                             );
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
                        debug!("Warning: Unhandled item kind in Pass 2: {}", item_kind_name);
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
        debug!("[ContractHandling] Pass 2: Processing complete.");
        Ok(())
    }

}

// Helper function to recursively parse mapping types
fn parse_mapping_recursive(
    current_node: TsNode, // This is a type_name node
    source: &str,
    key_types: &mut Vec<String>, // Accumulates key types
) -> Result<(String, String, bool)> { // Returns (final_value_type, full_type_string, is_mapping_flag)

    // Check if the current_node (a type_name node) represents a mapping
    // by looking for 'key_type' and 'value_type' fields, as per CST.
    if let (Some(key_type_field_node), Some(value_type_field_node)) = (
        current_node.child_by_field_name("key_type"),
        current_node.child_by_field_name("value_type"),
    ) {
        // current_node is a mapping.
        // key_type_field_node is the node for the key's type (e.g., primitive_type, user_defined_type).
        // Its text is the key type string.
        let key_type_str = get_node_text(&key_type_field_node, source).to_string();
        key_types.push(key_type_str.clone());

        // value_type_field_node is a type_name node for the value.
        // Recursively parse this value_type_field_node.
        let (final_value_type, nested_value_str, _is_nested_mapping) =
            parse_mapping_recursive(value_type_field_node, source, key_types)?;
        
        let current_level_full_str = format!("mapping({} => {})", key_type_str, nested_value_str);
        return Ok((final_value_type, current_level_full_str, true));
    }

    // Base case: current_node is not a mapping (it's a primitive, user_defined_type, array_type, etc.)
    // It doesn't have 'key_type' and 'value_type' fields.
    let type_str = get_node_text(&current_node, source).to_string();
    Ok((type_str.clone(), type_str, false))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cg::{
            CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, Visibility,
        };
    use crate::parser::get_solidity_language;
    
    use tree_sitter::Parser;

    fn run_contract_handling(
        source_code: &str,
    ) -> Result<(CallGraph, CallGraphGeneratorContext)> {
        let mut parser = Parser::new();
        let sol_lang = get_solidity_language();
        parser
            .set_language(&sol_lang) // Use the fetched language
            .expect("Error loading Solidity grammar");
        let tree = parser
            .parse(source_code, None)
            .context("Failed to parse source code")?;
        let mut graph = CallGraph::new();
        let mut ctx = CallGraphGeneratorContext::default();
        let contract_handler = ContractHandling::default();

        let input = CallGraphGeneratorInput {
            source: source_code.to_string(),
            tree,
            solidity_lang: sol_lang.clone(), // Use the fetched language here
            // Add other necessary fields if they become required by ContractHandling
        };

        contract_handler.generate(input, &mut ctx, &mut graph)?;
        Ok((graph, ctx))
    }

    #[test]
    fn test_mapping_state_variables() -> Result<()> {
        let source_code = r#"
            contract TestMappings {
                mapping(address => uint) public balanceOf;
                mapping(address => mapping(address => uint)) public allowance;
                mapping(bytes32 => mapping(uint256 => mapping(address => bool))) internal nestedMap;
                mapping(address => UserStruct) userInfos; // Assuming UserStruct is defined elsewhere or not relevant for type string
                struct UserStruct { uint id; }
            }
        "#;

        let (_graph, ctx) = run_contract_handling(source_code)?;

        // Check state_var_types
        assert_eq!(
            ctx.state_var_types
                .get(&("TestMappings".to_string(), "balanceOf".to_string())),
            Some(&"mapping(address => uint)".to_string())
        );
        assert_eq!(
            ctx.state_var_types
                .get(&("TestMappings".to_string(), "allowance".to_string())),
            Some(&"mapping(address => mapping(address => uint))".to_string())
        );
        assert_eq!(
            ctx.state_var_types
                .get(&("TestMappings".to_string(), "nestedMap".to_string())),
            Some(&"mapping(bytes32 => mapping(uint256 => mapping(address => bool)))".to_string())
        );
        assert_eq!(
            ctx.state_var_types
                .get(&("TestMappings".to_string(), "userInfos".to_string())),
            Some(&"mapping(address => UserStruct)".to_string()) // This will be captured as string
        );


        // Check contract_mappings for balanceOf
        let balance_of_key = ("TestMappings".to_string(), "balanceOf".to_string());
        let balance_of_info = ctx.contract_mappings.get(&balance_of_key).unwrap();
        assert_eq!(balance_of_info.name, "balanceOf");
        assert_eq!(balance_of_info.visibility, Visibility::Public);
        assert_eq!(balance_of_info.key_types, vec!["address".to_string()]);
        assert_eq!(balance_of_info.value_type, "uint".to_string());
        assert_eq!(balance_of_info.full_type_str, "mapping(address => uint)");

        // Check contract_mappings for allowance
        let allowance_key = ("TestMappings".to_string(), "allowance".to_string());
        let allowance_info = ctx.contract_mappings.get(&allowance_key).unwrap();
        assert_eq!(allowance_info.name, "allowance");
        assert_eq!(allowance_info.visibility, Visibility::Public);
        assert_eq!(
            allowance_info.key_types,
            vec!["address".to_string(), "address".to_string()]
        );
        assert_eq!(allowance_info.value_type, "uint".to_string());
        assert_eq!(
            allowance_info.full_type_str,
            "mapping(address => mapping(address => uint))"
        );

        // Check contract_mappings for nestedMap
        let nested_map_key = ("TestMappings".to_string(), "nestedMap".to_string());
        let nested_map_info = ctx.contract_mappings.get(&nested_map_key).unwrap();
        assert_eq!(nested_map_info.name, "nestedMap");
        assert_eq!(nested_map_info.visibility, Visibility::Internal); // Based on 'internal' keyword
        assert_eq!(
            nested_map_info.key_types,
            vec!["bytes32".to_string(), "uint256".to_string(), "address".to_string()]
        );
        assert_eq!(nested_map_info.value_type, "bool".to_string());
        assert_eq!(
            nested_map_info.full_type_str,
            "mapping(bytes32 => mapping(uint256 => mapping(address => bool)))"
        );
        
        // Check contract_mappings for userInfos (mapping to a struct)
        let user_infos_key = ("TestMappings".to_string(), "userInfos".to_string());
        let user_infos_info = ctx.contract_mappings.get(&user_infos_key).unwrap();
        assert_eq!(user_infos_info.name, "userInfos");
        assert_eq!(user_infos_info.visibility, Visibility::Internal); // Default for state vars without explicit visibility
        assert_eq!(user_infos_info.key_types, vec!["address".to_string()]);
        assert_eq!(user_infos_info.value_type, "UserStruct".to_string());
        assert_eq!(user_infos_info.full_type_str, "mapping(address => UserStruct)");


        Ok(())
    }
}
