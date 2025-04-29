use std::{collections::HashMap, iter};

use crate::{cg::{CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorStep, NodeInfo, NodeType, Visibility}, parser::get_node_text};
use anyhow::{anyhow, Context, Result};
use tree_sitter::{Node as TsNode, Query, QueryCursor};
use streaming_iterator::StreamingIterator;


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

        // --- Pass 1: Find Definitions (Functions, Constructors, Modifiers, State Vars) ---
        // Query to find definitions and their scopes.
        // Captures:
        // @contract_name: Name of the containing contract (optional)
        // @function_name, @modifier_name: Name of the definition
        // @function_def, @modifier_def, @constructor_def: The definition node itself
        // @var_name, @var_type: State variable name and type
        // @contract_identifier_node: The identifier node for any contract declaration
        let definition_query_str = r#"
            ; Contract identifier (captures name node for ALL contracts)
            (contract_declaration
              name: (identifier) @contract_identifier_node @contract_name_for_map
            )

            ; Contract inheritance (captures which contracts inherit from which interfaces/contracts)
            (contract_declaration
              name: (identifier) @contract_name
              (inheritance_specifier
                ancestor: (user_defined_type
                  (identifier) @inherited_name
                )
              )
            )

            ; Interface inheritance (captures which interfaces inherit from which interfaces)
            (interface_declaration
              name: (identifier) @interface_name
              (inheritance_specifier
                ancestor: (user_defined_type
                  (identifier) @inherited_name
                )
              )
            )

            ; Interface inheritance (captures which interfaces inherit from which interfaces)
            (interface_declaration
              name: (identifier) @interface_name
              (inheritance_specifier
                ancestor: (user_defined_type
                  (identifier) @inherited_name
                )
              )
            )

            ; Interface definition
            (interface_declaration
              name: (identifier) @interface_name
            ) @interface_def

            ; Function within an interface
            (interface_declaration
                (identifier) @interface_name ; Interface scope
                (contract_body ; Interfaces use contract_body
                    (function_definition
                        (identifier) @function_name
                        (_)?  ; parameters
                        [(visibility) @visibility_node]?
                    ) @function_def
                )
            )

            ; Library definition
            (library_declaration
                name: (identifier) @library_name
            ) @library_def

            ; Function within a library
            (library_declaration
                (identifier) @library_name ; Library scope
                (contract_body ; Libraries use contract_body
                    (function_definition
                        (identifier) @function_name
                        (_)?  ; parameters
                        [(visibility) @visibility_node]?
                    ) @function_def
                )
            )

            ; Function within a contract
            (contract_declaration
                (identifier) @contract_name
                (contract_body
                    (function_definition 
                        (identifier) @function_name
                        (_)?  ; parameters
                        [(visibility) @visibility_node]?
                    ) @function_def
                )
            )

            ; Modifier within a contract
            (contract_declaration 
                (identifier) @contract_name 
                (contract_body
                    (modifier_definition 
                        (identifier) @modifier_name
                        (_)?  ; parameters
                        [(visibility) @visibility_node]?
                    ) @modifier_def
                )
            )

            ; Constructor within a contract
            (contract_declaration 
                (identifier) @contract_name 
                (contract_body
                    (constructor_definition
                        (_)  ; parameters
                        [(visibility) @visibility_node]?
                    ) @constructor_def
                )
            )

            ; Top-level function (outside contract)
            (source_file
                (function_definition 
                    (identifier) @function_name
                    (_)  ; parameters
                    [(visibility) @visibility_node]?
                ) @function_def
            )

            ; State variable within a contract
            (contract_declaration
                name: (identifier) @contract_identifier_node @contract_name ; Capture node and text
                (contract_body
                    (state_variable_declaration
                        (type_name) @var_type
                        (identifier) @var_name
                    ) @state_var_def ; Capture the whole declaration node
                )
            )

            ; Using directive within a contract
            (contract_declaration
              name: (identifier) @contract_name ; Capture contract scope
              (contract_body
                (using_directive
                  (type_alias (identifier) @using_library_name) ; Library name
                  source: (_) @using_type_or_wildcard_node ; Capture the source node (type_name or '*')
                ) @using_directive_node ; Capture the whole directive if needed
              )
            )
        "#;
        let definition_query = Query::new(&input.solidity_lang, definition_query_str)
            .context("Failed to create definition query")?;

        let root_node = input.tree.root_node();
        let source_bytes = input.source.as_bytes();

        let mut matches =
            definition_cursor.matches(&definition_query, root_node, |node: TsNode| {
                iter::once(&source_bytes[node.byte_range()])
            });

        matches.advance();

        while let Some(match_) = matches.get() {
            // --- Initialize variables for this match ---
            let mut contract_name_opt: Option<String> = None; // For associating definitions (funcs, mods, etc.)
            let mut node_name_opt: Option<String> = None; // For func/mod/constructor name
            let mut node_type_opt: Option<NodeType> = None;
            let mut definition_ts_node_opt: Option<TsNode> = None; // The actual definition node
            let mut visibility_opt: Option<Visibility> = None;
            let mut contract_identifier_node_opt: Option<TsNode> = None; // Node for contract identifier itself
            let mut contract_name_for_map_opt: Option<String> = None; // Name captured specifically for all_contracts map
            let mut interface_name_opt: Option<String> = None; // For interface scope/name
            let mut library_name_opt: Option<String> = None; // For library scope
            let mut library_def_node_opt: Option<TsNode> = None; // Node for library definition (reused for interface def)
            let mut var_name_opt: Option<String> = None; // State variable name
            let mut var_type_opt: Option<String> = None; // State variable type
            let mut state_var_def_node_opt: Option<TsNode> = None; // Node for state var definition
            let mut is_state_var = false; // Flag if the match is for a state variable
            let mut is_library_def = false; // Flag if the match is for a library definition
            let mut is_interface_def = false; // Flag if the match is for an interface definition
            let mut is_using_directive = false; // Flag for using directive
            let mut is_inheritance = false; // Flag for inheritance
            let mut inherited_name_opt: Option<String> = None; // For inherited interface/contract name

            // Captures specific to using directives
            let mut using_library_name_opt: Option<String> = None;
            let mut using_type_or_wildcard_node_opt: Option<TsNode> = None;

            // --- Process captures for this specific match ---
            for capture in match_.captures {
                let capture_name = &definition_query.capture_names()[capture.index as usize];
                let captured_ts_node = capture.node;
                let text = get_node_text(&captured_ts_node, &input.source);

                match capture_name.as_ref() {
                    // Captures specifically for populating the all_contracts map
                    "contract_identifier_node" => {
                        contract_identifier_node_opt = Some(captured_ts_node)
                    }
                    "contract_name_for_map" => contract_name_for_map_opt = Some(text.to_string()),

                    // Captures for identifying definitions (functions, constructors, modifiers, state vars, libraries)
                    "contract_name" => contract_name_opt = Some(text.to_string()), // Contract scope for funcs/mods/ctors/vars
                    "library_name" => library_name_opt = Some(text.to_string()), // Library scope for funcs or the library itself
                    "library_def" => {
                        is_library_def = true;
                        library_def_node_opt = Some(captured_ts_node);
                        // Library name is captured separately by the 'name:' field in the query pattern
                    }
                    "interface_name" => {
                        interface_name_opt = Some(text.to_string());
                        // Also use library_name_opt for scope resolution within interface definitions/functions,
                        // but only if it's not already set (e.g., by a library name).
                        if library_name_opt.is_none() {
                            library_name_opt = Some(text.to_string());
                        }
                    }
                    "interface_def" => {
                        is_interface_def = true;
                        library_def_node_opt = Some(captured_ts_node); // Reuse library_def_node_opt for interface node
                    }
                    "function_name" => node_name_opt = Some(text.to_string()),
                    "modifier_name" => node_name_opt = Some(text.to_string()),
                    "visibility_node" => {
                        visibility_opt = match text {
                            "public" => Some(Visibility::Public),
                            "private" => Some(Visibility::Private),
                            "internal" => Some(Visibility::Internal),
                            "external" => Some(Visibility::External),
                            _ => None, // Default visibility handled later
                        };
                    }
                    "function_def" => {
                        node_type_opt = Some(NodeType::Function);
                        definition_ts_node_opt = Some(captured_ts_node);
                    }
                    "modifier_def" => {
                        node_type_opt = Some(NodeType::Modifier);
                        definition_ts_node_opt = Some(captured_ts_node);
                    }
                    "constructor_def" => {
                        // Constructor name is the contract name
                        node_name_opt = contract_name_opt.clone();
                        node_type_opt = Some(NodeType::Constructor);
                        definition_ts_node_opt = Some(captured_ts_node);
                        // Mark this contract as having an explicit constructor
                        if let Some(c_name) = &contract_name_opt {
                            ctx.contracts_with_explicit_constructors
                                .insert(c_name.clone());
                        }
                    }
                    "var_name" => var_name_opt = Some(text.to_string()),
                    "var_type" => var_type_opt = Some(text.to_string()),
                    "state_var_def" => {
                        is_state_var = true; // Mark that this match is for a state variable
                        state_var_def_node_opt = Some(captured_ts_node); // Capture the definition node
                    }
                    // Captures for using directives
                    "using_library_name" => using_library_name_opt = Some(text.to_string()),
                    "using_type_or_wildcard_node" => {
                        using_type_or_wildcard_node_opt = Some(captured_ts_node)
                    }
                    "using_directive_node" => is_using_directive = true, // Mark as using directive match
                    "inherited_name" => {
                        is_inheritance = true;
                        inherited_name_opt = Some(text.to_string());
                    }

                    _ => {} // Ignore other captures like @call_expr etc. if they exist in this query
                }
            }

            // This block uses the captures specifically designed to find *all* contract names
            // and their identifier nodes, regardless of what else is in the contract.
            if let (Some(name_for_map), Some(identifier_node)) =
                (contract_name_for_map_opt, contract_identifier_node_opt)
            {
                if !ctx.all_contracts.contains_key(&name_for_map) {
                    // Create NodeInfo from TsNode
                    let node_info = NodeInfo {
                        span: (identifier_node.start_byte(), identifier_node.end_byte()),
                        kind: identifier_node.kind().to_string(),
                    };
                    ctx.all_contracts.insert(name_for_map.clone(), node_info); // Store NodeInfo
                }
            }
            // --- Handle interface definition ---
            if is_interface_def {
                // Use the dedicated interface_name_opt here
                if let (Some(interface_name), Some(interface_def_node)) =
                    (interface_name_opt.clone(), library_def_node_opt)
                // Still using library_def_node_opt for the TsNode
                {
                    let span = (
                        interface_def_node.start_byte(),
                        interface_def_node.end_byte(),
                    );
                    // Interfaces don't have visibility in the same way contracts do, use Default
                    let node_id = graph.add_node(
                        interface_name.clone(),
                        NodeType::Interface,
                        Some(interface_name.clone()), // Store interface name for lookup consistency
                        Visibility::Default,          // Interfaces don't have explicit visibility
                        span,
                    );
                    // Create NodeInfo from TsNode
                    let node_info = NodeInfo {
                        span: (interface_def_node.start_byte(), interface_def_node.end_byte()),
                        kind: interface_def_node.kind().to_string(),
                    };
                    // Add interface to the context
                    ctx.all_interfaces
                        .insert(interface_name.clone(), node_info.clone()); // Store NodeInfo clone
                    // Initialize empty vector for interface functions
                    ctx.interface_functions
                        .entry(interface_name.clone())
                        .or_default();
                    // Add interface definition node info
                    ctx.definition_nodes_info.push((
                        node_id,
                        node_info, // Store NodeInfo
                        Some(interface_name), // Pass interface name as scope context
                    ));
                }
            }
            // --- Handle inheritance relationships ---
            if is_inheritance {
                if let (Some(iface_name), Some(inherited_name)) =
                    (interface_name_opt.clone(), inherited_name_opt.clone())
                {
                    // Interface inheriting from another interface/contract
                    ctx.interface_inherits
                        .entry(iface_name)
                        .or_default()
                        .push(inherited_name);
                } else if let (Some(contract_name), Some(inherited_name)) =
                    (contract_name_opt.clone(), inherited_name_opt.clone())
                {
                    // Contract inheriting from interface or contract
                    // Distinguish between implementing an interface and inheriting from a contract/interface
                    // Check if the inherited name is a known interface
                    if ctx.all_interfaces.contains_key(&inherited_name) {
                        // Contract implements an interface
                        ctx.contract_implements
                            .entry(contract_name.clone()) // Use clone for entry key
                            .or_default()
                            .push(inherited_name.clone()); // Clone inherited name
                    }
                    // Store all inheritance relationships (contract -> inherited)
                    ctx.contract_inherits
                        .entry(contract_name) // Use original contract_name
                        .or_default()
                        .push(inherited_name); // Use original inherited_name
                } else {
                    // This case should ideally not happen if the query is correct
                    eprintln!(
                        "Warning: Inheritance detected but no inheriting entity name (contract/interface) found. Inherited: {:?}",
                        inherited_name_opt
                    );
                }
            }
            // Handle state variable definitions
            if is_state_var {
                if let (
                    Some(contract_name),
                    Some(var_name),
                    Some(var_type),
                    Some(state_var_def_node),
                ) = (
                    contract_name_opt.clone(),
                    var_name_opt,
                    var_type_opt,
                    state_var_def_node_opt,
                ) {
                    // Store the type information for later use in call resolution
                    ctx.state_var_types
                        .insert((contract_name.clone(), var_name.clone()), var_type);

                    // Add the StorageVariable node to the graph
                    let span = (
                        state_var_def_node.start_byte(),
                        state_var_def_node.end_byte(),
                    );
                    // Storage variables don't have standard visibility keywords, use Default
                    let node_id = graph.add_node(
                        var_name.clone(),
                        NodeType::StorageVariable,
                        Some(contract_name.clone()), // Associate with the contract
                        Visibility::Default,         // Use default visibility
                        span,
                    );

                    // Store the node ID in the context for lookup during read/write detection
                    let storage_var_key = (Some(contract_name), var_name);
                    ctx.storage_var_nodes.insert(storage_var_key, node_id);

                } else {
                    eprintln!(
                        "Warning: Incomplete capture for state variable definition: {:?}",
                        match_
                            .captures
                            .iter()
                            .map(|c| (
                                &*definition_query.capture_names()[c.index as usize],
                                get_node_text(&c.node, &input.source)
                            ))
                            .collect::<Vec<_>>()
                    );
                }
            } else if is_using_directive {
                if let (Some(contract_name), Some(library_name), Some(type_or_wildcard_node)) = (
                    contract_name_opt.clone(), // Need the contract scope
                    using_library_name_opt,
                    using_type_or_wildcard_node_opt,
                ) {
                    let type_or_wildcard_text =
                        get_node_text(&type_or_wildcard_node, &input.source).to_string();

                    // The key is (Scope, TypeOrWildcard)
                    // Scope is Some(ContractName) for contract-level using directives
                    let key = (Some(contract_name), type_or_wildcard_text);

                    // Add the library to the vector for this key
                    ctx.using_for_directives
                        .entry(key)
                        .or_default()
                        .push(library_name);
                } else {
                    eprintln!(
                        "Warning: Incomplete capture for using directive: {:?}",
                        match_
                            .captures
                            .iter()
                            .map(|c| (
                                &*definition_query.capture_names()[c.index as usize],
                                get_node_text(&c.node, &input.source)
                            ))
                            .collect::<Vec<_>>()
                    );
                }
            } else if is_library_def {
                if let (Some(lib_name), Some(lib_def_node)) =
                    (library_name_opt.clone(), library_def_node_opt)
                {
                    let span = (lib_def_node.start_byte(), lib_def_node.end_byte());
                    // Libraries don't have visibility in the same way contracts do, use Default
                    // We use the library name as the node name and also store it in the 'contract_name' field
                    // conceptually, similar to how constructors use the contract name.
                    let node_id = graph.add_node(
                        lib_name.clone(),
                        NodeType::Library,
                        Some(lib_name.clone()), // Store library name here for lookup consistency
                        Visibility::Default, // Libraries don't have explicit visibility like functions
                        span,
                    );
                    // Create NodeInfo from TsNode
                    let node_info = NodeInfo {
                        span: (lib_def_node.start_byte(), lib_def_node.end_byte()),
                        kind: lib_def_node.kind().to_string(),
                    };
                    // Add library definition node info
                    ctx.definition_nodes_info
                        .push((node_id, node_info.clone(), Some(lib_name.clone()))); // Store NodeInfo clone
                    // Add library to the context map
                    ctx.all_libraries.insert(lib_name, node_info); // Store NodeInfo
                } else {
                    eprintln!(
                        "Warning: Incomplete capture for library definition: {:?}",
                        match_
                            .captures
                            .iter()
                            .map(|c| (
                                &*definition_query.capture_names()[c.index as usize],
                                get_node_text(&c.node, &input.source)
                            ))
                            .collect::<Vec<_>>()
                    );
                }
            // Handle function/modifier/constructor definitions (within contracts, libraries, OR interfaces)
            } else if let (Some(name), Some(type_), Some(def_node)) =
                (node_name_opt, node_type_opt, definition_ts_node_opt)
            {
                // Determine the scope: prioritize interface, then contract, then library
                // Use the interface_name_opt captured specifically for this match if available.
                let scope_name_opt = interface_name_opt
                    .clone() // Use interface name if captured for this match
                    .or_else(|| contract_name_opt.clone()) // Fallback to contract name
                    .or_else(|| library_name_opt.clone()); // Fallback to library name

                let visibility = visibility_opt.unwrap_or_else(|| match type_ {
                    NodeType::Constructor => Visibility::Public, // Only in contracts
                    NodeType::Function => {
                        // Functions in libraries are implicitly internal if no visibility specified
                        // Functions in contracts default to public (before 0.5.0) or internal (0.5.0+)
                        // We assume modern Solidity defaults (internal for contract funcs, always internal for library funcs)
                        // Top-level functions default to internal (as per solc docs, though often implicitly public in practice?)
                        // Let's stick to internal as the safe default if unspecified.
                        Visibility::Internal
                    }
                    NodeType::Modifier => Visibility::Internal, // Only in contracts
                    NodeType::Library
                    | NodeType::Interface
                    | NodeType::Evm
                    | NodeType::EventListener | NodeType::StorageVariable => Visibility::Default, // Should not happen here
                });

                let span = (def_node.start_byte(), def_node.end_byte());
                let node_id = graph.add_node(
                    name.clone(),
                    type_,
                    scope_name_opt.clone(),
                    visibility,
                    span,
                );
                // Create NodeInfo from TsNode
                let node_info = NodeInfo {
                    span: (def_node.start_byte(), def_node.end_byte()),
                    kind: def_node.kind().to_string(),
                };
                // Pass the correct scope (contract or library name) to definition_nodes_info
                ctx.definition_nodes_info
                    .push((node_id, node_info, scope_name_opt.clone())); // Store NodeInfo
                // If this is a function in an interface, add it to the interface_functions map
                if let Some(scope_name) = &scope_name_opt {
                    // Check all_interfaces which now stores NodeInfo
                    if ctx.all_interfaces.contains_key(scope_name) {
                        ctx.interface_functions
                            .entry(scope_name.clone())
                            .or_default()
                            .push(name);
                    }
                }
            // Note: Interface definitions are handled separately above and won't reach here.
            // Library definitions are handled by `is_library_def`.
            // State variables are handled by `is_state_var`.
            // Using directives are handled by `is_using_directive`.
            // Functions/Modifiers/Constructors are handled by the `else if let` above.
            } else if contract_identifier_node_opt.is_some() {
                // This branch now specifically handles the contract identifier match used
                // only for populating ctx.all_contracts, which doesn't need further processing here.
                // No warning needed.
            } else {
                // This case should only be reached for truly unexpected query matches
                // or potentially the top-level source_file match if not filtered.
                // Avoid warning for state vars (already handled) and contract identifiers (handled above).
                if !is_state_var {
                    eprintln!(
                        "Warning: Unhandled query match or incomplete capture: {:?}",
                        match_
                            .captures
                            .iter()
                            .map(|c| (
                                &*definition_query.capture_names()[c.index as usize],
                                get_node_text(&c.node, &input.source)
                            ))
                            .collect::<Vec<_>>()
                    );
                }
            }
            matches.advance();
        }

        // --- Add Default Constructor Nodes ---
        // Iterate through all identified contracts
        for (contract_name, identifier_node) in &ctx.all_contracts {
            // Check if this contract already has an explicit constructor recorded
            if !ctx
                .contracts_with_explicit_constructors
                .contains(contract_name)
            {
                // If not, add a default public constructor node
                let span = identifier_node.span; // Use contract identifier span
                let constructor_name = contract_name.clone(); // Explicitly define constructor name for clarity
                let node_id = graph.add_node(
                    constructor_name.clone(),    // Constructor name is the contract name
                    NodeType::Constructor,       // Type is Constructor
                    Some(contract_name.clone()), // Belongs to this contract
                    Visibility::Public,          // Default constructors are public
                    span,                        // Span of the contract identifier
                );
                // Log the details AND the key that should be inserted by add_node
                let _lookup_key = (Some(contract_name.clone()), constructor_name.clone());
                // Mark as unused
            }
        }

        Ok(())
    }
}


