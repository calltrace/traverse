use crate::cg_dot;
use crate::parser::{get_node_text, SolidityAST};
use anyhow::{anyhow, Context, Result}; // Add anyhow!
use language::{Language, Solidity};
use std::collections::{HashMap, HashSet}; // Import HashSet
use std::iter;
use std::marker::PhantomData;
use streaming_iterator::StreamingIterator;
use tree_sitter::{Node as TsNode, Query, QueryCursor, Tree};

// Constants for synthetic node names
pub(crate) const EVM_NODE_NAME: &str = "EVM";
pub(crate) const EVENT_LISTENER_NODE_NAME: &str = "EventListener";

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum EdgeType {
    Call,
    Return,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum NodeType {
    Function,
    Constructor,
    Modifier,
    Library,       // Added Library type
    Interface,     // Added Interface type
    Evm,           // Synthetic node for EVM interaction
    EventListener, // Synthetic node for event listeners
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Visibility {
    Public,
    Private,
    Internal,
    External,
    Default,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub id: usize,
    pub name: String,
    pub node_type: NodeType,
    pub contract_name: Option<String>,
    pub visibility: Visibility,
    pub span: (usize, usize),
    pub has_explicit_return: bool, // Added flag
}

// --- DOT Label Implementation ---

impl crate::cg_dot::ToDotLabel for Node {
    fn to_dot_label(&self) -> String {
        format!(
            "{}{}\n({})", // Use a literal newline character here
            self.contract_name
                .as_deref()
                .map(|c| format!("{}.", c))
                .unwrap_or_default(),
            self.name,
            format!("{:?}", self.node_type).to_lowercase()
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Edge {
    pub source_node_id: usize,
    pub target_node_id: usize,
    pub edge_type: EdgeType,
    pub call_site_span: (usize, usize),
    pub return_site_span: Option<(usize, usize)>,
    pub sequence_number: usize,
    pub returned_value: Option<String>,
    pub argument_names: Option<Vec<String>>, // Added: Store argument names/texts
    pub event_name: Option<String>,          // Added: Store event name for emits
}

impl crate::cg_dot::ToDotLabel for Edge {
    fn to_dot_label(&self) -> String {
        // Default label based on type, can be overridden in cg_dot.rs formatter
        match self.edge_type {
            EdgeType::Call => self.sequence_number.to_string(),
            EdgeType::Return => "ret".to_string(),
        }
    }
}

// --- Call Graph ---

#[derive(Debug, Clone, Default)]
pub struct CallGraph {
    pub nodes: Vec<Node>,
    pub edges: Vec<Edge>,
    pub(crate) node_lookup: HashMap<(Option<String>, String), usize>,
}

impl CallGraph {
    pub fn new() -> Self {
        Default::default()
    }

    pub(crate) fn add_node(
        &mut self,
        name: String,
        node_type: NodeType,
        contract_name: Option<String>,
        visibility: Visibility,
        span: (usize, usize),
    ) -> usize {
        let id = self.nodes.len();
        let node = Node {
            id,
            name: name.clone(),
            node_type,
            contract_name: contract_name.clone(),
            visibility,
            span,
            has_explicit_return: false,
        };
        self.nodes.push(node);

        self.node_lookup.insert((contract_name, name), id);
        id
    }

    pub(crate) fn add_edge(
        &mut self,
        source_node_id: usize,
        target_node_id: usize,
        edge_type: EdgeType,                      // Added type
        call_site_span: (usize, usize),           // Span of the call site (or function for return)
        return_site_span: Option<(usize, usize)>, // Span of the return site
        sequence_number: usize,                   // Sequence for calls, maybe 0 for returns
        returned_value: Option<String>,           // Added returned value
        argument_names: Option<Vec<String>>,      // Added: Argument names/texts
        event_name: Option<String>,               // Added: Event name for emits
    ) {
        // --- DEBUG: Log every edge addition attempt ---
        eprintln!(
            "[DEBUG add_edge] Attempting to add edge: {} -> {} (Type: {:?}, Seq: {}, RetVal: {:?}, Args: {:?})", // Added Args to log
            source_node_id, target_node_id, edge_type, sequence_number, returned_value, argument_names
        );
        // --- END DEBUG ---
        let edge = Edge {
            source_node_id,
            target_node_id,
            edge_type, // Store type
            call_site_span,
            return_site_span, // Store return span
            sequence_number,
            returned_value, // Store returned value
            argument_names, // Store argument names
            event_name,
        };
        self.edges.push(edge);
    }

    /// Finds a node ID based on its name and potential contract scope.
    /// Resolution logic:
    /// 1. Look for the name within the `current_contract` scope.
    /// 2. Look for the name as a free function (no contract scope).
    /// 3. **Fallback:** Search across all known contract scopes.
    /// TODO: Enhance resolution with proper type analysis for member access, inheritance, imports, etc.
    ///       This current fallback is a simplification and may be ambiguous if multiple

    pub fn iter_nodes(&self) -> impl Iterator<Item = &Node> {
        self.nodes.iter()
    }

    pub fn iter_edges(&self) -> impl Iterator<Item = &Edge> {
        self.edges.iter()
    }

    // Remove 'a, change ctx type, add input parameter
    pub fn add_explicit_return_edges(
        &mut self,
        input: &CallGraphGeneratorInput, // Add input parameter containing source, tree, lang
        ctx: &CallGraphGeneratorContext, // Remove 'a
    ) -> Result<()> {
        // --- DEBUG: Check edges originating from Node 4 at the start ---
        eprintln!(
            "[Address Debug add_explicit_return_edges START] Graph: {:p}, Total Edges: {}",
            self,
            self.edges.len()
        );
        let mut found_node4_edges = false;
        for (idx, edge) in self.edges.iter().enumerate() {
            if edge.source_node_id == 4 {
                // Node ID for UniswapV2ERC20._transfer
                let target_node_name =
                    self.nodes
                        .get(edge.target_node_id)
                        .map_or("?".to_string(), |n| {
                            format!(
                                "{}.{}",
                                n.contract_name.as_deref().unwrap_or("Global"),
                                n.name
                            )
                        });
                eprintln!(
                    "[DEBUG Node 4 Edge Check START] Edge Index {}: {} -> {} (Target: '{}'), Type: {:?}, Seq: {}",
                    idx, edge.source_node_id, edge.target_node_id, target_node_name, edge.edge_type, edge.sequence_number
                );
                found_node4_edges = true;
            }
        }
        if !found_node4_edges {
            eprintln!("[DEBUG Node 4 Edge Check START] No edges found originating from Node 4.");
        }
        // --- END DEBUG ---

        // 1. Build a map of callee -> Vec<(caller_id, call_sequence_number)> from existing Call edges
        let mut callee_to_callers_and_seq: HashMap<usize, Vec<(usize, usize)>> = HashMap::new();
        for edge in self.edges.iter() {
            if edge.edge_type == EdgeType::Call {
                callee_to_callers_and_seq
                    .entry(edge.target_node_id)
                    .or_default()
                    .push((edge.source_node_id, edge.sequence_number)); // Store caller and sequence
            }
        }

        // 2. Prepare return statement query (Revised)
        // This query looks for return statements possibly containing an expression.
        let return_query_str = r#"
            (return_statement
                (expression)? @return_value ; Optional: capture the returned expression node
            ) @return ; Capture the whole return statement node
        "#;
        // Use language from input
        let return_query = Query::new(&input.solidity_lang, return_query_str)
            .context("Failed to create return statement query")?;
        let mut return_cursor = QueryCursor::new();
        // Use source from input
        let source_bytes = input.source.as_bytes();

        // 3. Iterate through definition nodes to find return statements within them
        let mut new_return_edges: Vec<Edge> = Vec::new(); // Collect new edges separately
        let mut total_returns_found_by_query = 0; // DEBUG
        let mut total_returns_processed = 0; // DEBUG
        let mut _nodes_with_explicit_return_set = 0; // DEBUG - Mark as unused for now

        // Iterate through the definition nodes stored in the context (contains NodeInfo)
        for (callee_node_id, callee_node_info, _caller_contract_name_opt) in
            &ctx.definition_nodes_info
        {
            // Get the actual TsNode for the callee's definition using span from NodeInfo
            let definition_ts_node = input.tree.root_node()
                .descendant_for_byte_range(callee_node_info.span.0, callee_node_info.span.1)
                .ok_or_else(|| anyhow!("Failed to find definition TsNode for span {:?} in add_explicit_return_edges", callee_node_info.span))?;

            // Retrieve the corresponding Node struct from the graph
            // Need mutable access to set the flag
            let callee_node_exists = self.nodes.get(*callee_node_id).is_some();
            if !callee_node_exists {
                eprintln!(
                    "Warning: Node ID {} found in definition_nodes_info but not in graph.nodes",
                    callee_node_id
                );
                continue; // Skip if node not found in graph
            }
            // We'll get the node again inside the match block where we need it

            // --- DEBUG: Log callee being processed ---
            // Use immutable borrow here for logging before potential mutable borrow
            if let Some(callee_node_for_log) = self.nodes.get(*callee_node_id) {
                eprintln!(
                    "[DEBUG Returns] Processing callee: Node ID {}, Name: {}.{}, Type: {:?}",
                    callee_node_for_log.id,
                    callee_node_for_log
                        .contract_name
                        .as_deref()
                        .unwrap_or("Global"),
                    callee_node_for_log.name,
                    callee_node_for_log.node_type
                );
            }
            // --- END DEBUG ---

            // --- DEBUG: Print S-expression for a specific node (e.g., UniswapV2Pair.mint, ID 21) ---
            if *callee_node_id == 21 {
                eprintln!(
                    "[DEBUG AST Structure] S-expression for Node ID {}:\n{}",
                    callee_node_id,
                    definition_ts_node.to_sexp() // Use retrieved node
                );
            }
            // --- END DEBUG ---

            // Check node type using an immutable borrow first
            let node_type_for_check = self.nodes[*callee_node_id].node_type.clone(); // Clone needed if used later

            // Only look for returns inside functions/modifiers/constructors/libraries
            match node_type_for_check {
                // Use the cloned type
                NodeType::Function
                | NodeType::Modifier
                | NodeType::Constructor
                | NodeType::Library => {
                    // Interfaces don't have implementation/returns

                    // --- Check for *any* return statement to set the flag ---
                    // This check runs regardless of whether there are callers
                    let mut return_check_matches = return_cursor.matches(
                        &return_query,
                        definition_ts_node, // Use retrieved node
                        |node: TsNode| iter::once(&source_bytes[node.byte_range()]),
                    );
                    return_check_matches.advance(); // Advance once
                    if let Some(_) = return_check_matches.get() {
                        // Found at least one return statement, set the flag
                        if let Some(node_mut) = self.nodes.get_mut(*callee_node_id) {
                            if !node_mut.has_explicit_return {
                                // Avoid redundant writes/logs
                                node_mut.has_explicit_return = true;
                                _nodes_with_explicit_return_set += 1; // DEBUG - Use underscore prefix
                                eprintln!(
                                     "[DEBUG Returns Flag] Set has_explicit_return=true for Node ID {}",
                                     *callee_node_id
                                );
                            }
                        }
                    }
                    // --- End return flag check ---

                    // Find callers and their call sequences for this callee
                    if let Some(callers_info) = callee_to_callers_and_seq.get(callee_node_id) {
                        // Use callee_node_id directly
                        // Query for returns *only within this definition_ts_node*.
                        let mut matches = return_cursor.matches(
                            &return_query,
                            definition_ts_node, // Use retrieved node
                            |node: TsNode| iter::once(&source_bytes[node.byte_range()]),
                        );
                        matches.advance(); // Need to advance once initially

                        while let Some(match_) = matches.get() {
                            total_returns_found_by_query += 1; // DEBUG Increment count for each match found

                            let mut return_node_opt: Option<TsNode> = None;
                            let mut return_value_node_opt: Option<TsNode> = None;

                            // Find the @return and @return_value captures for this match
                            for capture in match_.captures {
                                let capture_name =
                                    &return_query.capture_names()[capture.index as usize];
                                match capture_name.as_ref() {
                                    "return" => return_node_opt = Some(capture.node),
                                    "return_value" => return_value_node_opt = Some(capture.node),
                                    _ => {}
                                }
                            }

                            if let Some(return_node) = return_node_opt {
                                total_returns_processed += 1; // DEBUG Increment count for processed returns
                                let return_span =
                                    (return_node.start_byte(), return_node.end_byte());
                                let return_kind = return_node.kind(); // DEBUG Get node kind

                                // --- DEBUG: Log return found by query ---
                                eprint!(
                                    "[DEBUG Returns]   Found return statement within definition: Kind='{}', Span={:?}. ",
                                    return_kind, return_span
                                );
                                // --- END DEBUG ---

                                // Query is scoped, so it's accepted
                                eprintln!(" => ACCEPTED (within definition node)");

                                // Extract the text of the returned value, if present
                                let returned_value_text = return_value_node_opt
                                    .map(|n| get_node_text(&n, &input.source).to_string()); // Use input.source

                                // Add return edges from this callee back to all its callers
                                for (caller_id, call_sequence) in callers_info {
                                    // Need immutable borrow of callee node here for its span
                                    let callee_node_span = self.nodes[*callee_node_id].span;
                                    new_return_edges.push(Edge {
                                        source_node_id: *callee_node_id, // Return starts from callee
                                        target_node_id: *caller_id,      // Return goes to caller
                                        edge_type: EdgeType::Return,
                                        call_site_span: callee_node_span, // Span of the function definition itself
                                        return_site_span: Some(return_span), // Span of the return statement
                                        sequence_number: *call_sequence, // Use the sequence number from the original call
                                        returned_value: returned_value_text.clone(), // Add the returned value text
                                        argument_names: None, // Return edges don't have call arguments
                                        event_name: None,
                                    });
                                }
                            } else {
                                // This case should not happen if the query is correct and finds a match
                                eprintln!("[DEBUG Returns]   Warning: Query matched but failed to extract @return capture.");
                            }
                            matches.advance(); // Advance after processing captures for the current match
                        }
                    }
                }
                _ => {} // Ignore other node types like Interface, ContractDefinition etc.
            }
        }

        // 4. Add the collected return edges to the graph
        eprintln!(
            "[DEBUG Returns Summary] Total returns found by query: {}", // DEBUG Summary
            total_returns_found_by_query
        );
        eprintln!(
            "[DEBUG Returns Summary] Total returns processed (found within definition): {}", // DEBUG Summary
            total_returns_processed
        );
        eprintln!(
            "[DEBUG Returns Summary] Total return edges generated: {}", // DEBUG Summary
            new_return_edges.len()
        );
        eprintln!(
            "[DEBUG add_explicit_return_edges] Edge count BEFORE extend: {}",
            self.edges.len()
        );
        eprintln!(
            "[Address Debug add_explicit_return_edges] Graph: {:p}",
            self
        ); // Added address log
        self.edges.extend(new_return_edges);
        eprintln!(
            "[DEBUG add_explicit_return_edges] Edge count AFTER extend: {}",
            self.edges.len()
        );

        Ok(())
    }
}

#[derive(Debug)]
pub struct CallGraphGeneratorInput {
    pub source: String,
    pub tree: Tree,
    pub solidity_lang: tree_sitter::Language,
}

// Implement Clone manually to ensure proper ownership transfer
impl Clone for CallGraphGeneratorInput {
    fn clone(&self) -> Self {
        Self {
            source: self.source.clone(),
            tree: self.tree.clone(),
            solidity_lang: self.solidity_lang.clone(),
        }
    }
}

/// Stores lifetime-independent information about a tree-sitter node.
#[derive(Debug, Clone)]
pub struct NodeInfo {
    pub span: (usize, usize),
    pub kind: String,
    // Add other relevant lifetime-independent info if needed (e.g., text?)
}


// Remove 'a lifetime - Context now stores NodeInfo which owns its data
#[derive(Debug, Clone, Default)] // Add Default
pub struct CallGraphGeneratorContext {
    pub state_var_types: HashMap<(String, String), String>,
    pub using_for_directives: HashMap<(Option<String>, String), Vec<String>>,
    // Store NodeInfo instead of TsNode<'a>
    pub definition_nodes_info: Vec<(usize, NodeInfo, Option<String>)>,
    pub all_contracts: HashMap<String, NodeInfo>,
    pub contracts_with_explicit_constructors: HashSet<String>,
    pub all_libraries: HashMap<String, NodeInfo>,
    pub all_interfaces: HashMap<String, NodeInfo>,
    pub interface_functions: HashMap<String, Vec<String>>,
    pub contract_implements: HashMap<String, Vec<String>>,
    pub interface_inherits: HashMap<String, Vec<String>>,
}


pub trait CallGraphGeneratorStep {
    fn name(&self) -> &'static str;
    /// Configure the step with settings.
    fn config(&mut self, config: &HashMap<String, String>);
    /// Generate part of the call graph.
    fn generate(
        &self,
        input: CallGraphGeneratorInput,
        ctx: &mut CallGraphGeneratorContext, // Remove 'a
        graph: &mut CallGraph,
    ) -> Result<()>;
}

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
                    ) @state_var_def ; Capture the whole declaration for context if needed
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
            let mut is_state_var = false; // Flag if the match is for a state variable
            let mut is_library_def = false; // Flag if the match is for a library definition
            let mut is_interface_def = false; // Flag if the match is for an interface definition
                                              // Removed duplicate is_state_var and is_library_def flags
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
                    "state_var_def" => is_state_var = true, // Mark that this match is for a state variable
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
                    // Assume contract only implements interfaces for now, but could inherit contracts too.
                    // Storing in contract_implements, might need refinement if base contracts are tracked.
                    ctx.contract_implements
                        .entry(contract_name)
                        .or_default()
                        .push(inherited_name);
                } else {
                    // This case should ideally not happen if the query is correct
                    eprintln!(
                        "Warning: Inheritance detected but no inheriting entity name (contract/interface) found. Inherited: {:?}",
                        inherited_name_opt
                    );
                }
            }

            // --- Handle specific definition types (Functions, Modifiers, Constructors, State Vars) ---

            // Handle state variable definitions
            if is_state_var {
                if let (Some(contract_name), Some(var_name), Some(var_type)) =
                    (contract_name_opt.clone(), var_name_opt, var_type_opt)
                {
                    // Store the type information for later use in call resolution
                    ctx.state_var_types
                        .insert((contract_name, var_name), var_type);
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
                    | NodeType::EventListener => Visibility::Default, // Should not happen here
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

#[derive(Default)] // Add Default derive
pub struct CallsHandling {
    // Add field to store config
    config: HashMap<String, String>,
}

// Remove 'a lifetime
impl CallGraphGeneratorStep for CallsHandling {
    fn name(&self) -> &'static str {
        "Calls-Handling"
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

        let source_bytes = input.source.as_bytes(); // Keep for iter::once closure below
                                                    // Updated query to capture the call_expression node with each specific pattern.
        let call_query_str = r#"
            ; --- Specific call structures ---

            ; Simple identifier call: foo()
            (call_expression
              function: (expression (identifier) @call_name)
            ) @call_expr_node ; Capture the call_expression here

            ; Member access call: obj.method() or chained_expr.method()
            (call_expression
              function: (expression (member_expression
                object: (_) @member_object_node
                property: (identifier) @member_property
              ))
            ) @call_expr_node ; Capture the call_expression here

            ; Type cast call heuristic: TypeName(expression)
            (call_expression
              function: (expression (identifier) @type_cast_name)
              (call_argument)
              (#match? @type_cast_name "^[A-Z]")
            ) @call_expr_node ; Capture the call_expression here

            ; Constructor call via call_expression: new Contract(...)
            ; Captures the call_expression containing the new_expression.
            (call_expression
                function: (expression (new_expression (type_name (identifier) @new_contract_name_in_call))) ; Use different name
            ) @call_expr_node ; Capture the call_expression here
            ; but the main loop prioritizes @call_expr_node matches now.
            (new_expression
                (type_name (identifier) @new_contract_name)
            ) @new_expression_node

            ; --- Emit statement ---
            (emit_statement
              name: (expression (identifier) @emit_event_name) ; Correct field name: 'name'
              ; Arguments are direct children (call_argument), handled by extract_arguments
            ) @emit_statement_node ; Capture the whole emit statement
        "#;
        let call_query = Query::new(&input.solidity_lang, call_query_str)
            .context("Failed to create call query")?;

        let mut call_sequence_counter: usize = 0; // Initialize GLOBAL sequence counter

        // Iterate through definition_nodes_info which now contains NodeInfo
        for (caller_node_id, caller_node_info, caller_contract_name_opt) in
            &ctx.definition_nodes_info // Borrowing ctx, but info inside is owned/static
        {
            // Get the actual TsNode for the caller's definition using the span from NodeInfo
            let definition_ts_node = input.tree.root_node()
                .descendant_for_byte_range(caller_node_info.span.0, caller_node_info.span.1)
                .ok_or_else(|| anyhow!("Failed to find definition TsNode for span {:?} in CallsHandling", caller_node_info.span))?;

            // Track processed call/emit spans within this caller function to avoid duplicates
            let mut processed_call_spans: HashSet<(usize, usize)> = HashSet::new();
            // --- DEBUG: Log the caller node being processed ---
            let caller_node_name = graph
                .nodes
                .get(*caller_node_id)
                .map_or("?".to_string(), |n| {
                    format!(
                        "{}.{}",
                        n.contract_name.as_deref().unwrap_or("Global"),
                        n.name
                    )
                });
            eprintln!("[CallsHandling DEBUG] Processing calls/emits within Caller Node ID: {} (Name: '{}', Contract: {:?})", caller_node_id, caller_node_name, caller_contract_name_opt);
            // --- END DEBUG ---

            let mut call_cursor = QueryCursor::new();
            // Sequence counter is now initialized outside the loop

            let mut call_matches =
                call_cursor.matches(&call_query, definition_ts_node, |node: TsNode| { // Use retrieved node
                    iter::once(&source_bytes[node.byte_range()])
                });

            call_matches.advance();
            while let Some(call_match) = call_matches.get() {
                // --- DEBUG: Log captures for this match ---
                eprintln!("[CallsHandling DEBUG]   Match found. Captures:");
                for capture in call_match.captures {
                    let capture_name = &call_query.capture_names()[capture.index as usize];
                    let node = capture.node;
                    eprintln!(
                         "[CallsHandling DEBUG]     - Capture Name: '{}', Node Kind: '{}', Span: {:?}",
                         capture_name, node.kind(), (node.start_byte(), node.end_byte())
                     );
                }
                // --- END DEBUG ---

                // --- Check if this match is an emit_statement ---
                let emit_statement_node_opt = call_match
                    .nodes_for_capture_index(
                        call_query
                            .capture_index_for_name("emit_statement_node")
                            .unwrap_or(u32::MAX), // Use MAX if capture doesn't exist
                    )
                    .next();

                if let Some(emit_node) = emit_statement_node_opt {
                    // --- Handle Emit Statement ---
                    let emit_span = (emit_node.start_byte(), emit_node.end_byte());
                    eprintln!("[CallsHandling DEBUG]       Processing Emit Statement at span {:?}", emit_span);

                    // --- Check for duplicates based on emit span ---
                    if !processed_call_spans.insert(emit_span) {
                        eprintln!("[CallsHandling DEBUG]       Skipping duplicate processing for emit statement at span {:?}", emit_span);
                        call_matches.advance();
                        continue;
                    }
                    // --- End duplicate check ---

                    call_sequence_counter += 1; // Increment sequence for the emit event

                    // Extract event name
                    let event_name_opt = call_match
                        .nodes_for_capture_index(
                            call_query
                                .capture_index_for_name("emit_event_name")
                                .unwrap_or(u32::MAX),
                        )
                        .next()
                        .map(|n| get_node_text(&n, &input.source).to_string());

                    // Extract arguments (using the same helper as call_expression)
                    let argument_texts = extract_arguments(emit_node, &input); // Pass the emit_node itself

                    if let Some(event_name) = event_name_opt {
                        eprintln!("[CallsHandling DEBUG]         Event Name: '{}', Args: {:?}", event_name, argument_texts);

                        // --- Get or Create EVM Node ---
                        let evm_key = (None, EVM_NODE_NAME.to_string());
                        let evm_node_id = if let Some(id) = graph.node_lookup.get(&evm_key) {
                            *id // Node already exists, use its ID
                        } else {
                            // Node doesn't exist, create it first
                            eprintln!("[CallsHandling DEBUG]           Creating EVM node.");
                            let new_id = graph.add_node(
                                EVM_NODE_NAME.to_string(),
                                NodeType::Evm,
                                None,
                                Visibility::Default,
                                (0, 0), // Placeholder span for synthetic nodes
                            );
                            // Then insert it into the lookup map
                            graph.node_lookup.insert(evm_key, new_id);
                            new_id // Use the new ID
                        };


                        // --- Get or Create Event Listener Node ---
                        let listener_key = (None, EVENT_LISTENER_NODE_NAME.to_string());
                        let listener_node_id = if let Some(id) = graph.node_lookup.get(&listener_key) {
                             *id // Node already exists, use its ID
                        } else {
                             // Node doesn't exist, create it first
                             eprintln!("[CallsHandling DEBUG]           Creating EventListener node.");
                             let new_id = graph.add_node(
                                EVENT_LISTENER_NODE_NAME.to_string(),
                                NodeType::EventListener,
                                None,
                                Visibility::Default,
                                (0, 0), // Placeholder span
                            );
                             // Then insert it into the lookup map
                             graph.node_lookup.insert(listener_key, new_id);
                             new_id // Use the new ID
                        };


                        // --- Add Edge 1: Caller -> EVM ---
                        eprintln!("[CallsHandling DEBUG]         >>> Adding edge (Emit Caller->EVM): CallerID={}, TargetID={}, Seq={}, Event='{}'", caller_node_id, evm_node_id, call_sequence_counter, event_name);
                        graph.add_edge(
                            *caller_node_id,
                            evm_node_id,
                            EdgeType::Call,
                            emit_span, // Span of the emit statement
                            None,
                            call_sequence_counter,
                            None, // No return value for emit itself
                            Some(argument_texts.clone()), // Pass extracted args
                            Some(event_name.clone()), // Pass event name
                        );

                        // --- Add Edge 2: EVM -> Event Listener ---
                        eprintln!("[CallsHandling DEBUG]         >>> Adding edge (Emit EVM->Listener): CallerID={}, TargetID={}, Seq={}, Event='{}'", evm_node_id, listener_node_id, call_sequence_counter, event_name);
                        graph.add_edge(
                            evm_node_id,
                            listener_node_id,
                            EdgeType::Call,
                            emit_span, // Span of the emit statement
                            None,
                            call_sequence_counter, // Use SAME sequence number
                            None, // No return value
                            Some(argument_texts), // Pass extracted args again
                            Some(event_name), // Pass event name again
                        );

                    } else {
                         eprintln!("[CallsHandling DEBUG]       >>> Emit statement missing event name. Span: {:?}", emit_span);
                    }

                } else {
                    // --- Try to get the call_expression node for this match (Original Logic) ---
                    let call_expr_node_opt = call_match
                        .nodes_for_capture_index(
                            call_query
                                .capture_index_for_name("call_expr_node")
                                .ok_or_else(|| anyhow!("Capture @call_expr_node not found in query"))?,
                        )
                        .next();

                    if let Some(call_expr_node) = call_expr_node_opt {
                        // --- We have a call expression, process it ---
                        let call_span = (call_expr_node.start_byte(), call_expr_node.end_byte());

                        // --- Check if we already processed this exact call expression span ---
                        if !processed_call_spans.insert(call_span) {
                            eprintln!("[CallsHandling DEBUG]       Skipping duplicate processing for call expression at span {:?}", call_span);
                            call_matches.advance(); // Move to the next match
                            continue; // Skip the rest of the loop for this match
                        }
                        // --- End duplicate check ---

                        // Check if this call_expr contains a 'new' expression internally.
                        // We still need to handle the 'new Contract()' case specifically for constructor resolution.
                        let new_expr_inside_call_opt = call_expr_node
                            .child_by_field_name("function")
                            .and_then(|func_node| func_node.child(0)) // Assuming function -> expression -> new_expression
                            .filter(|n| n.kind() == "new_expression");

                        if let Some(new_expr_node) = new_expr_inside_call_opt {
                            // --- Handle Constructor Call (via `new` inside call_expression) ---
                            let new_contract_name_opt = new_expr_node
                                .child_by_field_name("type_name")
                                .and_then(|tn| tn.child(0)) // type_name -> identifier or user_defined_type
                                .and_then(|id_or_udt| {
                                    if id_or_udt.kind() == "identifier" {
                                        Some(id_or_udt)
                                    } else if id_or_udt.kind() == "user_defined_type" {
                                        id_or_udt.child(0).filter(|n| n.kind() == "identifier")
                                    } else {
                                        None
                                    }
                                })
                                .map(|n| get_node_text(&n, &input.source).to_string());

                            if let Some(new_contract_name) = new_contract_name_opt {
                                call_sequence_counter += 1; // Increment sequence for constructor call
                                let constructor_key =
                                    (Some(new_contract_name.clone()), new_contract_name.clone());
                                if let Some(target_node_id) = graph.node_lookup.get(&constructor_key) {
                                    eprintln!("[CallsHandling DEBUG]       >>> Adding edge (new in call): CallerID={}, TargetID={}, Seq={}", caller_node_id, target_node_id, call_sequence_counter);
                                    // Extract arguments for constructor call
                                    let constructor_args = extract_arguments(call_expr_node, &input); // Use helper
                                    graph.add_edge(
                                        *caller_node_id,
                                        *target_node_id,
                                        EdgeType::Call,
                                        call_span,
                                        None,
                                        call_sequence_counter,
                                        None,
                                        Some(constructor_args), // Pass extracted args
                                        None, // No event name for constructor calls
                                    );
                                } else {
                                    eprintln!("[CallsHandling DEBUG]       >>> Constructor call unresolved: new {}. Span: {:?}", new_contract_name, call_span);
                                }
                            }
                        } else {
                            // --- Handle Regular Function/Member Call ---
                            call_sequence_counter += 1; // Increment sequence

                            // S-expression log
                            eprintln!("[CallsHandling DEBUG]       Call Expression Node S-Expr (Span: {:?}):\n{}", call_span, call_expr_node.to_sexp());

                            // Extract arguments
                            let argument_texts = extract_arguments(call_expr_node, &input); // Use helper
                            eprintln!(
                                "[CallsHandling DEBUG]       Extracted Arguments: {:?}",
                                argument_texts
                            );

                            // Resolve target
                            let target_node_id_opt = resolve_call_target(
                                call_expr_node,
                                *caller_node_id,
                                caller_contract_name_opt,
                                graph,
                                ctx,
                                &input,
                                &call_query, // Pass query
                            )?;

                            // Add edge
                            if let Some(target_node_id) = target_node_id_opt {
                                eprintln!("[CallsHandling DEBUG]       >>> Adding edge: CallerID={}, TargetID={}, Seq={}", caller_node_id, target_node_id, call_sequence_counter);
                                graph.add_edge(
                                    *caller_node_id,
                                    target_node_id,
                                    EdgeType::Call,
                                    call_span,
                                    None,
                                    call_sequence_counter,
                                    None,
                                    Some(argument_texts.clone()),
                                    None, // No event name for regular calls
                                );
                            } else {
                                eprintln!(
                                    "[CallsHandling DEBUG]       >>> Call unresolved. Span: {:?}",
                                    call_span
                                );
                            }
                        }
                    } else {
                        // --- This branch handles matches that are neither emit nor call_expression ---
                        // Check if it was purely a `new_expression_node` match (e.g., assignment)
                        let new_expr_node_opt = call_match
                            .nodes_for_capture_index(
                                call_query
                                    .capture_index_for_name("new_expression_node")
                                    .unwrap_or(u32::MAX),
                            )
                            .next();

                        if new_expr_node_opt.is_none() {
                            // If neither emit_node, call_expr_node, nor new_expr_node was captured, log a warning.
                            eprintln!("Warning: Query match found but failed to extract @emit_statement_node, @call_expr_node, or @new_expression_node. Match details: {:?}", call_match.captures.iter().map(|c| (call_query.capture_names()[c.index as usize].clone(), c.node.kind())).collect::<Vec<_>>());
                        }
                        // If it was *only* a new_expression_node match, we ignore it for call graph edges.
                    }
                }
                call_matches.advance();
            }
        }

        Ok(())
    }
}

// --- Helper Function Implementations ---

/// Helper function to extract argument texts from a call expression node.
fn extract_arguments<'a>(
    call_expr_node: TsNode<'a>,
    input: &CallGraphGeneratorInput,
) -> Vec<String> {
    let mut argument_texts: Vec<String> = Vec::new();
    eprintln!(
        "[CallsHandling DEBUG] Extracting arguments for Call Expr Node: Kind='{}', Span={:?}",
        call_expr_node.kind(),
        (call_expr_node.start_byte(), call_expr_node.end_byte())
    );

    let mut cursor = call_expr_node.walk(); // Walk the children of the call expression node directly
    for child_node in call_expr_node.children(&mut cursor) {
        eprintln!(
            "[CallsHandling DEBUG]   Checking child of call_expr: Kind='{}', Span={:?}",
            child_node.kind(),
            (child_node.start_byte(), child_node.end_byte())
        );
        // Check if the child is a 'call_argument' node
        if child_node.kind() == "call_argument" {
            eprintln!("[CallsHandling DEBUG]     Found call_argument node.");
            // The expression is typically the first child of 'call_argument'
            if let Some(expression_node) = child_node.child(0) {
                eprintln!("[CallsHandling DEBUG]       Found expression node inside call_argument: Kind='{}', Span={:?}", expression_node.kind(), (expression_node.start_byte(), expression_node.end_byte()));
                let arg_text = get_node_text(&expression_node, &input.source).to_string();
                eprintln!("[CallsHandling DEBUG]       Extracted text: '{}'", arg_text);
                argument_texts.push(arg_text);
            } else {
                eprintln!("[CallsHandling DEBUG]       Could not find expression node (child 0) inside call_argument.");
            }
        }
    }

    if argument_texts.is_empty() {
        eprintln!(
            "[CallsHandling DEBUG]   No 'call_argument' children found for this call expression."
        );
        // Log children if none found
        let mut cursor_debug = call_expr_node.walk();
        eprintln!("[CallsHandling DEBUG]   Children of call expression node were:");
        for child_node_debug in call_expr_node.children(&mut cursor_debug) {
            eprintln!(
                "[CallsHandling DEBUG]     - Kind: '{}', Span: {:?}",
                child_node_debug.kind(),
                (child_node_debug.start_byte(), child_node_debug.end_byte())
            );
        }
    }

    argument_texts // Return the extracted arguments
}

/// Resolves the target node ID for a given call expression node.
fn resolve_call_target(
    call_expr_node: TsNode,
    caller_node_id: usize,
    caller_contract_name_opt: &Option<String>,
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext,
    input: &CallGraphGeneratorInput,
    call_query: &Query, // Needed if we re-query inside
) -> Result<Option<usize>> {
    // let source_bytes = input.source.as_bytes(); // Not needed directly here if using get_node_text
    let call_span = (call_expr_node.start_byte(), call_expr_node.end_byte());

    // 1. Analyze the 'function' part of the call_expr_node
    let function_node = call_expr_node
        .child_by_field_name("function")
        .context("Call expression node missing 'function' child")?;

    // --- Case A: Simple Identifier Call: foo() ---
    // Check if function is `expression -> identifier`
    if function_node.kind() == "expression"
        && function_node.child_count() == 1
        && function_node
            .child(0)
            .map_or(false, |n| n.kind() == "identifier")
    {
        let function_identifier_node = function_node.child(0).unwrap();
        let simple_call_name = get_node_text(&function_identifier_node, &input.source).to_string();
        eprintln!("[Resolve Target DEBUG] Simple call: '{}'", simple_call_name);

        // NOTE: Type cast/constructor calls like `TypeName(...)` are now handled
        //       during type resolution in `resolve_expression_type`.
        //       This function now only resolves actual function/modifier calls.

        // Resolution logic
        // 1. Look within the current contract scope
        if let Some(contract_name) = caller_contract_name_opt {
            let key = (Some(contract_name.clone()), simple_call_name.clone());
            if let Some(id) = graph.node_lookup.get(&key) {
                eprintln!(
                    "[Resolve Target DEBUG]   Found in contract scope: Node ID {}",
                    id
                );
                return Ok(Some(*id));
            }
        }
        // 2. Look for a free function
        let free_function_key = (None, simple_call_name.clone());
        if let Some(id) = graph.node_lookup.get(&free_function_key) {
            eprintln!(
                "[Resolve Target DEBUG]   Found free function: Node ID {}",
                id
            );
            return Ok(Some(*id));
        }
        // 3. Check if the simple name matches a known Contract or Interface name.
        // This handles cases that might look like constructor calls (`ContractName()`)
        // or type casts (`InterfaceName(...)`) when not using the `new` keyword.
        // The `resolve_expression_type` handles the distinction based on arguments.
        if ctx.all_contracts.contains_key(&simple_call_name)
            || ctx.all_interfaces.contains_key(&simple_call_name)
        // Also check interfaces
        {
            // Treat both contract and interface names similarly here for lookup.
            // If it's a contract, look for the constructor.
            // If it's an interface, this lookup might fail (interfaces don't have constructors),
            // but recognizing it as a type is important for `resolve_expression_type`.
            // The actual node added for an interface itself uses (Some(InterfaceName), InterfaceName) as the key.
            let type_key = (Some(simple_call_name.clone()), simple_call_name.clone());
            if let Some(id) = graph.node_lookup.get(&type_key) {
                eprintln!(
                    "[Resolve Target DEBUG]   Found constructor match (simple call): Node ID {}",
                    id
                );
                return Ok(Some(*id));
            }
        }

        eprintln!(
            "Warning: Simple call target '{}' not found. Span: {:?}",
            simple_call_name, call_span
        );
        return Ok(None);
    }

    // --- Case B: Member Access Call: obj.method(), expr.method1().method2() ---
    // Check if function is `expression -> member_expression`
    if function_node.kind() == "expression"
        && function_node.child_count() == 1
        && function_node
            .child(0)
            .map_or(false, |n| n.kind() == "member_expression")
    {
        let member_expr_node = function_node.child(0).unwrap();
        let object_node = member_expr_node
            .child_by_field_name("object")
            .context("Member expression missing 'object'")?;
        let property_node = member_expr_node
            .child_by_field_name("property")
            .context("Member expression missing 'property'")?;

        if property_node.kind() == "identifier" {
            let property_name = get_node_text(&property_node, &input.source).to_string();
            eprintln!(
                "[Resolve Target DEBUG] Member/Chained call, property: '{}'",
                property_name
            );

            // Resolve the type of the object_node
            let object_type_name_opt = resolve_expression_type( // Call helper
                object_node,
                caller_node_id, // Pass caller context for resolution
                caller_contract_name_opt,
                graph,
                ctx,
                input, // Pass input
                call_query,
            )?;

            if let Some(object_type_name) = object_type_name_opt {
                eprintln!(
                    "[Resolve Target DEBUG]   Object type resolved to: '{}'",
                    object_type_name
                );
                // Now resolve the property based on the object's type
                return resolve_member_or_library_call( // Call helper
                    &object_type_name,
                    &property_name,
                    caller_contract_name_opt, // Pass caller's scope for 'using for' lookups
                    graph,
                    ctx,
                    input, // Pass input
                    Some(object_node), // Pass object node for context if needed
                    call_span,         // Pass call span for warnings
                );
            } else {
                eprintln!("Warning: Could not resolve type for member call object. Object node kind: '{}', Span: {:?}", object_node.kind(), (object_node.start_byte(), object_node.end_byte()));
                return Ok(None);
            }
        } else {
            eprintln!(
                "Warning: Member expression property is not an identifier. Kind: '{}', Span: {:?}",
                property_node.kind(),
                (property_node.start_byte(), property_node.end_byte())
            );
            return Ok(None);
        }
    }

    // --- Case C: Constructor Call: new Contract() ---
    // Check if function is `expression -> new_expression`
    if function_node.kind() == "expression"
        && function_node.child_count() == 1
        && function_node
            .child(0)
            .map_or(false, |n| n.kind() == "new_expression")
    {
        let new_expr_node = function_node.child(0).unwrap();
        if let Some(type_name_node) = new_expr_node.child_by_field_name("type_name") {
            // Assuming type_name -> identifier or user_defined_type -> identifier
            let mut identifier_node_opt: Option<TsNode> = None;
            if type_name_node.child_count() > 0 {
                let first_child = type_name_node.child(0).unwrap();
                if first_child.kind() == "identifier" {
                    identifier_node_opt = Some(first_child);
                } else if first_child.kind() == "user_defined_type"
                    && first_child.child_count() > 0
                    && first_child.child(0).unwrap().kind() == "identifier"
                {
                    identifier_node_opt = Some(first_child.child(0).unwrap());
                }
            }

            if let Some(identifier_node) = identifier_node_opt {
                let contract_name = get_node_text(&identifier_node, &input.source).to_string();
                eprintln!(
                    "[Resolve Target DEBUG] Constructor call: 'new {}'",
                    contract_name
                );
                // Resolve to the constructor node (explicit or default)
                let constructor_key = (Some(contract_name.clone()), contract_name.clone());
                if let Some(id) = graph.node_lookup.get(&constructor_key) {
                    eprintln!("[Resolve Target DEBUG]   Found constructor node: ID {}", id);
                    return Ok(Some(*id));
                } else {
                    eprintln!(
                        "Warning: Constructor node for 'new {}' not found in graph. Span: {:?}",
                        contract_name, call_span
                    );
                    return Ok(None);
                }
            }
        }
    }

    // --- Fallback / Unhandled Case ---
    eprintln!(
        "Warning: Unhandled call expression function structure. Function node kind: '{}', Span: {:?}",
        function_node.kind(),
        (function_node.start_byte(), function_node.end_byte())
    );
    Ok(None)
}

/// Attempts to parse the return type string from a function definition node.
/// TODO: Handle multiple return types (tuples). Currently returns the first found or the tuple itself as string.
// Remove 'a, change ctx type, input is already present
pub(crate) fn get_function_return_type(
    target_node_id: usize,
    ctx: &CallGraphGeneratorContext,
    input: &CallGraphGeneratorInput,
) -> Option<String> {
    // Find the NodeInfo corresponding to the target_node_id
    let definition_node_info_opt = ctx
        .definition_nodes_info
        .iter()
        .find(|(id, _, _)| *id == target_node_id)
        .map(|(_, node_info, _)| node_info); // Extract the NodeInfo

    if let Some(definition_node_info) = definition_node_info_opt {
         // Get the actual TsNode using the span from NodeInfo and the input tree
        let definition_ts_node = match input.tree.root_node()
            .descendant_for_byte_range(definition_node_info.span.0, definition_node_info.span.1) {
            Some(node) => node,
            None => {
                eprintln!("[Return Type Parse DEBUG] Failed to find TsNode for span {:?} for node ID {}", definition_node_info.span, target_node_id);
                return None;
            }
        };

        eprintln!("[Return Type Parse DEBUG] Analyzing definition node ID {} for return type. S-Expr:\n{}", target_node_id, definition_ts_node.to_sexp()); // Use retrieved node

        // Query to find the actual type name node within the standard return structure.
        // NOTE: This simplified query only handles `returns (type)`.
        // It does NOT handle named return parameters like `returns (uint value)`
        // or returns from modifiers. This is sufficient for the current test case.
        let return_type_query_str = r#"
            (function_definition
              return_type: (return_type_definition
                (parameter type: (type_name) @return_type_name_node)
              )
            )
        "#;
        // Note: Using a static query cache might be more efficient if this runs often.
        let return_type_query = match Query::new(&input.solidity_lang, return_type_query_str) {
            Ok(q) => q,
            Err(e) => {
                eprintln!(
                    "[Return Type Parse DEBUG] Failed to create query for node ID {}: {}",
                    target_node_id, e
                );
                return None;
            }
        };
        eprintln!(
            "[Return Type Parse DEBUG] Query created successfully for node ID {}.",
            target_node_id
        );

        let mut cursor = QueryCursor::new();
        let source_bytes = input.source.as_bytes();

        // Run the query only on the specific definition node
        eprintln!("[Return Type Parse DEBUG] Running matches on definition node...");
        let mut matches = cursor.matches(
            &return_type_query,
            definition_ts_node, // Use retrieved node
            |node: TsNode| iter::once(&source_bytes[node.byte_range()]),
        );
        matches.advance(); // Advance once

        eprintln!("[Return Type Parse DEBUG] Checking for match result..."); // New log

        if let Some(match_) = matches.get() {
            eprintln!(
                "[Return Type Parse DEBUG] Match found! Processing {} captures...",
                match_.captures.len()
            ); // New log
               // Find the @return_type_name_node capture
            let mut found_expected_capture = false; // Flag to track if we found the right capture
            for (cap_idx, capture) in match_.captures.iter().enumerate() {
                // Added index for logging
                let capture_name = &return_type_query.capture_names()[capture.index as usize];
                eprintln!(
                    "[Return Type Parse DEBUG]   Capture {}: Name='{}', Node Kind='{}', Text='{}'",
                    cap_idx,
                    capture_name,
                    capture.node.kind(),
                    get_node_text(&capture.node, &input.source)
                ); // New log

                if *capture_name == "return_type_name_node" {
                    // Use the new capture name
                    eprintln!("[Return Type Parse DEBUG]     Match on '@return_type_name_node'!"); // New log
                    found_expected_capture = true; // Mark that we found it
                    let type_name_node = capture.node;
                    // Get the text of the type_name node directly
                    let type_name_text = get_node_text(&type_name_node, &input.source).to_string();

                    if !type_name_text.is_empty() {
                        eprintln!(
                            "[Return Type Parse DEBUG] Found single return type name: '{}'",
                            type_name_text
                        );
                        return Some(type_name_text); // Return the captured type name text
                    } else {
                        eprintln!("[Return Type Parse DEBUG] Found empty return type name node.");
                        // Let it fall through to return None at the end if text is empty
                    }
                    // TODO: Handle multiple return types if the query is extended later
                } else {
                    eprintln!("[Return Type Parse DEBUG]     Capture name '{}' did not match expected '@return_type_name_node'.", capture_name);
                    // New log
                }
            }
            // If the loop finishes without returning, check the flag
            if !found_expected_capture {
                eprintln!("[Return Type Parse DEBUG] Loop finished. Query matched but the '@return_type_name_node' capture was not found among the captures for node ID {}.", target_node_id);
            } else {
                eprintln!("[Return Type Parse DEBUG] Loop finished. Found '@return_type_name_node' capture but it resulted in empty text or didn't return for node ID {}.", target_node_id);
            }
        } else {
            eprintln!(
                "[Return Type Parse DEBUG] Query found no return type match for node ID {}.",
                target_node_id
            ); // Refined log
        }
    } else {
        eprintln!(
            "[Return Type Parse DEBUG] Definition TsNode not found for node ID {}",
            target_node_id
        );
    }

    eprintln!(
        "[Return Type Parse DEBUG] Function returning None for node ID {}.",
        target_node_id
    ); // New log before returning None
    None // Return type not found or parsing failed
}

/// Resolves the Solidity type name for a given expression node.
/// This is crucial for resolving member access and chained calls.
/// TODO: This implementation is basic and needs significant enhancement.
// Remove 'a, change ctx type, input is already present
fn resolve_expression_type(
    expr_node: TsNode, // Keep TsNode<'a> here as it comes from local query
    caller_node_id: usize, // For context if needed
    caller_contract_name_opt: &Option<String>,
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext,
    input: &CallGraphGeneratorInput,
    call_query: &Query, // May need query if recursing
) -> Result<Option<String>> {
    // let source_bytes = input.source.as_bytes(); // Not needed directly here if using get_node_text
    eprintln!(
        "[Resolve Type DEBUG] Resolving type for node kind: '{}', text: '{}'",
        expr_node.kind(),
        get_node_text(&expr_node, &input.source).trim()
    );

    match expr_node.kind() {
        // --- Base Cases ---
        "identifier" => {
            let name = get_node_text(&expr_node, &input.source).to_string();
            // 1. Check state variables in current contract scope
            if let Some(contract_name) = caller_contract_name_opt {
                let key = (contract_name.clone(), name.clone());
                if let Some(type_name) = ctx.state_var_types.get(&key) {
                    eprintln!(
                        "[Resolve Type DEBUG]   Identifier '{}' resolved to state var type '{}'",
                        name, type_name
                    );
                    return Ok(Some(type_name.clone()));
                }
            }
            // 2. Check if it's a contract name (implies type is the contract itself)
            if ctx.all_contracts.contains_key(&name) {
                eprintln!(
                    "[Resolve Type DEBUG]   Identifier '{}' resolved to contract type '{}'",
                    name, name
                );
                return Ok(Some(name));
            }
            // 3. Check if it's a library name
            if ctx.all_libraries.contains_key(&name) {
                eprintln!(
                    "[Resolve Type DEBUG]   Identifier '{}' resolved to library type '{}'",
                    name, name
                );
                return Ok(Some(name));
            }
            // 4. Check if it's an interface name
            if ctx.all_interfaces.contains_key(&name) {
                eprintln!(
                    "[Resolve Type DEBUG]   Identifier '{}' resolved to interface type '{}'",
                    name, name
                );
                return Ok(Some(name));
            }
            // TODO: Check local variables/parameters (requires symbol table extension)
            eprintln!(
                "[Resolve Type DEBUG]   Identifier '{}' type not resolved (state/contract/interface/local?).",
                name
            );
            Ok(None)
        }
        "primitive_type" => {
            let type_name = get_node_text(&expr_node, &input.source).to_string();
            eprintln!("[Resolve Type DEBUG]   Primitive type '{}'", type_name);
            Ok(Some(type_name))
        }
        "string_literal" => {
            eprintln!("[Resolve Type DEBUG]   String literal -> string");
            Ok(Some("string".to_string())) // Or string memory? Be consistent.
        }
        "number_literal" => {
            // Heuristic: Assume uint256 for number literals unless context suggests otherwise
            eprintln!("[Resolve Type DEBUG]   Number literal -> uint256 (heuristic)");
            Ok(Some("uint256".to_string()))
        }
        "boolean_literal" => {
            eprintln!("[Resolve Type DEBUG]   Boolean literal -> bool");
            Ok(Some("bool".to_string()))
        }
        "hex_literal" => {
            // Heuristic: Assume bytes or address? Let's go with bytes for now.
            eprintln!("[Resolve Type DEBUG]   Hex literal -> bytes (heuristic)");
            Ok(Some("bytes".to_string()))
        }

        // --- Recursive Cases ---
        "member_expression" => {
            let object_node = expr_node
                .child_by_field_name("object")
                .context("Member expression missing object")?;
            let property_node = expr_node
                .child_by_field_name("property")
                .context("Member expression missing property")?;

            if property_node.kind() == "identifier" {
                let property_name = get_node_text(&property_node, &input.source).to_string();
                // Resolve the type of the object first
                let object_type_name_opt = resolve_expression_type( // Recursive call
                    object_node,
                    caller_node_id,
                    caller_contract_name_opt,
                    graph,
                    ctx,
                    input, // Pass input
                    call_query,
                )?;

                if let Some(object_type_name) = object_type_name_opt {
                    eprintln!(
                        "[Resolve Type DEBUG]   Member expr: Object type='{}', Property='{}'",
                        object_type_name, property_name
                    );
                    // Now find the type of the property *within* the object's type
                    // 1. Check if the object type is a contract/library and the property is a state variable
                    if let Some(type_name) = ctx
                        .state_var_types
                        .get(&(object_type_name.clone(), property_name.clone()))
                    {
                        eprintln!(
                            "[Resolve Type DEBUG]     Property resolved to state var type '{}'",
                            type_name
                        );
                        return Ok(Some(type_name.clone()));
                    }
                    // 2. Check if the object type is a contract/library/interface and the property is a function
                    let function_key = (Some(object_type_name.clone()), property_name.clone());
                    if let Some(target_node_id) = graph.node_lookup.get(&function_key) {
                        // Found a function member (e.g., Contract.func, Library.func, Interface.func)
                        // Get its return type
                        eprintln!("[Resolve Type DEBUG]     Property resolved to function member. Node ID: {}. Getting return type.", target_node_id);
                        let return_type = get_function_return_type(*target_node_id, ctx, input); // Pass input
                        eprintln!("[Resolve Type DEBUG]       Return type from get_function_return_type: {:?}", return_type);
                        // Return the actual type or None if void/unparsed
                        return Ok(return_type);
                    }

                    // TODO: Check 'using for' libraries type resolution more robustly if needed.

                    // 3. Check built-in properties (e.g., array.length, address.balance)
                    // TODO: Implement built-in property type resolution

                    eprintln!(
                        "[Resolve Type DEBUG]     Property '{}' type not resolved within type '{}' (checked direct members, TODO: builtins, using-for).",
                        property_name, object_type_name
                    );
                    Ok(None) // Property not found or type couldn't be determined
                } else {
                    eprintln!("[Resolve Type DEBUG]   Member expr: Could not resolve object type.");
                    Ok(None) // Object type couldn't be resolved
                }
            } else {
                eprintln!("[Resolve Type DEBUG]   Member expr: Property is not an identifier.");
                Ok(None) // Property is not a simple identifier
            }
        }
        "call_expression" => {
            let function_node = expr_node
                .child_by_field_name("function")
                .context("Call expression missing function")?;
            let arguments_node_opt = expr_node.child_by_field_name("arguments"); // Check if arguments exist

            // --- Prioritized Type Cast / Constructor Check ---
            // Check if function is `expression -> identifier` AND the identifier is a known type name.
            // This handles `TypeName(...)` which acts as a cast or constructor call.
            if function_node.kind() == "expression"
                && function_node.child_count() == 1
                && function_node
                    .child(0)
                    .map_or(false, |n| n.kind() == "identifier")
            {
                let identifier_node = function_node.child(0).unwrap();
                let potential_type_name = get_node_text(&identifier_node, &input.source).to_string();

                // Check if the identifier is a known Contract, Interface, or Library name
                if ctx.all_contracts.contains_key(&potential_type_name)
                    || ctx.all_interfaces.contains_key(&potential_type_name)
                    || ctx.all_libraries.contains_key(&potential_type_name)
                {
                    // It's a type cast or constructor call used as an expression.
                    // The type of this expression is the type name itself.
                    eprintln!(
                        "[Resolve Type DEBUG]   Call expr '{}' resolved as type cast/constructor to type '{}'",
                        get_node_text(&expr_node, &input.source).trim(),
                        potential_type_name
                    );
                    return Ok(Some(potential_type_name));
                }
                // If the identifier is not a known type, fall through to treat as a regular function call.
                eprintln!(
                    "[Resolve Type DEBUG]   Call expr '{}' function identifier '{}' is not a known type. Treating as function call.",
                    get_node_text(&expr_node, &input.source).trim(),
                    potential_type_name
                );
            }
            // --- End Prioritized Check ---

            // --- Resolve as a regular function call and get its return type ---
            // This path is taken if it's not a TypeName(...) pattern, or if the TypeName wasn't recognized.
            eprintln!(
                "[Resolve Type DEBUG]   Call expr '{}': Resolving as function call to determine return type.",
                get_node_text(&expr_node, &input.source).trim()
            );
            let target_node_id_opt = resolve_call_target( // Call helper
                expr_node,
                caller_node_id,
                caller_contract_name_opt,
                graph,
                ctx,
                input, // Pass input
                call_query,
            )?;

            if let Some(target_node_id) = target_node_id_opt {
                let target_node = &graph.nodes[target_node_id];
                eprintln!("[Resolve Type DEBUG]     Call resolved to '{}.{}'. Attempting return type analysis.", target_node.contract_name.as_deref().unwrap_or("Global"), target_node.name);

                // --- Attempt to parse return type from definition ---
                let return_type_opt = get_function_return_type(target_node_id, ctx, input); // Pass input

                // --- DEBUG: Log the result of get_function_return_type ---
                eprintln!(
                        "[Resolve Type DEBUG]       get_function_return_type result for Node ID {}: {:?}",
                        target_node_id, return_type_opt
                    );
                // --- END DEBUG ---

                if let Some(return_type) = return_type_opt {
                    eprintln!(
                        "[Resolve Type DEBUG]       Return type parsed as: '{}'",
                        return_type
                    );
                    // TODO: Handle tuple return types properly if needed downstream
                    return Ok(Some(return_type));
                } else {
                    // Fallback placeholder if parsing fails OR function returns void
                    let placeholder_type = format!(
                        "placeholder_return_type_of_{}.{}",
                        target_node.contract_name.as_deref().unwrap_or("Global"),
                        target_node.name
                    );
                    eprintln!("[Resolve Type DEBUG]       Failed to parse return type or function is void. Using placeholder: '{}'", placeholder_type);
                    // Decide on placeholder: maybe "void" or a specific placeholder?
                    // Using a placeholder helps distinguish from unresolved types.
                    return Ok(Some(placeholder_type)); // Return the generated placeholder
                }
                // --- End return type parsing ---
            } else {
                eprintln!("[Resolve Type DEBUG]     Call expr: Function call did not resolve to a target node.");
                Ok(None)
            }
        }
        "expression" => {
            // If we get a generic 'expression' node, try resolving its first child
            if expr_node.child_count() > 0 {
                eprintln!("[Resolve Type DEBUG]   Expression node, resolving first child...");
                return resolve_expression_type( // Recursive call
                    expr_node.child(0).unwrap(),
                    caller_node_id,
                    caller_contract_name_opt,
                    graph,
                    ctx,
                    input, // Pass input
                    call_query,
                );
            } else {
                eprintln!("[Resolve Type DEBUG]   Empty expression node.");
                Ok(None)
            }
        }
        // TODO: Handle other expression kinds: binary_expression, unary_expression, tuple_expression, array_access, etc.
        _ => {
            eprintln!(
                "[Resolve Type DEBUG]   Unhandled expression kind: {}",
                expr_node.kind()
            );
            Ok(None)
        } // Default case: type not resolved
    }
}

/// Resolves a call target when it's known to be a member access or potentially a library call via 'using for'.
// Remove 'a, change ctx type, add input param
pub(crate) fn resolve_member_or_library_call(
    object_type_name: &str, // The resolved type of the object (e.g., "Counter", "uint256", "IActionFactory")
    property_name: &str, // The method/property being accessed (e.g., "increment", "add", "createAction")
    caller_contract_name_opt: &Option<String>, // Scope for 'using for' lookup
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext,
    input: &CallGraphGeneratorInput, // Add input (might be needed for future enhancements or logging)
    _object_node_opt: Option<TsNode>, // Keep TsNode<'a> here (comes from local query) - Mark unused
    call_span: (usize, usize),            // Span of the original call expression for warnings
) -> Result<Option<usize>> {
    // --- DEBUG: Log input parameters ---
    eprintln!(
         "[Resolve Member/Lib DEBUG] Resolving member/library call. Object Type: '{}', Property: '{}', Caller Scope: {:?}, Call Span: {:?}",
         object_type_name, property_name, caller_contract_name_opt, call_span
     );
    // --- END DEBUG ---

    // --- Priority 1: Interface Implementation Resolution (if object type is an interface) ---
    // Check if the object_type_name itself indicates an interface
    if ctx.all_interfaces.contains_key(object_type_name) {
        eprintln!(
            "[Resolve Member/Lib DEBUG]   Object type '{}' is an interface. Looking for implementations.",
            object_type_name
        );
        let interface_name = object_type_name;
        let method_name = property_name;

        // Find contracts implementing this interface (and potentially inherited interfaces)
        // TODO: Need recursive resolution for interface inheritance
        let mut implementing_contracts = Vec::new();
        for (contract, implemented_interfaces) in &ctx.contract_implements {
            if implemented_interfaces.contains(&interface_name.to_string()) {
                implementing_contracts.push(contract.clone());
            }
            // TODO: Add check for inherited interfaces here
        }

        eprintln!(
            "[Resolve Member/Lib DEBUG]     Implementing contracts found: {:?}",
            implementing_contracts
        );

        let mut potential_targets = Vec::new();
        for contract_name in &implementing_contracts {
            let target_key = (Some(contract_name.clone()), method_name.to_string());
            if let Some(node_id) = graph.node_lookup.get(&target_key).copied() {
                potential_targets.push(node_id);
            } else {
                eprintln!("Warning: Implementation '{}.{}' for interface '{}' call not found in graph nodes. Span: {:?}", contract_name, method_name, interface_name, call_span);
            }
        }

        match potential_targets.len() {
            0 => {
                eprintln!("Warning: No implementing contracts found or method '{}.{}' not found in any implementation for interface '{}' call. Span: {:?}", implementing_contracts.first().map(|s| s.as_str()).unwrap_or("<unknown>"), method_name, interface_name, call_span);
                // Fall through to check 'using for' or direct member on interface itself (less common)
            }
            1 => {
                let target_id = potential_targets[0];
                eprintln!("Info: Resolved interface call '{}.{}' to single implementation: Contract '{}', Node ID {}. Span: {:?}", interface_name, method_name, implementing_contracts[0], target_id, call_span);
                return Ok(Some(target_id));
            }
            _ => {
                // Multiple implementations - apply heuristic (e.g., most outgoing edges)
                eprintln!("Info: Ambiguous interface call '{}.{}' with {} implementations. Applying heuristic.", interface_name, method_name, potential_targets.len());
                let mut best_target_node_id: Option<usize> = None;
                let mut max_outgoing_edges: isize = -1;

                for &target_id in &potential_targets {
                    let outgoing_edge_count = graph
                        .edges
                        .iter()
                        .filter(|edge| {
                            edge.source_node_id == target_id && edge.edge_type == EdgeType::Call
                        })
                        .count() as isize;

                    if outgoing_edge_count > max_outgoing_edges {
                        max_outgoing_edges = outgoing_edge_count;
                        best_target_node_id = Some(target_id);
                    } else if outgoing_edge_count == max_outgoing_edges
                        && best_target_node_id.is_none()
                    {
                        best_target_node_id = Some(target_id); // Pick first if tie at 0 or first encountered max
                    }
                }

                if let Some(chosen_id) = best_target_node_id {
                    let chosen_node = graph.nodes.get(chosen_id);
                    let chosen_contract_name = chosen_node
                        .and_then(|n| n.contract_name.as_deref())
                        .unwrap_or("<unknown>");
                    let final_edge_count = max_outgoing_edges.max(0);
                    eprintln!("Info: Resolved ambiguous interface call '{}.{}' to implementation in Contract '{}' ({} outgoing calls heuristic), Node ID {}. Span: {:?}", interface_name, method_name, chosen_contract_name, final_edge_count, chosen_id, call_span);
                    return Ok(Some(chosen_id));
                } else {
                    eprintln!("Warning: Could not select an implementation for ambiguous interface call '{}.{}'. Span: {:?}", interface_name, method_name, call_span);
                    // Fall through
                }
            }
        }
        // If interface resolution didn't return, continue to other checks...
    }

    // --- Priority 2: Direct Member Lookup on the Object Type (Contract/Library) ---
    // Check if object_type_name is a contract/library and property_name is a function/modifier within it
    // Skip if it was an interface (already handled above)
    if !ctx.all_interfaces.contains_key(object_type_name) {
        let direct_lookup_key = (
            Some(object_type_name.to_string()),
            property_name.to_string(),
        );
        if let Some(id) = graph.node_lookup.get(&direct_lookup_key) {
            eprintln!(
                "[Resolve Member/Lib DEBUG]   Found direct member match: Node ID {}",
                id
            );
            return Ok(Some(*id));
        }
    }

    // --- Priority 3: 'using for' Directives (if applicable) ---
    // This applies if the object_type_name is a type like uint256, address, etc., or '*'
    let mut potential_libraries = Vec::new();
    if let Some(caller_contract_name) = caller_contract_name_opt {
        // Check for specific type: (Some(Contract), Type)
        let specific_type_key = (
            Some(caller_contract_name.clone()),
            object_type_name.to_string(),
        );
        if let Some(libs) = ctx.using_for_directives.get(&specific_type_key) {
            potential_libraries.extend(libs.iter().cloned());
        }
        // Check for wildcard type: (Some(Contract), "*")
        let wildcard_key = (Some(caller_contract_name.clone()), "*".to_string());
        if let Some(libs) = ctx.using_for_directives.get(&wildcard_key) {
            potential_libraries.extend(libs.iter().cloned());
        }
        // TODO: Check global 'using for' directives (key: (None, Type) and (None, "*")) if supported
    }
    // Remove duplicates
    potential_libraries.sort_unstable();
    potential_libraries.dedup();

    if !potential_libraries.is_empty() {
        eprintln!(
            "[Resolve Member/Lib DEBUG]   Checking 'using for' libraries: {:?}",
            potential_libraries
        );
        for library_name in potential_libraries {
            let library_method_key = (Some(library_name.clone()), property_name.to_string());
            if let Some(id) = graph.node_lookup.get(&library_method_key) {
                eprintln!(
                    "[Resolve Member/Lib DEBUG]     Found match in library '{}': Node ID {}",
                    library_name, id
                );
                // TODO: Handle ambiguity if multiple libraries match? Solidity has rules.
                return Ok(Some(*id));
            }
        }
    }

    // --- Priority 3: Interface Implementation Resolution ---
    // Check if object_type_name is an interface
    if ctx.all_interfaces.contains_key(object_type_name) {
        eprintln!(
            "[Resolve Member/Lib DEBUG]   Object type '{}' is an interface. Looking for implementations.",
            object_type_name
        );
        let interface_name = object_type_name;
        let method_name = property_name;

        // Find contracts implementing this interface (and potentially inherited interfaces)
        // TODO: Need recursive resolution for interface inheritance
        let mut implementing_contracts = Vec::new();
        for (contract, implemented_interfaces) in &ctx.contract_implements {
            if implemented_interfaces.contains(&interface_name.to_string()) {
                implementing_contracts.push(contract.clone());
            }
            // TODO: Add check for inherited interfaces here
        }

        eprintln!(
            "[Resolve Member/Lib DEBUG]     Implementing contracts found: {:?}",
            implementing_contracts
        );

        let mut potential_targets = Vec::new();
        for contract_name in &implementing_contracts {
            let target_key = (Some(contract_name.clone()), method_name.to_string());
            if let Some(node_id) = graph.node_lookup.get(&target_key).copied() {
                potential_targets.push(node_id);
            } else {
                eprintln!("Warning: Implementation '{}.{}' for interface '{}' call not found in graph nodes. Span: {:?}", contract_name, method_name, interface_name, call_span);
            }
        }

        match potential_targets.len() {
            0 => {
                eprintln!("Warning: No implementing contracts found or method '{}.{}' not found in any implementation for interface '{}' call. Span: {:?}", implementing_contracts.first().map(|s| s.as_str()).unwrap_or("<unknown>"), method_name, interface_name, call_span);
                return Ok(None);
            }
            1 => {
                let target_id = potential_targets[0];
                eprintln!("Info: Resolved interface call '{}.{}' to single implementation: Contract '{}', Node ID {}. Span: {:?}", interface_name, method_name, implementing_contracts[0], target_id, call_span);
                return Ok(Some(target_id));
            }
            _ => {
                // Multiple implementations - apply heuristic (e.g., most outgoing edges)
                eprintln!("Info: Ambiguous interface call '{}.{}' with {} implementations. Applying heuristic.", interface_name, method_name, potential_targets.len());
                let mut best_target_node_id: Option<usize> = None;
                let mut max_outgoing_edges: isize = -1;

                for &target_id in &potential_targets {
                    let outgoing_edge_count = graph
                        .edges
                        .iter()
                        .filter(|edge| {
                            edge.source_node_id == target_id && edge.edge_type == EdgeType::Call
                        })
                        .count() as isize;

                    if outgoing_edge_count > max_outgoing_edges {
                        max_outgoing_edges = outgoing_edge_count;
                        best_target_node_id = Some(target_id);
                    } else if outgoing_edge_count == max_outgoing_edges
                        && best_target_node_id.is_none()
                    {
                        best_target_node_id = Some(target_id); // Pick first if tie at 0 or first encountered max
                    }
                }

                if let Some(chosen_id) = best_target_node_id {
                    let chosen_node = graph.nodes.get(chosen_id);
                    let chosen_contract_name = chosen_node
                        .and_then(|n| n.contract_name.as_deref())
                        .unwrap_or("<unknown>");
                    let final_edge_count = max_outgoing_edges.max(0);
                    eprintln!("Info: Resolved ambiguous interface call '{}.{}' to implementation in Contract '{}' ({} outgoing calls heuristic), Node ID {}. Span: {:?}", interface_name, method_name, chosen_contract_name, final_edge_count, chosen_id, call_span);
                    return Ok(Some(chosen_id));
                } else {
                    eprintln!("Warning: Could not select an implementation for ambiguous interface call '{}.{}'. Span: {:?}", interface_name, method_name, call_span);
                    return Ok(None);
                }
            }
        }
    }

    // --- Not Found ---
    eprintln!(
        "Warning: Member/Library call target '{}.{}' could not be resolved. Span: {:?}",
        object_type_name, property_name, call_span
    );
    Ok(None)
}

// Remove 'a lifetime
pub struct CallGraphGeneratorPipeline {
    steps: Vec<Box<dyn CallGraphGeneratorStep>>, // Remove 'a
    enabled_steps: HashSet<String>, // Track enabled step names
    // Remove _marker
}

// Implement Default for Pipeline
impl Default for CallGraphGeneratorPipeline {
    fn default() -> Self {
        Self::new()
    }
}


// Remove 'a lifetime
impl CallGraphGeneratorPipeline {
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            enabled_steps: HashSet::new(), // Initialize the set
            // Remove _marker
        }
    }

    /// Adds a step to the pipeline. Steps are enabled by default.
    pub fn add_step(&mut self, step: Box<dyn CallGraphGeneratorStep>) { // Remove 'a
        self.enabled_steps.insert(step.name().to_string()); // Enable by default
        self.steps.push(step);
    }

    /// Enables a step by its name.
    pub fn enable_step(&mut self, name: &str) {
        self.enabled_steps.insert(name.to_string());
    }

    /// Disables a step by its name.
    pub fn disable_step(&mut self, name: &str) {
        self.enabled_steps.remove(name);
    }

    pub fn run(
        &mut self,
        input: CallGraphGeneratorInput,
        ctx: &mut CallGraphGeneratorContext, // Remove 'a
        graph: &mut CallGraph,
        config: &HashMap<String, String>, // Keep config map parameter for run
    ) -> Result<()> {
        // First pass: configure enabled steps
        for step in self.steps.iter_mut() {
            if self.enabled_steps.contains(step.name()) {
                step.config(config);
            }
        }
        // Second pass: generate using enabled steps
        for step in &self.steps {
            if self.enabled_steps.contains(step.name()) {
                eprintln!(
                    "[Address Debug Pipeline] Running step '{}', Graph: {:p}",
                    step.name(),
                    graph
                ); // Added address log
                   // Call generate only if the step is enabled
                let step_input = input.clone(); // Clone input for each step
                step.generate(step_input, ctx, graph)?;
            }
        }
        Ok(())
    }
}
