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

#[derive(Debug, Default)]
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

    // Add context parameter
    pub fn add_explicit_return_edges<'a>(
        &mut self,
        source: &'a str,
        ctx: &'a CallGraphGeneratorContext<'a>, // Use context containing definition nodes
        solidity_lang: &'a tree_sitter::Language,
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
        let return_query = Query::new(solidity_lang, return_query_str)
            .context("Failed to create return statement query")?;
        let mut return_cursor = QueryCursor::new();
        let source_bytes = source.as_bytes();

        // 3. Iterate through definition nodes to find return statements within them
        let mut new_return_edges: Vec<Edge> = Vec::new(); // Collect new edges separately
        let mut total_returns_found_by_query = 0; // DEBUG
        let mut total_returns_processed = 0; // DEBUG
        let mut _nodes_with_explicit_return_set = 0; // DEBUG - Mark as unused for now

        // Iterate through the definition nodes stored in the context
        for (callee_node_id, definition_ts_node, _caller_contract_name_opt) in
            &ctx.definition_nodes_info
        {
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
                    definition_ts_node.to_sexp()
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
                        *definition_ts_node, // Query within the specific definition node
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
                            *definition_ts_node, // Query within the specific definition node
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
                                    .map(|n| get_node_text(&n, source).to_string());

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

pub struct CallGraphGeneratorInput<'a> {
    pub source: &'a str,
    pub tree: &'a Tree,
    pub solidity_lang: &'a tree_sitter::Language,
}
pub struct CallGraphGeneratorContext<'a> {
    pub state_var_types: HashMap<(String, String), String>,
    // Maps (Scope: ContractName or None for global, TypeName or "*") -> Vec<LibraryName>
    pub using_for_directives: HashMap<(Option<String>, String), Vec<String>>,
    pub definition_nodes_info: Vec<(usize, TsNode<'a>, Option<String>)>,
    pub all_contracts: HashMap<String, TsNode<'a>>,
    pub contracts_with_explicit_constructors: HashSet<String>,
    pub all_libraries: HashMap<String, TsNode<'a>>, // Track libraries
    pub all_interfaces: HashMap<String, TsNode<'a>>, // Track interfaces
    pub interface_functions: HashMap<String, Vec<String>>, // Map interface name to function names
    pub contract_implements: HashMap<String, Vec<String>>, // Map contract name to interfaces it implements
    pub interface_inherits: HashMap<String, Vec<String>>, // Map interface name to interfaces it inherits from
}

pub trait CallGraphGeneratorStep<'a> {
    fn name(&self) -> &'static str;
    /// Configure the step with settings.
    fn config(&mut self, config: &HashMap<String, String>);
    /// Generate part of the call graph.
    fn generate(
        &self,
        input: &'a CallGraphGeneratorInput<'a>,
        ctx: &mut CallGraphGeneratorContext<'a>,
        graph: &mut CallGraph,
    ) -> Result<()>;
}

#[derive(Default)] // Add Default derive
pub struct ContractHandling {
    // Add field to store config
    config: HashMap<String, String>,
}

impl<'a> CallGraphGeneratorStep<'a> for ContractHandling {
    fn name(&self) -> &'static str {
        "Contract-Handling"
    }

    fn config(&mut self, config: &HashMap<String, String>) {
        self.config = config.clone(); // Store the configuration
    }

    fn generate(
        &self,
        input: &'a CallGraphGeneratorInput<'a>,
        ctx: &mut CallGraphGeneratorContext<'a>,
        graph: &mut CallGraph,
        // Config parameter removed from generate
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
            let mut contract_identifier_node_opt: Option<TsNode<'a>> = None; // Node for contract identifier itself
            let mut contract_name_for_map_opt: Option<String> = None; // Name captured specifically for all_contracts map
            let mut interface_name_opt: Option<String> = None; // For interface scope/name
            let mut library_name_opt: Option<String> = None; // For library scope
            let mut library_def_node_opt: Option<TsNode<'a>> = None; // Node for library definition (reused for interface def)
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
            let mut using_type_or_wildcard_node_opt: Option<TsNode<'a>> = None;

            // --- Process captures for this specific match ---
            for capture in match_.captures {
                let capture_name = &definition_query.capture_names()[capture.index as usize];
                let captured_ts_node = capture.node;
                let text = get_node_text(&captured_ts_node, input.source);

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
                    ctx.all_contracts
                        .insert(name_for_map.clone(), identifier_node);
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
                    // Add interface to the context
                    ctx.all_interfaces
                        .insert(interface_name.clone(), interface_def_node);
                    // Initialize empty vector for interface functions
                    ctx.interface_functions
                        .entry(interface_name.clone())
                        .or_default();
                    // Add interface definition node info
                    ctx.definition_nodes_info.push((
                        node_id,
                        interface_def_node,
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
                                get_node_text(&c.node, input.source)
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
                        get_node_text(&type_or_wildcard_node, input.source).to_string();

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
                                get_node_text(&c.node, input.source)
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
                    // Add library definition node info for potential future use (e.g., finding 'using for')
                    // We pass the library name as the 'contract_name_opt' for context.
                    ctx.definition_nodes_info
                        .push((node_id, lib_def_node, Some(lib_name.clone())));
                    // Add library to the context map
                    ctx.all_libraries.insert(lib_name, lib_def_node);
                } else {
                    eprintln!(
                        "Warning: Incomplete capture for library definition: {:?}",
                        match_
                            .captures
                            .iter()
                            .map(|c| (
                                &*definition_query.capture_names()[c.index as usize],
                                get_node_text(&c.node, input.source)
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
                // Pass the correct scope (contract or library name) to definition_nodes_info
                ctx.definition_nodes_info
                    .push((node_id, def_node, scope_name_opt.clone()));
                // If this is a function in an interface, add it to the interface_functions map
                if let Some(scope_name) = &scope_name_opt {
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
                                get_node_text(&c.node, input.source)
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
                let span = (identifier_node.start_byte(), identifier_node.end_byte()); // Use contract identifier span
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

impl CallGraphGeneratorStep<'_> for CallsHandling {
    fn name(&self) -> &'static str {
        "Calls-Handling"
    }

    fn config(&mut self, config: &HashMap<String, String>) {
        self.config = config.clone(); // Store the configuration
    }

    fn generate(
        &self,
        input: &CallGraphGeneratorInput,
        ctx: &mut CallGraphGeneratorContext,
        graph: &mut CallGraph,
        // Config parameter removed from generate
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

        for (caller_node_id, definition_ts_node, caller_contract_name_opt) in
            &ctx.definition_nodes_info
        // Borrow here
        {
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
                call_cursor.matches(&call_query, *definition_ts_node, |node: TsNode| {
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
                        .map(|n| get_node_text(&n, input.source).to_string());

                    // Extract arguments (using the same helper as call_expression)
                    let argument_texts = extract_arguments(emit_node, input); // Pass the emit_node itself

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
                                .map(|n| get_node_text(&n, input.source).to_string());

                            if let Some(new_contract_name) = new_contract_name_opt {
                                call_sequence_counter += 1; // Increment sequence for constructor call
                                let constructor_key =
                                    (Some(new_contract_name.clone()), new_contract_name.clone());
                                if let Some(target_node_id) = graph.node_lookup.get(&constructor_key) {
                                    eprintln!("[CallsHandling DEBUG]       >>> Adding edge (new in call): CallerID={}, TargetID={}, Seq={}", caller_node_id, target_node_id, call_sequence_counter);
                                    // Extract arguments for constructor call
                                    let constructor_args = extract_arguments(call_expr_node, input); // Use helper
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
                            let argument_texts = extract_arguments(call_expr_node, input); // Use helper
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
                                input,
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
    input: &'a CallGraphGeneratorInput<'a>,
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
                let arg_text = get_node_text(&expression_node, input.source).to_string();
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
fn resolve_call_target<'a>(
    call_expr_node: TsNode<'a>,
    caller_node_id: usize,
    caller_contract_name_opt: &Option<String>,
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext<'a>,
    input: &'a CallGraphGeneratorInput<'a>,
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
        let simple_call_name = get_node_text(&function_identifier_node, input.source).to_string();
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
            let property_name = get_node_text(&property_node, input.source).to_string();
            eprintln!(
                "[Resolve Target DEBUG] Member/Chained call, property: '{}'",
                property_name
            );

            // Resolve the type of the object_node
            let object_type_name_opt = resolve_expression_type(
                object_node,
                caller_node_id, // Pass caller context for resolution
                caller_contract_name_opt,
                graph,
                ctx,
                input,
                call_query,
            )?;

            if let Some(object_type_name) = object_type_name_opt {
                eprintln!(
                    "[Resolve Target DEBUG]   Object type resolved to: '{}'",
                    object_type_name
                );
                // Now resolve the property based on the object's type
                return resolve_member_or_library_call(
                    &object_type_name,
                    &property_name,
                    caller_contract_name_opt, // Pass caller's scope for 'using for' lookups
                    graph,
                    ctx,
                    input,
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
                let contract_name = get_node_text(&identifier_node, input.source).to_string();
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
fn get_function_return_type<'a>(
    target_node_id: usize,
    ctx: &CallGraphGeneratorContext<'a>,
    input: &'a CallGraphGeneratorInput<'a>,
) -> Option<String> {
    // Find the definition TsNode corresponding to the target_node_id
    let definition_ts_node_opt = ctx
        .definition_nodes_info
        .iter()
        .find(|(id, _, _)| *id == target_node_id)
        .map(|(_, ts_node, _)| *ts_node); // Extract the TsNode

    if let Some(definition_ts_node) = definition_ts_node_opt {
        eprintln!("[Return Type Parse DEBUG] Analyzing definition node ID {} for return type. S-Expr:\n{}", target_node_id, definition_ts_node.to_sexp()); // Added S-Expr log HERE

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
            definition_ts_node, // Query only within this node
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
                    get_node_text(&capture.node, input.source)
                ); // New log

                if *capture_name == "return_type_name_node" {
                    // Use the new capture name
                    eprintln!("[Return Type Parse DEBUG]     Match on '@return_type_name_node'!"); // New log
                    found_expected_capture = true; // Mark that we found it
                    let type_name_node = capture.node;
                    // Get the text of the type_name node directly
                    let type_name_text = get_node_text(&type_name_node, input.source).to_string();

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
fn resolve_expression_type<'a>(
    expr_node: TsNode<'a>,
    caller_node_id: usize, // For context if needed
    caller_contract_name_opt: &Option<String>,
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext<'a>,
    input: &'a CallGraphGeneratorInput<'a>,
    call_query: &Query, // May need query if recursing
) -> Result<Option<String>> {
    // let source_bytes = input.source.as_bytes(); // Not needed directly here if using get_node_text
    eprintln!(
        "[Resolve Type DEBUG] Resolving type for node kind: '{}', text: '{}'",
        expr_node.kind(),
        get_node_text(&expr_node, input.source).trim()
    );

    match expr_node.kind() {
        // --- Base Cases ---
        "identifier" => {
            let name = get_node_text(&expr_node, input.source).to_string();
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
            let type_name = get_node_text(&expr_node, input.source).to_string();
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
                let property_name = get_node_text(&property_node, input.source).to_string();
                // Resolve the type of the object first
                let object_type_name_opt = resolve_expression_type(
                    object_node,
                    caller_node_id,
                    caller_contract_name_opt,
                    graph,
                    ctx,
                    input,
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
                        let return_type = get_function_return_type(*target_node_id, ctx, input);
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
                let potential_type_name = get_node_text(&identifier_node, input.source).to_string();

                // Check if the identifier is a known Contract, Interface, or Library name
                if ctx.all_contracts.contains_key(&potential_type_name)
                    || ctx.all_interfaces.contains_key(&potential_type_name)
                    || ctx.all_libraries.contains_key(&potential_type_name)
                {
                    // It's a type cast or constructor call used as an expression.
                    // The type of this expression is the type name itself.
                    eprintln!(
                        "[Resolve Type DEBUG]   Call expr '{}' resolved as type cast/constructor to type '{}'",
                        get_node_text(&expr_node, input.source).trim(),
                        potential_type_name
                    );
                    return Ok(Some(potential_type_name));
                }
                // If the identifier is not a known type, fall through to treat as a regular function call.
                eprintln!(
                    "[Resolve Type DEBUG]   Call expr '{}' function identifier '{}' is not a known type. Treating as function call.",
                    get_node_text(&expr_node, input.source).trim(),
                    potential_type_name
                );
            }
            // --- End Prioritized Check ---

            // --- Resolve as a regular function call and get its return type ---
            // This path is taken if it's not a TypeName(...) pattern, or if the TypeName wasn't recognized.
            eprintln!(
                "[Resolve Type DEBUG]   Call expr '{}': Resolving as function call to determine return type.",
                get_node_text(&expr_node, input.source).trim()
            );
            let target_node_id_opt = resolve_call_target(
                expr_node,
                caller_node_id,
                caller_contract_name_opt,
                graph,
                ctx,
                input,
                call_query,
            )?;

            if let Some(target_node_id) = target_node_id_opt {
                let target_node = &graph.nodes[target_node_id];
                eprintln!("[Resolve Type DEBUG]     Call resolved to '{}.{}'. Attempting return type analysis.", target_node.contract_name.as_deref().unwrap_or("Global"), target_node.name);

                // --- Attempt to parse return type from definition ---
                let return_type_opt = get_function_return_type(target_node_id, ctx, input);

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
                return resolve_expression_type(
                    expr_node.child(0).unwrap(),
                    caller_node_id,
                    caller_contract_name_opt,
                    graph,
                    ctx,
                    input,
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
fn resolve_member_or_library_call<'a>(
    object_type_name: &str, // The resolved type of the object (e.g., "Counter", "uint256", "IActionFactory")
    property_name: &str, // The method/property being accessed (e.g., "increment", "add", "createAction")
    caller_contract_name_opt: &Option<String>, // Scope for 'using for' lookup
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext<'a>,
    _input: &'a CallGraphGeneratorInput<'a>, // Needed for warnings? - Mark unused
    _object_node_opt: Option<TsNode<'a>>, // Optional: Original object node for context/warnings (currently unused)
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

pub struct CallGraphGeneratorPipeline<'a> {
    steps: Vec<Box<dyn CallGraphGeneratorStep<'a>>>,
    enabled_steps: HashSet<String>, // Track enabled step names
    _marker: PhantomData<&'a ()>,
}

impl<'a> CallGraphGeneratorPipeline<'a> {
    pub fn new() -> Self {
        Self {
            steps: Vec::new(),
            enabled_steps: HashSet::new(), // Initialize the set
            _marker: PhantomData,
        }
    }

    /// Adds a step to the pipeline. Steps are enabled by default.
    pub fn add_step(&mut self, step: Box<dyn CallGraphGeneratorStep<'a>>) {
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
        input: &'a CallGraphGeneratorInput<'a>,
        ctx: &mut CallGraphGeneratorContext<'a>,
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
                step.generate(input, ctx, graph)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_solidity;

    fn find_node<'a>(graph: &'a CallGraph, name: &str, contract: Option<&str>) -> Option<&'a Node> {
        graph
            .iter_nodes()
            .find(|n| n.name == name && n.contract_name.as_deref() == contract)
    }

    fn assert_visibility(node: &Node, expected: Visibility) {
        assert_eq!(
            node.visibility, expected,
            "Node '{}' should have {:?} visibility, but has {:?}",
            node.name, expected, node.visibility
        );
    }

    #[test]
    fn test_simple_contract_call() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;

        contract Simple {
            function foo() public pure {}
            function bar() private pure {
                foo();
            }
            constructor() {
                foo();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        // Nodes: foo, bar, constructor (explicit)
        assert_eq!(graph.nodes.len(), 3, "Should find 3 nodes");
        assert_eq!(graph.edges.len(), 2, "Should find 2 edges");

        let foo_node = find_node(&graph, "foo", Some("Simple")).expect("foo node not found");
        let bar_node = find_node(&graph, "bar", Some("Simple")).expect("bar node not found");
        let constructor_node =
            find_node(&graph, "Simple", Some("Simple")).expect("constructor node not found");

        assert_eq!(foo_node.node_type, NodeType::Function);
        assert_eq!(bar_node.node_type, NodeType::Function);
        assert_eq!(constructor_node.node_type, NodeType::Constructor);

        assert_visibility(&foo_node, Visibility::Public);
        assert_visibility(&bar_node, Visibility::Private);
        assert_visibility(&constructor_node, Visibility::Public);

        assert_eq!(graph.nodes[0].id, foo_node.id);
        assert_eq!(graph.nodes[1].id, bar_node.id);
        assert_eq!(graph.nodes[2].id, constructor_node.id);

        assert_eq!(graph.edges[0].source_node_id, bar_node.id);
        assert_eq!(graph.edges[0].target_node_id, foo_node.id);
        assert_eq!(graph.edges[0].sequence_number, 1, "bar -> foo sequence"); // Sequence is 1 within bar
        assert_eq!(graph.edges[1].source_node_id, constructor_node.id);
        assert_eq!(graph.edges[1].target_node_id, foo_node.id);
        assert_eq!(
            graph.edges[1].sequence_number,
            2, // Global sequence counter makes this the 2nd call found
            "constructor -> foo sequence"
        ); // Sequence is 2 based on global counter

        assert_eq!(graph.iter_nodes().count(), 3);
        assert_eq!(graph.iter_edges().count(), 2);

        Ok(())
    }

    #[test]
    fn test_modifier_call() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;

        contract Modifiers {
            modifier onlyAdmin() {
                checkAdmin();
                _;
            }

            function checkAdmin() internal pure {}

            function restricted() public onlyAdmin {}
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        // Nodes: onlyAdmin, checkAdmin, restricted, + default constructor
        assert_eq!(graph.nodes.len(), 4, "Should find 4 nodes");

        assert_eq!(graph.edges.len(), 1, "Should find 1 edge");

        let mod_node = find_node(&graph, "onlyAdmin", Some("Modifiers")).expect("modifier node");
        let check_node =
            find_node(&graph, "checkAdmin", Some("Modifiers")).expect("checkAdmin node");
        let restricted_node =
            find_node(&graph, "restricted", Some("Modifiers")).expect("restricted node");

        assert_eq!(mod_node.node_type, NodeType::Modifier);
        assert_eq!(check_node.node_type, NodeType::Function);
        assert_eq!(restricted_node.node_type, NodeType::Function);

        assert_eq!(graph.nodes[0].id, mod_node.id);
        assert_eq!(graph.nodes[1].id, check_node.id);
        assert_eq!(graph.nodes[2].id, restricted_node.id);

        assert_eq!(graph.edges[0].source_node_id, mod_node.id);
        assert_eq!(graph.edges[0].target_node_id, check_node.id);
        assert_eq!(
            graph.edges[0].sequence_number, 1,
            "onlyAdmin -> checkAdmin sequence"
        ); // Sequence is 1 within onlyAdmin

        Ok(())
    }

    #[test]
    fn test_free_function_call() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;

        function helper() pure returns (uint) {
            return 1;
        }

        contract Caller {
            function callHelper() public pure returns (uint) {
                return helper();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        // Nodes: helper, callHelper, + default constructor for Caller
        assert_eq!(graph.nodes.len(), 3, "Should find 3 nodes");
        assert_eq!(graph.edges.len(), 1, "Should find 1 edge");

        let helper_node = find_node(&graph, "helper", None).expect("helper node");
        let caller_node = find_node(&graph, "callHelper", Some("Caller")).expect("callHelper node");

        assert_eq!(helper_node.node_type, NodeType::Function);
        assert_eq!(helper_node.contract_name, None);
        assert_eq!(caller_node.node_type, NodeType::Function);
        assert_eq!(caller_node.contract_name, Some("Caller".to_string()));

        assert_eq!(graph.nodes[0].id, helper_node.id);
        assert_eq!(graph.nodes[1].id, caller_node.id);

        assert_eq!(graph.edges[0].source_node_id, caller_node.id);
        assert_eq!(graph.edges[0].target_node_id, helper_node.id);
        assert_eq!(
            graph.edges[0].sequence_number, 1,
            "callHelper -> helper sequence"
        ); // Sequence is 1 within callHelper

        Ok(())
    }

    #[test]
    fn test_no_calls() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;
        contract NoCalls {
            function a() public pure {}
            function b() public pure {}
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        assert_eq!(graph.nodes.len(), 3, "Should find 3 nodes");
        assert_eq!(graph.edges.len(), 0, "Should find 0 edges");
        Ok(())
    }

    #[test]
    fn test_call_order_within_function() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;
        contract CallOrder {
            function callee1() public pure {}
            function callee2() public pure {}
            function caller() public pure {
                callee2();
                callee1();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        // Nodes: callee1, callee2, caller, + default constructor for CallOrder
        assert_eq!(graph.nodes.len(), 4, "Should find 4 nodes");
        assert_eq!(graph.edges.len(), 2, "Should find 2 edges");

        let c1_node = find_node(&graph, "callee1", Some("CallOrder")).unwrap();
        let c2_node = find_node(&graph, "callee2", Some("CallOrder")).unwrap();
        let caller_node = find_node(&graph, "caller", Some("CallOrder")).unwrap();

        assert_eq!(graph.nodes[0].id, c1_node.id);
        assert_eq!(graph.nodes[1].id, c2_node.id);
        assert_eq!(graph.nodes[2].id, caller_node.id);

        assert_eq!(
            graph.edges[0].source_node_id, caller_node.id,
            "Edge 0 source"
        );
        assert_eq!(
            graph.edges[0].target_node_id, c2_node.id,
            "Edge 0 target should be callee2 (first call)"
        );
        assert_eq!(graph.edges[0].sequence_number, 1, "First call sequence");

        assert_eq!(
            graph.edges[1].source_node_id, caller_node.id,
            "Edge 1 source"
        );
        assert_eq!(
            graph.edges[1].target_node_id, c1_node.id,
            "Edge 1 target should be callee1 (second call)"
        );
        assert_eq!(graph.edges[1].sequence_number, 2, "Second call sequence");

        Ok(())
    }

    #[test]
    fn test_empty_source() -> Result<()> {
        let source = "pragma solidity ^0.8.0;";
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        assert_eq!(graph.nodes.len(), 0, "Should find 0 nodes");
        assert_eq!(graph.edges.len(), 0, "Should find 0 edges");
        Ok(())
    }

    #[test]
    fn test_unresolved_call() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;
        contract Unresolved {
            function callNonExistent() public {
                nonExistent();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        assert_eq!(
            graph.nodes.len(),
            2,
            "Should find 2 nodes (caller + default ctor)"
        );
        assert_eq!(
            graph.edges.len(),
            0,
            "Should find 0 edges (call is unresolved)"
        );
        Ok(())
    }

    #[test]
    fn test_inter_contract_call() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;

        contract Counter {
            uint count;
            function increment() public {
                count += 1;
            }
        }

        contract CounterCaller {
            Counter public myCounter;

            constructor(address counterAddress) {
                myCounter = Counter(counterAddress);
            }

            function callIncrement() public {
                myCounter.increment();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        // Nodes: Counter.increment, CounterCaller.constructor, CounterCaller.callIncrement, + default constructor for Counter
        assert_eq!(graph.nodes.len(), 4, "Should find 4 nodes");

        let counter_inc_node = find_node(&graph, "increment", Some("Counter"))
            .expect("Counter.increment node not found");
        let caller_ctor_node = find_node(&graph, "CounterCaller", Some("CounterCaller"))
            .expect("CounterCaller.constructor node not found");
        let caller_call_node = find_node(&graph, "callIncrement", Some("CounterCaller"))
            .expect("CounterCaller.callIncrement node not found");

        assert_eq!(counter_inc_node.node_type, NodeType::Function);
        assert_eq!(caller_ctor_node.node_type, NodeType::Constructor);
        assert_eq!(caller_call_node.node_type, NodeType::Function);

        assert_eq!(graph.nodes[0].id, counter_inc_node.id);
        assert_eq!(graph.nodes[1].id, caller_ctor_node.id);
        assert_eq!(graph.nodes[2].id, caller_call_node.id);

        // Edges:
        // 1. CounterCaller::constructor -> Counter::constructor (default)
        // 2. CounterCaller::callIncrement -> Counter::increment
        assert_eq!(
            graph.edges.len(),
            2,
            "Should find 2 edges (constructor call + member call)"
        );

        // Find the specific edge for callIncrement -> increment
        let call_inc_edge = graph
            .edges
            .iter()
            .find(|e| e.source_node_id == caller_call_node.id)
            .expect("Edge from callIncrement not found");

        assert_eq!(
            call_inc_edge.source_node_id, caller_call_node.id,
            "Edge source should be callIncrement"
        );
        assert_eq!(
            call_inc_edge.target_node_id, counter_inc_node.id,
            "Edge target should be Counter.increment"
        );
        // Note: Sequence number check might need adjustment if constructor call affects it.
        // Let's assume the sequence counter increments for both calls.
        // Call 1: Constructor -> Constructor (sequence 1)
        // Call 2: callIncrement -> increment (sequence 2)
        assert_eq!(
            call_inc_edge.sequence_number,
            2, // Adjusted sequence
            "callIncrement -> increment sequence"
        );

        Ok(())
    }

    #[test]
    fn test_dot_escape_string_via_module() {
        assert_eq!(cg_dot::escape_dot_string(""), "");
        assert_eq!(cg_dot::escape_dot_string("simple"), "simple");
        assert_eq!(
            cg_dot::escape_dot_string("with \"quotes\""),
            "with \\\"quotes\\\""
        );
        assert_eq!(cg_dot::escape_dot_string("new\nline"), "new\\nline");
        assert_eq!(cg_dot::escape_dot_string("back\\slash"), "back\\\\slash");
        assert_eq!(cg_dot::escape_dot_string("<html>"), "\\<html\\>");
        assert_eq!(cg_dot::escape_dot_string("{record}"), "\\{record\\}");
    }

    #[test]
    fn test_return_boolean_literal() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;

        contract BoolReturn {
            function returnsTrue() internal pure returns (bool) {
                return true; // Return a boolean literal
            }

            function callsReturnTrue() public pure returns (bool) {
                return returnsTrue();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        // Explicitly add return edges AFTER the pipeline run
        graph.add_explicit_return_edges(source, &ctx, &Solidity.get_tree_sitter_language())?; // Pass ctx here

        // Nodes: returnsTrue, callsReturnTrue, + default constructor
        assert_eq!(graph.nodes.len(), 3, "Should find 3 nodes");
        // Edges: Call (callsReturnTrue -> returnsTrue), Return (returnsTrue -> callsReturnTrue)
        assert_eq!(
            graph.edges.len(),
            2,
            "Should find 2 edges (1 call, 1 return)"
        );

        let returns_true_node =
            find_node(&graph, "returnsTrue", Some("BoolReturn")).expect("returnsTrue node");
        let calls_return_true_node =
            find_node(&graph, "callsReturnTrue", Some("BoolReturn")).expect("callsReturnTrue node");

        // Find the return edge
        let return_edge = graph
            .iter_edges()
            .find(|e| {
                e.edge_type == EdgeType::Return
                    && e.source_node_id == returns_true_node.id
                    && e.target_node_id == calls_return_true_node.id
            })
            .expect("Return edge from returnsTrue to callsReturnTrue not found");

        // Assert the returned value is captured correctly
        assert_eq!(
            return_edge.returned_value,
            Some("true".to_string()),
            "Return edge should capture 'true'"
        );

        Ok(())
    }

    #[test]
    fn test_pipeline_execution() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;

        contract SimplePipeline {
            function foo() public pure {}
            function bar() private pure {
                foo(); // Call within the contract
            }
            constructor() {
                // No call in constructor for simplicity in this test
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Create empty config

        let mut pipeline = CallGraphGeneratorPipeline::new(); // Pipeline needs to be mutable
        pipeline.add_step(Box::new(ContractHandling::default())); // Use default constructor
        pipeline.add_step(Box::new(CallsHandling::default())); // Use default constructor
        pipeline.run(&input, &mut ctx, &mut graph, &config)?; // Pass config to run

        assert_eq!(graph.nodes.len(), 3, "Pipeline: Should find 3 nodes");
        assert_eq!(graph.edges.len(), 1, "Pipeline: Should find 1 edge");

        let foo_node =
            find_node(&graph, "foo", Some("SimplePipeline")).expect("Pipeline: foo node not found");
        let bar_node =
            find_node(&graph, "bar", Some("SimplePipeline")).expect("Pipeline: bar node not found");
        let constructor_node = find_node(&graph, "SimplePipeline", Some("SimplePipeline"))
            .expect("Pipeline: constructor node not found");

        assert_eq!(foo_node.node_type, NodeType::Function);
        assert_eq!(bar_node.node_type, NodeType::Function);
        assert_eq!(constructor_node.node_type, NodeType::Constructor);

        assert_visibility(&foo_node, Visibility::Public);
        assert_visibility(&bar_node, Visibility::Private);
        assert_visibility(&constructor_node, Visibility::Public); // Explicit constructor defaults to public if no visibility specified

        // Check node order (assuming definition order)
        assert_eq!(graph.nodes[0].id, foo_node.id);
        assert_eq!(graph.nodes[1].id, bar_node.id);
        assert_eq!(graph.nodes[2].id, constructor_node.id);

        // Check the single edge: bar() calls foo()
        assert_eq!(graph.edges[0].source_node_id, bar_node.id);
        assert_eq!(graph.edges[0].target_node_id, foo_node.id);
        assert_eq!(graph.edges[0].edge_type, EdgeType::Call);
        assert_eq!(
            graph.edges[0].sequence_number, 1,
            "Pipeline: bar -> foo sequence"
        ); // First call found globally

        Ok(())
    }

    #[test]
    fn test_pipeline_step_enable_disable() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;

        contract EnableDisableTest {
            function target() public pure {}
            function caller() public pure {
                target();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // --- Test with CallsHandling disabled ---
        let mut ctx_disabled = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph_disabled = CallGraph::new();
        let mut pipeline_disabled = CallGraphGeneratorPipeline::new();

        pipeline_disabled.add_step(Box::new(ContractHandling::default()));
        pipeline_disabled.add_step(Box::new(CallsHandling::default()));

        // Disable the CallsHandling step
        pipeline_disabled.disable_step("Calls-Handling");

        pipeline_disabled.run(&input, &mut ctx_disabled, &mut graph_disabled, &config)?;

        // Nodes: target, caller, + default constructor
        assert_eq!(
            graph_disabled.nodes.len(),
            3,
            "Disabled: Should find 3 nodes (ContractHandling ran)"
        );
        assert_eq!(
            graph_disabled.edges.len(),
            0,
            "Disabled: Should find 0 edges (CallsHandling disabled)"
        );

        // --- Test with CallsHandling enabled (default) ---
        let mut ctx_enabled = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph_enabled = CallGraph::new();
        let mut pipeline_enabled = CallGraphGeneratorPipeline::new();

        pipeline_enabled.add_step(Box::new(ContractHandling::default()));
        pipeline_enabled.add_step(Box::new(CallsHandling::default()));
        // No need to explicitly enable, it's enabled by default after add_step

        pipeline_enabled.run(&input, &mut ctx_enabled, &mut graph_enabled, &config)?;

        assert_eq!(graph_enabled.nodes.len(), 3, "Enabled: Should find 3 nodes");
        assert_eq!(
            graph_enabled.edges.len(),
            1,
            "Enabled: Should find 1 edge (CallsHandling ran)"
        );

        let target_node = find_node(&graph_enabled, "target", Some("EnableDisableTest"))
            .expect("Enabled: target node");
        let caller_node = find_node(&graph_enabled, "caller", Some("EnableDisableTest"))
            .expect("Enabled: caller node");

        assert_eq!(graph_enabled.edges[0].source_node_id, caller_node.id);
        assert_eq!(graph_enabled.edges[0].target_node_id, target_node.id);

        Ok(())
    }

    #[test]
    fn test_using_for_directive_extraction() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        library SafeMath {
            function add(uint256 a, uint256 b) internal pure returns (uint256) {
                return a + b;
            }
        }

        contract MyContract {
            using SafeMath for uint256; // Contract-level using directive

            uint256 public value;

            function increment(uint256 _amount) public {
                value = value.add(_amount); // Call resolution not tested here
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(), // Start with an empty map
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new(); // Graph is needed but won't be asserted on heavily

        // Create and run ONLY the ContractHandling step
        let contract_handler = ContractHandling::default();
        contract_handler.generate(&input, &mut ctx, &mut graph)?;

        // Assertions on the context (ctx)
        assert_eq!(
            ctx.using_for_directives.len(),
            1,
            "Should find exactly one 'using for' directive entry"
        );

        let expected_key = (Some("MyContract".to_string()), "uint256".to_string());
        let expected_value = vec!["SafeMath".to_string()];

        assert!(
            ctx.using_for_directives.contains_key(&expected_key),
            "Context should contain key for (Some(MyContract), uint256)"
        );
        assert_eq!(
            ctx.using_for_directives.get(&expected_key),
            Some(&expected_value),
            "The value for the key should be vec![\"SafeMath\"]"
        );

        // Optional: Basic check on nodes created by ContractHandling
        assert!(
            find_node(&graph, "add", Some("SafeMath")).is_some(),
            "Library function node should exist"
        );
        assert!(
            find_node(&graph, "increment", Some("MyContract")).is_some(),
            "Contract function node should exist"
        );
        assert!(
            find_node(&graph, "MyContract", Some("MyContract")).is_some(),
            "Default constructor node should exist"
        );

        Ok(())
    }

    #[test]
    fn test_library_definition_and_usage() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        library MathUtils {
            function isEven(uint256 a) internal pure returns (bool) {
                return a % 2 == 0;
            }
        }

        contract ExampleContract {
            using MathUtils for uint256; // This line is parsed but not yet used for call resolution by CallsHandling

            function checkNumberIsEven(uint256 _num) public pure returns (bool) {
                // Call resolution for _num.isEven() requires changes in CallsHandling
                return _num.isEven();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // Nodes:
        // 1. Library: MathUtils
        // 2. Function: MathUtils.isEven
        // 3. Function: ExampleContract.checkNumberIsEven
        // 4. Constructor: ExampleContract (default)
        assert_eq!(
            graph.nodes.len(),
            4,
            "Should find 4 nodes (library, lib func, contract func, default ctor)"
        );

        // Verify Library Node
        let lib_node = find_node(&graph, "MathUtils", Some("MathUtils"))
            .expect("Library node MathUtils not found");
        assert_eq!(lib_node.node_type, NodeType::Library);
        assert_eq!(
            lib_node.contract_name,
            Some("MathUtils".to_string()),
            "Library node should store its own name as scope"
        );
        assert_visibility(&lib_node, Visibility::Default); // Libraries don't have visibility

        // Verify Library Function Node
        let lib_func_node = find_node(&graph, "isEven", Some("MathUtils"))
            .expect("Library function node MathUtils.isEven not found");
        assert_eq!(lib_func_node.node_type, NodeType::Function);
        assert_eq!(
            lib_func_node.contract_name,
            Some("MathUtils".to_string()),
            "Library function scope"
        );
        assert_visibility(&lib_func_node, Visibility::Internal); // Explicitly internal

        // Verify Contract Function Node
        let contract_func_node = find_node(&graph, "checkNumberIsEven", Some("ExampleContract"))
            .expect("Contract function node ExampleContract.checkNumberIsEven not found");
        assert_eq!(contract_func_node.node_type, NodeType::Function);
        assert_eq!(
            contract_func_node.contract_name,
            Some("ExampleContract".to_string()),
            "Contract function scope"
        );
        assert_visibility(&contract_func_node, Visibility::Public); // Explicitly public

        // Verify Default Constructor Node
        let constructor_node = find_node(&graph, "ExampleContract", Some("ExampleContract"))
            .expect("Default constructor node ExampleContract not found");
        assert_eq!(constructor_node.node_type, NodeType::Constructor);
        assert_visibility(&constructor_node, Visibility::Public); // Default constructors are public

        // Verify Edge (Requires CallsHandling update)
        // TODO: Uncomment and adjust this assertion after CallsHandling is updated
        //       to resolve calls using 'using for'.
        // assert_eq!(graph.edges.len(), 1, "Should find 1 edge (checkNumberIsEven -> isEven)");
        // let edge = &graph.edges[0];
        // assert_eq!(edge.source_node_id, contract_func_node.id);
        // assert_eq!(edge.target_node_id, lib_func_node.id);
        // assert_eq!(edge.edge_type, EdgeType::Call);

        // For now, assert no edges are created by the current CallsHandling
        assert_eq!(
            graph.edges.len(),
            0,
            "Should find 0 edges currently (CallsHandling needs update for 'using for')"
        );

        Ok(())
    }

    #[test]
    fn test_using_for_call_resolution() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        library MathUtils {
            function isEven(uint256 a) internal pure returns (bool) {
                return a % 2 == 0;
            }
        }

        contract ExampleContract {
            using MathUtils for uint256; // Directive to be used by CallsHandling

            uint256 number; // State variable to call method on

            function checkNumberIsEven() public view returns (bool) {
                // This call should be resolved via 'using for'
                return number.isEven();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Nodes:
        // 1. Library: MathUtils
        // 2. Function: MathUtils.isEven
        // 3. Function: ExampleContract.checkNumberIsEven
        // 4. Constructor: ExampleContract (default)
        assert_eq!(
            graph.nodes.len(),
            4,
            "Should find 4 nodes (library, lib func, contract func, default ctor)"
        );

        // Verify Library Function Node
        let lib_func_node = find_node(&graph, "isEven", Some("MathUtils"))
            .expect("Library function node MathUtils.isEven not found");
        assert_eq!(lib_func_node.node_type, NodeType::Function);
        assert_visibility(&lib_func_node, Visibility::Internal);

        // Verify Contract Function Node
        let contract_func_node = find_node(&graph, "checkNumberIsEven", Some("ExampleContract"))
            .expect("Contract function node ExampleContract.checkNumberIsEven not found");
        assert_eq!(contract_func_node.node_type, NodeType::Function);
        assert_visibility(&contract_func_node, Visibility::Public);

        // Verify Edge (checkNumberIsEven -> isEven)
        assert_eq!(
            graph.edges.len(),
            1,
            "Should find exactly 1 edge (checkNumberIsEven -> isEven via 'using for')"
        );
        let edge = &graph.edges[0];
        assert_eq!(
            edge.source_node_id, contract_func_node.id,
            "Edge source should be checkNumberIsEven"
        );
        assert_eq!(
            edge.target_node_id, lib_func_node.id,
            "Edge target should be MathUtils.isEven"
        );
        assert_eq!(edge.edge_type, EdgeType::Call, "Edge type should be Call");
        assert_eq!(edge.sequence_number, 1, "Edge sequence number should be 1"); // First call within checkNumberIsEven

        Ok(())
    }

    #[test]
    fn test_interface_definition() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        interface IERC20 {
            function totalSupply() external view returns (uint256);
            function balanceOf(address account) external view returns (uint256);
            function transfer(address recipient, uint256 amount) external returns (bool);
        }

        contract TokenImplementation is IERC20 {
            uint256 private _totalSupply;
            mapping(address => uint256) private _balances;

            function totalSupply() external view override returns (uint256) {
                return _totalSupply;
            }

            function balanceOf(address account) external view override returns (uint256) {
                return _balances[account];
            }

            function transfer(address recipient, uint256 amount) external override returns (bool) {
                // Implementation details omitted
                return true;
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Verify interface was captured
        assert!(
            ctx.all_interfaces.contains_key("IERC20"),
            "Interface IERC20 should be captured"
        );

        // Verify interface functions were captured
        let interface_functions = ctx
            .interface_functions
            .get("IERC20")
            .expect("IERC20 functions missing");
        assert!(
            interface_functions.contains(&"totalSupply".to_string()),
            "totalSupply function missing"
        );
        assert!(
            interface_functions.contains(&"balanceOf".to_string()),
            "balanceOf function missing"
        );
        assert!(
            interface_functions.contains(&"transfer".to_string()),
            "transfer function missing"
        );

        // Verify interface node was created
        let interface_node =
            find_node(&graph, "IERC20", Some("IERC20")).expect("IERC20 node missing");
        assert_eq!(
            interface_node.node_type,
            NodeType::Interface,
            "Node type should be Interface"
        );

        // Verify interface function nodes were created
        let total_supply_node =
            find_node(&graph, "totalSupply", Some("IERC20")).expect("totalSupply node missing");
        let balance_of_node =
            find_node(&graph, "balanceOf", Some("IERC20")).expect("balanceOf node missing");
        let transfer_node =
            find_node(&graph, "transfer", Some("IERC20")).expect("transfer node missing");

        assert_eq!(total_supply_node.node_type, NodeType::Function);
        assert_eq!(balance_of_node.node_type, NodeType::Function);
        assert_eq!(transfer_node.node_type, NodeType::Function);

        // Verify implementation functions were created
        let impl_total_supply = find_node(&graph, "totalSupply", Some("TokenImplementation"))
            .expect("Implementation totalSupply missing");
        let impl_balance_of = find_node(&graph, "balanceOf", Some("TokenImplementation"))
            .expect("Implementation balanceOf missing");
        let impl_transfer = find_node(&graph, "transfer", Some("TokenImplementation"))
            .expect("Implementation transfer missing");

        assert_eq!(impl_total_supply.node_type, NodeType::Function);
        assert_eq!(impl_balance_of.node_type, NodeType::Function);
        assert_eq!(impl_transfer.node_type, NodeType::Function);

        Ok(())
    }

    #[test]
    fn test_interface_inheritance() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        // Base interface
        interface IERC20 {
            function totalSupply() external view returns (uint256);
            function balanceOf(address account) external view returns (uint256);
        }

        // Extended interface that inherits from base
        interface IERC20Extended is IERC20 {
            function name() external view returns (string memory);
            function symbol() external view returns (string memory);
        }

        // Contract implementing the extended interface
        contract CompleteToken is IERC20Extended {
            string private _name;
            string private _symbol;
            uint256 private _totalSupply;
            mapping(address => uint256) private _balances;

            constructor(string memory name_, string memory symbol_) {
                _name = name_;
                _symbol = symbol_;
            }

            function name() external view override returns (string memory) {
                return _name;
            }

            function symbol() external view override returns (string memory) {
                return _symbol;
            }

            function totalSupply() external view override returns (uint256) {
                return _totalSupply;
            }

            function balanceOf(address account) external view override returns (uint256) {
                return _balances[account];
            }
        }

        // Contract implementing only the base interface
        contract BasicToken is IERC20 {
            uint256 private _totalSupply;
            mapping(address => uint256) private _balances;

            function totalSupply() external view override returns (uint256) {
                return _totalSupply;
            }

            function balanceOf(address account) external view override returns (uint256) {
                return _balances[account];
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // 1. Verify interfaces were captured
        assert!(
            ctx.all_interfaces.contains_key("IERC20"),
            "Interface IERC20 should be captured"
        );
        assert!(
            ctx.all_interfaces.contains_key("IERC20Extended"),
            "Interface IERC20Extended should be captured"
        );

        println!("ctx.contract_implements: {:?}", ctx.contract_implements);
        println!("ctx.interface_inherits: {:?}", ctx.interface_inherits);

        // 2. Verify interface inheritance relationship
        let ierc20_extended_implements = ctx
            .interface_inherits
            .get("IERC20Extended")
            .expect("IERC20Extended inheritance relationship missing");
        assert!(
            ierc20_extended_implements.contains(&"IERC20".to_string()),
            "IERC20Extended should implement IERC20"
        );

        // 3. Verify contract implementation relationships
        let complete_token_implements = ctx
            .contract_implements
            .get("CompleteToken")
            .expect("CompleteToken implementation relationship missing");
        assert!(
            complete_token_implements.contains(&"IERC20Extended".to_string()),
            "CompleteToken should implement IERC20Extended"
        );

        let basic_token_implements = ctx
            .contract_implements
            .get("BasicToken")
            .expect("BasicToken implementation relationship missing");
        assert!(
            basic_token_implements.contains(&"IERC20".to_string()),
            "BasicToken should implement IERC20"
        );

        // 4. Verify interface functions were captured
        let ierc20_functions = ctx
            .interface_functions
            .get("IERC20")
            .expect("IERC20 functions missing");
        assert!(
            ierc20_functions.contains(&"totalSupply".to_string())
                && ierc20_functions.contains(&"balanceOf".to_string()),
            "IERC20 functions incomplete"
        );

        let ierc20_extended_functions = ctx
            .interface_functions
            .get("IERC20Extended")
            .expect("IERC20Extended functions missing");
        assert!(
            ierc20_extended_functions.contains(&"name".to_string())
                && ierc20_extended_functions.contains(&"symbol".to_string()),
            "IERC20Extended functions incomplete"
        );

        // 5. Verify nodes were created for all interfaces and contracts
        assert!(
            find_node(&graph, "IERC20", Some("IERC20")).is_some(),
            "IERC20 node missing"
        );
        assert!(
            find_node(&graph, "IERC20Extended", Some("IERC20Extended")).is_some(),
            "IERC20Extended node missing"
        );
        assert!(
            find_node(&graph, "CompleteToken", Some("CompleteToken")).is_some(),
            "CompleteToken constructor node missing"
        );
        assert!(
            find_node(&graph, "BasicToken", Some("BasicToken")).is_some(),
            "BasicToken constructor node missing"
        );

        // 6. Verify function nodes were created
        // Base interface functions
        assert!(
            find_node(&graph, "totalSupply", Some("IERC20")).is_some(),
            "IERC20.totalSupply node missing"
        );
        assert!(
            find_node(&graph, "balanceOf", Some("IERC20")).is_some(),
            "IERC20.balanceOf node missing"
        );

        // Extended interface functions
        assert!(
            find_node(&graph, "name", Some("IERC20Extended")).is_some(),
            "IERC20Extended.name node missing"
        );
        assert!(
            find_node(&graph, "symbol", Some("IERC20Extended")).is_some(),
            "IERC20Extended.symbol node missing"
        );

        // CompleteToken implementation functions
        assert!(
            find_node(&graph, "name", Some("CompleteToken")).is_some(),
            "CompleteToken.name node missing"
        );
        assert!(
            find_node(&graph, "symbol", Some("CompleteToken")).is_some(),
            "CompleteToken.symbol node missing"
        );
        assert!(
            find_node(&graph, "totalSupply", Some("CompleteToken")).is_some(),
            "CompleteToken.totalSupply node missing"
        );
        assert!(
            find_node(&graph, "balanceOf", Some("CompleteToken")).is_some(),
            "CompleteToken.balanceOf node missing"
        );

        // BasicToken implementation functions
        assert!(
            find_node(&graph, "totalSupply", Some("BasicToken")).is_some(),
            "BasicToken.totalSupply node missing"
        );
        assert!(
            find_node(&graph, "balanceOf", Some("BasicToken")).is_some(),
            "BasicToken.balanceOf node missing"
        );

        Ok(())
    }

    #[test]
    fn test_interface_invocation_single_implementation() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        interface ICounter {
            function increment() external;
        }

        contract Counter is ICounter {
            uint public count;
            function increment() external override {
                count += 1;
            }
        }

        contract CounterUser {
            ICounter public _counter; // State variable of interface type

            constructor(address counterAddress) {
                _counter = ICounter(counterAddress); // Assume setup elsewhere
            }

            function useCounter() public {
                _counter.increment(); // Call via interface
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // --- Run the pipeline ---
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Nodes:
        // 1. Interface: ICounter
        // 2. Interface Func: ICounter.increment
        // 3. Contract Func: Counter.increment
        // 4. Contract Ctor: Counter (default)
        // 5. Contract Func: CounterUser.useCounter
        // 6. Contract Ctor: CounterUser (explicit)
        assert_eq!(graph.nodes.len(), 6, "Should find 6 nodes");

        // Verify interface and implementation details in context (populated by ContractHandling)
        assert!(
            ctx.all_interfaces.contains_key("ICounter"),
            "Context should contain ICounter interface"
        );
        assert!(
            ctx.interface_functions
                .get("ICounter")
                .map_or(false, |funcs| funcs.contains(&"increment".to_string())),
            "Context should contain ICounter.increment function"
        );
        assert!(
            ctx.contract_implements
                .get("Counter")
                .map_or(false, |ifaces| ifaces.contains(&"ICounter".to_string())),
            "Context should show Counter implements ICounter"
        );
        // Check that ContractHandling populated the state variable type
        assert!(
            ctx.state_var_types
                .contains_key(&("CounterUser".to_string(), "_counter".to_string())),
            "Context should contain type info for CounterUser._counter"
        );
        assert_eq!(
            ctx.state_var_types
                .get(&("CounterUser".to_string(), "_counter".to_string())),
            Some(&"ICounter".to_string()),
            "CounterUser._counter type should be ICounter"
        );

        // Find relevant nodes
        let user_use_node = find_node(&graph, "useCounter", Some("CounterUser"))
            .expect("CounterUser.useCounter node missing");
        let impl_inc_node = find_node(&graph, "increment", Some("Counter"))
            .expect("Counter.increment node missing");
        let _iface_inc_node = find_node(&graph, "increment", Some("ICounter"))
            .expect("ICounter.increment node missing"); // Keep for node count check

        // Edges:
        // 1. CounterUser.constructor -> ICounter.constructor (implicit/type cast - not currently tracked)
        // 2. CounterUser.useCounter -> Counter.increment (via interface resolution)
        // Note: Constructor call resolution might add another edge depending on implementation.
        // We focus on the interface call edge here.
        let interface_call_edge = graph
            .edges
            .iter()
            .find(|e| e.source_node_id == user_use_node.id)
            .expect("Interface call edge not found");

        assert_eq!(
            interface_call_edge.source_node_id, user_use_node.id,
            "Edge source should be CounterUser.useCounter"
        );
        assert_eq!(
            interface_call_edge.target_node_id, impl_inc_node.id,
            "Edge target should be Counter.increment (the implementation)"
        );
        assert_eq!(
            interface_call_edge.edge_type,
            EdgeType::Call,
            "Edge type should be Call"
        );
        // Sequence number depends on constructor processing, let's check it's > 0
        assert!(
            interface_call_edge.sequence_number > 0,
            "Edge sequence number should be positive"
        );

        // Check total number of edges (might be 1 or 2 depending on constructor call resolution)
        assert!(
            graph.edges.len() >= 1 && graph.edges.len() <= 2,
            "Should find 1 or 2 edges (interface call + optional constructor call)"
        );

        Ok(())
    }

    #[test]
    fn test_chained_call_resolution() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        library SafeMath {
            function add(uint256 a, uint256 b) internal pure returns (uint256) { return a + b; }
            function sub(uint256 a, uint256 b) internal pure returns (uint256) { return a - b; }
        }

        contract ChainedCalls {
            using SafeMath for uint256;

            uint256 public value;

            function complexUpdate(uint256 _add, uint256 _sub) public {
                // Chained call: value.add(_add).sub(_sub)
                value = value.add(_add).sub(_sub);
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Nodes:
        // 1. Library: SafeMath
        // 2. Function: SafeMath.add
        // 3. Function: SafeMath.sub
        // 4. Function: ChainedCalls.complexUpdate
        // 5. Constructor: ChainedCalls (default)
        assert_eq!(
            graph.nodes.len(),
            5,
            "Should find 5 nodes (library, 2 lib funcs, contract func, default ctor)"
        );

        // Find relevant nodes
        let lib_add_node =
            find_node(&graph, "add", Some("SafeMath")).expect("SafeMath.add node missing");
        let lib_sub_node =
            find_node(&graph, "sub", Some("SafeMath")).expect("SafeMath.sub node missing");
        let contract_update_node = find_node(&graph, "complexUpdate", Some("ChainedCalls"))
            .expect("ChainedCalls.complexUpdate node missing");

        // Edges:
        // 1. complexUpdate -> SafeMath.add (for value.add(_add))
        // 2. complexUpdate -> SafeMath.sub (for (...).sub(_sub))
        assert_eq!(
            graph.edges.len(),
            2,
            "Should find 2 edges for the chained call"
        );

        // Verify edge 1: complexUpdate -> add
        let edge_to_add = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == contract_update_node.id && e.target_node_id == lib_add_node.id
            })
            .expect("Edge complexUpdate -> add missing");
        assert_eq!(edge_to_add.edge_type, EdgeType::Call);
        // Sequence number depends on parsing order, check it's 1 or 2
        assert!(
            edge_to_add.sequence_number == 1 || edge_to_add.sequence_number == 2,
            "Edge complexUpdate -> add sequence invalid"
        );

        // Verify edge 2: complexUpdate -> sub
        let edge_to_sub = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == contract_update_node.id && e.target_node_id == lib_sub_node.id
            })
            .expect("Edge complexUpdate -> sub missing");
        assert_eq!(edge_to_sub.edge_type, EdgeType::Call);
        // Sequence number depends on parsing order, check it's 1 or 2 and different from add edge
        assert!(
            edge_to_sub.sequence_number == 1 || edge_to_sub.sequence_number == 2,
            "Edge complexUpdate -> sub sequence invalid"
        );
        assert_ne!(
            edge_to_add.sequence_number, edge_to_sub.sequence_number,
            "Sequence numbers for add and sub should be different"
        );

        Ok(())
    }

    #[test]
    fn test_explicit_return_edge_generation() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.0;

        contract ReturnTest {
            function returnsValue() internal pure returns (uint) {
                return 42; // Explicit return with value
            }

            function returnsNothingExplicitly() internal pure {
                return; // Explicit empty return
            }

            function noReturnStatement() internal pure {
                // No explicit return statement
                uint x = 1;
            }

            function caller() public pure {
                uint val = returnsValue();
                returnsNothingExplicitly();
                noReturnStatement();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new();

        // Run pipeline to get nodes and call edges
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Extract necessary info before mutable borrow ---
        let caller_node_id = find_node(&graph, "caller", Some("ReturnTest"))
            .expect("caller node")
            .id;
        let returns_value_node_id = find_node(&graph, "returnsValue", Some("ReturnTest"))
            .expect("returnsValue node")
            .id;
        let returns_nothing_node_id =
            find_node(&graph, "returnsNothingExplicitly", Some("ReturnTest"))
                .expect("returnsNothingExplicitly node")
                .id;
        let no_return_node_id = find_node(&graph, "noReturnStatement", Some("ReturnTest"))
            .expect("noReturnStatement node")
            .id;

        // Get the call edges to find their sequence numbers
        let call_to_value_seq = graph
            .iter_edges()
            .find(|e| {
                e.source_node_id == caller_node_id && e.target_node_id == returns_value_node_id
            })
            .expect("Call edge to returnsValue")
            .sequence_number;
        let call_to_nothing_seq = graph
            .iter_edges()
            .find(|e| {
                e.source_node_id == caller_node_id && e.target_node_id == returns_nothing_node_id
            })
            .expect("Call edge to returnsNothingExplicitly")
            .sequence_number;
        // We don't need the sequence number for the call to noReturnStatement for the return edge checks
        // let _call_to_noreturn_seq = graph.iter_edges().find(|e| e.source_node_id == caller_node_id && e.target_node_id == no_return_node_id).expect("Call edge to noReturnStatement").sequence_number;

        // --- Immutable borrows end here ---

        // Add explicit return edges (Mutable borrow)
        graph.add_explicit_return_edges(source, &ctx, &solidity_lang)?;

        // --- Start new immutable borrows for assertions ---

        // Assertions
        // Nodes: caller, returnsValue, returnsNothingExplicitly, noReturnStatement, + default constructor
        assert_eq!(graph.nodes.len(), 5, "Should find 5 nodes");
        // Edges: 3 calls + 2 returns = 5 edges
        assert_eq!(
            graph.edges.len(),
            5,
            "Should find 3 call edges and 2 return edges"
        );

        // 1. Check return edge from returnsValue
        let return_from_value_edge = graph
            .iter_edges()
            .find(|e| {
                e.edge_type == EdgeType::Return
                    && e.source_node_id == returns_value_node_id
                    && e.target_node_id == caller_node_id
            })
            .expect("Return edge from returnsValue not found");

        assert_eq!(
            return_from_value_edge.sequence_number, call_to_value_seq,
            "Sequence number mismatch for returnsValue return"
        );
        assert_eq!(
            return_from_value_edge.returned_value,
            Some("42".to_string()),
            "Returned value mismatch for returnsValue"
        );
        assert!(
            return_from_value_edge.return_site_span.is_some(),
            "Return site span missing for returnsValue"
        );

        // 2. Check return edge from returnsNothingExplicitly
        let return_from_nothing_edge = graph
            .iter_edges()
            .find(|e| {
                e.edge_type == EdgeType::Return
                    && e.source_node_id == returns_nothing_node_id
                    && e.target_node_id == caller_node_id
            })
            .expect("Return edge from returnsNothingExplicitly not found");

        assert_eq!(
            return_from_nothing_edge.sequence_number, call_to_nothing_seq,
            "Sequence number mismatch for returnsNothingExplicitly return"
        );
        // The query captures the `return_statement` node, but the optional `expression` capture (@return_value) will be None for `return;`
        assert_eq!(
            return_from_nothing_edge.returned_value, None,
            "Returned value should be None for empty return"
        );
        assert!(
            return_from_nothing_edge.return_site_span.is_some(),
            "Return site span missing for returnsNothingExplicitly"
        );

        // 3. Check NO return edge from noReturnStatement
        let no_return_edge = graph
            .iter_edges()
            .find(|e| e.edge_type == EdgeType::Return && e.source_node_id == no_return_node_id);
        assert!(
            no_return_edge.is_none(),
            "Should be no return edge from noReturnStatement"
        );

        Ok(())
    }

    #[test]
    fn test_direct_library_call() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        library Lib {
            function doSomething() internal pure returns (uint256) {
                return 1;
            }
        }

        contract Caller {
            function callLib() public pure returns (uint256) {
                // Direct library call
                return Lib.doSomething();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Nodes:
        // 1. Library: Lib
        // 2. Function: Lib.doSomething
        // 3. Function: Caller.callLib
        // 4. Constructor: Caller (default)
        assert_eq!(
            graph.nodes.len(),
            4,
            "Should find 4 nodes (library, lib func, contract func, default ctor)"
        );

        // Find relevant nodes
        let lib_func_node =
            find_node(&graph, "doSomething", Some("Lib")).expect("Lib.doSomething node missing");
        let contract_call_node =
            find_node(&graph, "callLib", Some("Caller")).expect("Caller.callLib node missing");

        // Verify Edge (callLib -> doSomething)
        assert_eq!(
            graph.edges.len(),
            1,
            "Should find exactly 1 edge (callLib -> doSomething)"
        );
        let edge = &graph.edges[0];
        assert_eq!(
            edge.source_node_id, contract_call_node.id,
            "Edge source should be Caller.callLib"
        );
        assert_eq!(
            edge.target_node_id, lib_func_node.id,
            "Edge target should be Lib.doSomething"
        );
        assert_eq!(edge.edge_type, EdgeType::Call, "Edge type should be Call");
        assert_eq!(edge.sequence_number, 1, "Edge sequence number should be 1");

        Ok(())
    }

    #[test]
    fn test_chained_library_call_resolution() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        library SafeMath {
            function mul(uint256 a, uint256 b) internal pure returns (uint256) { return a * b; }
            function sub(uint256 a, uint256 b) internal pure returns (uint256) { return a - b; }
        }

        contract ChainedLibCalls {
            using SafeMath for uint256;

            uint256 public balance;
            // uint256 public amountIn; // Not strictly needed for this test's focus

            function complexUpdate() public {
                // Direct chained call: result_of_mul.sub(...)
                // Mimics balance.mul(1000).sub(amountIn.mul(3)) structure's outer chain
                balance = balance.mul(1000).sub(3);
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Nodes:
        // 1. Library: SafeMath
        // 2. Function: SafeMath.mul
        // 3. Function: SafeMath.sub
        // 4. Function: ChainedLibCalls.complexUpdate
        // 5. Constructor: ChainedLibCalls (default)
        assert_eq!(
            graph.nodes.len(),
            5,
            "Should find 5 nodes (library, 2 lib funcs, contract func, default ctor)"
        );

        // Find relevant nodes
        let lib_mul_node =
            find_node(&graph, "mul", Some("SafeMath")).expect("SafeMath.mul node missing");
        let lib_sub_node =
            find_node(&graph, "sub", Some("SafeMath")).expect("SafeMath.sub node missing");
        let contract_update_node = find_node(&graph, "complexUpdate", Some("ChainedLibCalls"))
            .expect("ChainedLibCalls.complexUpdate node missing");

        // Edges:
        // 1. complexUpdate -> SafeMath.mul (for balance.mul(1000))
        // 2. complexUpdate -> SafeMath.sub (for (...).sub(3))
        assert_eq!(
            graph.edges.len(),
            2,
            "Should find 2 edges for the chained library call"
        );

        // Verify edge 1: complexUpdate -> mul
        let edge_to_mul = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == contract_update_node.id && e.target_node_id == lib_mul_node.id
            })
            .expect("Edge complexUpdate -> mul missing");
        assert_eq!(edge_to_mul.edge_type, EdgeType::Call);
        // Sequence number depends on parsing order, check it's 1 or 2
        assert!(
            edge_to_mul.sequence_number == 1 || edge_to_mul.sequence_number == 2,
            "Edge complexUpdate -> mul sequence invalid"
        );

        // Verify edge 2: complexUpdate -> sub
        let edge_to_sub = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == contract_update_node.id && e.target_node_id == lib_sub_node.id
            })
            .expect("Edge complexUpdate -> sub missing");
        assert_eq!(edge_to_sub.edge_type, EdgeType::Call);
        // Sequence number depends on parsing order, check it's 1 or 2 and different from mul edge
        assert!(
            edge_to_sub.sequence_number == 1 || edge_to_sub.sequence_number == 2,
            "Edge complexUpdate -> sub sequence invalid"
        );
        assert_ne!(
            edge_to_mul.sequence_number, edge_to_sub.sequence_number,
            "Sequence numbers for mul and sub should be different"
        );

        Ok(())
    }

    #[test]
    fn test_interface_call_resolution_factory_pattern() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        // Interface for the action contract
        interface IAction {
            function performAction() external returns (bool);
        }

        // Implementation of the action contract
        contract ActionImpl is IAction {
            function performAction() external override returns (bool) {
                // Implementation logic
                return true;
            }
        }

        // Interface for the factory
        interface IActionFactory {
            function createAction() external returns (IAction);
        }

        // Factory contract that creates ActionImpl instances
        contract ActionFactory is IActionFactory {
            function createAction() external override returns (IAction) {
                // Creates a new instance of ActionImpl
                return new ActionImpl();
            }
        }

        // Contract that uses the factory to get an action contract and call it
        contract ActionCaller {
            address public factoryAddress; // Store factory address

            constructor(address _factoryAddress) {
                factoryAddress = _factoryAddress;
                // Factory interaction moved to triggerAction
            }

            function triggerAction() public returns (bool) {
                // Chained call: Cast address -> call factory -> call action
                // This mimics the IUniswapV2Factory(factory).feeTo() pattern
                // 1. IActionFactory(factoryAddress) -> Cast
                // 2. .createAction() -> Calls ActionFactory.createAction (returns IAction)
                // 3. .performAction() -> Calls ActionImpl.performAction on the returned IAction
                return IActionFactory(factoryAddress).createAction().performAction();
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Nodes:
        // Interfaces: IAction, IActionFactory
        // Interface Funcs: IAction.performAction, IActionFactory.createAction
        // Contracts: ActionImpl, ActionFactory, ActionCaller
        // Contract Funcs: ActionImpl.performAction, ActionFactory.createAction, ActionCaller.triggerAction
        // Contract Ctors: ActionImpl (default), ActionFactory (default), ActionCaller (explicit)
        // Nodes:
        // Interfaces: IAction, IActionFactory (2)
        // Interface Funcs: IAction.performAction, IActionFactory.createAction (2)
        // Contract Funcs: ActionImpl.performAction, ActionFactory.createAction, ActionCaller.triggerAction (3)
        // Contract Ctors: ActionImpl (default), ActionFactory (default), ActionCaller (explicit) (3)
        // Total: 2 + 2 + 3 + 3 = 10 nodes
        assert_eq!(
            graph.nodes.len(),
            10, // Corrected expected node count
            "Should find 10 nodes (interfaces, funcs, contracts, ctors)"
        );

        // Verify context population (ContractHandling)
        assert!(
            ctx.all_interfaces.contains_key("IAction"),
            "Context should contain IAction interface"
        );
        assert!(
            ctx.all_interfaces.contains_key("IActionFactory"),
            "Context should contain IActionFactory interface"
        );
        assert!(
            ctx.interface_functions
                .get("IAction")
                .map_or(false, |funcs| funcs.contains(&"performAction".to_string())),
            "Context should contain IAction.performAction function"
        );
        assert!(
            ctx.interface_functions
                .get("IActionFactory")
                .map_or(false, |funcs| funcs.contains(&"createAction".to_string())),
            "Context should contain IActionFactory.createAction function"
        );
        assert!(
            ctx.contract_implements
                .get("ActionImpl")
                .map_or(false, |ifaces| ifaces.contains(&"IAction".to_string())),
            "Context should show ActionImpl implements IAction"
        );
        assert!(
            ctx.contract_implements
                .get("ActionFactory")
                .map_or(false, |ifaces| ifaces
                    .contains(&"IActionFactory".to_string())),
            "Context should show ActionFactory implements IActionFactory"
        );
        assert!(
            ctx.state_var_types
                .contains_key(&("ActionCaller".to_string(), "factoryAddress".to_string())), // Corrected variable name
            "Context should contain type info for ActionCaller.factoryAddress"
        );
        assert_eq!(
            ctx.state_var_types
                .get(&("ActionCaller".to_string(), "factoryAddress".to_string())), // Corrected variable name
            Some(&"address".to_string()), // Corrected type (it's stored as address)
            "ActionCaller.factoryAddress type should be address"
        );

        // Find relevant nodes for the core assertion
        let caller_trigger_node = find_node(&graph, "triggerAction", Some("ActionCaller"))
            .expect("ActionCaller.triggerAction node missing");
        let impl_perform_node = find_node(&graph, "performAction", Some("ActionImpl"))
            .expect("ActionImpl.performAction node missing");
        let _caller_ctor_node = find_node(&graph, "ActionCaller", Some("ActionCaller")) // Mark unused
            .expect("ActionCaller constructor node missing");
        let factory_create_node = find_node(&graph, "createAction", Some("ActionFactory"))
            .expect("ActionFactory.createAction node missing");
        let impl_ctor_node = find_node(&graph, "ActionImpl", Some("ActionImpl"))
            .expect("ActionImpl constructor node missing");

        // Verify Edges
        // 1. ActionCaller.triggerAction -> ActionFactory.createAction (via `IActionFactory(factoryAddress).createAction()`)
        // 2. ActionFactory.createAction -> ActionImpl.constructor (via `new ActionImpl()`)
        // 3. ActionCaller.triggerAction -> ActionImpl.performAction (via chained call `(...).performAction()`)
        println!("Edges: {:?}", graph.edges);
        assert_eq!(
            graph.edges.len(),
            3,
            "Should find 3 edges (trigger->factory.create, factory.create->impl.ctor, trigger->impl.perform)"
        );

        // Verify Edge 1: Caller TriggerAction -> Factory CreateAction
        let edge_trigger_to_factory = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == caller_trigger_node.id
                    && e.target_node_id == factory_create_node.id
            })
            .expect("Edge ActionCaller.triggerAction -> ActionFactory.createAction missing");
        assert_eq!(edge_trigger_to_factory.edge_type, EdgeType::Call);

        // Verify Edge 2: Factory CreateAction -> Impl Constructor
        let edge_factory_to_impl_ctor = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == factory_create_node.id && e.target_node_id == impl_ctor_node.id
            })
            .expect("Edge ActionFactory.createAction -> ActionImpl.ctor missing");
        assert_eq!(edge_factory_to_impl_ctor.edge_type, EdgeType::Call);

        // Verify Edge 3: Caller TriggerAction -> Impl PerformAction (Chained Call)
        let edge_trigger_to_impl = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == caller_trigger_node.id
                    && e.target_node_id == impl_perform_node.id
            })
            .expect("Edge ActionCaller.triggerAction -> ActionImpl.performAction missing");
        assert_eq!(
            edge_trigger_to_impl.source_node_id, caller_trigger_node.id,
            "Edge source should be ActionCaller.triggerAction"
        );
        assert_eq!(
            edge_trigger_to_impl.target_node_id, impl_perform_node.id,
            "Edge target should be ActionImpl.performAction (the implementation)"
        );
        assert_eq!(
            edge_trigger_to_impl.edge_type,
            EdgeType::Call,
            "Edge type should be Call"
        );

        // Check sequence numbers within triggerAction
        // Call 1: .createAction() -> edge_trigger_to_factory
        // Call 2: .performAction() -> edge_trigger_to_impl
        // Sequence numbers depend on the order tree-sitter finds the call_expression nodes.
        // Let's assert they are 1 and 2 in some order.
        let seq1 = edge_trigger_to_factory.sequence_number;
        let seq2 = edge_trigger_to_impl.sequence_number;
        assert!(
            (seq1 == 1 && seq2 == 2) || (seq1 == 2 && seq2 == 1),
            "Sequence numbers within triggerAction should be 1 and 2 (found {} and {})",
            seq1,
            seq2
        );

        Ok(())
    }

    #[test]
    fn test_argument_capturing() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        contract CalleeContract {
            function externalTarget(uint amount, string memory message) public pure returns (bool) {
                return amount > 0 && bytes(message).length > 0;
            }
        }

        contract CallerContract {
            CalleeContract public calleeInstance;

            constructor(address _callee) {
                calleeInstance = CalleeContract(_callee);
            }

            function internalTarget(uint value) internal pure returns (uint) {
                return value * 2;
            }

            // Scenario 1: Intra-contract call with arguments
            function callInternal(uint data) public pure returns (uint) {
                return internalTarget(data + 1); // Argument: data + 1
            }

            // Scenario 2: Contract-to-contract call with arguments
            function callExternal(uint num, string memory text) public returns (bool) {
                return calleeInstance.externalTarget(num, text); // Arguments: num, text
            }

            // Scenario 3: Public function (simulating user call) with arguments
            function entryPoint(uint startValue, address recipient) public pure {
                // Arguments: startValue, recipient
                uint _ = startValue; // Use args to avoid warnings
                address _ = recipient;
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Find nodes
        let caller_internal_node = find_node(&graph, "callInternal", Some("CallerContract"))
            .expect("CallerContract.callInternal node missing");
        let target_internal_node = find_node(&graph, "internalTarget", Some("CallerContract"))
            .expect("CallerContract.internalTarget node missing");
        let caller_external_node = find_node(&graph, "callExternal", Some("CallerContract"))
            .expect("CallerContract.callExternal node missing");
        let target_external_node = find_node(&graph, "externalTarget", Some("CalleeContract"))
            .expect("CalleeContract.externalTarget node missing");
        let entry_point_node = find_node(&graph, "entryPoint", Some("CallerContract"))
            .expect("CallerContract.entryPoint node missing");
        let _caller_ctor_node = find_node(&graph, "CallerContract", Some("CallerContract")) // Mark unused
            .expect("CallerContract constructor node missing");
        let _callee_ctor_node = find_node(&graph, "CalleeContract", Some("CalleeContract")) // Mark unused
            .expect("CalleeContract constructor node missing");

        // 1. Intra-contract call: callInternal -> internalTarget
        let intra_contract_edge = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == caller_internal_node.id
                    && e.target_node_id == target_internal_node.id
            })
            .expect("Edge callInternal -> internalTarget missing");

        assert_eq!(
            intra_contract_edge.argument_names,
            Some(vec!["data + 1".to_string()]),
            "Intra-contract call arguments mismatch"
        );

        // 2. Contract-to-contract call: callExternal -> externalTarget
        let inter_contract_edge = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == caller_external_node.id
                    && e.target_node_id == target_external_node.id
            })
            .expect("Edge callExternal -> externalTarget missing");

        assert_eq!(
            inter_contract_edge.argument_names,
            Some(vec!["num".to_string(), "text".to_string()]),
            "Inter-contract call arguments mismatch"
        );

        // 3. User-to-contract call (entryPoint): No direct edge generated for user calls,
        //    but we can check the node itself exists.
        //    Argument capturing is tested by the other two scenarios which rely on the same mechanism.
        assert_eq!(entry_point_node.name, "entryPoint");
        assert_eq!(
            entry_point_node.contract_name,
            Some("CallerContract".to_string())
        );

        // Check total edges (callInternal->internalTarget, callExternal->externalTarget, constructor->constructor)
        assert_eq!(graph.edges.len(), 3, "Expected 3 edges");

        Ok(())
    }

    #[test]
    fn test_simple_emit_statement() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        contract EventEmitter {
            event ValueChanged(uint256 indexed oldValue, uint256 newValue);

            uint256 private _value;

            function updateValue(uint256 newValue) public {
                uint256 oldValue = _value;
                _value = newValue;
                emit ValueChanged(oldValue, newValue); // Simple emit
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---

        // Nodes:
        // 1. Function: EventEmitter.updateValue
        // 2. Constructor: EventEmitter (default)
        // 3. Synthetic: EVM
        // 4. Synthetic: EventListener
        assert_eq!(
            graph.nodes.len(),
            4,
            "Should find 4 nodes (updateValue, default ctor, EVM, EventListener)"
        );

        // Find relevant nodes
        let update_value_node = find_node(&graph, "updateValue", Some("EventEmitter"))
            .expect("EventEmitter.updateValue node missing");
        let evm_node = find_node(&graph, EVM_NODE_NAME, None).expect("EVM node missing");
        let listener_node = find_node(&graph, EVENT_LISTENER_NODE_NAME, None)
            .expect("EventListener node missing");

        assert_eq!(evm_node.node_type, NodeType::Evm);
        assert_eq!(listener_node.node_type, NodeType::EventListener);

        // Edges:
        // 1. updateValue -> EVM
        // 2. EVM -> EventListener
        assert_eq!(graph.edges.len(), 2, "Should find 2 edges for the emit");

        // Verify Edge 1: updateValue -> EVM
        let edge_func_to_evm = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == update_value_node.id && e.target_node_id == evm_node.id
            })
            .expect("Edge updateValue -> EVM missing");

        assert_eq!(edge_func_to_evm.edge_type, EdgeType::Call);
        assert_eq!(edge_func_to_evm.sequence_number, 1, "Emit sequence number should be 1");
        assert_eq!(
            edge_func_to_evm.event_name,
            Some("ValueChanged".to_string()),
            "Event name mismatch on func->EVM edge"
        );
        assert_eq!(
            edge_func_to_evm.argument_names,
            Some(vec!["oldValue".to_string(), "newValue".to_string()]),
            "Arguments mismatch on func->EVM edge"
        );

        // Verify Edge 2: EVM -> EventListener
        let edge_evm_to_listener = graph
            .edges
            .iter()
            .find(|e| e.source_node_id == evm_node.id && e.target_node_id == listener_node.id)
            .expect("Edge EVM -> EventListener missing");

        assert_eq!(edge_evm_to_listener.edge_type, EdgeType::Call);
        assert_eq!(
            edge_evm_to_listener.sequence_number, 1,
            "Sequence number should be the same for both emit edges"
        );
        assert_eq!(
            edge_evm_to_listener.event_name,
            Some("ValueChanged".to_string()),
            "Event name mismatch on EVM->Listener edge"
        );
        assert_eq!(
            edge_evm_to_listener.argument_names,
            Some(vec!["oldValue".to_string(), "newValue".to_string()]),
            "Arguments mismatch on EVM->Listener edge"
        );

        Ok(())
    }


    #[test]
    fn test_interface_call_resolution_factory_pattern_no_return() -> Result<()> {
        let source = r#"
        pragma solidity ^0.8.20;

        // Interface for the action contract
        interface IAction {
            function performAction() external returns (bool);
        }

        // Implementation of the action contract
        contract ActionImpl is IAction {
            function performAction() external override returns (bool) {
                // Implementation logic
                return true;
            }
        }

        // Interface for the factory
        interface IActionFactory {
            function createAction() external returns (IAction);
        }

        // Factory contract that creates ActionImpl instances
        contract ActionFactory is IActionFactory {
            function createAction() external override returns (IAction) {
                // Creates a new instance of ActionImpl
                return new ActionImpl();
            }
        }

        // Contract that uses the factory to get an action contract and call it
        contract ActionCaller {
            address public factoryAddress; // Store factory address

            constructor(address _factoryAddress) {
                factoryAddress = _factoryAddress;
            }

            function triggerAction() public { // Changed: No return value
                // Chained call: Cast address -> call factory -> call action
                // Result is not returned, just executed.
                IActionFactory(factoryAddress).createAction().performAction(); // Changed: No return statement
            }
        }
        "#;
        let ast = parse_solidity(source)?;
        let solidity_lang = Solidity.get_tree_sitter_language();

        let input = CallGraphGeneratorInput {
            source,
            tree: &ast.tree,
            solidity_lang: &solidity_lang,
        };
        let mut ctx = CallGraphGeneratorContext {
            state_var_types: HashMap::new(),
            definition_nodes_info: Vec::new(),
            all_contracts: HashMap::new(),
            contracts_with_explicit_constructors: HashSet::new(),
            using_for_directives: HashMap::new(),
            all_interfaces: HashMap::new(),
            interface_functions: HashMap::new(),
            contract_implements: HashMap::new(),
            interface_inherits: HashMap::new(),
            all_libraries: HashMap::new(),
        };
        let mut graph = CallGraph::new();
        let config: HashMap<String, String> = HashMap::new(); // Empty config

        // Run the full pipeline
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        pipeline.run(&input, &mut ctx, &mut graph, &config)?;

        // --- Assertions ---
        // These assertions should be identical to the original test,
        // as the call structure is the same.

        // Nodes:
        // Interfaces: IAction, IActionFactory (2)
        // Interface Funcs: IAction.performAction, IActionFactory.createAction (2)
        // Contracts: ActionImpl, ActionFactory, ActionCaller
        // Contract Funcs: ActionImpl.performAction, ActionFactory.createAction, ActionCaller.triggerAction (3)
        // Contract Ctors: ActionImpl (default), ActionFactory (default), ActionCaller (explicit) (3)
        // Total: 2 + 2 + 3 + 3 = 10 nodes
        assert_eq!(
            graph.nodes.len(),
            10,
            "NoReturn: Should find 10 nodes (interfaces, funcs, contracts, ctors)"
        );

        // Find relevant nodes for the core assertion
        let caller_trigger_node = find_node(&graph, "triggerAction", Some("ActionCaller"))
            .expect("NoReturn: ActionCaller.triggerAction node missing");
        let impl_perform_node = find_node(&graph, "performAction", Some("ActionImpl"))
            .expect("NoReturn: ActionImpl.performAction node missing");
        let _caller_ctor_node =
            find_node(&graph, "ActionCaller", Some("ActionCaller")) // Mark unused
                .expect("NoReturn: ActionCaller constructor node missing");
        let factory_create_node = find_node(&graph, "createAction", Some("ActionFactory"))
            .expect("NoReturn: ActionFactory.createAction node missing");
        let impl_ctor_node = find_node(&graph, "ActionImpl", Some("ActionImpl"))
            .expect("NoReturn: ActionImpl constructor node missing");

        // Verify Edges
        // 1. ActionCaller.triggerAction -> ActionFactory.createAction (via `IActionFactory(factoryAddress).createAction()`)
        // 2. ActionFactory.createAction -> ActionImpl.constructor (via `new ActionImpl()`)
        // 3. ActionCaller.triggerAction -> ActionImpl.performAction (via chained call `(...).performAction()`)
        println!("NoReturn Edges: {:?}", graph.edges);
        assert_eq!(
            graph.edges.len(),
            3,
            "NoReturn: Should find 3 edges (trigger->factory.create, factory.create->impl.ctor, trigger->impl.perform)"
        );

        // Verify Edge 1: Caller TriggerAction -> Factory CreateAction
        let edge_trigger_to_factory = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == caller_trigger_node.id
                    && e.target_node_id == factory_create_node.id
            })
            .expect(
                "NoReturn: Edge ActionCaller.triggerAction -> ActionFactory.createAction missing",
            );
        assert_eq!(edge_trigger_to_factory.edge_type, EdgeType::Call);

        // Verify Edge 2: Factory CreateAction -> Impl Constructor
        let edge_factory_to_impl_ctor = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == factory_create_node.id && e.target_node_id == impl_ctor_node.id
            })
            .expect("NoReturn: Edge ActionFactory.createAction -> ActionImpl.ctor missing");
        assert_eq!(edge_factory_to_impl_ctor.edge_type, EdgeType::Call);

        // Verify Edge 3: Caller TriggerAction -> Impl PerformAction (Chained Call)
        let edge_trigger_to_impl = graph
            .edges
            .iter()
            .find(|e| {
                e.source_node_id == caller_trigger_node.id
                    && e.target_node_id == impl_perform_node.id
            })
            .expect(
                "NoReturn: Edge ActionCaller.triggerAction -> ActionImpl.performAction missing",
            );
        assert_eq!(
            edge_trigger_to_impl.source_node_id, caller_trigger_node.id,
            "NoReturn: Edge source should be ActionCaller.triggerAction"
        );
        assert_eq!(
            edge_trigger_to_impl.target_node_id, impl_perform_node.id,
            "NoReturn: Edge target should be ActionImpl.performAction (the implementation)"
        );
        assert_eq!(
            edge_trigger_to_impl.edge_type,
            EdgeType::Call,
            "NoReturn: Edge type should be Call"
        );

        // Check sequence numbers within triggerAction
        let seq1 = edge_trigger_to_factory.sequence_number;
        let seq2 = edge_trigger_to_impl.sequence_number;
        assert!(
            (seq1 == 1 && seq2 == 2) || (seq1 == 2 && seq2 == 1),
            "NoReturn: Sequence numbers within triggerAction should be 1 and 2 (found {} and {})",
            seq1,
            seq2
        );

        Ok(())
    }
}
