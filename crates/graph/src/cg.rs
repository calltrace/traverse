use crate::cg_dot;
use crate::parser::{get_node_text, SolidityAST};
use anyhow::{Context, Result};
use language::{Language, Solidity};
use std::collections::{HashMap, HashSet}; // Import HashSet
use std::iter;
use streaming_iterator::StreamingIterator;
use tree_sitter::{Node as TsNode, Query, QueryCursor, Tree};

// --- Graph Primitives ---

/// Type of edge in the call graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum EdgeType {
    Call,
    Return,
}

/// Type of node in the call graph.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum NodeType {
    Function,
    Constructor,
    Modifier,
}

/// Visibility of a function, constructor, or modifier.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Visibility {
    Public,
    Private,
    Internal,
    External,
    Default,
}

/// Represents a node in the call graph (function, constructor, modifier).
#[derive(Debug, Clone)]
pub struct Node {
    /// Unique identifier (index in the `CallGraph::nodes` vector).
    pub id: usize,
    /// Name of the function, modifier, or contract (for constructor).
    pub name: String,
    /// Type of the node.
    pub node_type: NodeType,
    /// Name of the contract containing this node, if any.
    pub contract_name: Option<String>,
    /// Visibility of the node.
    pub visibility: Visibility,
    /// Byte range span in the original source code.
    pub span: (usize, usize),
}

// --- DOT Label Implementation ---

impl crate::cg_dot::ToDotLabel for Node {
    fn to_dot_label(&self) -> String {
        // Ensure a literal newline is used here, which will be escaped by escape_dot_string later
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

/// Represents a directed edge in the call graph.
#[derive(Debug, Clone, PartialEq)]
pub struct Edge {
    /// ID of the node where the edge originates.
    pub source_node_id: usize,
    /// ID of the node where the edge terminates.
    pub target_node_id: usize,
    /// Type of the edge (call or return).
    pub edge_type: EdgeType,
    /// Byte range span of the call expression (for Call edges),
    /// or the function definition (for Return edges).
    pub call_site_span: (usize, usize),
    /// Byte range span of the return statement (for Return edges).
    pub return_site_span: Option<(usize, usize)>,
    /// The sequence number of this call within its source node (1-based, for Call edges).
    /// Sequence number is less relevant for Return edges, typically set to 0.
    pub sequence_number: usize,
    /// Optional text representation of the value/expression being returned.
    pub returned_value: Option<String>,
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

/// Represents the Solidity call graph with ordering semantics.
#[derive(Debug, Default)]
pub struct CallGraph {
    /// Nodes (definitions) stored in the order they appear in the source.
    pub nodes: Vec<Node>,
    /// Edges (calls) stored in the order they are discovered
    /// (traversing functions in definition order, then calls within functions).
    pub edges: Vec<Edge>,
    /// Internal mapping for quick node lookup.
    /// Key: (Option<ContractName>, FunctionOrModifierName) -> Value: Node ID (index in `nodes` Vec)
    pub(crate) node_lookup: HashMap<(Option<String>, String), usize>,
}

impl CallGraph {
    /// Creates a new, empty call graph.
    pub fn new() -> Self {
        Default::default()
    }

    /// Adds a node to the graph and the lookup table.
    /// Returns the ID (index) of the newly added node.
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
        };
        self.nodes.push(node);

        self.node_lookup.insert((contract_name, name), id);
        id
    }

    /// Adds an edge to the graph.
    pub(crate) fn add_edge(
        &mut self,
        source_node_id: usize,
        target_node_id: usize,
        edge_type: EdgeType,                      // Added type
        call_site_span: (usize, usize),           // Span of the call site (or function for return)
        return_site_span: Option<(usize, usize)>, // Span of the return site
        sequence_number: usize,                   // Sequence for calls, maybe 0 for returns
        returned_value: Option<String>,           // Added returned value
    ) {
        let edge = Edge {
            source_node_id,
            target_node_id,
            edge_type, // Store type
            call_site_span,
            return_site_span, // Store return span
            sequence_number,
            returned_value, // Store returned value
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

    // --- Iterators ---

    /// Returns an iterator over the nodes in definition order.
    pub fn iter_nodes(&self) -> impl Iterator<Item = &Node> {
        self.nodes.iter()
    }

    /// Returns an iterator over the edges in the order they were found
    /// (reflecting call order within functions, processed function-by-function).
    pub fn iter_edges(&self) -> impl Iterator<Item = &Edge> {
        self.edges.iter()
    }

    /// Adds explicit return edges based on return statements found in the AST.
    /// This should be called *after* the initial graph generation.
    pub fn add_explicit_return_edges(
        &mut self,
        source: &str,
        tree: &Tree,
        solidity_lang: &tree_sitter::Language,
    ) -> Result<()> {
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

        // 2. Prepare return statement query
        // Capture the return statement itself (@return) and optionally the expression within it (@return_value)
        let return_query_str = r#"
            (return_statement
                (expression)? @return_value
            ) @return
        "#;
        let return_query = Query::new(solidity_lang, return_query_str)
            .context("Failed to create return statement query")?;
        let mut return_cursor = QueryCursor::new();
        let source_bytes = source.as_bytes();

        // 3. Iterate through nodes to find return statements within them
        let mut new_return_edges: Vec<Edge> = Vec::new(); // Collect new edges separately

        for callee_node in self.nodes.iter() {
            // Only look for returns inside functions/modifiers/constructors
            match callee_node.node_type {
                NodeType::Function | NodeType::Modifier | NodeType::Constructor => {
                    let callee_def_span = callee_node.span;

                    // Find callers and their call sequences for this callee
                    if let Some(callers_info) = callee_to_callers_and_seq.get(&callee_node.id) {
                        // Query for returns within the *entire tree* but filter by span.
                        // This is less efficient than querying only the definition node,
                        // but avoids needing to store/retrieve the TsNode for each definition.
                        let mut matches = return_cursor.matches(
                            &return_query,
                            tree.root_node(), // Query from root
                            |node: TsNode| iter::once(&source_bytes[node.byte_range()]),
                        );
                        matches.advance(); // Need to advance once initially
                        while let Some(match_) = matches.get() {
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
                                let return_span =
                                    (return_node.start_byte(), return_node.end_byte());

                                // Check if the return statement is within the callee's definition span
                                if return_span.0 >= callee_def_span.0
                                    && return_span.1 <= callee_def_span.1
                                {
                                    // Extract the text of the returned value, if present
                                    let returned_value_text = return_value_node_opt
                                        .map(|n| get_node_text(&n, source).to_string());

                                    // Add return edges from this callee back to all its callers
                                    for (caller_id, call_sequence) in callers_info {
                                        new_return_edges.push(Edge {
                                            source_node_id: callee_node.id, // Return starts from callee
                                            target_node_id: *caller_id,     // Return goes to caller
                                            edge_type: EdgeType::Return,
                                            call_site_span: callee_node.span, // Span of the function definition itself
                                            return_site_span: Some(return_span), // Span of the return statement
                                            sequence_number: *call_sequence, // Use the sequence number from the original call
                                            returned_value: returned_value_text.clone(), // Add the returned value text
                                        });
                                    }
                                    // Optimization: If a return is found for this callee,
                                    // we might assume it applies to all callers and break
                                    // the inner loop if needed, but let's add edges for all callers for now.
                                }
                            }
                            matches.advance(); // Advance after processing captures for the current match
                        }
                    }
                } //_ => {} // Ignore other node types like ContractDefinition
            }
        }

        // 4. Add the collected return edges to the graph
        self.edges.extend(new_return_edges);

        Ok(())
    }
}

// --- Generator ---

/// Generates a `CallGraph` from a `SolidityAST`.
pub struct CallGraphGenerator<'a> {
    source: &'a str,
    tree: &'a Tree,
    solidity_lang: tree_sitter::Language,
    /// Tracks types of state variables for resolving member calls.
    /// Key: (ContractName, VariableName) -> Value: TypeName
    state_var_types: HashMap<(String, String), String>,
}

impl<'a> CallGraphGenerator<'a> {
    /// Creates a new generator for the given AST.
    pub fn new(ast: &'a SolidityAST) -> Result<Self> {
        let solidity = Solidity; // Assuming Language trait impl
        Ok(Self {
            source: &ast.source,
            tree: &ast.tree,
            solidity_lang: solidity.get_tree_sitter_language(),
            state_var_types: HashMap::new(),
        })
    }

    /// Performs the call graph generation.
    pub fn generate(mut self) -> Result<CallGraph> {
        // Note: Takes ownership via `mut self`
        let mut graph = CallGraph::new();
        let mut definition_cursor = QueryCursor::new();

        // --- Pass 1: Find Definitions (Functions, Constructors, Modifiers, State Vars) ---
        // Query to find definitions and their scopes.
        // Captures:
        // @contract_name: Name of the containing contract (optional)
        // @function_name, @modifier_name: Name of the definition
        // @function_def, @modifier_def, @constructor_def: The definition node itself
        // @var_name, @var_type: State variable name and type
        // @contract_identifier_node: The identifier node for any contract declaration
        // @contract_name_for_map: The name associated with @contract_identifier_node (for all_contracts map)
        let definition_query_str = r#"
            ; Contract identifier (captures name node for ALL contracts)
            (contract_declaration
              name: (identifier) @contract_identifier_node @contract_name_for_map
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
        "#;
        let definition_query = Query::new(&self.solidity_lang, definition_query_str)
            .context("Failed to create definition query")?;

        // Store info needed for Pass 2: (Node ID, Definition Node, Contract Name Option)
        let mut definition_nodes_info: Vec<(usize, TsNode<'a>, Option<String>)> = Vec::new();
        // Track contracts to handle default constructors
        let mut all_contracts: HashMap<String, TsNode<'a>> = HashMap::new(); // Map name to identifier node
        let mut contracts_with_explicit_constructors: HashSet<String> = HashSet::new();

        let root_node = self.tree.root_node();
        let source_bytes = self.source.as_bytes();

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
            let mut var_name_opt: Option<String> = None; // State variable name
            let mut var_type_opt: Option<String> = None; // State variable type
            let mut is_state_var = false; // Flag if the match is for a state variable

            // --- Process captures for this specific match ---
            for capture in match_.captures {
                let capture_name = &definition_query.capture_names()[capture.index as usize];
                let captured_ts_node = capture.node;
                let text = get_node_text(&captured_ts_node, self.source);

                match capture_name.as_ref() {
                    // Captures specifically for populating the all_contracts map
                    "contract_identifier_node" => {
                        contract_identifier_node_opt = Some(captured_ts_node)
                    }
                    "contract_name_for_map" => contract_name_for_map_opt = Some(text.to_string()),

                    // Captures for identifying definitions (functions, constructors, modifiers, state vars)
                    "contract_name" => contract_name_opt = Some(text.to_string()), // Still needed for associating defs
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
                            contracts_with_explicit_constructors.insert(c_name.clone());
                        }
                    }
                    "var_name" => var_name_opt = Some(text.to_string()),
                    "var_type" => var_type_opt = Some(text.to_string()),
                    "state_var_def" => is_state_var = true, // Mark that this match is for a state variable
                    _ => {} // Ignore other captures like @call_expr etc. if they exist in this query
                }
            }

            // --- Populate all_contracts map using the dedicated captures ---
            // This block uses the captures specifically designed to find *all* contract names
            // and their identifier nodes, regardless of what else is in the contract.
            if let (Some(name_for_map), Some(identifier_node)) =
                (contract_name_for_map_opt, contract_identifier_node_opt)
            {
                if !all_contracts.contains_key(&name_for_map) {
                    all_contracts.insert(name_for_map.clone(), identifier_node);
                }
            }

            // --- Handle specific definition types (Functions, Modifiers, Constructors, State Vars) ---

            // Handle state variable definitions
            if is_state_var {
                if let (Some(contract_name), Some(var_name), Some(var_type)) =
                    (contract_name_opt.clone(), var_name_opt, var_type_opt)
                {
                    // Store the type information for later use in call resolution
                    self.state_var_types
                        .insert((contract_name, var_name), var_type);
                } else {
                    eprintln!(
                        "Warning: Incomplete capture for state variable definition: {:?}",
                        match_
                            .captures
                            .iter()
                            .map(|c| (
                                &*definition_query.capture_names()[c.index as usize],
                                get_node_text(&c.node, self.source)
                            ))
                            .collect::<Vec<_>>()
                    );
                }
            // Handle function/modifier/constructor definitions
            } else if let (Some(name), Some(type_), Some(def_node)) =
                (node_name_opt, node_type_opt, definition_ts_node_opt)
            {
                let visibility = visibility_opt.unwrap_or_else(|| match type_ {
                    NodeType::Constructor => Visibility::Public,
                    NodeType::Function => {
                        if contract_name_opt.is_some() {
                            Visibility::Internal
                        } else {
                            Visibility::Public
                        }
                    }
                    NodeType::Modifier => Visibility::Internal,
                });

                let span = (def_node.start_byte(), def_node.end_byte());
                let node_id =
                    graph.add_node(name, type_, contract_name_opt.clone(), visibility, span);
                definition_nodes_info.push((node_id, def_node, contract_name_opt));
            } else {
                eprintln!(
                    "Warning: Incomplete capture for definition match: {:?}",
                    match_
                        .captures
                        .iter()
                        .map(|c| (
                            &*definition_query.capture_names()[c.index as usize],
                            get_node_text(&c.node, self.source)
                        ))
                        .collect::<Vec<_>>()
                );
            }
            matches.advance();
        }

        // --- Add Default Constructor Nodes ---
        // Iterate through all identified contracts
        for (contract_name, identifier_node) in &all_contracts {
            // Check if this contract already has an explicit constructor recorded
            if !contracts_with_explicit_constructors.contains(contract_name) {
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
                let lookup_key = (Some(contract_name.clone()), constructor_name.clone());
            }
        }

        // --- Pass 2: Find Calls within each Definition ---
        let call_query_str = r#"
            ; Simple identifier call: foo()
            ; Matches foo(...) - might also match type casts like Type(...)
            (call_expression
              function: (expression (identifier) @call_name)
            ) @call_expr

            ; Member access call: obj.method()
            ; Matches obj.method(...) based on AST analysis
            (call_expression
              function: (expression (member_expression
                object: (identifier) @member_object      
                property: (identifier) @member_property
              ))
            ) @call_expr

            ; TODO: Add specific patterns for `new Contract()` if needed
        "#;
        let call_query = Query::new(&self.solidity_lang, call_query_str)
            .context("Failed to create call query")?;

        let mut call_sequence_counter: usize = 0; // Initialize GLOBAL sequence counter

        for (caller_node_id, definition_ts_node, caller_contract_name_opt) in &definition_nodes_info
        // Borrow here
        {
            let mut call_cursor = QueryCursor::new();
            // Sequence counter is now initialized outside the loop

            let mut call_matches =
                call_cursor.matches(&call_query, *definition_ts_node, |node: TsNode| {
                    iter::once(&source_bytes[node.byte_range()])
                });

            call_matches.advance();
            while let Some(call_match) = call_matches.get() {
                let mut simple_call_name_opt: Option<String> = None;
                let mut member_object_opt: Option<String> = None;
                let mut member_property_opt: Option<String> = None;
                let mut call_expr_node_opt: Option<TsNode> = None;

                for capture in call_match.captures {
                    let capture_name = &call_query.capture_names()[capture.index as usize];
                    let captured_ts_node = capture.node;
                    let text = get_node_text(&captured_ts_node, self.source);

                    match capture_name.as_ref() {
                        "call_name" => simple_call_name_opt = Some(text.to_string()),
                        "member_object" => member_object_opt = Some(text.to_string()),
                        "member_property" => member_property_opt = Some(text.to_string()),
                        "call_expr" => call_expr_node_opt = Some(captured_ts_node),
                        _ => {}
                    }
                }
                if let Some(call_expr_node) = call_expr_node_opt {
                    let call_span = (call_expr_node.start_byte(), call_expr_node.end_byte());
                    let mut target_node_id_opt: Option<usize> = None;

                    // --- Resolve Call Target ---
                    if let (Some(object_name), Some(property_name)) =
                        (member_object_opt, member_property_opt)
                    {
                        // --- Member Access Call ---
                        if let Some(contract_name) = caller_contract_name_opt {
                            // Look up the type of the object variable in the current contract's state variables
                            if let Some(object_type_name) = self
                                .state_var_types
                                .get(&(contract_name.clone(), object_name.clone()))
                            {
                                // Found the type, now look up the method in that contract type's scope
                                let target_key =
                                    (Some(object_type_name.clone()), property_name.clone());
                                target_node_id_opt = graph.node_lookup.get(&target_key).copied();

                                if target_node_id_opt.is_none() {
                                    eprintln!(
                                        "Warning: Member call target '{}.{}' not found in type '{}' (resolved from variable '{}'). Span: {:?}",
                                        object_type_name, property_name, object_type_name, object_name, call_span
                                    );
                                }
                            } else {
                                // TODO: Could also check local variables/parameters if symbol table is extended
                                eprintln!(
                                    "Warning: Type lookup failed for member call object '{}' in contract '{}'. State var key used: ({:?}, {:?}). Span: {:?}",
                                    object_name, contract_name, contract_name, object_name, call_span
                                );
                            }
                        } else {
                            eprintln!(
                                "Warning: Member call '{}'.{} occurred outside a contract context? Span: {:?}",
                                object_name, property_name, call_span
                            );
                        }
                    } else if let Some(simple_call_name) = simple_call_name_opt {
                        // Simple Call (e.g., foo())
                        // 1. Look within the current contract scope
                        if let Some(contract_name) = caller_contract_name_opt {
                            let key = (Some(contract_name.clone()), simple_call_name.clone());
                            if let Some(id) = graph.node_lookup.get(&key) {
                                target_node_id_opt = Some(*id);
                            }
                        }
                        // 2. If not found in contract, look for a free function (no contract scope)
                        if target_node_id_opt.is_none() {
                            let free_function_key = (None, simple_call_name.clone());
                            if let Some(id) = graph.node_lookup.get(&free_function_key) {
                                target_node_id_opt = Some(*id);
                            }
                        }

                        // 3. *** Check if it's a constructor call ***
                        // If the simple_call_name matches a known contract name,
                        // it's likely a constructor call (either explicit or default).
                        if target_node_id_opt.is_none() {
                            // First, check if the call name matches a known contract from Pass 1
                            if all_contracts.contains_key(&simple_call_name) {
                                // If it's a known contract, try to look up its constructor node
                                // Key: (Some(ContractName), ContractName)
                                let constructor_key =
                                    (Some(simple_call_name.clone()), simple_call_name.clone());
                                if let Some(id) = graph.node_lookup.get(&constructor_key) {
                                    target_node_id_opt = Some(*id);
                                }
                            }
                        }
                    }
                    // --- Add Edge if Resolved ---
                    if let Some(target_node_id) = target_node_id_opt {
                        call_sequence_counter += 1; // Increment sequence for this call
                        graph.add_edge(
                            *caller_node_id,
                            target_node_id,
                            EdgeType::Call,        // Specify edge type
                            call_span,             // This is the call_site_span
                            None,                  // No return span for call edges
                            call_sequence_counter, // Pass sequence number
                            None,                  // No returned value for call edges
                        );
                   }
                }
                call_matches.advance();
            }
        }

        Ok(graph)
    }
}

// --- Tests ---
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
        let generator = CallGraphGenerator::new(&ast)?;
        let graph = generator.generate()?;

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
        let generator = CallGraphGenerator::new(&ast)?;
        let graph = generator.generate()?;

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
        let generator = CallGraphGenerator::new(&ast)?;
        let graph = generator.generate()?;

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
        let generator = CallGraphGenerator::new(&ast)?;
        let graph = generator.generate()?;

        // Nodes: a, b, + default constructor for NoCalls
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
        let generator = CallGraphGenerator::new(&ast)?;
        let graph = generator.generate()?;

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
        let generator = CallGraphGenerator::new(&ast)?;
        let graph = generator.generate()?;

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
        let generator = CallGraphGenerator::new(&ast)?;
        let graph = generator.generate()?;

        // Nodes: callNonExistent, + default constructor for Unresolved
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
        let generator = CallGraphGenerator::new(&ast)?;
        let graph = generator.generate()?;

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
        let generator = CallGraphGenerator::new(&ast)?;
        let mut graph = generator.generate()?; // Generate initial graph (calls only)

        // Explicitly add return edges
        graph.add_explicit_return_edges(source, &ast.tree, &Solidity.get_tree_sitter_language())?;

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
}
