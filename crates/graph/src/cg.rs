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
// --- Helper Function Implementations ---

/// Helper function to extract argument texts from a call expression node.
pub(crate) fn extract_arguments<'a>(
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
pub(crate) fn resolve_call_target(
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
