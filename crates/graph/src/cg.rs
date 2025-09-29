use crate::interface_resolver::BindingRegistry; // Added
use crate::manifest::Manifest; // Added
use crate::parser::get_node_text;
use anyhow::{anyhow, Context, Result}; // Add anyhow!
use std::collections::{HashMap, HashSet}; // Import HashSet
use std::iter;
use streaming_iterator::StreamingIterator;
use tracing::debug;
use tree_sitter::{Node as TsNode, Query, QueryCursor, Tree};

// Constants for synthetic node names
pub(crate) const EVM_NODE_NAME: &str = "EVM";
pub(crate) const EVENT_LISTENER_NODE_NAME: &str = "EventListener";
pub(crate) const _REQUIRE_NODE_NAME: &str = "Require"; // Added for require statements
pub(crate) const IF_CONDITION_NODE_NAME: &str = "IfCondition"; // Added for if statements
pub(crate) const THEN_BLOCK_NODE_NAME: &str = "ThenBlock"; // Added for then blocks
pub(crate) const ELSE_BLOCK_NODE_NAME: &str = "ElseBlock"; // Added for else blocks
pub(crate) const WHILE_CONDITION_NODE_NAME: &str = "WhileCondition"; // Added for while statements
pub(crate) const WHILE_BLOCK_NODE_NAME: &str = "WhileBlock"; // Added for while blocks
pub(crate) const FOR_CONDITION_NODE_NAME: &str = "ForCondition"; // Added for for statements
pub(crate) const FOR_BLOCK_NODE_NAME: &str = "ForBlock"; // Added for for blocks

#[derive(
    Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, serde::Serialize, serde::Deserialize,
)]
pub enum EdgeType {
    Call,
    Return,
    StorageRead,          // Represents reading from a storage variable
    StorageWrite,         // Represents writing to a storage variable
    Require,              // Represents a require check
    IfConditionBranch,    // Represents the edge to an if-condition
    ThenBranch,           // Represents the true branch of an if statement
    ElseBranch,           // Represents the false branch of an if statement
    WhileConditionBranch, // Represents the edge to a while-condition
    WhileBodyBranch,      // Represents the edge from a while-condition to its body
    ForConditionBranch,   // Represents the edge to a for-condition
    ForBodyBranch,        // Represents the edge from a for-condition to its body
}

#[derive(
    Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, serde::Serialize, serde::Deserialize,
)]
pub enum NodeType {
    Function,
    Interface,
    Constructor,
    Modifier,
    Library,          // Added Library type
    StorageVariable,  // Represents a state variable in storage
    Evm,              // Synthetic node for EVM interaction
    EventListener,    // Synthetic node for event listeners
    RequireCondition, // Synthetic node for require checks
    IfStatement,      // Synthetic node for an if condition
    ThenBlock,        // Synthetic node for a then block
    ElseBlock,        // Synthetic node for an else block
    WhileStatement,   // Synthetic node for a while condition
    WhileBlock,       // Synthetic node for a while block
    ForCondition,     // Synthetic node for a for condition
    ForBlock,         // Synthetic node for a for block
}

#[derive(
    Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd, serde::Serialize, serde::Deserialize,
)]
pub enum Visibility {
    Public,
    Private,
    Internal,
    External,
    Default,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Node {
    pub id: usize,
    pub name: String,
    pub node_type: NodeType,
    pub contract_name: Option<String>,
    pub visibility: Visibility,
    pub span: (usize, usize),
    pub has_explicit_return: bool,            // Added flag
    pub declared_return_type: Option<String>, // Added: Store declared return type of the function
    pub parameters: Vec<ParameterInfo>,       // Added: Store parameters for function-like nodes
    pub revert_message: Option<String>, // Added: Store revert message for RequireCondition nodes
    pub condition_expression: Option<String>, // Added: Store raw condition expression for RequireCondition nodes
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq, PartialOrd, Ord)] // Added derive for ParameterInfo
pub struct ParameterInfo {
    pub name: String,
    pub param_type: String,
    pub description: Option<String>, // Keep description if used by codegen
}

/// Information about a state variable mapping.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MappingInfo {
    pub name: String,
    pub visibility: Visibility,
    pub key_types: Vec<String>, // For mapping(k1 => mapping(k2 => v)), this would be [k1_type_str, k2_type_str]
    pub value_type: String,     // The final value type string
    pub span: (usize, usize),   // Span of the full state variable declaration
    pub full_type_str: String,  // e.g., "mapping(address => mapping(uint => bool))"
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

/// Parameters for creating a new edge
pub struct EdgeParams {
    pub source_node_id: usize,
    pub target_node_id: usize,
    pub edge_type: EdgeType,
    pub call_site_span: (usize, usize),
    pub return_site_span: Option<(usize, usize)>,
    pub sequence_number: usize,
    pub returned_value: Option<String>,
    pub argument_names: Option<Vec<String>>,
    pub event_name: Option<String>,
    pub declared_return_type: Option<String>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
    pub declared_return_type: Option<String>, // Added: Store declared return type for Return edges
}

impl crate::cg_dot::ToDotLabel for Edge {
    fn to_dot_label(&self) -> String {
        // Default label based on type, can be overridden in cg_dot.rs formatter
        match self.edge_type {
            EdgeType::Call => self.sequence_number.to_string(),
            EdgeType::Return => "ret".to_string(),
            EdgeType::StorageRead => "read".to_string(),
            EdgeType::StorageWrite => "write".to_string(),
            EdgeType::Require => "require".to_string(), // Add label for Require
            EdgeType::IfConditionBranch => "if_cond".to_string(),
            EdgeType::ThenBranch => "then".to_string(),
            EdgeType::ElseBranch => "else".to_string(),
            EdgeType::WhileConditionBranch => "while_cond".to_string(),
            EdgeType::WhileBodyBranch => "while_body".to_string(),
            EdgeType::ForConditionBranch => "for_cond".to_string(),
            EdgeType::ForBodyBranch => "for_body".to_string(),
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

    pub fn add_node(
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
            declared_return_type: None, // Initialize with None
            parameters: Vec::new(),     // Initialize parameters as empty
            revert_message: None,       // Initialize new field
            condition_expression: None, // Initialize new field
        };
        self.nodes.push(node);

        self.node_lookup.insert((contract_name, name), id);
        id
    }

    pub fn add_edge(&mut self, params: EdgeParams) {
        debug!(
            "Attempting to add edge: {} -> {} (Type: {:?}, Seq: {}, RetVal: {:?}, Args: {:?}, DeclRetType: {:?})",
            params.source_node_id, params.target_node_id, params.edge_type, params.sequence_number,
            params.returned_value, params.argument_names, params.declared_return_type
        );
        let edge = Edge {
            source_node_id: params.source_node_id,
            target_node_id: params.target_node_id,
            edge_type: params.edge_type,
            call_site_span: params.call_site_span,
            return_site_span: params.return_site_span,
            sequence_number: params.sequence_number,
            returned_value: params.returned_value,
            argument_names: params.argument_names,
            event_name: params.event_name,
            declared_return_type: params.declared_return_type,
        };
        self.edges.push(edge);
    }

    /// Finds a node ID based on its name and potential contract scope.
    /// Resolution logic:
    /// 1. Look for the name within the `current_contract` scope.
    /// 2. Look for the name as a free function (no contract scope).
    /// 3. **Fallback:** Search across all known contract scopes.
    ///    TODO: Enhance resolution with proper type analysis for member access, inheritance, imports, etc.
    ///    This current fallback is a simplification and may be ambiguous if multiple
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
        debug!(
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
                debug!(
                    "Edge Index {}: {} -> {} (Target: '{}'), Type: {:?}, Seq: {}",
                    idx,
                    edge.source_node_id,
                    edge.target_node_id,
                    target_node_name,
                    edge.edge_type,
                    edge.sequence_number
                );
                found_node4_edges = true;
            }
        }
        if !found_node4_edges {
            debug!("No edges found originating from Node 4.");
        }

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
            let callee_node_exists = self.nodes.get(*callee_node_id).is_some();
            if !callee_node_exists {
                debug!(
                    "Warning: Node ID {} found in definition_nodes_info but not in graph.nodes",
                    callee_node_id
                );
                continue; // Skip if node not found in graph
            }

            // Use immutable borrow here for logging before potential mutable borrow
            if let Some(callee_node_for_log) = self.nodes.get(*callee_node_id) {
                debug!(
                    "Processing callee: Node ID {}, Name: {}.{}, Type: {:?}",
                    callee_node_for_log.id,
                    callee_node_for_log
                        .contract_name
                        .as_deref()
                        .unwrap_or("Global"),
                    callee_node_for_log.name,
                    callee_node_for_log.node_type
                );
            }

            if *callee_node_id == 21 {
                debug!(
                    "S-expression for Node ID {}:\n{}",
                    callee_node_id,
                    definition_ts_node.to_sexp()
                );
            }

            // Check node type and update Node struct (has_explicit_return and declared_return_type)
            let node_type_for_check = self.nodes[*callee_node_id].node_type.clone();
            match node_type_for_check {
                NodeType::Function
                | NodeType::Modifier
                | NodeType::Constructor
                | NodeType::Library => {
                    // Get declared return type ONCE for this callee, ONLY if it's a callable type
                    // This is used to update the Node and also for any Return Edges created later.
                    let actual_declared_ret_type =
                        get_function_return_type(*callee_node_id, ctx, input);

                    // Update the Node struct in the graph
                    if let Some(node_mut) = self.nodes.get_mut(*callee_node_id) {
                        // Set has_explicit_return
                        if !node_mut.has_explicit_return {
                            let mut return_check_matches = return_cursor.matches(
                                &return_query,
                                definition_ts_node,
                                |node: TsNode| iter::once(&source_bytes[node.byte_range()]),
                            );
                            return_check_matches.advance(); // Advance once
                            if return_check_matches.get().is_some() {
                                node_mut.has_explicit_return = true;
                                _nodes_with_explicit_return_set += 1;
                                debug!(
                                    "Set has_explicit_return=true for Node ID {}",
                                    *callee_node_id
                                );
                            }
                        }
                        // Set declared_return_type (clone from the one fetched above)
                        node_mut.declared_return_type = actual_declared_ret_type.clone();
                        if actual_declared_ret_type.is_some() {
                            debug!(
                                "Set declared_return_type='{:?}' for Node ID {}",
                                actual_declared_ret_type, *callee_node_id
                            );
                        }
                    }

                    // Find callers and their call sequences for this callee
                    if let Some(callers_info) = callee_to_callers_and_seq.get(callee_node_id) {
                        let mut matches = return_cursor.matches(
                            &return_query,
                            definition_ts_node,
                            |node: TsNode| iter::once(&source_bytes[node.byte_range()]),
                        );
                        matches.advance(); // Need to advance once initially

                        while let Some(match_) = matches.get() {
                            total_returns_found_by_query += 1;

                            let mut return_node_opt: Option<TsNode> = None;
                            let mut return_value_node_opt: Option<TsNode> = None;

                            for capture in match_.captures {
                                let capture_name =
                                    &return_query.capture_names()[capture.index as usize];
                                match &**capture_name {
                                    "return" => return_node_opt = Some(capture.node),
                                    "return_value" => return_value_node_opt = Some(capture.node),
                                    _ => {}
                                }
                            }

                            if let Some(return_node) = return_node_opt {
                                total_returns_processed += 1;
                                let return_span =
                                    (return_node.start_byte(), return_node.end_byte());
                                let return_kind = return_node.kind();

                                debug!(
                                    "  Found return statement within definition: Kind='{}', Span={:?}. => ACCEPTED (within definition node)",
                                    return_kind, return_span
                                );

                                let returned_value_text = return_value_node_opt
                                    .map(|n| get_node_text(&n, &input.source).to_string());

                                for (caller_id, call_sequence) in callers_info {
                                    let callee_node_span = self.nodes[*callee_node_id].span;
                                    // Use actual_declared_ret_type obtained earlier for the edge
                                    new_return_edges.push(Edge {
                                        source_node_id: *callee_node_id,
                                        target_node_id: *caller_id,
                                        edge_type: EdgeType::Return,
                                        call_site_span: callee_node_span, // Span of the function definition itself
                                        return_site_span: Some(return_span), // Span of the return statement
                                        sequence_number: *call_sequence, // Use the sequence number from the original call
                                        returned_value: returned_value_text.clone(), // Add the returned value text
                                        argument_names: None, // Return edges don't have call arguments
                                        event_name: None,
                                        declared_return_type: actual_declared_ret_type.clone(), // Use the fetched type
                                    });
                                }
                            } else {
                                // This case should not happen if the query is correct and finds a match
                                debug!("  Warning: Query matched but failed to extract @return capture.");
                            }
                            matches.advance(); // Advance after processing captures for the current match
                        }
                    }
                }
                _ => {} // Ignore other node types like Interface, ContractDefinition etc.
            }
        }

        // 4. Add the collected return edges to the graph
        debug!(
            "Total returns found by query: {}", // DEBUG Summary
            total_returns_found_by_query
        );
        debug!(
            "Total returns processed (found within definition): {}", // DEBUG Summary
            total_returns_processed
        );
        debug!(
            "Total return edges generated: {}", // DEBUG Summary
            new_return_edges.len()
        );
        debug!("Edge count BEFORE extend: {}", self.edges.len());
        debug!(
            "[Address Debug add_explicit_return_edges] Graph: {:p}",
            self
        ); // Added address log
        self.edges.extend(new_return_edges);
        debug!("Edge count AFTER extend: {}", self.edges.len());

        Ok(())
    }

    /// Retrieves the list of ancestor contracts for a given contract name,
    /// including the contract itself, ordered from most specific to most general.
    /// Uses the inheritance information stored in the context.
    pub(crate) fn get_ancestor_contracts(
        &self,
        contract_name: &str,
        ctx: &CallGraphGeneratorContext,
    ) -> Vec<String> {
        let mut ancestors = Vec::new();
        let mut queue = std::collections::VecDeque::new();
        let mut visited = HashSet::new(); // Prevent cycles and redundant lookups

        queue.push_back(contract_name.to_string());
        visited.insert(contract_name.to_string());

        while let Some(current_contract) = queue.pop_front() {
            // Add the current contract to the front of the ancestors list
            // to maintain the order from specific to general after reversal.
            ancestors.push(current_contract.clone());

            // Look up direct parents (contracts and interfaces)
            if let Some(parents) = ctx.contract_inherits.get(&current_contract) {
                for parent_name in parents {
                    if visited.insert(parent_name.clone()) {
                        queue.push_back(parent_name.clone());
                    }
                }
            }
            // Also consider inherited interfaces if necessary, though storage vars are usually in contracts.
            // If interfaces could define constants used in storage access, this might be needed.
            // if let Some(parents) = ctx.interface_inherits.get(&current_contract) { ... }
        }

        // The queue processing naturally explores breadth-first, but the order
        // we add to `ancestors` and the final reversal ensures linearization
        // (though Solidity's C3 linearization is more complex, this provides
        // a reasonable lookup order: self -> parents -> grandparents...).
        ancestors.reverse(); // Reverse to get the order: self, parent, grandparent...
        ancestors
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
    pub contract_implements: HashMap<String, Vec<String>>, // Contract -> List of Interfaces it implements
    pub interface_inherits: HashMap<String, Vec<String>>, // Interface -> List of Interfaces it inherits from
    pub contract_inherits: HashMap<String, Vec<String>>, // Contract -> List of Contracts/Interfaces it inherits from
    pub storage_var_nodes: HashMap<(Option<String>, String), usize>, // (ContractScope, VarName) -> Node ID
    /// Stores detailed information about declared mappings.
    /// Key: (contract_name, mapping_variable_name)
    pub contract_mappings: HashMap<(String, String), MappingInfo>,
    // Added for interface binding resolution
    pub manifest: Option<Manifest>,
    pub binding_registry: Option<BindingRegistry>,
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

pub(crate) fn extract_function_parameters(
    fn_like_ts_node: TsNode, // This is function_definition or constructor_definition
    source: &str,
) -> Vec<ParameterInfo> {
    let mut parameters = Vec::new();
    debug!(
        "[cg::extract_function_parameters DEBUG] Analyzing TsNode kind: '{}', text snippet: '{}'",
        fn_like_ts_node.kind(),
        get_node_text(&fn_like_ts_node, source)
            .chars()
            .take(70)
            .collect::<String>()
    );

    let mut child_cursor = fn_like_ts_node.walk();
    // Iterate through direct children of fn_like_ts_node (constructor_definition or function_definition)
    // The 'parameter' nodes are direct children, not nested under a 'parameter_list' node.
    for child_node in fn_like_ts_node.children(&mut child_cursor) {
        debug!("[cg::extract_function_parameters DEBUG] Child of fn_like_ts_node: Kind='{}', Text='{}'", child_node.kind(), get_node_text(&child_node, source).chars().take(30).collect::<String>());
        if child_node.kind() == "parameter" {
            // These are the actual parameter definitions
            let type_node = child_node.child_by_field_name("type");
            let name_node = child_node.child_by_field_name("name");

            // Both type and name are expected for function/constructor parameters
            if let (Some(tn), Some(nn)) = (type_node, name_node) {
                let param_name = crate::parser::get_node_text(&nn, source).to_string();
                let param_type = crate::parser::get_node_text(&tn, source).to_string();
                debug!("[cg::extract_function_parameters DEBUG]   Extracted param: name='{}', type='{}'", param_name, param_type);
                parameters.push(ParameterInfo {
                    name: param_name,
                    param_type,
                    description: None,
                });
            } else {
                debug!("[cg::extract_function_parameters DEBUG]   Found 'parameter' node (Kind: '{}', Text: '{}') but missing type or name field.", child_node.kind(), get_node_text(&child_node, source).chars().take(30).collect::<String>());
            }
        }
    }

    // Log if no parameters were found, which might indicate an issue if parameters were expected.
    if parameters.is_empty() {
        // Check if the function/constructor signature actually has parameters in the source
        // This is a heuristic based on text, as we've already failed to parse them structurally.
        let signature_text = get_node_text(&fn_like_ts_node, source);
        if signature_text.contains('(')
            && signature_text.contains(')')
            && !signature_text.contains("()")
        {
            // A more robust check would be to see if there are any 'parameter' kind nodes between '(' and ')'
            let mut has_parameter_nodes_in_signature = false;
            let mut temp_cursor = fn_like_ts_node.walk();
            for child in fn_like_ts_node.children(&mut temp_cursor) {
                if child.kind() == "parameter" {
                    has_parameter_nodes_in_signature = true;
                    break;
                }
            }

            if has_parameter_nodes_in_signature {
                debug!("[cg::extract_function_parameters DEBUG] No parameters extracted, but 'parameter' nodes were found as direct children. This indicates the loop logic might be incorrect or tree-sitter grammar differs from expectation.");
            } else {
                debug!("[cg::extract_function_parameters DEBUG] No parameters extracted, and no 'parameter' nodes found as direct children. This might be a function/constructor with no parameters, or an issue with identifying 'parameter' nodes.");
            }
        } else {
            debug!("[cg::extract_function_parameters DEBUG] No parameters extracted. This appears to be a function/constructor with no parameters based on signature text: '{}'", signature_text.chars().take(50).collect::<String>());
        }
    }

    debug!(
        "[cg::extract_function_parameters DEBUG] For node kind '{}', extracted {} parameters: {:?}",
        fn_like_ts_node.kind(),
        parameters.len(),
        parameters
    );
    parameters
}

pub(crate) fn extract_arguments<'a>(
    call_expr_node: TsNode<'a>,
    input: &CallGraphGeneratorInput,
) -> Vec<String> {
    let mut argument_texts: Vec<String> = Vec::new();
    debug!(
        "[cg::extract_arguments DEBUG] Extracting argument texts for Call Expr Node: Kind='{}', Span=({:?})",
        call_expr_node.kind(),
        (call_expr_node.start_byte(), call_expr_node.end_byte())
    );

    if let Some(arguments_field_node) = call_expr_node.child_by_field_name("arguments") {
        debug!("[cg::extract_arguments DEBUG]   Found 'arguments' field. Iterating its children.");
        let mut arg_cursor = arguments_field_node.walk();
        for arg_node in arguments_field_node.children(&mut arg_cursor) {
            // Iterate children of 'arguments'
            // These children should be 'call_argument' nodes, which are expressions
            if arg_node.kind() == "call_argument" {
                // Ensure it's a call_argument
                let arg_text = get_node_text(&arg_node, &input.source).to_string();
                debug!("[cg::extract_arguments DEBUG]     Extracted argument text (from 'arguments' field, child is call_argument): '{}'", arg_text);
                argument_texts.push(arg_text);
            }
        }
    } else {
        // If 'arguments' field is not found, iterate direct children of call_expr_node
        // and look for 'call_argument' nodes. This handles cases like `require(arg)`.
        debug!("[cg::extract_arguments DEBUG]   No 'arguments' field found. Iterating direct children of call_expression for 'call_argument' nodes.");
        let mut direct_child_cursor = call_expr_node.walk();
        for child_node in call_expr_node.children(&mut direct_child_cursor) {
            if child_node.kind() == "call_argument" {
                // child_node is the 'call_argument', which is an _expression node.
                let arg_text = get_node_text(&child_node, &input.source).to_string();
                debug!("[cg::extract_arguments DEBUG]     Extracted argument text (direct child call_argument): '{}'", arg_text);
                argument_texts.push(arg_text);
            }
        }
        if argument_texts.is_empty() {
            debug!("[cg::extract_arguments DEBUG]   No 'call_argument' nodes found as direct children either (after checking 'arguments' field).");
        }
    }
    argument_texts
}

pub(crate) fn extract_argument_nodes<'a>(call_expr_node: TsNode<'a>) -> Vec<TsNode<'a>> {
    let mut argument_nodes_ts: Vec<TsNode<'a>> = Vec::new();
    debug!(
        "[cg::extract_argument_nodes DEBUG] Extracting argument nodes for Call Expr Node: Kind='{}', Span=({:?})",
        call_expr_node.kind(),
        (call_expr_node.start_byte(), call_expr_node.end_byte())
    );

    if let Some(arguments_field_node) = call_expr_node.child_by_field_name("arguments") {
        debug!(
            "[cg::extract_argument_nodes DEBUG]   Found 'arguments' field. Iterating its children."
        );
        let mut arg_cursor = arguments_field_node.walk();
        for arg_node in arguments_field_node.children(&mut arg_cursor) {
            // Iterate children of 'arguments'
            if arg_node.kind() == "call_argument" {
                // Ensure it's a call_argument
                debug!("[cg::extract_argument_nodes DEBUG]     Extracted argument node (from 'arguments' field, child is call_argument): Kind='{}', Span=({:?})", arg_node.kind(), (arg_node.start_byte(), arg_node.end_byte()));
                argument_nodes_ts.push(arg_node);
            }
        }
    } else {
        debug!("[cg::extract_argument_nodes DEBUG]   No 'arguments' field found. Iterating direct children of call_expression for 'call_argument' nodes.");
        let mut direct_child_cursor = call_expr_node.walk();
        for child_node in call_expr_node.children(&mut direct_child_cursor) {
            if child_node.kind() == "call_argument" {
                // child_node is the 'call_argument', which is an _expression node.
                debug!("[cg::extract_argument_nodes DEBUG]     Extracted argument node (direct child call_argument): Kind='{}', Span=({:?})", child_node.kind(), (child_node.start_byte(), child_node.end_byte()));
                argument_nodes_ts.push(child_node);
            }
        }
        if argument_nodes_ts.is_empty() {
            debug!("[cg::extract_argument_nodes DEBUG]   No 'call_argument' nodes found as direct children either (after checking 'arguments' field).");
        }
    }
    argument_nodes_ts
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
        let definition_ts_node = match input
            .tree
            .root_node()
            .descendant_for_byte_range(definition_node_info.span.0, definition_node_info.span.1)
        {
            Some(node) => node,
            None => {
                debug!(
                    "[Return Type Parse DEBUG] Failed to find TsNode for span {:?} for node ID {}",
                    definition_node_info.span, target_node_id
                );
                return None;
            }
        };

        debug!("[Return Type Parse DEBUG] Analyzing definition node ID {} for return type. S-Expr:\n{}", target_node_id, definition_ts_node.to_sexp()); // Use retrieved node

        // Query to find the actual type name node within the standard return structure.
        // NOTE: This simplified query only handles `returns (type)`.
        // It does NOT handle named return parameters like `returns (uint value)`
        // or returns from modifiers. This is sufficient for the current test case.
        // Added pattern for interface function definitions.
        let return_type_query_str = r#"
            [
              (function_definition
                return_type: (return_type_definition
                  (parameter
                    type: (type_name) @return_type_name_node
                  )
                )
              )
              (interface_declaration (_                       ;; Same for interfaces
                (function_definition
                  return_type: (return_type_definition
                    (parameter
                      type: (type_name) @return_type_name_node
                    )
                  )
                )
              ))
            ]
        "#;
        // Note: Using a static query cache might be more efficient if this runs often.
        let return_type_query = match Query::new(&input.solidity_lang, return_type_query_str) {
            Ok(q) => q,
            Err(e) => {
                debug!(
                    "[Return Type Parse DEBUG] Failed to create query for node ID {}: {}",
                    target_node_id, e
                );
                return None;
            }
        };
        debug!(
            "[Return Type Parse DEBUG] Query created successfully for node ID {}.",
            target_node_id
        );

        let mut cursor = QueryCursor::new();
        let source_bytes = input.source.as_bytes();

        // Run the query only on the specific definition node
        debug!("[Return Type Parse DEBUG] Running matches on definition node...");
        let mut matches = cursor.matches(
            &return_type_query,
            definition_ts_node, // Use retrieved node
            |node: TsNode| iter::once(&source_bytes[node.byte_range()]),
        );
        matches.advance(); // Advance once

        debug!("[Return Type Parse DEBUG] Checking for match result..."); // New log

        if let Some(match_) = matches.get() {
            debug!(
                "[Return Type Parse DEBUG] Match found! Processing {} captures...",
                match_.captures.len()
            ); // New log
               // Find the @return_type_name_node capture
            let mut found_expected_capture = false; // Flag to track if we found the right capture
            for (cap_idx, capture) in match_.captures.iter().enumerate() {
                // Added index for logging
                let capture_name = &return_type_query.capture_names()[capture.index as usize];
                debug!(
                    "[Return Type Parse DEBUG]   Capture {}: Name='{}', Node Kind='{}', Text='{}'",
                    cap_idx,
                    capture_name,
                    capture.node.kind(),
                    get_node_text(&capture.node, &input.source)
                ); // New log

                if *capture_name == "return_type_name_node" {
                    // Use the new capture name
                    debug!("[Return Type Parse DEBUG]     Match on '@return_type_name_node'!"); // New log
                    found_expected_capture = true; // Mark that we found it
                    let type_name_node = capture.node;
                    // Get the text of the type_name node directly
                    let type_name_text = get_node_text(&type_name_node, &input.source).to_string();

                    if !type_name_text.is_empty() {
                        debug!(
                            "[Return Type Parse DEBUG] Found single return type name: '{}'",
                            type_name_text
                        );
                        return Some(type_name_text); // Return the captured type name text
                    } else {
                        debug!("[Return Type Parse DEBUG] Found empty return type name node.");
                        // Let it fall through to return None at the end if text is empty
                    }
                    // TODO: Handle multiple return types if the query is extended later
                } else {
                    debug!("[Return Type Parse DEBUG]     Capture name '{}' did not match expected '@return_type_name_node'.", capture_name);
                    // New log
                }
            }
            // If the loop finishes without returning, check the flag
            if !found_expected_capture {
                debug!("[Return Type Parse DEBUG] Loop finished. Query matched but the '@return_type_name_node' capture was not found among the captures for node ID {}.", target_node_id);
            } else {
                debug!("[Return Type Parse DEBUG] Loop finished. Found '@return_type_name_node' capture but it resulted in empty text or didn't return for node ID {}.", target_node_id);
            }
        } else {
            debug!(
                "[Return Type Parse DEBUG] Query found no return type match for node ID {}.",
                target_node_id
            ); // Refined log
        }
    } else {
        debug!(
            "[Return Type Parse DEBUG] Definition TsNode not found for node ID {}",
            target_node_id
        );
    }

    debug!(
        "[Return Type Parse DEBUG] Function returning None for node ID {}.",
        target_node_id
    ); // New log before returning None
    None // Return type not found or parsing failed
}

// Remove 'a lifetime
pub struct CallGraphGeneratorPipeline {
    steps: Vec<Box<dyn CallGraphGeneratorStep>>, // Remove 'a
    enabled_steps: HashSet<String>,              // Track enabled step names
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
    pub fn add_step(&mut self, step: Box<dyn CallGraphGeneratorStep>) {
        // Remove 'a
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
                debug!(
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
