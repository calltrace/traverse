use std::{
    collections::{HashMap, HashSet},
    iter,
};

use crate::{
    cg::{
        extract_arguments, resolve_call_target, CallGraph, CallGraphGeneratorContext,
        CallGraphGeneratorInput, CallGraphGeneratorStep, EdgeType, NodeInfo, NodeType, Visibility,
        EVENT_LISTENER_NODE_NAME, EVM_NODE_NAME, REQUIRE_NODE_NAME,
    },
    chains::{analyze_chained_call, ResolvedTarget, TypeError}, // Import items from chains module
    parser::get_node_text,
};
use anyhow::{anyhow, Context, Result};
use streaming_iterator::StreamingIterator;
use tree_sitter::{Node as TsNode, Query, QueryCursor};

// Type alias for the key used to track unique storage edges
// type StorageEdgeKey = (usize, usize, EdgeType, (usize, usize)); // No longer needed with deferred approach

// --- NEW: Struct to hold modification data for deferred edge addition ---
#[derive(Debug, Clone, PartialEq, Eq)] // Added PartialEq, Eq for deduplication
struct GraphModification {
    source_node_id: usize,
    target_node_id: usize,
    edge_type: EdgeType,
    span: (usize, usize),
    modifier: Option<String>,
    // sequence_number: usize, // Sequence number assigned *after* sorting
    return_value: Option<String>,
    arguments: Option<Vec<String>>,
    event_name: Option<String>,
    sort_span_start: usize, // Span start of the *originating* source element (call, emit, assignment, etc.)
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

            ; --- Storage Access ---
            ; Capture assignment expression (write target extracted in Rust)
            (assignment_expression) @assignment_expr

            ; Capture identifiers used in expressions (potential read)
            ; Exclude identifiers that are part of a call's function name
            ; Exclude identifiers that are the left side of an assignment
            (identifier) @read_candidate
            ; Add more negative checks if needed (e.g., type names, event names, member access object/property)
            (#not-match? @read_candidate "^(require|assert|revert|emit|new)$") ; Exclude keywords/builtins used like functions
            ; --- Filtering for function names, assignment LHS, type names, etc., will be done in Rust code ---

            ; --- Require statement ---
            ; Matches require(condition, "message")
            (call_expression
              function: (expression (identifier) @require_identifier)
              (#eq? @require_identifier "require")
              ; Arguments are direct children (call_argument), handled by extract_arguments
            ) @require_call_node ; Capture the whole call expression for require
        "#;
        let call_query = Query::new(&input.solidity_lang, call_query_str)
            .context("Failed to create call query")?;

        // let mut call_sequence_counter: usize = 0; // Removed: Sequence assigned globally after sorting

        // --- Pre-filter executable nodes to avoid borrow checker issues ---
        let executable_nodes_info: Vec<(usize, NodeInfo, Option<String>)> = ctx
            .definition_nodes_info
            .iter()
            .filter(|(id, _, _)| {
                // Only process nodes that can contain calls/emits
                matches!(
                    graph.nodes.get(*id).map(|n| &n.node_type),
                    Some(NodeType::Function)
                        | Some(NodeType::Modifier)
                        | Some(NodeType::Constructor)
                )
            })
            .map(|(id, info, contract_opt)| (*id, info.clone(), contract_opt.clone())) // Clone the needed data
            .collect();
        // --- End pre-filtering ---

        // Iterate through the pre-filtered executable nodes
        for (caller_node_id, caller_node_info, caller_contract_name_opt) in executable_nodes_info {
            // Get the actual TsNode for the caller's definition using the span from NodeInfo
            let definition_ts_node = input
                .tree
                .root_node()
                .descendant_for_byte_range(caller_node_info.span.0, caller_node_info.span.1)
                .ok_or_else(|| {
                    anyhow!(
                        "Failed to find definition TsNode for span {:?} in CallsHandling",
                        caller_node_info.span
                    )
                })?;

            // --- NEW: Collect modifications for this function body ---
            let mut modifications: Vec<GraphModification> = Vec::new();

            // Track processed call/emit spans within this caller function to avoid duplicates *during collection*
            // let mut processed_call_spans: HashSet<(usize, usize)> = HashSet::new(); // Removed: Deduplication handled by sorting/query structure

            // Track processed storage read/write edges within this caller function to avoid duplicates *during collection*
            // let mut processed_storage_edges: HashSet<StorageEdgeKey> = HashSet::new(); // Removed: Deduplication handled by sorting/query structure

            // Initialize sequence counter for THIS specific caller function
            // let mut call_sequence_counter: usize = 0; // Removed: Sequence assigned globally after sorting

            // --- DEBUG: Log the caller node being processed ---
            let caller_node_name = graph
                .nodes
                .get(caller_node_id) // Remove dereference
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

            // --- Collect potential call, new, and emit nodes first ---
            let mut potential_nodes: Vec<(TsNode, (usize, usize), &str)> = Vec::new(); // Store node, span, and type (call/new/emit/require/write/read)

            // Capture indices for all relevant node types
            let call_expr_capture_index = call_query
                .capture_index_for_name("call_expr_node")
                .unwrap_or(u32::MAX);
            let new_expr_capture_index = call_query
                .capture_index_for_name("new_expression_node")
                .unwrap_or(u32::MAX);
            let emit_capture_index = call_query
                .capture_index_for_name("emit_statement_node")
                .unwrap_or(u32::MAX);
            let assignment_capture_index = call_query
                .capture_index_for_name("assignment_expr")
                .unwrap_or(u32::MAX);
            let read_candidate_capture_index = call_query
                .capture_index_for_name("read_candidate")
                .unwrap_or(u32::MAX);
            let require_call_capture_index = call_query
                .capture_index_for_name("require_call_node")
                .unwrap_or(u32::MAX);

            let mut call_matches =
                call_cursor.matches(&call_query, definition_ts_node, |node: TsNode| {
                    // Use retrieved node
                    iter::once(&source_bytes[node.byte_range()])
                });

            // --- First Pass: Collect all potential interaction nodes, prioritizing specific types ---
            let mut collected_nodes_map: HashMap<(usize, usize), (TsNode, &str)> = HashMap::new(); // Use map to handle prioritization during collection

            while let Some(match_) = call_matches.next() {
                for capture in match_.captures {
                    let capture_index = capture.index;
                    let node = capture.node;
                    let span = (node.start_byte(), node.end_byte());
                    let current_type: Option<&str> = if capture_index == require_call_capture_index
                    {
                        Some("require")
                    } else if capture_index == emit_capture_index {
                        Some("emit") // Prioritize emit over generic call if applicable
                    } else if capture_index == new_expr_capture_index {
                        Some("new") // Prioritize new over generic call
                    } else if capture_index == call_expr_capture_index {
                        Some("call")
                    } else if capture_index == assignment_capture_index {
                        Some("write")
                    } else if capture_index == read_candidate_capture_index {
                        Some("read")
                    } else {
                        None // Ignore other captures like @call_name etc.
                    };

                    if let Some(node_type) = current_type {
                        collected_nodes_map
                            .entry(span)
                            .and_modify(|existing| {
                                // Prioritization logic: require > emit > new > call > write > read
                                let existing_priority = match existing.1 {
                                    "require" => 5,
                                    "emit" => 4,
                                    "new" => 3,
                                    "call" => 2,
                                    "write" => 1,
                                    "read" => 0,
                                    _ => -1,
                                };
                                let new_priority = match node_type {
                                    "require" => 5,
                                    "emit" => 4,
                                    "new" => 3,
                                    "call" => 2,
                                    "write" => 1,
                                    "read" => 0,
                                    _ => -1,
                                };
                                // Only update if the new type has higher or equal priority
                                // (equal priority case handles potential multiple captures of same type for same node)
                                if new_priority >= existing_priority {
                                    *existing = (node, node_type);
                                }
                            })
                            .or_insert((node, node_type));
                    }
                }
            }

            // Convert map into Vec<(TsNode, span, &str)> and sort by span start
            potential_nodes = collected_nodes_map
                .into_iter() // Iterate over key-value pairs (span, (node, type))
                .map(|(span, (node, node_type))| (node, span, node_type)) // Map to (node, span, type)
                .collect();
            potential_nodes.sort_by_key(|k| (k.1 .0, k.1 .1)); // Sort by span start (k.1.0), then end (k.1.1)

            // --- Second Pass: Process sorted, deduplicated nodes and collect GraphModifications ---
            for (node, span, node_type) in potential_nodes {
                match node_type {
                    "call" | "new" => {
                        // --- Handle Calls and 'new' Expressions ---
                        // Check if this node is contained within another call/new node we already processed
                        // This ensures we only process the *outermost* call in a chain initially.
                        // analyze_chained_call will handle the inner parts.
                        let is_nested = modifications.iter().any(|m| {
                            // Check if the current node's span is contained within a previously processed call/new span
                            (m.edge_type == EdgeType::Call) // Check if it was a call modification (covers constructor calls too)
                                && m.span.0 <= span.0 && m.span.1 >= span.1 && m.span != span
                            // Check containment
                        });

                        if is_nested {
                            eprintln!("[CallsHandling DEBUG]       Skipping nested call/new node at span {:?}, handled by outer call.", span);
                            continue;
                        }

                        eprintln!("[CallsHandling DEBUG]       Processing Outermost Node via analyze_chained_call: Kind='{}', Span={:?}", node.kind(), span);

                        match analyze_chained_call(
                            node, // Start analysis from the outermost node
                            caller_node_id,
                            &caller_contract_name_opt,
                            ctx,
                            graph,
                            &input.source,
                            &input.solidity_lang,
                            &input,
                            None, // Top-level call within this caller
                        ) {
                            Ok(steps) => {
                                eprintln!(
                                    "[CallsHandling DEBUG]         analyze_chained_call returned {} steps.",
                                    steps.len()
                                );
                                if steps.is_empty() {
                                    eprintln!("[CallsHandling DEBUG]         analyze_chained_call returned 0 steps for node: {}", get_node_text(&node, &input.source));
                                }

                                // Collect modifications for the main call chain steps
                                for step in &steps {
                                    eprintln!("[CallsHandling DEBUG]           Processing Step: Target={:?}, Args={:?}", step.target, step.arguments);
                                    if let Some(target_node_id) =
                                        resolve_target_to_node_id(&step.target, graph, ctx)
                                    {
                                        eprintln!("[CallsHandling DEBUG]             >>> Collecting modification (from chain step): CallerID={}, TargetID={}, StepSpan={:?}", caller_node_id, target_node_id, step.call_expr_span);
                                        modifications.push(GraphModification {
                                            source_node_id: caller_node_id,
                                            target_node_id,
                                            edge_type: EdgeType::Call, // Or ConstructorCall if target is constructor? analyze_chained_call needs refinement?
                                            span: (
                                                step.call_expr_span.0.into(),
                                                step.call_expr_span.1.into(),
                                            ),
                                            modifier: None,
                                            return_value: None, // Not tracked here
                                            arguments: Some(step.arguments.clone()),
                                            event_name: None,
                                            // Use the function span start for sorting chained calls correctly
                                            sort_span_start: step.function_span.0,
                                        });
                                    } else {
                                        eprintln!("[CallsHandling DEBUG]             >>> Target for step {:?} did not resolve to a node ID. Skipping modification.", step.target);
                                    }
                                }

                                // --- Analyze target function bodies for internal 'new' calls ---
                                for step in steps {
                                    // Borrow steps here to use it again later
                                    // Extract function details if the target is a Function or an InterfaceMethod with a Function implementation
                                    let function_details: Option<(String, String, usize)> =
                                        match &step.target {
                                            ResolvedTarget::Function {
                                                contract_name: Some(c_name),
                                                function_name: f_name,
                                                ..
                                            } => {
                                                let key = (Some(c_name.clone()), f_name.clone());
                                                graph
                                                    .node_lookup
                                                    .get(&key)
                                                    .copied()
                                                    .map(|id| (c_name.clone(), f_name.clone(), id))
                                            }
                                            ResolvedTarget::InterfaceMethod {
                                                implementation: Some(impl_target),
                                                ..
                                            } => {
                                                if let ResolvedTarget::Function {
                                                    contract_name: Some(c_name),
                                                    function_name: f_name,
                                                    ..
                                                } = &**impl_target
                                                {
                                                    let key =
                                                        (Some(c_name.clone()), f_name.clone());
                                                    graph.node_lookup.get(&key).copied().map(|id| {
                                                        (c_name.clone(), f_name.clone(), id)
                                                    })
                                                } else {
                                                    None
                                                }
                                            }
                                            _ => None,
                                        };

                                    if let Some((
                                        target_contract_name,
                                        target_function_name,
                                        target_function_node_id,
                                    )) = function_details
                                    {
                                        // Find the definition TsNode for this target function
                                        if let Some((_, target_node_info, _)) = ctx
                                            .definition_nodes_info
                                            .iter()
                                            .find(|(id, _, _)| *id == target_function_node_id)
                                        {
                                            let target_definition_ts_node =
                                                input.tree.root_node().descendant_for_byte_range(
                                                    target_node_info.span.0,
                                                    target_node_info.span.1,
                                                );

                                            if let Some(target_def_node) = target_definition_ts_node
                                            {
                                                eprintln!("[CallsHandling DEBUG]           Analyzing body of target function \'{}.{}\' (NodeID {}) for \'new\' calls.", target_contract_name, target_function_name, target_function_node_id);

                                                // Simplified query to find just the new_expression node.
                                                let new_query_str = r#"
                                                    (new_expression) @new_expr_node
                                                "#;
                                                let new_query = Query::new(
                                                    &input.solidity_lang,
                                                    new_query_str,
                                                )
                                                .context(
                                                    "Failed to create simplified internal 'new' query",
                                                )?;
                                                let mut new_cursor = QueryCursor::new();
                                                let mut new_matches = new_cursor.matches(
                                                    &new_query,
                                                    target_def_node,
                                                    |node: TsNode| {
                                                        iter::once(&source_bytes[node.byte_range()])
                                                    },
                                                );

                                                while let Some(new_match) = new_matches.next() {
                                                    if let Some(capture) =
                                                        new_match.captures.first()
                                                    {
                                                        let new_expr_node = capture.node;
                                                        let new_span = (
                                                            new_expr_node.start_byte(),
                                                            new_expr_node.end_byte(),
                                                        );

                                                        // Manually extract the contract name
                                                        let new_contract_name_opt = new_expr_node
                                                            .child_by_field_name("name")
                                                            .and_then(|type_name_node| {
                                                                let mut name_cursor =
                                                                    type_name_node.walk();
                                                                let mut queue =
                                                                    std::collections::VecDeque::new(
                                                                    );
                                                                queue.push_back(type_name_node);
                                                                while let Some(current) =
                                                                    queue.pop_front()
                                                                {
                                                                    if current.kind()
                                                                        == "identifier"
                                                                    {
                                                                        return Some(
                                                                            get_node_text(
                                                                                &current,
                                                                                &input.source,
                                                                            )
                                                                            .to_string(),
                                                                        );
                                                                    }
                                                                    for child in current
                                                                        .children(&mut name_cursor)
                                                                    {
                                                                        queue.push_back(child);
                                                                    }
                                                                }
                                                                None
                                                            });

                                                        if let Some(new_contract_name) =
                                                            new_contract_name_opt
                                                        {
                                                            eprintln!("[CallsHandling DEBUG]             Found internal new_expression: '{}' (Node ID: {})", new_contract_name, new_expr_node.id());

                                                            // Find the constructor node ID
                                                            let constructor_key = (
                                                                Some(new_contract_name.clone()),
                                                                new_contract_name.clone(),
                                                            );
                                                            if let Some(constructor_node_id) = graph
                                                                .node_lookup
                                                                .get(&constructor_key)
                                                                .copied()
                                                            {
                                                                // Extract arguments
                                                                let mut new_args = Vec::new();
                                                                if let Some(parent_call) =
                                                                    new_expr_node.parent().filter(
                                                                        |p| {
                                                                            p.kind()
                                                                                == "call_expression"
                                                                        },
                                                                    )
                                                                {
                                                                    new_args = extract_arguments(
                                                                        parent_call,
                                                                        &input,
                                                                    );
                                                                } else {
                                                                    let mut cursor =
                                                                        new_expr_node.walk();
                                                                    for child in new_expr_node
                                                                        .children(&mut cursor)
                                                                    {
                                                                        if child.kind()
                                                                            == "arguments"
                                                                        {
                                                                            new_args =
                                                                                extract_arguments(
                                                                                    new_expr_node,
                                                                                    &input,
                                                                                );
                                                                            break;
                                                                        }
                                                                    }
                                                                }

                                                                eprintln!("[CallsHandling DEBUG]             >>> Collecting modification (Internal \'new\'): SourceID={}, TargetID={}, NewSpan={:?}, Args={:?}", target_function_node_id, constructor_node_id, new_span, new_args);
                                                                modifications.push(
                                                                    GraphModification {
                                                                        source_node_id:
                                                                            target_function_node_id, // Source is the function containing 'new'
                                                                        target_node_id:
                                                                            constructor_node_id, // Target is the constructor
                                                                        edge_type: EdgeType::Call, // Treat constructor call as Call
                                                                        span: new_span,
                                                                        modifier: None,
                                                                        return_value: None,
                                                                        arguments: Some(new_args),
                                                                        event_name: None,
                                                                        // Use the sort span start of the call step that triggered this internal analysis
                                                                        // This groups the internal 'new' logically with the call that caused it.
                                                                        sort_span_start: step
                                                                            .call_expr_span
                                                                            .0,
                                                                    },
                                                                );
                                                            } else {
                                                                eprintln!("[CallsHandling DEBUG]             >>> Constructor node not found for internal new {}", new_contract_name);
                                                            }
                                                        }
                                                    }
                                                }
                                            } else {
                                                eprintln!("[CallsHandling DEBUG]           Could not find definition TsNode for target function ID {}", target_function_node_id);
                                            }
                                        } else {
                                            eprintln!("[CallsHandling DEBUG]           Could not find NodeInfo for target function ID {}", target_function_node_id);
                                        }
                                    }
                                }
                            }
                            Err(e) => {
                                eprintln!("[CallsHandling DEBUG]       Error during analyze_chained_call for span {:?}: {:?}", span, e);
                            }
                        }
                    }
                    "write" => {
                        // --- Handle Storage Writes ---
                        let assignment_node = node;
                        let assignment_span = span;
                        if let Some(lhs_node) = assignment_node.child_by_field_name("left") {
                            let mut target_identifier_node: Option<TsNode> = None;
                            let mut queue = std::collections::VecDeque::new();
                            queue.push_back(lhs_node);
                            while let Some(current) = queue.pop_front() {
                                if current.kind() == "identifier" {
                                    target_identifier_node = Some(current);
                                }
                                let mut cursor = current.walk();
                                if current.kind() != "identifier" {
                                    for child in current.children(&mut cursor) {
                                        queue.push_back(child);
                                    }
                                }
                            }

                            if let Some(write_target_node) = target_identifier_node {
                                let var_name = get_node_text(&write_target_node, &input.source);
                                // --- Use inheritance-aware resolution ---
                                if let Some(var_node_id) = resolve_storage_variable(
                                    &caller_contract_name_opt,
                                    &var_name,
                                    graph,
                                    ctx,
                                ) {
                                    // --- End inheritance-aware
                                    // The check for NodeType::StorageVariable is now inside resolve_storage_variable
                                    // if graph
                                    //     .nodes
                                    //     .get(var_node_id)
                                    //     .map_or(false, |n| n.node_type == NodeType::StorageVariable)
                                    // {
                                    eprintln!("[Storage DEBUG Deferred] Collecting WRITE modification: CallerID={}, VarID={}, VarName='{}', AssignmentSpan={:?}", caller_node_id, var_node_id, var_name, assignment_span);
                                    modifications.push(GraphModification {
                                        source_node_id: caller_node_id,
                                        target_node_id: var_node_id,
                                        edge_type: EdgeType::StorageWrite,
                                        span: assignment_span, // Span of the whole assignment
                                        modifier: None,
                                        return_value: None,
                                        arguments: None,
                                        event_name: None,
                                        sort_span_start: assignment_span.0, // Use assignment start for sorting
                                    });
                                    // } else {
                                    //     eprintln!("[Storage DEBUG Deferred] Write target '{}' (NodeID {}) is not a StorageVariable.", var_name, var_node_id);
                                    // }
                                } else {
                                    eprintln!("[Storage DEBUG Deferred] Write target '{}' could not be resolved via inheritance.", var_name);
                                }
                            }
                        }
                    }
                    "read" => {
                        // --- Handle Storage Reads ---
                        let read_candidate_node = node;
                        let read_span = span;
                        let var_name = get_node_text(&read_candidate_node, &input.source);
                        // --- Use inheritance-aware resolution ---
                        if let Some(var_node_id) = resolve_storage_variable(
                            &caller_contract_name_opt,
                            &var_name,
                            graph,
                            ctx,
                        ) {
                        // --- End inheritance-aware resolution ---
                            // The check for NodeType::StorageVariable is now inside resolve_storage_variable
                            // if graph
                            //     .nodes
                            //     .get(var_node_id)
                            //     .map_or(false, |n| n.node_type == NodeType::StorageVariable)
                            // {
                                // --- Filtering Logic ---
                                let mut skip_read_edge = false;
                                if let Some(parent) = read_candidate_node.parent() {
                                let mut skip_read_edge = false;
                                if let Some(parent) = read_candidate_node.parent() {
                                    let parent_kind = parent.kind();
                                    let parent_field_name: Option<&str> = {
                                        let mut cursor = parent.walk();
                                        let mut field_name_result: Option<&str> = None;
                                        if cursor.goto_first_child() {
                                            loop {
                                                if cursor.node().id() == read_candidate_node.id() {
                                                    field_name_result = cursor.field_name();
                                                    break;
                                                }
                                                if !cursor.goto_next_sibling() {
                                                    break;
                                                }
                                            }
                                        }
                                        field_name_result
                                    };

                                    // Check if identifier is the function name in a call
                                    if parent_kind == "expression" {
                                        if let Some(grandparent) = parent.parent() {
                                            if grandparent.kind() == "call_expression"
                                                && grandparent
                                                    .child_by_field_name("function")
                                                    .map_or(false, |func_child| {
                                                        func_child.id() == parent.id()
                                                    })
                                            {
                                                skip_read_edge = true;
                                            }
                                        }
                                        if let Some(expr_grandparent) = parent.parent() {
                                            if expr_grandparent.kind() == "expression" {
                                                if let Some(call_great_grandparent) =
                                                    expr_grandparent.parent()
                                                {
                                                    if call_great_grandparent.kind()
                                                        == "call_expression"
                                                        && call_great_grandparent
                                                            .child_by_field_name("function")
                                                            .map_or(false, |func_child| {
                                                                func_child.id()
                                                                    == expr_grandparent.id()
                                                            })
                                                    {
                                                        skip_read_edge = true;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    // Check if identifier is the left-hand side of an assignment
                                    let mut current_ancestor = read_candidate_node;
                                    while let Some(ancestor) = current_ancestor.parent() {
                                        if ancestor.kind() == "assignment_expression" {
                                            if let Some(lhs_of_ancestor) =
                                                ancestor.child_by_field_name("left")
                                            {
                                                let mut queue = std::collections::VecDeque::new();
                                                queue.push_back(lhs_of_ancestor);
                                                let mut is_descendant = false;
                                                while let Some(lhs_desc) = queue.pop_front() {
                                                    if lhs_desc.id() == read_candidate_node.id() {
                                                        is_descendant = true;
                                                        break;
                                                    }
                                                    let mut lhs_desc_cursor = lhs_desc.walk(); // Create cursor inside loop
                                                    for child in
                                                        lhs_desc.children(&mut lhs_desc_cursor)
                                                    {
                                                        queue.push_back(child);
                                                    }
                                                }
                                                if is_descendant {
                                                    skip_read_edge = true;
                                                    break;
                                                }
                                            }
                                        }
                                        if ancestor.kind() == "function_definition"
                                            || ancestor.kind() == "modifier_definition"
                                        {
                                            break;
                                        }
                                        current_ancestor = ancestor;
                                    }
                                    // Check other non-read contexts
                                    if !skip_read_edge
                                        && (parent_kind == "type_name"
                                            || parent_kind == "user_defined_type")
                                    {
                                        skip_read_edge = true;
                                    }
                                    if !skip_read_edge
                                        && (parent_kind.ends_with("_definition")
                                            || parent_kind.ends_with("_declaration"))
                                        && parent_field_name == Some("name")
                                    {
                                        skip_read_edge = true;
                                    }
                                    if !skip_read_edge
                                        && parent_kind == "emit_statement"
                                        && parent_field_name == Some("name")
                                    {
                                        skip_read_edge = true;
                                    }
                                    if !skip_read_edge
                                        && parent_kind == "parameter"
                                        && parent_field_name == Some("name")
                                    {
                                        skip_read_edge = true;
                                    }
                                    if !skip_read_edge
                                        && parent_kind == "variable_declaration"
                                        && parent_field_name == Some("name")
                                    {
                                        skip_read_edge = true;
                                    }
                                }
                                // --- End Filtering Logic ---

                                if skip_read_edge {
                                    eprintln!("[Storage DEBUG Deferred] Skipping READ modification for '{}' due to filtering. Span={:?}", var_name, read_span);
                                } else {
                                    eprintln!("[Storage DEBUG Deferred] Collecting READ modification: CallerID={}, VarID={}, VarName='{}', Span={:?}", caller_node_id, var_node_id, var_name, read_span);
                                    modifications.push(GraphModification {
                                        source_node_id: caller_node_id,
                                        target_node_id: var_node_id,
                                        edge_type: EdgeType::StorageRead,
                                        span: read_span,
                                        modifier: None,
                                        return_value: None,
                                        arguments: None,
                                        event_name: None,
                                        sort_span_start: read_span.0, // Use read identifier start for sorting
                                    });
                                }
                            } else {
                                eprintln!("[Storage DEBUG Deferred] Read candidate '{}' (NodeID {}) is not a StorageVariable.", var_name, var_node_id);
                            }
                        } else {
                            eprintln!("[Storage DEBUG Deferred] Read candidate '{}' could not be resolved.", var_name);
                        }
                    }
                    "require" => {
                        // --- Handle Require Calls ---
                        let require_node = node;
                        let require_span = span;
                        // --- BEGIN REQUIRE DEBUG ---
                        eprintln!(
                            "[Require DEBUG] Found require call at span {:?}. Node text: '{}'",
                            require_span,
                            get_node_text(&require_node, &input.source)
                        );
                        // --- END REQUIRE DEBUG ---
                        eprintln!(
                            "[CallsHandling DEBUG]       Processing Require Statement at span {:?}",
                            require_span
                        );

                        // Extract arguments
                        let argument_texts = extract_arguments(require_node, &input);

                        // --- Get or Create Require Node ---
                        let require_key = (None, REQUIRE_NODE_NAME.to_string());
                        let require_node_id = if let Some(id) = graph.node_lookup.get(&require_key)
                        {
                            *id
                        } else {
                            eprintln!("[CallsHandling DEBUG]           Creating Require node.");
                            let new_id = graph.add_node(
                                REQUIRE_NODE_NAME.to_string(),
                                NodeType::RequireCondition,
                                None,
                                Visibility::Default,
                                (0, 0),
                            );
                            graph.node_lookup.insert(require_key, new_id); // Insert after add_node
                            new_id
                        };

                        // --- Collect Modification: Caller -> Require ---
                        // --- BEGIN REQUIRE DEBUG ---
                        eprintln!("[Require DEBUG]   Collecting modification: Source={}, Target={}, Type=Require, Span={:?}, Args={:?}", caller_node_id, require_node_id, require_span, argument_texts);
                        // --- END REQUIRE DEBUG ---
                        eprintln!("[CallsHandling DEBUG]         >>> Collecting modification (Require Caller->RequireNode): CallerID={}, TargetID={}, Args={:?}", caller_node_id, require_node_id, argument_texts);
                        modifications.push(GraphModification {
                            source_node_id: caller_node_id,
                            target_node_id: require_node_id,
                            edge_type: EdgeType::Require, // Use specific Require type
                            span: require_span,
                            modifier: None,
                            return_value: None,
                            arguments: Some(argument_texts),
                            event_name: None,
                            sort_span_start: require_span.0, // Use require call start for sorting
                        });
                    }
                    "emit" => {
                        // --- Handle Emit Statements ---
                        let emit_node = node;
                        let emit_span = span;
                        eprintln!(
                            "[CallsHandling DEBUG]       Processing Emit Statement at span {:?}",
                            emit_span
                        );

                        // Extract event name
                        let event_name_opt = emit_node
                            .child_by_field_name("name")
                            .and_then(|name_node| name_node.child(0))
                            .filter(|id_node| id_node.kind() == "identifier")
                            .map(|id_node| get_node_text(&id_node, &input.source).to_string());

                        // Extract arguments
                        let argument_texts = extract_arguments(emit_node, &input);

                        if let Some(event_name) = event_name_opt {
                            eprintln!(
                                "[CallsHandling DEBUG]         Event Name: '{}', Args: {:?}",
                                event_name, argument_texts
                            );

                            // --- Get or Create EVM Node ---
                            let evm_key = (None, EVM_NODE_NAME.to_string());
                            let evm_node_id = if let Some(id) = graph.node_lookup.get(&evm_key) {
                                *id
                            } else {
                                eprintln!("[CallsHandling DEBUG]           Creating EVM node.");
                                let new_id = graph.add_node(
                                    EVM_NODE_NAME.to_string(),
                                    NodeType::Evm,
                                    None,
                                    Visibility::Default,
                                    (0, 0),
                                );
                                graph.node_lookup.insert(evm_key, new_id); // Insert after add_node
                                new_id
                            };

                            // --- Get or Create Event Listener Node ---
                            let listener_key = (None, EVENT_LISTENER_NODE_NAME.to_string());
                            let listener_node_id =
                                if let Some(id) = graph.node_lookup.get(&listener_key) {
                                    *id
                                } else {
                                    eprintln!(
                                    "[CallsHandling DEBUG]           Creating EventListener node."
                                );
                                    let new_id = graph.add_node(
                                        EVENT_LISTENER_NODE_NAME.to_string(),
                                        NodeType::EventListener,
                                        None,
                                        Visibility::Default,
                                        (0, 0),
                                    );
                                    graph.node_lookup.insert(listener_key, new_id); // Insert after add_node
                                    new_id
                                };
                            // --- Collect Modification 1: Caller -> EVM ---
                            eprintln!("[CallsHandling DEBUG]         >>> Collecting modification (Emit Caller->EVM): CallerID={}, TargetID={}, Event='{}'", caller_node_id, evm_node_id, event_name);
                            modifications.push(GraphModification {
                                source_node_id: caller_node_id,
                                target_node_id: evm_node_id,
                                edge_type: EdgeType::Call,
                                span: emit_span,
                                modifier: None,
                                return_value: None,
                                arguments: Some(argument_texts.clone()),
                                event_name: Some(event_name.clone()),
                                sort_span_start: emit_span.0, // Use emit statement start for sorting
                            });

                            // --- Collect Modification 2: EVM -> Event Listener ---
                            eprintln!("[CallsHandling DEBUG]         >>> Collecting modification (Emit EVM->Listener): CallerID={}, TargetID={}, Event='{}'", evm_node_id, listener_node_id, event_name);
                            // Note: We use the *same* sort_span_start as the Caller->EVM edge.
                            // This ensures they are processed consecutively after sorting.
                            // The sequence number assignment will handle their relative order.
                            modifications.push(GraphModification {
                                source_node_id: evm_node_id,
                                target_node_id: listener_node_id,
                                edge_type: EdgeType::Call,
                                span: emit_span, // Use emit_node span
                                modifier: None,
                                return_value: None,
                                arguments: Some(argument_texts),
                                event_name: Some(event_name),
                                sort_span_start: emit_span.0, // Use emit statement start for sorting
                            });
                        } else {
                            eprintln!("[CallsHandling DEBUG]       >>> Emit statement missing event name. Span: {:?}", emit_span);
                        }
                    }
                    _ => {
                        // Should not happen based on query and collection logic
                        eprintln!(
                            "[CallsHandling WARNING] Encountered unexpected node type '{}' during processing.",
                            node_type
                        );
                    }
                }
            }

            // --- Final Step: Sort modifications and add edges with sequence numbers ---
            eprintln!(
                "[CallsHandling DEBUG] Sorting and adding {} modifications for Caller Node ID: {}",
                modifications.len(),
                caller_node_id
            );
            modifications.sort_by_key(|m| m.sort_span_start);

            // --- Deduplicate modifications ---
            // We dedup based on all fields after sorting by the originating span start.
            // This ensures that if the exact same interaction (source, target, type, span, args, etc.)
            // was somehow collected multiple times, we only add it once.
            // The sorting ensures that identical items are adjacent for dedup() to work.
            let original_len = modifications.len();
            modifications.dedup();
            let deduped_len = modifications.len();
            if original_len != deduped_len {
                eprintln!(
                    "[CallsHandling DEBUG] Deduplicated {} modifications for Caller Node ID: {}",
                    original_len - deduped_len,
                    caller_node_id
                );
            }
            // --- End Deduplication ---

            let mut call_sequence_counter: usize = 0; // Reset sequence counter for each function body
            for modification in modifications {
                // Iterate over the deduplicated list
                call_sequence_counter += 1; // Increment sequence number for each edge added
                                            // --- BEGIN ADD EDGE DEBUG ---
                eprintln!("[Add Edge DEBUG] Adding Edge (Seq: {}): Source={}, Target={}, Type={:?}, Span={:?}, OrigSpanStart={}",
                    call_sequence_counter,
                    modification.source_node_id,
                    modification.target_node_id,
                    modification.edge_type, // Check this value specifically
                    modification.span,
                    modification.sort_span_start
                );
                // --- END ADD EDGE DEBUG ---
                eprintln!("[CallsHandling DEBUG]   Adding Edge (Seq: {}): {:?} -> {:?} ({:?}), Span: {:?}, OrigSpanStart: {}",
                    call_sequence_counter,
                    modification.source_node_id,
                    modification.target_node_id,
                    modification.edge_type,
                    modification.span,
                    modification.sort_span_start
                );
                graph.add_edge(
                    modification.source_node_id,
                    modification.target_node_id,
                    modification.edge_type,
                    modification.span,
                    None, // Modifier span is not tracked in GraphModification currently
                    call_sequence_counter, // Assign the determined sequence number
                    modification.return_value,
                    modification.arguments,
                    modification.event_name,
                );
            }
        }

        Ok(())
    }
}

/// Resolves a storage variable name to its node ID, considering inheritance.
/// It searches the current contract and then its ancestors.
fn resolve_storage_variable(
    current_contract_name_opt: &Option<String>,
    variable_name: &str,
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext,
) -> Option<usize> {
    let contract_name = match current_contract_name_opt {
        Some(name) => name,
        None => {
            // Storage variables cannot exist outside a contract scope
            eprintln!("[Storage Resolve DEBUG] Attempted to resolve variable '{}' outside contract scope.", variable_name);
            return None;
        }
    };

    let ancestors = graph.get_ancestor_contracts(contract_name, ctx);
    eprintln!(
        "[Storage Resolve DEBUG] Resolving variable '{}' in contract '{}'. Ancestors: {:?}",
        variable_name, contract_name, ancestors
    );

    for ancestor_name in ancestors {
        let key = (Some(ancestor_name.clone()), variable_name.to_string());
        eprintln!("[Storage Resolve DEBUG]   Checking key: {:?}", key);
        if let Some(node_id) = graph.node_lookup.get(&key) {
            // Verify it's actually a storage variable node
            if graph
                .nodes
                .get(*node_id)
                .map_or(false, |n| n.node_type == NodeType::StorageVariable)
            {
                eprintln!("[Storage Resolve DEBUG]     Found storage variable node ID {} in contract '{}'", *node_id, ancestor_name);
                return Some(*node_id);
            } else {
                eprintln!("[Storage Resolve DEBUG]     Found node ID {} for key {:?} but it's not a StorageVariable.", *node_id, key);
            }
        }
    }

    eprintln!(
        "[Storage Resolve DEBUG] Variable '{}' not found in contract '{}' or its ancestors.",
        variable_name, contract_name
    );
    None
}

// Helper function to resolve ResolvedTarget to a node ID in the graph
fn resolve_target_to_node_id(
    target: &crate::chains::ResolvedTarget, // Use full path
    graph: &CallGraph,
    _ctx: &CallGraphGeneratorContext, // Mark ctx as unused for now, might need later
) -> Option<usize> {
    match target {
        crate::chains::ResolvedTarget::Function {
            contract_name,
            function_name,
            .. // Ignore node_type for lookup
        } => {
            let key = (contract_name.clone(), function_name.clone());
            let result = graph.node_lookup.get(&key).copied(); // Look up directly
            eprintln!(
                "[Resolve Target ID] Function Lookup: Key=({:?}, '{}') -> Result={:?}",
                contract_name, function_name, result
            );
            result
        }
        crate::chains::ResolvedTarget::InterfaceMethod {
            implementation: Some(impl_target), // If a concrete implementation exists
            ..
        } => {
            // Recursively resolve the implementation target
            eprintln!(
                "[Resolve Target ID] InterfaceMethod: Resolving implementation target: {:?}",
                impl_target
            );
            resolve_target_to_node_id(impl_target, graph, _ctx)
        }
        crate::chains::ResolvedTarget::InterfaceMethod {
            implementation: None, // No concrete implementation found/resolved
            interface_name,
            method_name,
            ..
        } => {
            // Link to the interface method node itself.
            let key = (Some(interface_name.clone()), method_name.clone());
            let result = graph.node_lookup.get(&key).copied();
            eprintln!(
                "[Resolve Target ID] InterfaceMethod (Abstract): Lookup Key=({:?}, '{}') -> Result={:?}",
                Some(interface_name), method_name, result
            );
            if result.is_none() {
                eprintln!("[Resolve Target ID] Warning: Node for abstract interface method {}.{} not found in graph lookup.", interface_name, method_name);
            }
            result // Return the lookup result (Option<usize>)
        }
        crate::chains::ResolvedTarget::BuiltIn { object_type, name } => {
            // Built-ins don't have dedicated nodes in our graph currently
            eprintln!(
                "[Resolve Target ID] Info: Skipping edge creation for built-in {}.{}.",
                object_type, name
            );
            None
        }
        crate::chains::ResolvedTarget::NotCallable { reason } => {
            eprintln!(
                "[Resolve Target ID] Info: Skipping edge creation for non-callable target: {}",
                reason
            );
            None
        }
        crate::chains::ResolvedTarget::External { address_expr } => {
            // External calls don't resolve to a specific node in our graph
            eprintln!(
                "[Resolve Target ID] Info: Skipping edge creation for external call to address expr: {}",
                address_expr
            );
            None
        }
        crate::chains::ResolvedTarget::TypeCast { type_name } => {
            // Type casts don't resolve to a callable node for edge creation.
            eprintln!(
                "[Resolve Target ID] Info: Skipping edge creation for type cast to '{}'.",
                type_name
            );
            None
        }
    }
}
