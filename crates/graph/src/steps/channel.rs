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

// Struct to hold modification data for deferred edge addition ---
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
    sort_span_start: usize, // Span start of the *outermost* originating source element (call, emit, assignment, etc.)
    chain_index: Option<usize>, // Index within a call chain from analyze_chained_call
    execution_priority: i32, // Relative execution order within the sort_span_start group
}

#[derive(Default)] 
pub struct CallsHandling {
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
            (identifier) @read_candidate_identifier
            ; Add more negative checks if needed (e.g., type names, event names, member access object/property)
            (#not-match? @read_candidate_identifier "^(require|assert|revert|emit|new)$") ; Exclude keywords/builtins used like functions
            ; --- Filtering for function names, assignment LHS, type names, etc., will be done in Rust code ---

            ; Capture array access expressions (base is potential read/write target)
            ; The base of the array access will be extracted in Rust code.
            (array_access) @read_candidate_subscript

            ; --- Require statement ---
            ; Matches require(condition, "message")
            (call_expression
              function: (expression (identifier) @require_identifier)
              (#eq? @require_identifier "require")
              ; Arguments are direct children (call_argument), handled by extract_arguments
            ) @require_call_node ; Capture the whole call expression for require

            ; --- Delete statement (unary expression) ---
            (unary_expression
              operator: "delete" @delete_operator ; Identify the delete operator
              argument: (_) @delete_target ; Capture the expression being deleted
            ) @delete_expression_node ; Capture the whole unary expression for delete
        "#;
        let call_query = Query::new(&input.solidity_lang, call_query_str)
            .context("Failed to create call query")?;

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

            // Collect modifications for this function body ---
            let mut modifications: Vec<GraphModification> = Vec::new();
            let caller_node_name = graph
                .nodes
                .get(caller_node_id)
                .map_or("?".to_string(), |n| {
                    format!(
                        "{}.{}",
                        n.contract_name.as_deref().unwrap_or("Global"),
                        n.name
                    )
                });
            eprintln!("[CallsHandling DEBUG] Processing calls/emits within Caller Node ID: {} (Name: '{}', Contract: {:?})", caller_node_id, caller_node_name, caller_contract_name_opt);

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
            let read_identifier_capture_index =
                call_query // Renamed
                    .capture_index_for_name("read_candidate_identifier")
                    .unwrap_or(u32::MAX);
            let read_subscript_capture_index =
                call_query // New
                    .capture_index_for_name("read_candidate_subscript")
                    .unwrap_or(u32::MAX);

            let require_call_capture_index = call_query
                .capture_index_for_name("require_call_node")
                .unwrap_or(u32::MAX);
            let delete_expression_capture_index = call_query
                .capture_index_for_name("delete_expression_node")
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
                    } else if capture_index == read_identifier_capture_index {
                        // Renamed
                        Some("read_identifier") // Differentiate
                    } else if capture_index == read_subscript_capture_index {
                        // New
                        Some("read_subscript") // Differentiate
                    } else if capture_index == delete_expression_capture_index {
                        Some("delete") // Add delete type
                    } else {
                        None // Ignore other captures like @call_name etc.
                    };

                    if let Some(node_type) = current_type {
                        collected_nodes_map
                            .entry(span)
                            .and_modify(|existing| {
                                // Prioritization logic: require > emit > new > call > write/delete > read
                                let existing_priority = match existing.1 {
                                    "require" => 6,
                                    "emit" => 5,
                                    "new" => 4,
                                    "call" => 3,
                                    "write" => 2,
                                    "delete" => 2, // Same priority as write
                                    "read_identifier" => 1,
                                    "read_subscript" => 1,
                                    _ => -1,
                                };
                                let new_priority = match node_type {
                                    "require" => 6,
                                    "emit" => 5,
                                    "new" => 4,
                                    "call" => 3,
                                    "write" => 2,
                                    "delete" => 2, // Same priority as write
                                    "read_identifier" => 1,
                                    "read_subscript" => 1,
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

            // Convert map into Vec<(TsNode, span, &str)>
            potential_nodes = collected_nodes_map
                .into_iter() // Iterate over key-value pairs (span, (node, type))
                .map(|(span, (node, node_type))| (node, span, node_type)) // Map to (node, span, type)
                .collect();

            // Define type priorities for sorting: higher values processed first.
            // We use negative priority in sort_by_key to achieve this.
            let type_priority = |node_type_str: &str| -> i32 {
                match node_type_str {
                    "require" => 6,
                    "emit" => 5,
                    "new" => 4,
                    "call" => 3,
                    "write" | "delete" => 2,
                    "read_identifier" | "read_subscript" => 1,
                    _ => 0, // Default for unknown types
                }
            };
            // Sort by span start, then by negative type priority (so higher priority types come first), then by span end.
            potential_nodes.sort_by_key(|k| (k.1 .0, -type_priority(k.2), k.1 .1));

            // --- Second Pass: Process sorted, deduplicated nodes and collect GraphModifications ---
            for (node, span, node_type) in potential_nodes {
                // Find enclosing assignment start for priority/sorting ---
                let enclosing_assignment_start = find_enclosing_assignment_start(node);
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
                            None, // Top-level call, no original node needed for error reporting here
                            node.start_byte(), // Pass the start byte of the outermost node as originating span start
                        ) {
                            Ok(steps) => {
                                eprintln!(
                                    "[CallsHandling DEBUG]         analyze_chained_call returned {} steps.",
                                    steps.len()
                                );
                                if steps.is_empty() {}

                                // Collect modifications for the main call chain steps
                                // Use enumerate to get the index (starting from 1) within the chain returned by analyze_chained_call
                                for (chain_index_zero_based, step) in steps.iter().enumerate() {
                                    let chain_index = chain_index_zero_based + 1; // Start index from 1
                                    eprintln!("[CallsHandling DEBUG]           Processing Step (Chain Index {}): Target={:?}, Args={:?}", chain_index, step.target, step.arguments);

                                    // --- Handle Regular Targets (including those previously treated as BuiltIn) ---
                                    if let Some(target_node_id) =
                                        resolve_target_to_node_id(&step.target, graph, ctx)
                                    {
                                        eprintln!("[CallsHandling DEBUG]             >>> Collecting modification (Regular Call): CallerID={}, TargetID={}, StepSpan={:?}, ChainIndex={}, Priority={}", caller_node_id, target_node_id, step.call_expr_span, chain_index, chain_index);
                                        modifications.push(GraphModification {
                                            source_node_id: caller_node_id, // Source is the caller
                                            target_node_id, // Target is the resolved node
                                            edge_type: EdgeType::Call, // Could be ConstructorCall too, but handled by target node type later
                                            span: (
                                                step.call_expr_span.0.into(),
                                                step.call_expr_span.1.into(),
                                            ),
                                            modifier: None,
                                            return_value: None, // Not tracked here for regular calls either
                                            arguments: Some(step.arguments.clone()),
                                            event_name: None, // No special event name for regular calls
                                            sort_span_start: enclosing_assignment_start
                                                .unwrap_or(step.originating_span_start),
                                            chain_index: Some(chain_index), // Store the 1-based index within the chain
                                            execution_priority: (chain_index as i32) * 10,
                                        });
                                    }
                                    // --- Handle Unresolved Targets (including BuiltIns that don't resolve to nodes) ---
                                    else {
                                        // --- Check for storage-mutating built-ins on state variables ---
                                        if let ResolvedTarget::BuiltIn { name, object_type } =
                                            &step.target
                                        {
                                            // Use the new builtin helper to check for mutation
                                            if crate::builtin::is_mutating_builtin(
                                                name,
                                                object_type,
                                            ) {
                                                if let Some(base_var_name) =
                                                    &step.base_object_identifier_for_builtin
                                                {
                                                    // Attempt to resolve this base variable name as a storage variable
                                                    if let Some(var_node_id) =
                                                        resolve_storage_variable(
                                                            &caller_contract_name_opt,
                                                            base_var_name,
                                                            graph,
                                                            ctx,
                                                        )
                                                    {
                                                        // It's a storage variable! Add a StorageWrite edge.
                                                        eprintln!("[Storage DEBUG Deferred] Collecting WRITE modification (BuiltIn Call): CallerID={}, VarID={}, VarName='{}', BuiltIn='{}', CallSpan={:?}", caller_node_id, var_node_id, base_var_name, name, step.call_expr_span);
                                                        modifications.push(GraphModification {
                                                            source_node_id: caller_node_id,
                                                            target_node_id: var_node_id,
                                                            edge_type: EdgeType::StorageWrite,
                                                            span: step.call_expr_span, // Span of the push/pop call
                                                            modifier: None,
                                                            return_value: None,
                                                            arguments: Some(step.arguments.clone()), // Include args passed to push/pop
                                                            event_name: None,
                                                            // Use the call's start span for sorting
                                                            sort_span_start: step.call_expr_span.0,
                                                            chain_index: None, // Not part of a typical call chain index
                                                            // Priority 1000 for writes
                                                            execution_priority: 1000,
                                                        });
                                                    } else {
                                                        eprintln!("[CallsHandling DEBUG]             >>> BuiltIn '{}' called on '{}', but it did not resolve to a storage variable.", name, base_var_name);
                                                    }
                                                } else {
                                                    eprintln!("[CallsHandling DEBUG]             >>> BuiltIn '{}' called, but base object identifier was not captured.", name);
                                                }
                                            } else {
                                                eprintln!("[CallsHandling DEBUG]             >>> BuiltIn '{}' is not push/pop. Skipping storage write check.", name);
                                            }
                                        } else {
                                            eprintln!("[CallsHandling DEBUG]             >>> Target for step {:?} did not resolve to a node ID and is not a BuiltIn. Skipping modification.", step.target);
                                        }
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
                                                                        // Note: This internal 'new' inherits the sort context of the *outer* call step.
                                                                        sort_span_start: enclosing_assignment_start.unwrap_or(step.originating_span_start),
                                                                        chain_index: None, // Not part of the main chain index sequence
                                                                        execution_priority: 0, // (0 * 10)
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
                        if let Some(lhs_node_expr) = assignment_node.child_by_field_name("left") {
                            // Helper to get the actual node if wrapped in 'expression'
                            fn get_actual_target_node(mut n: TsNode) -> TsNode {
                                while n.kind() == "expression" && n.named_child_count() == 1 {
                                    if let Some(child) = n.named_child(0) {
                                        n = child;
                                    } else {
                                        break;
                                    }
                                }
                                n
                            }

                            // Helper to find the base identifier of a complex expression (identifier, member_expression, array_access)
                            fn find_base_identifier_recursive<'a>(
                                n: TsNode<'a>,
                                input_source: &str,
                            ) -> Option<TsNode<'a>> {
                                let actual_node = get_actual_target_node(n);
                                match actual_node.kind() {
                                    "identifier" => Some(actual_node),
                                    "member_expression" => {
                                        if let Some(object_expr) =
                                            actual_node.child_by_field_name("object")
                                        {
                                            find_base_identifier_recursive(
                                                object_expr,
                                                input_source,
                                            )
                                        } else {
                                            eprintln!("[Storage Write DEBUG] Member expression missing object: {}", get_node_text(&actual_node, input_source));
                                            None
                                        }
                                    }
                                    "array_access" => {
                                        if let Some(base_expr) =
                                            actual_node.child_by_field_name("base")
                                        {
                                            find_base_identifier_recursive(base_expr, input_source)
                                        } else {
                                            eprintln!("[Storage Write DEBUG] Array access missing base: {}", get_node_text(&actual_node, input_source));
                                            None
                                        }
                                    }
                                    _ => {
                                        // Fallback: if it's not one of the above, try a direct BFS for an identifier as a last resort.
                                        // This might be too broad, but could catch simple wrappers.
                                        let mut queue = std::collections::VecDeque::new();
                                        queue.push_back(actual_node);
                                        while let Some(current) = queue.pop_front() {
                                            if current.kind() == "identifier" {
                                                return Some(current);
                                            }
                                            if current.kind() != "identifier" {
                                                // Avoid re-queueing if current is already ident
                                                let mut cursor = current.walk();
                                                for child in current.children(&mut cursor) {
                                                    queue.push_back(child);
                                                }
                                            }
                                        }
                                        eprintln!("[Storage Write DEBUG] Could not find base identifier for LHS node kind: {}, text: {}", actual_node.kind(), get_node_text(&actual_node, input_source));
                                        None
                                    }
                                }
                            }

                            if let Some(write_target_node) =
                                find_base_identifier_recursive(lhs_node_expr, &input.source)
                            {
                                let var_name = get_node_text(&write_target_node, &input.source);
                                eprintln!(
                                    "[Storage Write DEBUG] LHS base identifier: '{}', span: {:?}",
                                    var_name,
                                    (write_target_node.start_byte(), write_target_node.end_byte())
                                );
                                // --- Use inheritance-aware resolution ---
                                if let Some(var_node_id) = resolve_storage_variable(
                                    &caller_contract_name_opt,
                                    &var_name,
                                    graph,
                                    ctx,
                                ) {
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
                                        sort_span_start: assignment_span.0,
                                        chain_index: None,
                                        execution_priority: 1000,
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
                    "read_identifier" => {
                        // --- Handle Storage Reads from Identifiers ---
                        let read_candidate_node = node; // This is an identifier node
                        let read_span = span; // Span of the identifier
                        let var_name = get_node_text(&read_candidate_node, &input.source);

                        // Determine if this read is the object of a subsequent call OR an argument to a call/emit/require
                        let mut is_read_for_call = false;
                        let mut is_read_for_emit_or_require = false;
                        let mut parent_op_span_start = read_span.0; // Default to own span start (will be updated)
                        let mut current_node = read_candidate_node;
                        let mut found_member_expr: Option<TsNode> = None;
                        let mut found_parent_op_node: Option<TsNode> = None;

                        // Traverse upwards from the read identifier to find the relevant member_expression,
                        // potentially passing through type_cast_expression or expression nodes.
                        loop {
                            if let Some(parent) = current_node.parent() {
                                match parent.kind() {
                                    "member_expression" => {
                                        // Check if current_node (identifier or type_cast) is the 'object'
                                        let object_node = parent.child_by_field_name("object");
                                        if object_node
                                            .map_or(false, |obj| obj.id() == current_node.id())
                                        {
                                            found_member_expr = Some(parent);
                                            break; // Found the relevant member expression
                                        }
                                        // If current_node is not the object, stop ascending this path
                                        break;
                                    }
                                    "type_cast_expression" => {
                                        // Check if current_node (identifier) is the 'value' being cast
                                        let value_node = parent.child_by_field_name("value");
                                        // The value might be wrapped in an expression node, e.g., Type( (expr) )
                                        let mut is_value = value_node
                                            .map_or(false, |val| val.id() == current_node.id());
                                        if !is_value
                                            && value_node.map_or(false, |val| {
                                                val.kind() == "expression"
                                                    && val.child_count() > 0
                                                    && val.child(0).unwrap().id()
                                                        == current_node.id()
                                            })
                                        {
                                            is_value = true;
                                        }

                                        if is_value {
                                            // Continue searching upwards from the type_cast node itself
                                            current_node = parent; // Ascend from the type_cast node
                                            continue;
                                        }
                                        // If current_node is not the value, stop ascending this path
                                        break;
                                    }
                                    // Allow traversing through intermediate structural nodes like expressions, arguments etc.
                                    "expression" | "call_argument" | "arguments"
                                    | "call_expression" => {
                                        // Allow traversing through call_expression (likely a type cast)
                                        // We might need more specific checks here if this proves too broad,
                                        // e.g., check if the call_expression looks like a type cast.
                                        current_node = parent;
                                        continue;
                                    }
                                    // Stop if we hit function boundaries or other non-relevant structural nodes
                                    "function_definition"
                                    | "modifier_definition"
                                    | "contract_body"
                                    | "source_file"
                                    | "block"
                                    | "variable_declaration_statement"
                                    | "assignment_expression"
                                    | "return_statement" => {
                                        break;
                                    }
                                    // Add other potential parent kinds if needed
                                    _ => {
                                        // Stop ascending if we hit an unexpected node kind
                                        eprintln!("[DEBUG Read Link] Stopping ascent at unexpected parent kind: {}", parent.kind());
                                        break;
                                    }
                                }
                            } else {
                                break; // Reached root
                            }
                        }

                        // Check if the read is the object of a member access that leads to a call
                        if let Some(member_expr) = found_member_expr {
                            let mut func_part_node = member_expr;
                            loop {
                                if let Some(parent) = func_part_node.parent() {
                                    match parent.kind() {
                                        "call_expression" => {
                                            let function_field_node =
                                                parent.child_by_field_name("function");
                                            if function_field_node.map_or(false, |func_node| {
                                                func_node.id() == func_part_node.id()
                                            }) {
                                                is_read_for_call = true;
                                                found_parent_op_node = Some(parent); // Store the call node
                                                parent_op_span_start = parent.start_byte(); // Use call expression's start
                                                eprintln!("[Storage DEBUG Deferred V3] Read '{}' identified as object (via member_expr at {:?}) for call at span ({}, {}). Using call's start {} for parent_op_span_start.", var_name, member_expr.byte_range(), parent.start_byte(), parent.end_byte(), parent_op_span_start);
                                                break; // Found the call
                                            }
                                            break; // Not the function part of this call
                                        }
                                        "expression" | "type_cast_expression" => {
                                            func_part_node = parent;
                                            continue; // Ascend through wrappers
                                        }
                                        _ => {
                                            eprintln!("[DEBUG Read Link] Stopping ascent towards call at unexpected parent kind: {}", parent.kind());
                                            break;
                                        }
                                    }
                                } else {
                                    break;
                                } // Reached root
                            }
                        }

                        // Check if the read is an argument to a call, emit, or require
                        // Only check if it wasn't already identified as the object of a call
                        if !is_read_for_call {
                            let mut arg_part_node = read_candidate_node;
                            loop {
                                if let Some(parent) = arg_part_node.parent() {
                                    match parent.kind() {
                                        "call_argument" => {
                                            // Found a call_argument, check its parent (the call/emit/require)
                                            if let Some(grandparent) = parent.parent() {
                                                match grandparent.kind() {
                                                    "call_expression" => {
                                                        // Check if it's a regular call or require
                                                        let func_node = grandparent
                                                            .child_by_field_name("function");
                                                        let is_require = func_node
                                                            .and_then(|f| f.child(0)) // Get identifier node inside expression
                                                            .map_or(false, |id_node| {
                                                                get_node_text(
                                                                    &id_node,
                                                                    &input.source,
                                                                ) == "require"
                                                            });

                                                        is_read_for_emit_or_require = true; // Treat call args same as emit/require args for priority
                                                        found_parent_op_node = Some(grandparent);
                                                        parent_op_span_start =
                                                            grandparent.start_byte();
                                                        eprintln!("[Storage DEBUG Deferred V3] Read '{}' identified as argument for {} at span ({}, {}). Using op's start {} for parent_op_span_start.", var_name, if is_require {"require"} else {"call"}, grandparent.start_byte(), grandparent.end_byte(), parent_op_span_start);
                                                        break; // Found parent operation
                                                    }
                                                    "emit_statement" => {
                                                        is_read_for_emit_or_require = true;
                                                        found_parent_op_node = Some(grandparent);
                                                        parent_op_span_start =
                                                            grandparent.start_byte();
                                                        eprintln!("[Storage DEBUG Deferred V3] Read '{}' identified as argument for emit at span ({}, {}). Using emit's start {} for parent_op_span_start.", var_name, grandparent.start_byte(), grandparent.end_byte(), parent_op_span_start);
                                                        break; // Found parent operation
                                                    }
                                                    _ => { /* Argument of something else, ignore */
                                                    }
                                                }
                                            }
                                            // If grandparent check didn't break, stop ascending this path
                                            break;
                                        }
                                        // Allow ascending through structural nodes that might wrap the argument expression
                                        "expression"
                                        | "type_cast_expression"
                                        | "binary_expression"
                                        | "unary_expression"
                                        | "parenthesized_expression" => {
                                            arg_part_node = parent;
                                            continue;
                                        }
                                        // Stop at boundaries
                                        "function_definition"
                                        | "modifier_definition"
                                        | "contract_body"
                                        | "source_file"
                                        | "block"
                                        | "variable_declaration_statement"
                                        | "assignment_expression"
                                        | "return_statement"
                                        | "member_expression" => {
                                            // Stop if we hit a member access, handled above
                                            break;
                                        }
                                        _ => {
                                            eprintln!("[DEBUG Read Arg Link] Stopping ascent towards arg parent at unexpected kind: {}", parent.kind());
                                            break;
                                        }
                                    }
                                } else {
                                    break;
                                } // Reached root
                            }
                        }

                        // --- Use inheritance-aware resolution ---
                        if let Some(var_node_id) = resolve_storage_variable(
                            &caller_contract_name_opt,
                            &var_name,
                            graph,
                            ctx,
                        ) {
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

                                    // Check if identifier is a type name in a cast used for a member call ---
                                    // Handles cases like `TypeName(var).method()` where `TypeName` is the identifier.
                                    if !skip_read_edge && parent_kind == "type_name" {
                                        // Check if the type_name is part of a `new_expression` or `type_cast_expression`
                                        if let Some(grandparent) = parent.parent() {
                                            if grandparent.kind() == "new_expression" {
                                                // This is `new TypeName(...)`, already handled, but good to be explicit.
                                                skip_read_edge = true;
                                            } else if grandparent.kind() == "type_cast_expression" {
                                                // This is `TypeName(...)`. Check if this cast is the object of a member access.
                                                if let Some(great_grandparent) =
                                                    grandparent.parent()
                                                {
                                                    if great_grandparent.kind()
                                                        == "member_expression"
                                                    {
                                                        // Check if the type_cast_expression is the 'object' field
                                                        let object_node = great_grandparent
                                                            .child_by_field_name("object");
                                                        if object_node.map_or(false, |obj| {
                                                            obj.id() == grandparent.id()
                                                        }) {
                                                            // Now check if this member_expression is part of a call
                                                            if let Some(great_great_grandparent) =
                                                                great_grandparent.parent()
                                                            {
                                                                if great_great_grandparent.kind()
                                                                    == "call_expression"
                                                                {
                                                                    // --- More robust check for member_expression being the function ---
                                                                    let function_field_node =
                                                                        great_great_grandparent
                                                                            .child_by_field_name(
                                                                                "function",
                                                                            );
                                                                    let mut current_func_part =
                                                                        function_field_node;
                                                                    let mut is_function = false;
                                                                    // Loop to handle potential wrappers like 'expression'
                                                                    while let Some(node) =
                                                                        current_func_part
                                                                    {
                                                                        // Check if the current node in the function field traversal is our member_expression
                                                                        if node.id()
                                                                            == great_grandparent
                                                                                .id()
                                                                        {
                                                                            // great_grandparent is the member_expression
                                                                            is_function = true;
                                                                            break;
                                                                        }
                                                                        // If it's a simple wrapper (like 'expression' with one child), descend
                                                                        if (node.kind() == "expression" || node.kind() == "parenthesized_expression") && node.child_count() == 1 {
                                                                             current_func_part = node.child(0);
                                                                        } else {
                                                                            break; // Not the node we are looking for, and not a simple wrapper
                                                                        }
                                                                    }

                                                                    if is_function {
                                                                        eprintln!("[Storage DEBUG Deferred V2] Skipping READ for type name '{}' in cast-member-call", var_name);
                                                                        skip_read_edge = true;
                                                                    }
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    // Check if identifier is the function name in a simple call `func()`
                                    if !skip_read_edge && parent_kind == "expression" {
                                        // Added !skip_read_edge check here
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

                                    // --- Check if identifier is the property name in a member call ---
                                    // Handles `object.propertyName()` even if wrapped in expression, e.g., `call((object.propertyName))`
                                    if !skip_read_edge && parent_kind == "member_expression" {
                                        let member_expr_node = parent; // Keep track of the specific member_expression node
                                        let property_node_opt =
                                            member_expr_node.child_by_field_name("property");

                                        if property_node_opt.map_or(false, |prop| {
                                            prop.id() == read_candidate_node.id()
                                        }) {
                                            // Check if this member_expression (member_expr_node) is part of a call expression's function field by ascending
                                            let mut current_ancestor = member_expr_node;
                                            let mut is_function_of_call = false;

                                            loop {
                                                if let Some(ancestor) = current_ancestor.parent() {
                                                    match ancestor.kind() {
                                                        "call_expression" => {
                                                            let function_field_node_opt = ancestor
                                                                .child_by_field_name("function");
                                                            if let Some(function_field_node) =
                                                                function_field_node_opt
                                                            {
                                                                // Check if the function_field_node (or a node it wraps) is our original member_expr_node
                                                                let mut temp_node =
                                                                    function_field_node;
                                                                loop {
                                                                    if temp_node.id()
                                                                        == member_expr_node.id()
                                                                    {
                                                                        is_function_of_call = true;
                                                                        break;
                                                                    }
                                                                    // Descend through simple wrappers like 'expression' or 'parenthesized_expression'
                                                                    // Use named_child_count and named_child to avoid issues with anonymous nodes like '('
                                                                    if (temp_node.kind() == "expression" || temp_node.kind() == "parenthesized_expression") && temp_node.named_child_count() == 1 {
                                                                         if let Some(child) = temp_node.named_child(0) {
                                                                            temp_node = child;
                                                                            continue; // Continue descent
                                                                         }
                                                                    }
                                                                    break; // Not the node we are looking for, and not a simple wrapper we can descend through
                                                                }
                                                            }
                                                            if is_function_of_call {
                                                                break; // Found the relevant call expression
                                                            }
                                                            // If not the function of *this* call, maybe it's nested higher? Continue ascent.
                                                            current_ancestor = ancestor;
                                                            continue;
                                                        }
                                                        // Ascend through wrappers that might contain the call
                                                        "expression"
                                                        | "parenthesized_expression"
                                                        | "call_argument"
                                                        | "arguments" => {
                                                            current_ancestor = ancestor;
                                                            continue;
                                                        }
                                                        // Stop at boundaries or irrelevant nodes
                                                        "function_definition"
                                                        | "modifier_definition"
                                                        | "contract_body"
                                                        | "source_file"
                                                        | "block"
                                                        | "assignment_expression"
                                                        | "variable_declaration_statement" => {
                                                            break;
                                                        }
                                                        _ => {
                                                            // Stop ascent for other unexpected kinds
                                                            eprintln!("[DEBUG Read Prop Link] Stopping ascent towards call at unexpected parent kind: {}", ancestor.kind());
                                                            break;
                                                        }
                                                    }
                                                } else {
                                                    break; // Reached root
                                                }
                                            } // End loop

                                            if is_function_of_call {
                                                eprintln!("[Storage DEBUG Deferred] Skipping READ for function name in member call '{}'", var_name);
                                                skip_read_edge = true;
                                            }
                                        }
                                    }

                                    // Check if identifier is a type name used in a cast-as-call pattern ---
                                    // Handles `TypeName(var).method()` where TypeName(...) is parsed as a call_expression
                                    if !skip_read_edge {
                                        if let Some(parent_expr) = read_candidate_node.parent() {
                                            if parent_expr.kind() == "expression" {
                                                if let Some(cast_call_expr) = parent_expr.parent() {
                                                    if cast_call_expr.kind() == "call_expression"
                                                        && cast_call_expr
                                                            .child_by_field_name("function")
                                                            .map_or(false, |f| {
                                                                f.id() == parent_expr.id()
                                                            })
                                                    {
                                                        // Now check if this cast_call_expr is the object of a member_expression
                                                        let mut current_object_part =
                                                            cast_call_expr;
                                                        let mut member_expr_node_opt: Option<
                                                            TsNode,
                                                        > = None;

                                                        // Ascend through potential wrappers to find the member_expression where cast_call_expr is the object
                                                        loop {
                                                            if let Some(parent_of_object_part) =
                                                                current_object_part.parent()
                                                            {
                                                                if parent_of_object_part.kind() == "member_expression" {
                                                                    let object_field = parent_of_object_part.child_by_field_name("object");
                                                                    if object_field.map_or(false, |obj_node| obj_node.id() == current_object_part.id()) {
                                                                        member_expr_node_opt = Some(parent_of_object_part);
                                                                        break; // Found it
                                                                    }
                                                                    break; // Not the object field
                                                                } else if parent_of_object_part.kind() == "expression" || parent_of_object_part.kind() == "parenthesized_expression" {
                                                                    current_object_part = parent_of_object_part; // Ascend wrapper
                                                                } else {
                                                                    break; // Stop ascent
                                                                }
                                                            } else {
                                                                break; // Reached root
                                                            }
                                                        }

                                                        if let Some(member_expr_node) =
                                                            member_expr_node_opt
                                                        {
                                                            // Now check if this member_expression is the function of an outer call
                                                            let mut current_func_part =
                                                                member_expr_node;
                                                            let mut found_outer_call = false;
                                                            // Ascend through potential wrappers to find the outer call_expression where member_expr_node is the function
                                                            loop {
                                                                if let Some(parent_of_func_part) =
                                                                    current_func_part.parent()
                                                                {
                                                                    if parent_of_func_part.kind() == "call_expression" {
                                                                        let function_field = parent_of_func_part.child_by_field_name("function");
                                                                        if function_field.map_or(false, |func_node| func_node.id() == current_func_part.id()) {
                                                                            found_outer_call = true;
                                                                            break; // Found it
                                                                        }
                                                                        break; // Not the function field
                                                                    } else if parent_of_func_part.kind() == "expression" || parent_of_func_part.kind() == "parenthesized_expression" {
                                                                        current_func_part = parent_of_func_part; // Ascend wrapper
                                                                    } else {
                                                                        break; // Stop ascent
                                                                    }
                                                                } else {
                                                                    break; // Reached root
                                                                }
                                                            }

                                                            if found_outer_call {
                                                                eprintln!("[Storage DEBUG Deferred V4] Skipping READ for type name '{}' in cast-as-call pattern", var_name);
                                                                skip_read_edge = true;
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }

                                    // --- Check if identifier is the argument of a delete expression ---
                                    // Handles both `delete identifier` and `delete expression(identifier)`
                                    if !skip_read_edge {
                                        let mut delete_ancestor = parent; // Start with the direct parent
                                                                          // If the direct parent is just an expression wrapper, check the grandparent
                                        if parent_kind == "expression" {
                                            if let Some(grandparent) = parent.parent() {
                                                delete_ancestor = grandparent;
                                            }
                                        }

                                        // Check if the relevant ancestor is a 'delete' unary expression
                                        if delete_ancestor.kind() == "unary_expression" {
                                            let operator = delete_ancestor
                                                .child_by_field_name("operator")
                                                .map(|op| get_node_text(&op, &input.source));
                                            let argument_node_opt =
                                                delete_ancestor.child_by_field_name("argument");

                                            // Check if the operator is 'delete' and the argument node contains the read_candidate node
                                            if operator.as_deref() == Some("delete") {
                                                if let Some(argument_node) = argument_node_opt {
                                                    // Check if the read_candidate node is a descendant of the argument node
                                                    // (This handles cases like `delete a.b` where `a` is the read candidate but `a.b` is the argument)
                                                    // Or if the argument node *is* the read_candidate node (for `delete a`)
                                                    let mut is_descendant = false;
                                                    let mut queue =
                                                        std::collections::VecDeque::new();
                                                    queue.push_back(argument_node);
                                                    while let Some(current) = queue.pop_front() {
                                                        if current.id() == read_candidate_node.id()
                                                        {
                                                            is_descendant = true;
                                                            break;
                                                        }
                                                        let mut cursor = current.walk();
                                                        for child in current.children(&mut cursor) {
                                                            queue.push_back(child);
                                                        }
                                                    }

                                                    if is_descendant {
                                                        eprintln!("[Storage DEBUG Deferred] Skipping READ for delete argument '{}' (Ancestor: {}, Arg Node: {:?})", var_name, delete_ancestor.kind(), argument_node.kind());
                                                        skip_read_edge = true;
                                                    }
                                                }
                                            }
                                        }
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
                                        sort_span_start: enclosing_assignment_start
                                            .unwrap_or(parent_op_span_start),
                                        chain_index: None,
                                        execution_priority: {
                                            let mut prio = 0; // Default for standalone reads or reads in unlinked contexts
                                            if is_read_for_call || is_read_for_emit_or_require {
                                                if let Some(op_node) = found_parent_op_node {
                                                    // Find the parent operation's GraphModification to get its base priority
                                                    let parent_op_mod =
                                                        modifications.iter().find(|m| {
                                                            m.span.0 == op_node.start_byte()
                                                                && (m.edge_type == EdgeType::Call
                                                                    || m.edge_type
                                                                        == EdgeType::Require)
                                                            // Emit might not be in modifications yet if split
                                                        });
                                                    if let Some(parent_mod) = parent_op_mod {
                                                        // parent_mod.execution_priority is already scaled (e.g., C*10)
                                                        prio = parent_mod.execution_priority - 1;
                                                    // Read happens just before its call
                                                    } else {
                                                        // If op_node is emit: base_prio = 10. read_prio = 9.
                                                        // This part is tricky as 'modifications' is built iteratively.
                                                        // A simpler approach for now: if it's for a call/emit/require, give it a generic "before call" prio.
                                                        // Let's use a fixed low value if parent not found, to be refined.
                                                        // For now, if parent_op_node exists, assume it's important.
                                                        // If it's part of an assignment, it's less than the call.
                                                        // If not in assignment, it's -1 (current logic).
                                                        // This needs to align with the new scaled priorities.
                                                        // If parent_op_node is a call from a chain (e.g. C*10), this read is C*10-1.
                                                        // If parent_op_node is a simple require (EP 10), this read is 9.
                                                        // If parent_op_node is a simple emit (EP 10), this read is 9.
                                                        // If we can't find parent_mod, it's likely a standalone read or complex case.
                                                        // The existing logic for -1 (not in assign) vs 0 (in assign) is a fallback.
                                                        // Let's try to keep it simple: if we found a parent_op_node, it's related.
                                                        // The search for parent_mod is the key.
                                                        // If parent_mod is not found, it might be a read for an emit/require that isn't from a chain.
                                                        // In such cases, their EPs are 10. So read EP should be 9.
                                                        if parent_op_mod.is_none() {
                                                            if op_node.kind() == "call_expression"
                                                                || op_node.kind()
                                                                    == "emit_statement"
                                                            {
                                                                // Approx.
                                                                prio = 10 - 1; // Default "before simple call/emit"
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                            prio
                                        },
                                    });
                                }
                            }
                        } else {
                            eprintln!("[Storage DEBUG Deferred] Read candidate identifier '{}' could not be resolved to a storage variable.", var_name);
                        }
                    }
                    "read_subscript" => {
                        // New case for array_access
                        // --- Handle Storage Reads from Array Access ---
                        let array_access_node = node; // This is an array_access node
                        let array_access_span = span; // Span of the whole array_access node

                        // Helper to get the actual node if wrapped in 'expression'
                        fn get_actual_target_node(mut n: TsNode) -> TsNode {
                            while n.kind() == "expression" && n.named_child_count() == 1 {
                                if let Some(child) = n.named_child(0) {
                                    n = child;
                                } else {
                                    break;
                                }
                            }
                            n
                        }

                        // Helper to find the base identifier of a complex expression (identifier, member_expression, array_access)
                        fn find_base_identifier_recursive<'a>(
                            n: TsNode<'a>,
                            input_source: &str,
                        ) -> Option<TsNode<'a>> {
                            let actual_node = get_actual_target_node(n);
                            match actual_node.kind() {
                                "identifier" => Some(actual_node),
                                "member_expression" => {
                                    if let Some(object_expr) =
                                        actual_node.child_by_field_name("object")
                                    {
                                        find_base_identifier_recursive(object_expr, input_source)
                                    } else {
                                        eprintln!("[Storage ReadSub DEBUG] Member expression missing object: {}", get_node_text(&actual_node, input_source));
                                        None
                                    }
                                }
                                // array_access case here is for nested array accesses like arr[i][j] - base is arr[i]
                                // For a simple arr[i], the initial call to this function will have `actual_node` as `arr`.
                                "array_access" => {
                                    if let Some(base_expr) = actual_node.child_by_field_name("base")
                                    {
                                        find_base_identifier_recursive(base_expr, input_source)
                                    } else {
                                        eprintln!(
                                            "[Storage ReadSub DEBUG] Array access missing base: {}",
                                            get_node_text(&actual_node, input_source)
                                        );
                                        None
                                    }
                                }
                                _ => {
                                    eprintln!("[Storage ReadSub DEBUG] Could not find base identifier for node kind: {}, text: {}", actual_node.kind(), get_node_text(&actual_node, input_source));
                                    None
                                }
                            }
                        }

                        let mut base_identifier_for_read: Option<TsNode> = None;
                        let mut span_for_read_edge = array_access_span; // Default to whole array_access span

                        if let Some(base_expr_node) = array_access_node.child_by_field_name("base")
                        {
                            if let Some(ident_node) =
                                find_base_identifier_recursive(base_expr_node, &input.source)
                            {
                                base_identifier_for_read = Some(ident_node);
                                span_for_read_edge =
                                    (ident_node.start_byte(), ident_node.end_byte());
                            } else {
                                eprintln!("[Storage ReadSub DEBUG] Could not find base identifier from array_access base: {}", get_node_text(&base_expr_node, &input.source));
                            }
                        } else {
                            eprintln!("[Storage ReadSub DEBUG] Array access node missing 'base' child. Span: {:?}", array_access_span);
                        }

                        if let Some(actual_read_target_node) = base_identifier_for_read {
                            let var_name = get_node_text(&actual_read_target_node, &input.source);
                            eprintln!("[Storage ReadSub DEBUG] Base identifier for read: '{}', span for edge: {:?}", var_name, span_for_read_edge);

                            // --- Filtering Logic (similar to "read_identifier") ---
                            // `read_candidate_node` for filtering context is `array_access_node`
                            let read_candidate_node = array_access_node;
                            let mut is_read_for_call = false;
                            let mut is_read_for_emit_or_require = false;
                            let mut parent_op_span_start = array_access_span.0;
                            let mut current_node_for_filter = read_candidate_node;
                            let mut found_member_expr: Option<TsNode> = None;
                            let mut found_parent_op_node: Option<TsNode> = None;

                            // (Copy and adapt the filtering logic from "read_identifier" case,
                            // ensuring `read_candidate_node` is used for context checks,
                            // and `var_name` from `actual_read_target_node` is used for messages/resolution)

                            // Simplified version of filtering logic for brevity in this diff.
                            // The full filtering logic from "read_identifier" should be adapted here.
                            // Key check: is `array_access_node` LHS of an assignment?
                            let mut skip_read_edge = false;
                            let mut temp_ancestor = read_candidate_node;
                            loop {
                                if let Some(parent) = temp_ancestor.parent() {
                                    if parent.kind() == "assignment_expression" {
                                        if let Some(lhs_expr) = parent.child_by_field_name("left") {
                                            let actual_lhs = get_actual_target_node(lhs_expr);
                                            if actual_lhs.id() == read_candidate_node.id() {
                                                eprintln!("[Storage ReadSub DEBUG] Skipping read for LHS array_access: {}", var_name);
                                                skip_read_edge = true;
                                                break;
                                            }
                                        }
                                    }
                                    match parent.kind() {
                                        "function_definition"
                                        | "modifier_definition"
                                        | "contract_body"
                                        | "source_file"
                                        | "block" => break,
                                        _ => {}
                                    }
                                    temp_ancestor = parent;
                                } else {
                                    break;
                                }
                            }
                            // ... (The rest of the detailed filtering logic from "read_identifier" needs to be here,
                            // using `read_candidate_node` which is `array_access_node` for contextual decisions)
                            // For now, this is a placeholder for the more complex filtering.
                            // The existing filtering logic is quite extensive.

                            // --- (Assuming full filtering logic is applied and skip_read_edge is set accordingly) ---

                            if !skip_read_edge {
                                if let Some(var_node_id) = resolve_storage_variable(
                                    &caller_contract_name_opt,
                                    &var_name,
                                    graph,
                                    ctx,
                                ) {
                                    eprintln!("[Storage DEBUG Deferred] Collecting READ modification (from subscript): CallerID={}, VarID={}, VarName='{}', EdgeSpan={:?}", caller_node_id, var_node_id, var_name, span_for_read_edge);
                                    modifications.push(GraphModification {
                                        source_node_id: caller_node_id,
                                        target_node_id: var_node_id,
                                        edge_type: EdgeType::StorageRead,
                                        span: span_for_read_edge, // Use the span of the base identifier
                                        modifier: None,
                                        return_value: None,
                                        arguments: None,
                                        event_name: None,
                                        sort_span_start: enclosing_assignment_start
                                            .unwrap_or(parent_op_span_start), // Needs full filter logic for parent_op_span_start
                                        chain_index: None,
                                        execution_priority: 0, // Placeholder, needs full filter logic for priority
                                    });
                                } else {
                                    eprintln!("[Storage DEBUG Deferred] Read from subscript base '{}' could not be resolved to a storage variable.", var_name);
                                }
                            } else {
                                eprintln!("[Storage DEBUG Deferred] Skipping READ modification for subscript '{}' due to filtering. ArrayAccessSpan={:?}", var_name, array_access_span);
                            }
                        } else {
                            eprintln!("[Storage DEBUG Deferred] Could not extract base identifier for read from array_access at span {:?}", array_access_span);
                        }
                    }
                    "delete" => {
                        // --- Handle Delete Statements ---
                        let delete_expression_node = node; // Use the new node name
                        let delete_span = span;
                        eprintln!(
                            "[Storage DEBUG Deferred] Processing Delete Statement at span {:?}",
                            delete_span
                        );

                        // Find the expression being deleted (the 'argument' of the unary 'delete' expression)
                        let target_expr_node_opt =
                            delete_expression_node // Use the new node name
                                .child_by_field_name("argument"); // 'delete' is unary, target is 'argument'

                        if let Some(target_expr_node) = target_expr_node_opt {
                            // Find the base identifier within the target expression
                            let mut base_identifier_node: Option<TsNode> = None;
                            let mut queue = std::collections::VecDeque::new();
                            queue.push_back(target_expr_node);

                            while let Some(current) = queue.pop_front() {
                                if current.kind() == "identifier" {
                                    // Found an identifier, assume it's the base for now.
                                    // More robust logic might be needed for complex expressions like delete a.b.c[d]
                                    base_identifier_node = Some(current);
                                    break; // Take the first identifier found traversing down
                                }
                                // Only traverse children if not an identifier itself
                                let mut cursor = current.walk();
                                for child in current.children(&mut cursor) {
                                    queue.push_back(child);
                                }
                            }

                            if let Some(base_id_node) = base_identifier_node {
                                let var_name = get_node_text(&base_id_node, &input.source);
                                eprintln!(
                                    "[Storage DEBUG Deferred]   Delete target base identifier: '{}'",
                                    var_name
                                );

                                // Resolve the base identifier as a storage variable
                                if let Some(var_node_id) = resolve_storage_variable(
                                    &caller_contract_name_opt,
                                    &var_name,
                                    graph,
                                    ctx,
                                ) {
                                    eprintln!("[Storage DEBUG Deferred] Collecting DELETE (as WRITE) modification: CallerID={}, VarID={}, VarName='{}', DeleteSpan={:?}", caller_node_id, var_node_id, var_name, delete_span);
                                    modifications.push(GraphModification {
                                        source_node_id: caller_node_id,
                                        target_node_id: var_node_id,
                                        edge_type: EdgeType::StorageWrite, // Treat delete as a write
                                        span: delete_span, // Span of the whole delete statement
                                        modifier: None,
                                        return_value: None,
                                        arguments: None, // Delete has no arguments in this sense
                                        event_name: None,
                                        sort_span_start: delete_span.0, // Use delete statement start for sorting
                                        chain_index: None,
                                        execution_priority: 1000, // Priority 1000 for writes
                                    });
                                } else {
                                    eprintln!("[Storage DEBUG Deferred]   Delete target base '{}' did not resolve to a storage variable.", var_name);
                                }
                            } else {
                                eprintln!("[Storage DEBUG Deferred]   Could not find base identifier in delete target expression: {}", get_node_text(&target_expr_node, &input.source));
                            }
                        } else {
                            eprintln!("[Storage DEBUG Deferred]   Delete expression node missing 'argument' child. Span: {:?}", delete_span);
                        }
                    }
                    "require" => {
                        // --- Handle Require Calls ---
                        let require_node = node;
                        let require_span = span;
                        eprintln!(
                            "[Require DEBUG] Found require call at span {:?}. Node text: '{}'",
                            require_span,
                            get_node_text(&require_node, &input.source)
                        );
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
                        eprintln!("[Require DEBUG]   Collecting modification: Source={}, Target={}, Type=Require, Span={:?}, Args={:?}", caller_node_id, require_node_id, require_span, argument_texts);
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
                            chain_index: None,
                            execution_priority: 10, // Base priority 10 for require (1 * 10)
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
                                chain_index: None,
                                execution_priority: 10, // Base priority 10 for Emit Caller->EVM (1 * 10)
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
                                chain_index: None,
                                // --- UPDATED: Scaled priority for EVM->Listener ---
                                execution_priority: 20, // Base priority 20 for Emit EVM->Listener (2 * 10)
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
                "[CallsHandling DEBUG] Processing {} modifications for Caller Node ID: {}",
                modifications.len(),
                caller_node_id
            );

            eprintln!(
                "[Channel DEBUG] Modifications BEFORE sort for Caller {}:",
                caller_node_id
            );
            for (idx, m) in modifications.iter().enumerate() {
                eprintln!(
                     "[Channel DEBUG]   [{}] Source={}, Target={}, Type={:?}, Span=({},{}), SortKey=({}, {}, {}), ChainIdx={:?}, Args={:?}",
                     idx, m.source_node_id, m.target_node_id, m.edge_type, m.span.0, m.span.1,
                     m.sort_span_start, m.execution_priority, m.span.0, // Show full sort key
                     m.chain_index, m.arguments // Log chain index and args
                 );
            }
            // Sort first by the start span of the originating source element (grouping chains/related operations),
            // then by the actual start span of the specific modification (textual order tie-breaker),
            // finally by the index within that chain (ordering steps within the chain).
            // Sort first by the start span of the originating source element (grouping),
            // then by the calculated execution priority within that group,
            // finally by the actual start span of the specific modification (textual tie-breaker).
            modifications.sort_by_key(|m| (m.sort_span_start, m.execution_priority, m.span.0));

            eprintln!(
                "[Channel DEBUG] Modifications AFTER sort for Caller {}:",
                caller_node_id
            );
            for (idx, m) in modifications.iter().enumerate() {
                eprintln!(
                     "[Channel DEBUG]   [{}] Source={}, Target={}, Type={:?}, Span=({},{}), SortKey=({}, {}, {}), ChainIdx={:?}, Args={:?}",
                     idx, m.source_node_id, m.target_node_id, m.edge_type, m.span.0, m.span.1,
                     m.sort_span_start, m.execution_priority, m.span.0, // Show full sort key
                     m.chain_index, m.arguments // Log chain index and args
                 );
            }

            // --- Deduplicate modifications ---
            // We dedup based on all fields after sorting.
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

            eprintln!(
                "[Channel DEBUG] Modifications AFTER dedup for Caller {}:",
                caller_node_id
            );
            for (idx, m) in modifications.iter().enumerate() {
                eprintln!(
                     "[Channel DEBUG]   [{}] Source={}, Target={}, Type={:?}, Span=({},{}), SortKey=({}, {}, {}), ChainIdx={:?}, Args={:?}",
                     idx, m.source_node_id, m.target_node_id, m.edge_type, m.span.0, m.span.1,
                     m.sort_span_start, m.execution_priority, m.span.0, // Show full sort key
                     m.chain_index, m.arguments // Log chain index and args
                 );
            }

            let mut call_sequence_counter: usize = 0; // Reset sequence counter for each function body
            for modification in modifications {
                // Iterate over the deduplicated list
                call_sequence_counter += 1; // Increment sequence number for each edge added
                eprintln!("[Add Edge DEBUG] Assigning Seq: {} to Mod: Source={}, Target={}, Type={:?}, Span=({},{}), SortKey=({}, {}, {}), ChainIdx={:?}, Args={:?}",
                    call_sequence_counter, // Log the sequence number being assigned
                    modification.source_node_id,
                    modification.target_node_id,
                    modification.edge_type,
                    modification.span.0, modification.span.1, // Log span directly
                    modification.sort_span_start,
                    modification.execution_priority,
                    modification.span.0, // Log full sort key
                    modification.chain_index, // Log chain index
                    modification.arguments // Log arguments
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

/// Helper function to find the start byte of the enclosing assignment expression, if any.
fn find_enclosing_assignment_start(mut node: TsNode) -> Option<usize> {
    loop {
        if node.kind() == "assignment_expression" {
            return Some(node.start_byte());
        }
        // Stop searching upwards if we hit common function/block boundaries
        // or the root node, to avoid infinite loops or incorrect matches.
        match node.kind() {
            "function_definition"
            | "modifier_definition"
            | "constructor_definition"
            | "block"
            | "source_file" => return None,
            _ => {} // Continue searching upwards for other node kinds
        }
        if let Some(parent) = node.parent() {
            node = parent;
        } else {
            return None; // Reached root without finding assignment
        }
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
