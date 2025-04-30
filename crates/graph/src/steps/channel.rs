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
type StorageEdgeKey = (usize, usize, EdgeType, (usize, usize));

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

        let mut call_sequence_counter: usize = 0; // Initialize sequence counter GLOBALLY for this step run

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
            // Note: We use the original input.tree here, assuming it's still valid.
            // If input ownership was an issue, we'd need to adjust how input is passed/cloned.
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

            // Track processed call/emit spans within this caller function to avoid duplicates
            let mut processed_call_spans: HashSet<(usize, usize)> = HashSet::new();

            // Track processed storage read/write edges within this caller function to avoid duplicates
            let mut processed_storage_edges: HashSet<StorageEdgeKey> = HashSet::new();
            // Initialize sequence counter for THIS specific caller function
            let mut call_sequence_counter: usize = 0;
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
            let mut potential_nodes: Vec<(TsNode, (usize, usize), &str)> = Vec::new(); // Store node, span, and type (call/new/emit)

            let call_expr_capture_index = call_query
                .capture_index_for_name("call_expr_node")
                .unwrap_or(u32::MAX);
            let new_expr_capture_index = call_query
                .capture_index_for_name("new_expression_node")
                .unwrap_or(u32::MAX);
            let emit_capture_index = call_query
                .capture_index_for_name("emit_statement_node")
                .unwrap_or(u32::MAX);
            // Capture indices for storage access
            let assignment_capture_index = call_query
                .capture_index_for_name("assignment_expr")
                .unwrap_or(u32::MAX);
            let read_candidate_capture_index = call_query
                .capture_index_for_name("read_candidate")
                .unwrap_or(u32::MAX);
            let require_call_capture_index =
                call_query // Added
                    .capture_index_for_name("require_call_node")
                    .unwrap_or(u32::MAX);

            let mut call_matches =
                call_cursor.matches(&call_query, definition_ts_node, |node: TsNode| {
                    // Use retrieved node
                    iter::once(&source_bytes[node.byte_range()])
                });

            while let Some(match_) = call_matches.next() {
                for capture in match_.captures {
                    let capture_name = &call_query.capture_names()[capture.index as usize];
                    let node = capture.node;
                    let span = (node.start_byte(), node.end_byte());
                    match *capture_name {
                        // Dereference capture_name
                        "require_call_node" => potential_nodes.push((node, span, "require")),
                        "call_expr_node" => potential_nodes.push((node, span, "call")),
                        "new_expression_node" => potential_nodes.push((node, span, "new")),
                        "emit_statement_node" => potential_nodes.push((node, span, "emit")),
                        // --- Handle Assignment Expression for Storage Writes ---
                        "assignment_expr" => {
                            let assignment_node = node;
                            let assignment_span = span;
                            // eprintln!("[Storage DEBUG] Found Assignment Expr at span {:?}", assignment_span);

                            // Find the identifier being assigned TO (the LHS)
                            if let Some(lhs_node) = assignment_node.child_by_field_name("left") {
                                // Traverse the LHS to find the actual identifier node
                                // Handles simple identifiers, member access, etc.
                                // We want the *last* identifier in a chain (e.g., 'b' in a.b = ...)
                                let mut target_identifier_node: Option<TsNode> = None;
                                let mut queue = std::collections::VecDeque::new();
                                queue.push_back(lhs_node);

                                while let Some(current) = queue.pop_front() {
                                    if current.kind() == "identifier" {
                                        target_identifier_node = Some(current); // Keep updating to get the last one
                                    }
                                    let mut cursor = current.walk();
                                    // Only traverse deeper if not an identifier itself
                                    if current.kind() != "identifier" {
                                        for child in current.children(&mut cursor) {
                                            queue.push_back(child);
                                        }
                                    }
                                }

                                if let Some(write_target_node) = target_identifier_node {
                                    let var_name = get_node_text(&write_target_node, &input.source);
                                    let write_target_span = (
                                        write_target_node.start_byte(),
                                        write_target_node.end_byte(),
                                    );
                                    // eprintln!("[Storage DEBUG]   LHS Identifier found: '{}' at span {:?}", var_name, write_target_span);

                                    // Resolve the variable name to its definition node ID
                                    // TODO: Implement proper variable resolution (local, state, global, inherited)
                                    // Convert var_name (&str) to String for the lookup key
                                    let var_key =
                                        (caller_contract_name_opt.clone(), var_name.to_string());
                                    if let Some(var_node_id) =
                                        graph.node_lookup.get(&var_key).copied()
                                    {
                                        // Ensure the resolved node is actually a storage variable
                                        if graph.nodes.get(var_node_id).map_or(false, |n| {
                                            n.node_type == NodeType::StorageVariable
                                        }) {
                                            let edge_key: StorageEdgeKey = (
                                                caller_node_id,
                                                var_node_id,
                                                EdgeType::StorageWrite,
                                                assignment_span, // Use assignment span for uniqueness key
                                            );
                                            if processed_storage_edges.insert(edge_key) {
                                                eprintln!("[Storage DEBUG] Adding WRITE edge: CallerID={}, VarID={}, VarName='{}', AssignmentSpan={:?}", caller_node_id, var_node_id, var_name, assignment_span);
                                                graph.add_edge(
                                                    caller_node_id,
                                                    var_node_id,
                                                    EdgeType::StorageWrite,
                                                    assignment_span, // Span of the whole assignment for the edge
                                                    None,            // Modifier
                                                    0, // Sequence number - TODO: Integrate with call sequence?
                                                    None, // Return value
                                                    None, // Arguments
                                                    None, // Event name
                                                );
                                            }
                                        } else {
                                            // eprintln!("[Storage DEBUG]   LHS Identifier '{}' resolved to Node ID {} but it's not a StorageVariable.", var_name, var_node_id);
                                        }
                                    } else {
                                        // eprintln!("[Storage DEBUG]   LHS Identifier '{}' could not be resolved to a known node.", var_name);
                                    }
                                } else {
                                    // eprintln!("[Storage DEBUG]   Could not find target identifier within LHS node: {}", get_node_text(&lhs_node, &input.source));
                                }
                            } else {
                                // eprintln!("[Storage DEBUG]   Assignment expression missing LHS node.");
                            }
                        }
                        // --- Handle Read Candidate for Storage Reads ---
                        "read_candidate" => {
                            let read_candidate_node = node;
                            let read_span = span;
                            let var_name = get_node_text(&read_candidate_node, &input.source);
                            // eprintln!("[Storage DEBUG] Found Read Candidate: '{}' at span {:?}", var_name, read_span);

                            // Resolve the variable name to its definition node ID
                            // TODO: Implement proper variable resolution (local, state, global, inherited)
                            let var_key = (caller_contract_name_opt.clone(), var_name.to_string());
                            if let Some(var_node_id) = graph.node_lookup.get(&var_key).copied() {
                                // Ensure the resolved node is actually a storage variable
                                if graph
                                    .nodes
                                    .get(var_node_id)
                                    .map_or(false, |n| n.node_type == NodeType::StorageVariable)
                                {
                                    // --- Filter out identifiers used in non-read contexts ---
                                    let mut skip_read_edge = false;
                                    if let Some(parent) = read_candidate_node.parent() {
                                        let parent_kind = parent.kind();
                                        // Get the field name of the read_candidate_node within its parent
                                        let parent_field_name: Option<&str> = {
                                            let mut cursor = parent.walk();
                                            let mut field_name_result: Option<&str> = None;
                                            // Manually iterate children to avoid iterator borrow conflict
                                            if cursor.goto_first_child() {
                                                loop {
                                                    // Check if the current node is the one we're looking for
                                                    if cursor.node().id()
                                                        == read_candidate_node.id()
                                                    {
                                                        // Cursor is positioned at the child, get its field name
                                                        field_name_result = cursor.field_name();
                                                        break; // Found it
                                                    }
                                                    // Move to the next sibling
                                                    if !cursor.goto_next_sibling() {
                                                        break; // No more siblings
                                                    }
                                                }
                                                // Optional: Move cursor back to parent if needed later, though not necessary here
                                                // cursor.goto_parent();
                                            }
                                            field_name_result
                                        };

                                        // Check if identifier is the function name in a call
                                        if parent_kind == "expression" {
                                            // Simple call: foo()
                                            if let Some(grandparent) = parent.parent() {
                                                // Check if 'parent' is the node in the 'function' field of the 'grandparent' call_expression
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
                                                // member_expression -> expression
                                                if expr_grandparent.kind() == "expression" {
                                                    if let Some(call_great_grandparent) =
                                                        expr_grandparent.parent()
                                                    {
                                                        // expression -> call_expression
                                                        // Check if 'expr_grandparent' is the node in the 'function' field of the 'call_great_grandparent' call_expression
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
                                        // Check if any ancestor is assignment_expression and this node is within its 'left' child
                                        let mut current_ancestor = read_candidate_node;
                                        while let Some(ancestor) = current_ancestor.parent() {
                                            if ancestor.kind() == "assignment_expression" {
                                                if let Some(lhs_of_ancestor) =
                                                    ancestor.child_by_field_name("left")
                                                {
                                                    // Check if our read_candidate_node is a descendant of the LHS
                                                    let mut lhs_desc_cursor =
                                                        lhs_of_ancestor.walk();
                                                    let mut queue =
                                                        std::collections::VecDeque::new();
                                                    queue.push_back(lhs_of_ancestor);
                                                    let mut is_descendant = false;
                                                    while let Some(lhs_desc) = queue.pop_front() {
                                                        if lhs_desc.id() == read_candidate_node.id()
                                                        {
                                                            is_descendant = true;
                                                            break;
                                                        }
                                                        for child in
                                                            lhs_desc.children(&mut lhs_desc_cursor)
                                                        {
                                                            queue.push_back(child);
                                                        }
                                                    }
                                                    if is_descendant {
                                                        skip_read_edge = true;
                                                        break; // Found assignment ancestor where node is on LHS
                                                    }
                                                }
                                            }
                                            // Stop checking at function/modifier boundary
                                            if ancestor.kind() == "function_definition"
                                                || ancestor.kind() == "modifier_definition"
                                            {
                                                break;
                                            }
                                            current_ancestor = ancestor;
                                        }

                                        // Check if identifier is a type name
                                        if !skip_read_edge
                                            && (parent_kind == "type_name"
                                                || parent_kind == "user_defined_type")
                                        {
                                            skip_read_edge = true;
                                        }
                                        // Check if identifier is part of a declaration name
                                        if !skip_read_edge
                                            && (parent_kind.ends_with("_definition")
                                                || parent_kind.ends_with("_declaration"))
                                            && parent_field_name == Some("name")
                                        {
                                            skip_read_edge = true;
                                        }
                                        // Check if identifier is an event name in an emit statement
                                        if !skip_read_edge
                                            && parent_kind == "emit_statement"
                                            && parent_field_name == Some("name")
                                        {
                                            skip_read_edge = true;
                                        }
                                        // Check if identifier is a parameter name in a definition
                                        if !skip_read_edge
                                            && parent_kind == "parameter"
                                            && parent_field_name == Some("name")
                                        {
                                            skip_read_edge = true;
                                        }
                                        // Check if identifier is a variable declaration name
                                        if !skip_read_edge
                                            && parent_kind == "variable_declaration"
                                            && parent_field_name == Some("name")
                                        {
                                            skip_read_edge = true;
                                        }
                                    }

                                    if skip_read_edge {
                                        // eprintln!("[Storage DEBUG] Skipping READ edge for '{}' due to filtering.", var_name);
                                    } else {
                                        // --- Add Read Edge ---
                                        let edge_key: StorageEdgeKey = (
                                            caller_node_id,
                                            var_node_id,
                                            EdgeType::StorageRead,
                                            read_span,
                                        );
                                        if processed_storage_edges.insert(edge_key) {
                                            eprintln!("[Storage DEBUG] Adding READ edge: CallerID={}, VarID={}, VarName='{}', Span={:?}", caller_node_id, var_node_id, var_name, read_span);
                                            graph.add_edge(
                                                caller_node_id,
                                                var_node_id,
                                                EdgeType::StorageRead,
                                                read_span,
                                                None, // Modifier
                                                0, // Sequence number - TODO: Integrate with call sequence?
                                                None, // Return value
                                                None, // Arguments
                                                None, // Event name
                                            );
                                        }
                                    }
                                } else {
                                    // eprintln!("[Storage DEBUG] Read Candidate '{}' resolved to Node ID {} but it's not a StorageVariable.", var_name, var_node_id);
                                }
                            } else {
                                // eprintln!("[Storage DEBUG] Read Candidate '{}' could not be resolved to a known node.", var_name);
                            }
                        }
                        _ => {} // Ignore other captures like @call_name, @member_property etc.
                    }
                }
            }

            // Deduplicate potential nodes based on span (query might match same node multiple times with different patterns)
            // Sort nodes: First by span start, then by type priority ("require" > "emit" > "call" > "new")
            potential_nodes.sort_by(|a, b| {
                a.1.cmp(&b.1).then_with(|| {
                    // Define priority order for types
                    let priority = |t: &str| match t {
                        "require" => 0,
                        "emit" => 1,
                        "call" => 2,
                        "new" => 3,
                        _ => 4, // Other types (shouldn't happen here)
                    };
                    priority(a.2).cmp(&priority(b.2))
                })
            });

            potential_nodes.dedup_by_key(|k| k.1);

            // Separate nodes by type *after* deduplication
            let mut potential_call_or_new_nodes: Vec<(TsNode, (usize, usize))> = Vec::new();
            let mut potential_emit_nodes: Vec<(TsNode, (usize, usize))> = Vec::new();
            let mut potential_require_nodes: Vec<(TsNode, (usize, usize))> = Vec::new();

            for (node, span, type_) in potential_nodes.clone() {
                match type_ {
                    "call" | "new" => potential_call_or_new_nodes.push((node, span)),
                    "emit" => potential_emit_nodes.push((node, span)),
                    "require" => potential_require_nodes.push((node, span)),
                    _ => {} // Should not happen
                }
            }

            // Identify require node spans first to exclude them from generic call processing
            let require_node_spans: HashSet<(usize, usize)> = potential_nodes
                .iter()
                .filter(|(_, _, type_)| *type_ == "require")
                .map(|(_, span, _)| *span)
                .collect();

            // Separate into calls/news (excluding requires), emits, and requires
            let potential_call_or_new_nodes: Vec<(TsNode, (usize, usize))> = potential_nodes
                .iter()
                .filter(|(_, span, type_)| {
                    (*type_ == "call" || *type_ == "new") && !require_node_spans.contains(span)
                }) // Exclude require nodes
                .map(|(node, span, _)| (*node, *span)) // Clone node and span
                .collect();
            let potential_emit_nodes: Vec<(TsNode, (usize, usize))> = potential_nodes
                .iter()
                .filter(|(_, _, type_)| *type_ == "emit")
                .map(|(node, span, _)| (*node, *span)) // Clone node and span
                .collect();

            // Collect require nodes separately (they are always outermost in their own category)
            let potential_require_nodes: Vec<(TsNode, (usize, usize))> = potential_nodes
                .iter()
                .filter(|(_, _, type_)| *type_ == "require")
                .map(|(node, span, _)| (*node, *span))
                .collect();

            // --- Filter for outermost call/new nodes ---
            // A node is outermost if no other collected call/new node is its proper ancestor.
            let outermost_call_nodes: Vec<(TsNode, (usize, usize))> = potential_call_or_new_nodes
                .iter()
                .filter(|(node, span)| {
                    !potential_call_or_new_nodes
                        .iter()
                        .any(|(other_node, other_span)| {
                            // Check if other_node is a proper ancestor of node
                            other_span.0 <= span.0
                                && other_span.1 >= span.1
                                && other_span != span
                                && other_node.id() != node.id()
                        })
                })
                .cloned()
                .collect(); // Clone the TsNode and span

            // --- Process outermost calls/news ---
            for (call_node, span) in outermost_call_nodes {
                // This check might be redundant now but kept for safety
                if processed_call_spans.contains(&span) {
                    eprintln!("[CallsHandling DEBUG]       Skipping already processed outermost call/new span {:?}", span);
                    continue;
                }
                processed_call_spans.insert(span); // Mark as processed

                // Use analyze_chained_call for ALL call_expression and new_expression nodes found by the query.
                // analyze_chained_call is designed to handle both simple calls, member calls, and constructor calls (new).
                eprintln!("[CallsHandling DEBUG]       Processing Outermost Node via analyze_chained_call: Kind='{}', Span={:?}", call_node.kind(), span);

                match analyze_chained_call(
                    call_node, // Start analysis from the outermost node (call_expression or new_expression)
                    caller_node_id, // Remove dereference
                    &caller_contract_name_opt, // Pass as reference
                    ctx,
                    graph,
                    &input.source,        // Pass source string directly
                    &input.solidity_lang, // Pass language directly
                    &input,               // Pass the full input struct
                    None, // This is the top-level call within the current caller function
                ) {
                    Ok(steps) => {
                        eprintln!(
                            "[CallsHandling DEBUG]         analyze_chained_call returned {} steps.",
                            steps.len()
                        );
                        if steps.is_empty() {
                            eprintln!("[CallsHandling DEBUG]         analyze_chained_call returned 0 steps for node: {}", get_node_text(&call_node, &input.source));
                        }

                        // Iterate through each step and add an edge for the main call chain
                        for step in &steps {
                            // Borrow steps here to use it again later
                            eprintln!("[CallsHandling DEBUG]           Processing Step: Target={:?}, Args={:?}", step.target, step.arguments);
                            // Resolve the target of this step to a node ID
                            if let Some(target_node_id) =
                                resolve_target_to_node_id(&step.target, graph, ctx)
                            {
                                call_sequence_counter += 1; // Increment sequence for EACH valid step in the outer chain
                                eprintln!("[CallsHandling DEBUG]             >>> Adding edge (from chain step): CallerID={}, TargetID={}, Seq={}, StepSpan={:?}", caller_node_id, target_node_id, call_sequence_counter, step.call_expr_span);
                                graph.add_edge(
                                    caller_node_id, // Source is the original caller function (Remove dereference)
                                    target_node_id, // Target is the resolved node for this step
                                    EdgeType::Call,
                                    (step.call_expr_span.0.into(), step.call_expr_span.1.into()), // Use span from the step
                                    None,
                                    call_sequence_counter, // Use incremented sequence
                                    None,                  // Return value not tracked here
                                    Some(step.arguments.clone()), // Use arguments from the step (clone needed)
                                    None,                         // No event name
                                );
                            } else {
                                eprintln!("[CallsHandling DEBUG]             >>> Target for step {:?} did not resolve to a node ID. Skipping edge.", step.target);
                            }
                        }

                        // --- NEW: Analyze target function bodies for internal 'new' calls ---
                        for step in &steps {
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
                                            let key = (Some(c_name.clone()), f_name.clone());
                                            graph
                                                .node_lookup
                                                .get(&key)
                                                .copied()
                                                .map(|id| (c_name.clone(), f_name.clone(), id))
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

                                    if let Some(target_def_node) = target_definition_ts_node {
                                        eprintln!("[CallsHandling DEBUG]           Analyzing body of target function \'{}.{}\' (NodeID {}) for \'new\' calls.", target_contract_name, target_function_name, target_function_node_id);
                                        eprintln!("[CallsHandling DEBUG]             Target Def Node S-Expr: {}", target_def_node.to_sexp()); // DEBUG S-expression

                                        // Track processed new_expression nodes to avoid duplicates
                                        let mut processed_new_nodes: HashSet<usize> =
                                            HashSet::new();

                                        // Simplified query to find just the new_expression node.
                                        // We will extract the type name manually.
                                        let new_query_str = r#"
                                            (new_expression) @new_expr_node
                                        "#;

                                        let new_query =
                                            Query::new(&input.solidity_lang, new_query_str)
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

                                        let mut internal_call_seq = 0; // Local sequence for calls *within* this target function

                                        while let Some(new_match) = new_matches.next() {
                                            // Process captures - should only be @new_expr_node
                                            if let Some(capture) = new_match.captures.first() {
                                                let new_expr_node = capture.node;

                                                // Manually extract the contract name from the new_expression node
                                                let new_contract_name_opt = new_expr_node
                                                    .child_by_field_name("name") // Get the 'name' field (type_name)
                                                    .and_then(|type_name_node| {
                                                        // Look for identifier within type_name or its children (e.g., user_defined_type)
                                                        let mut name_cursor = type_name_node.walk();
                                                        let mut queue =
                                                            std::collections::VecDeque::new();
                                                        queue.push_back(type_name_node);
                                                        while let Some(current) = queue.pop_front()
                                                        {
                                                            if current.kind() == "identifier" {
                                                                return Some(
                                                                    get_node_text(
                                                                        &current,
                                                                        &input.source,
                                                                    )
                                                                    .to_string(),
                                                                );
                                                            }
                                                            for child in
                                                                current.children(&mut name_cursor)
                                                            {
                                                                queue.push_back(child);
                                                            }
                                                        }
                                                        None // Identifier not found
                                                    });

                                                if let Some(new_contract_name) =
                                                    new_contract_name_opt
                                                {
                                                    eprintln!("[CallsHandling DEBUG]             Found internal new_expression: '{}' (Node ID: {})", new_contract_name, new_expr_node.id()); // DEBUG: Confirm match found
                                                                                                                                                                                             // Skip if we've already processed this node
                                                    if processed_new_nodes
                                                        .contains(&new_expr_node.id())
                                                    {
                                                        continue;
                                                    }
                                                    processed_new_nodes.insert(new_expr_node.id());

                                                    // Find the constructor node ID for the contract being instantiated
                                                    let constructor_key = (
                                                        Some(new_contract_name.clone()),
                                                        new_contract_name.clone(),
                                                    ); // Constructor name is contract name
                                                    if let Some(constructor_node_id) = graph
                                                        .node_lookup
                                                        .get(&constructor_key)
                                                        .copied()
                                                    {
                                                        internal_call_seq += 1; // Increment internal sequence
                                                        let new_span = (
                                                            new_expr_node.start_byte(),
                                                            new_expr_node.end_byte(),
                                                        );

                                                        // Extract arguments - try different approaches based on context
                                                        let mut new_args = Vec::new();

                                                        // First check if the new_expression is within a call_expression
                                                        if let Some(parent_call) =
                                                            new_expr_node.parent().filter(|p| {
                                                                p.kind() == "call_expression"
                                                            })
                                                        {
                                                            // Extract arguments from the parent call_expression
                                                            new_args = extract_arguments(
                                                                parent_call,
                                                                &input,
                                                            );
                                                            eprintln!("[CallsHandling DEBUG]             Found parent call_expression for \'new {}\', extracted {} args", 
                                                                 new_contract_name, new_args.len());
                                                        } else {
                                                            // For standalone new_expression, look for arguments list directly within new_expression
                                                            // This handles cases like "return new Contract(...)"
                                                            let mut cursor = new_expr_node.walk();
                                                            for child in
                                                                new_expr_node.children(&mut cursor)
                                                            {
                                                                if child.kind() == "arguments" {
                                                                    // Found arguments node, extract its arguments
                                                                    // Pass the new_expr_node itself to extract_arguments
                                                                    new_args = extract_arguments(
                                                                        new_expr_node,
                                                                        &input,
                                                                    );
                                                                    eprintln!("[CallsHandling DEBUG]             Extracted {} args from \'new {}\' arguments node", 
                                                                         new_args.len(), new_contract_name);
                                                                    break;
                                                                }
                                                            }
                                                        }

                                                        eprintln!("[CallsHandling DEBUG]             >>> Adding edge (Internal \'new\'): SourceID={}, TargetID={}, InternalSeq={}, NewSpan={:?}, Args={:?}", 
                                                             target_function_node_id, constructor_node_id, internal_call_seq, new_span, new_args);

                                                        graph.add_edge(
                                                            target_function_node_id, // Source is the function containing \'new\'
                                                            constructor_node_id, // Target is the constructor being called
                                                            EdgeType::Call,
                                                            new_span,
                                                            None,
                                                            internal_call_seq, // Use internal sequence
                                                            None,
                                                            Some(new_args),
                                                            None,
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
                        /*

                        for step in steps { // Iterate again, consuming steps now
                            if let ResolvedTarget::Function { contract_name: Some(target_contract_name), function_name: target_function_name, .. } = &step.target {
                                let target_key = (Some(target_contract_name.clone()), target_function_name.clone());
                                if let Some(target_function_node_id) = graph.node_lookup.get(&target_key).copied() {
                                    // Find the definition TsNode for this target function
                                    if let Some((_, target_node_info, _)) = ctx.definition_nodes_info.iter().find(|(id, _, _)| *id == target_function_node_id) {
                                        let target_definition_ts_node = input.tree.root_node()
                                            .descendant_for_byte_range(target_node_info.span.0, target_node_info.span.1);

                                        if let Some(target_def_node) = target_definition_ts_node {
                                            eprintln!("[CallsHandling DEBUG]           Analyzing body of target function '{}' (NodeID {}) for 'new' calls.", target_function_name, target_function_node_id);
                                            // Query specifically for 'new' expressions within this function's body
                                            let new_query_str = r#"
                                                (new_expression
                                                    (type_name (identifier) @new_contract_name)
                                                ) @new_expr_node
                                            "#;
                                            let new_query = Query::new(&input.solidity_lang, new_query_str).context("Failed to create internal 'new' query")?;
                                            let mut new_cursor = QueryCursor::new();
                                            let mut new_matches = new_cursor.matches(&new_query, target_def_node, |node: TsNode| iter::once(&source_bytes[node.byte_range()]));

                                            let mut internal_call_seq = 0; // Local sequence for calls *within* this target function

                                            while let Some(new_match) = new_matches.next() {
                                                let mut new_contract_name_opt: Option<String> = None;
                                                let mut new_expr_node_opt: Option<TsNode> = None;

                                                for capture in new_match.captures {
                                                    let capture_name = &new_query.capture_names()[capture.index as usize];
                                                    match *capture_name { // Dereference
                                                        "new_contract_name" => new_contract_name_opt = Some(get_node_text(&capture.node, &input.source).to_string()),
                                                        "new_expr_node" => new_expr_node_opt = Some(capture.node),
                                                        _ => {}
                                                    }
                                                }

                                                if let (Some(new_contract_name), Some(new_expr_node)) = (new_contract_name_opt, new_expr_node_opt) {
                                                    // Find the constructor node ID for the contract being instantiated
                                                    let constructor_key = (Some(new_contract_name.clone()), new_contract_name.clone()); // Constructor name is contract name
                                                    if let Some(constructor_node_id) = graph.node_lookup.get(&constructor_key).copied() {
                                                        internal_call_seq += 1; // Increment internal sequence
                                                        let new_span = (new_expr_node.start_byte(), new_expr_node.end_byte());
                                                        let new_args = extract_arguments(new_expr_node.parent().unwrap_or(new_expr_node), &input); // Get args from parent call_expression if possible

                                                        eprintln!("[CallsHandling DEBUG]             >>> Adding edge (Internal 'new'): SourceID={}, TargetID={}, InternalSeq={}, NewSpan={:?}", target_function_node_id, constructor_node_id, internal_call_seq, new_span);
                                                        graph.add_edge(
                                                            target_function_node_id, // Source is the function containing 'new'
                                                            constructor_node_id,     // Target is the constructor being called
                                                            EdgeType::Call,
                                                            new_span,
                                                            None,
                                                            internal_call_seq, // Use internal sequence
                                                            None,
                                                            Some(new_args),
                                                            None,
                                                        );
                                                    } else {
                                                        eprintln!("[CallsHandling DEBUG]             >>> Constructor node not found for internal 'new {}'", new_contract_name);
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
                        // --- End NEW section ---
                        */
                    }
                    Err(e) => {
                        // Log the error from analyze_chained_call
                        eprintln!("[CallsHandling DEBUG]       Error during analyze_chained_call for span {:?}: {:?}", span, e);
                        // Optionally, decide if you want to add a placeholder edge or just skip
                    }
                }
            }

            // --- Process require calls ---
            for (require_node, span) in potential_require_nodes {
                if processed_call_spans.contains(&span) {
                    eprintln!(
                        "[CallsHandling DEBUG]       Skipping already processed require span {:?}",
                        span
                    );
                    continue;
                }
                processed_call_spans.insert(span); // Mark as processed

                eprintln!(
                    "[CallsHandling DEBUG]       Processing Require Statement at span {:?}",
                    span
                );
                call_sequence_counter += 1; // Increment sequence for the require call

                // Extract arguments (condition and optional message)
                let argument_texts = extract_arguments(require_node, &input);

                // --- Get or Create Require Node ---
                let require_key = (None, REQUIRE_NODE_NAME.to_string());
                let require_node_id = if let Some(id) = graph.node_lookup.get(&require_key) {
                    *id
                } else {
                    eprintln!("[CallsHandling DEBUG]           Creating Require node.");
                    let new_id = graph.add_node(
                        REQUIRE_NODE_NAME.to_string(),
                        NodeType::RequireCondition, // Use the new node type
                        None,
                        Visibility::Default,
                        (0, 0), // Synthetic node has no source span
                    );
                    graph.node_lookup.insert(require_key, new_id);
                    new_id
                };

                // --- Add Edge: Caller -> Require ---
                eprintln!("[CallsHandling DEBUG]         >>> Adding edge (Require Caller->RequireNode): CallerID={}, TargetID={}, Seq={}, Args={:?}", caller_node_id, require_node_id, call_sequence_counter, argument_texts);
                graph.add_edge(
                    caller_node_id,
                    require_node_id,
                    EdgeType::Call, // Treat require like a call for sequence/flow purposes
                    span,           // Use require_node span (the call_expression)
                    None,           // No return site span
                    call_sequence_counter,
                    None, // Require doesn't return a value in this context
                    Some(argument_texts),
                    None, // Not an event
                );
            }
            // --- Process emits ---
            // (Emit processing remains the same as before)
            for (emit_node, span) in potential_emit_nodes {
                if processed_call_spans.contains(&span) {
                    eprintln!(
                        "[CallsHandling DEBUG]       Skipping already processed emit span {:?}",
                        span
                    );
                    continue;
                }
                processed_call_spans.insert(span); // Mark as processed

                eprintln!(
                    "[CallsHandling DEBUG]       Processing Emit Statement at span {:?}",
                    span
                );

                call_sequence_counter += 1; // Increment sequence for the emit event

                // Extract event name using the query capture index on the emit_node
                // Need to run a mini-query or manually traverse children if not captured initially
                // Let's assume manual traversal for simplicity here:
                let event_name_opt = emit_node
                    .child_by_field_name("name")
                    .and_then(|name_node| name_node.child(0)) // name -> expression -> identifier
                    .filter(|id_node| id_node.kind() == "identifier")
                    .map(|id_node| get_node_text(&id_node, &input.source).to_string());

                // Extract arguments (using the same helper as call_expression)
                let argument_texts = extract_arguments(emit_node, &input); // Pass the emit_node itself

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
                        graph.node_lookup.insert(evm_key, new_id);
                        new_id
                    };

                    // --- Get or Create Event Listener Node ---
                    let listener_key = (None, EVENT_LISTENER_NODE_NAME.to_string());
                    let listener_node_id = if let Some(id) = graph.node_lookup.get(&listener_key) {
                        *id
                    } else {
                        eprintln!("[CallsHandling DEBUG]           Creating EventListener node.");
                        let new_id = graph.add_node(
                            EVENT_LISTENER_NODE_NAME.to_string(),
                            NodeType::EventListener,
                            None,
                            Visibility::Default,
                            (0, 0),
                        );
                        graph.node_lookup.insert(listener_key, new_id);
                        new_id
                    };

                    // --- Add Edge 1: Caller -> EVM ---
                    eprintln!("[CallsHandling DEBUG]         >>> Adding edge (Emit Caller->EVM): CallerID={}, TargetID={}, Seq={}, Event='{}'", caller_node_id, evm_node_id, call_sequence_counter, event_name);
                    graph.add_edge(
                        caller_node_id, // Remove dereference
                        evm_node_id,
                        EdgeType::Call,
                        span, // Use emit_node span
                        None,
                        call_sequence_counter,
                        None,
                        Some(argument_texts.clone()),
                        Some(event_name.clone()),
                    );

                    // --- Add Edge 2: EVM -> Event Listener ---
                    eprintln!("[CallsHandling DEBUG]         >>> Adding edge (Emit EVM->Listener): CallerID={}, TargetID={}, Seq={}, Event='{}'", evm_node_id, listener_node_id, call_sequence_counter, event_name);
                    graph.add_edge(
                        evm_node_id,
                        listener_node_id,
                        EdgeType::Call,
                        span, // Use emit_node span
                        None,
                        call_sequence_counter, // Use SAME sequence number
                        None,
                        Some(argument_texts),
                        Some(event_name),
                    );
                } else {
                    eprintln!("[CallsHandling DEBUG]       >>> Emit statement missing event name. Span: {:?}", span);
                }
            }
        }

        Ok(())
    }
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
            eprintln!("[Resolve Target ID] Info: Skipping edge creation for built-in {}.{}.", object_type, name);
            None
        }
        crate::chains::ResolvedTarget::NotCallable { reason } => {
            eprintln!("[Resolve Target ID] Info: Skipping edge creation for non-callable target: {}", reason);
            None
        }
        crate::chains::ResolvedTarget::External { address_expr } => {
            // External calls don't resolve to a specific node in our graph
             eprintln!("[Resolve Target ID] Info: Skipping edge creation for external call to address expr: {}", address_expr);
            None
        }
        crate::chains::ResolvedTarget::TypeCast { type_name } => {
            // Type casts don't resolve to a callable node for edge creation.
            eprintln!("[Resolve Target ID] Info: Skipping edge creation for type cast to '{}'.", type_name);
            None
        }
    }
}
