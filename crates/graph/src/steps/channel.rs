use std::{
    collections::{HashMap, HashSet},
    iter,
};

use crate::{
    cg::{
        extract_arguments, resolve_call_target, CallGraph, CallGraphGeneratorContext,
        CallGraphGeneratorInput, CallGraphGeneratorStep, EdgeType, NodeInfo, NodeType, Visibility,
        EVENT_LISTENER_NODE_NAME, EVM_NODE_NAME,
    },
    chains::{analyze_chained_call, ResolvedTarget, TypeError}, // Import items from chains module
    parser::get_node_text,
};
use anyhow::{anyhow, Context, Result}; // Add anyhow!
use streaming_iterator::StreamingIterator;
use tree_sitter::{Node as TsNode, Query, QueryCursor};

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

        let mut call_sequence_counter: usize = 0; // Initialize sequence counter GLOBALLY for this step run

        // Iterate through definition_nodes_info which now contains NodeInfo
        for (caller_node_id, caller_node_info, caller_contract_name_opt) in
            &ctx.definition_nodes_info
        {
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
                        "call_expr_node" => potential_nodes.push((node, span, "call")),
                        "new_expression_node" => potential_nodes.push((node, span, "new")),
                        "emit_statement_node" => potential_nodes.push((node, span, "emit")),
                        _ => {} // Ignore other captures like @call_name, @member_property etc.
                    }
                }
            }

            // Deduplicate potential nodes based on span (query might match same node multiple times with different patterns)
            potential_nodes.sort_by_key(|k| k.1);
            potential_nodes.dedup_by_key(|k| k.1);

            // Separate into calls/news and emits
            let potential_call_or_new_nodes: Vec<(TsNode, (usize, usize))> = potential_nodes
                .iter()
                .filter(|(_, _, type_)| *type_ == "call" || *type_ == "new")
                .map(|(node, span, _)| (*node, *span)) // Clone node and span
                .collect();
            let potential_emit_nodes: Vec<(TsNode, (usize, usize))> = potential_nodes
                .iter()
                .filter(|(_, _, type_)| *type_ == "emit")
                .map(|(node, span, _)| (*node, *span)) // Clone node and span
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
                    *caller_node_id,
                    caller_contract_name_opt,
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
                                    *caller_node_id, // Source is the original caller function
                                    target_node_id,  // Target is the resolved node for this step
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
                        *caller_node_id,
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
