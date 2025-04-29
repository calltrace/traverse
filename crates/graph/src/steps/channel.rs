use std::{collections::{HashMap, HashSet}, iter};

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

            // --- Collect potential call and emit nodes first ---
            let mut potential_call_nodes: Vec<(TsNode, (usize, usize))> = Vec::new();
            let mut potential_emit_nodes: Vec<(TsNode, (usize, usize))> = Vec::new();

            let call_expr_capture_index = call_query.capture_index_for_name("call_expr_node").unwrap_or(u32::MAX);
            let emit_capture_index = call_query.capture_index_for_name("emit_statement_node").unwrap_or(u32::MAX);
            let new_expr_capture_index = call_query.capture_index_for_name("new_expression_node").unwrap_or(u32::MAX); // Capture index for new_expression

            let mut call_matches =
                call_cursor.matches(&call_query, definition_ts_node, |node: TsNode| { // Use retrieved node
                    iter::once(&source_bytes[node.byte_range()])
                });

            while let Some(call_match) = call_matches.next() {
                 // Prioritize call_expression if both match (e.g., new inside call)
                 if let Some(node) = call_match.nodes_for_capture_index(call_expr_capture_index).next() {
                    potential_call_nodes.push((node, (node.start_byte(), node.end_byte())));
                 } else if let Some(node) = call_match.nodes_for_capture_index(new_expr_capture_index).next() {
                    // If it's *just* a new_expression (e.g., `Target t = new Target();`), treat it like a call for edge generation
                    potential_call_nodes.push((node, (node.start_byte(), node.end_byte())));
                 }

                 if let Some(node) = call_match.nodes_for_capture_index(emit_capture_index).next() {
                    potential_emit_nodes.push((node, (node.start_byte(), node.end_byte())));
                 }
            }

            // Deduplicate potential nodes based on span (query might match same node multiple times with different patterns)
            potential_call_nodes.sort_by_key(|k| k.1);
            potential_call_nodes.dedup_by_key(|k| k.1);
            potential_emit_nodes.sort_by_key(|k| k.1);
            potential_emit_nodes.dedup_by_key(|k| k.1);

            // --- Filter for outermost call nodes ---
            let outermost_call_nodes: Vec<(TsNode, (usize, usize))> = potential_call_nodes.iter().filter(|(node, span)| {
                !potential_call_nodes.iter().any(|(_other_node, other_span)| {
                    // Check if other_node is a proper ancestor of node
                    other_span.0 <= span.0 && other_span.1 >= span.1 && other_span != span
                })
            }).cloned().collect(); // Clone the TsNode and span

            // --- Process outermost calls ---
            for (call_node, span) in outermost_call_nodes {
                // This check might be redundant now but kept for safety
                if processed_call_spans.contains(&span) {
                    eprintln!("[CallsHandling DEBUG]       Skipping already processed outermost call span {:?}", span);
                    continue;
                }
                processed_call_spans.insert(span); // Mark as processed

                // Determine if it's a `new` expression or a regular call/member call
                if call_node.kind() == "new_expression" {
                     // --- Handle Standalone Constructor Call (e.g., Target t = new Target();) ---
                     eprintln!("[CallsHandling DEBUG]       Processing Standalone New Expression at span {:?}", span);
                     let new_contract_name_opt = call_node // `call_node` is the new_expression node here
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
                            eprintln!("[CallsHandling DEBUG]       >>> Adding edge (standalone new): CallerID={}, TargetID={}, Seq={}", caller_node_id, target_node_id, call_sequence_counter);
                            // Extract arguments for constructor call (might be siblings in parent assignment/variable declaration)
                            // For simplicity, we might not capture args perfectly here, focus is on the call edge.
                            // Let's use extract_arguments on the new_expression node itself, though it might not find them directly.
                            let constructor_args = extract_arguments(call_node, &input);
                            graph.add_edge(
                                *caller_node_id,
                                *target_node_id,
                                EdgeType::Call,
                                span, // Use the span of the new_expression
                                None,
                                call_sequence_counter,
                                None,
                                Some(constructor_args), // Pass potentially empty args
                                None, // No event name
                            );
                        } else {
                            eprintln!("[CallsHandling DEBUG]       >>> Constructor call unresolved: new {}. Span: {:?}", new_contract_name, span);
                        }
                    }

                } else if call_node.kind() == "call_expression" {
                    // --- Handle Regular Function/Member Call using analyze_chained_call ---
                    eprintln!("[CallsHandling DEBUG]       Processing Outermost Call Expression via analyze_chained_call at span {:?}", span);

                    // Call analyze_chained_call to get all steps
                    match analyze_chained_call(
                        call_node, // Start analysis from the outermost call expression
                        *caller_node_id,
                        caller_contract_name_opt,
                        ctx,
                        graph,
                        &input.source, // Pass source string directly
                        &input.solidity_lang, // Pass language directly
                        &input, // Pass the full input struct
                    ) {
                        Ok(steps) => {
                            eprintln!("[CallsHandling DEBUG]         analyze_chained_call returned {} steps.", steps.len());
                            if steps.is_empty() {
                                 eprintln!("[CallsHandling DEBUG]         analyze_chained_call returned 0 steps for node: {}", get_node_text(&call_node, &input.source));
                            }
                            // Iterate through each step and add an edge
                            for step in steps {
                                eprintln!("[CallsHandling DEBUG]           Processing Step: Target={:?}, Args={:?}", step.target, step.arguments);
                                // Resolve the target of this step to a node ID
                                if let Some(target_node_id) =
                                    resolve_target_to_node_id(&step.target, graph, ctx)
                                {
                                    call_sequence_counter += 1; // Increment sequence for EACH valid step
                                    eprintln!("[CallsHandling DEBUG]             >>> Adding edge (from chain step): CallerID={}, TargetID={}, Seq={}, StepSpan={:?}", caller_node_id, target_node_id, call_sequence_counter, step.call_expr_span);
                                    graph.add_edge(
                                        *caller_node_id, // Source is the original caller function
                                        target_node_id,  // Target is the resolved node for this step
                                        EdgeType::Call,
                                        (step.call_expr_span.0.into(), step.call_expr_span.1.into()), // Use span from the step
                                        None,
                                        call_sequence_counter, // Use incremented sequence
                                        None, // Return value not tracked here
                                        Some(step.arguments), // Use arguments from the step
                                        None, // No event name
                                    );
                                } else {
                                    eprintln!("[CallsHandling DEBUG]             >>> Target for step {:?} did not resolve to a node ID. Skipping edge.", step.target);
                                }
                            }
                        }
                        Err(e) => {
                            // Log the error from analyze_chained_call
                            eprintln!("[CallsHandling DEBUG]       Error during analyze_chained_call for span {:?}: {:?}", span, e);
                            // Optionally, decide if you want to add a placeholder edge or just skip
                        }
                    }
                } else {
                     eprintln!("[CallsHandling DEBUG]       Skipping unexpected outermost node kind: '{}' at span {:?}", call_node.kind(), span);
                }
            }

            // --- Process emits ---
            for (emit_node, span) in potential_emit_nodes {
                 if processed_call_spans.contains(&span) {
                    eprintln!("[CallsHandling DEBUG]       Skipping already processed emit span {:?}", span);
                    continue;
                }
                processed_call_spans.insert(span); // Mark as processed

                eprintln!("[CallsHandling DEBUG]       Processing Emit Statement at span {:?}", span);

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
                    eprintln!("[CallsHandling DEBUG]         Event Name: '{}', Args: {:?}", event_name, argument_texts);

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
            // Cannot link directly to an abstract interface method without implementation
            eprintln!("[Resolve Target ID] Warning: Cannot create edge to abstract interface method {}.{}. No implementation resolved.", interface_name, method_name);
            None
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
    }
}


