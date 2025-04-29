use std::{collections::{HashMap, HashSet}, iter};

use crate::{cg::{extract_arguments, resolve_call_target, CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorStep, EdgeType, NodeInfo, NodeType, Visibility, EVENT_LISTENER_NODE_NAME, EVM_NODE_NAME}, parser::get_node_text};
use anyhow::{anyhow, Context, Result}; // Add anyhow!
use tree_sitter::{Node as TsNode, Query, QueryCursor};
use streaming_iterator::StreamingIterator;

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


