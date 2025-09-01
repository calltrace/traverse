//! Module for resolving types and analyzing chained calls in Solidity expressions.

use crate::builtin;
use crate::cg::{
    CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, NodeInfo, NodeType,
};
use crate::parser::get_node_text;
use std::collections::VecDeque;
use streaming_iterator::StreamingIterator; // Not directly used, but QueryCursor uses it. Keep for now if indirect.
use tree_sitter::{Node as TsNode, Query, QueryCursor};

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TypeError {
    #[error("Query error: {0}")]
    QueryError(String),

    #[error("Missing child: {0}")]
    MissingChild(String),

    #[error("Failed to resolve type for expression '{expr}': {reason}")]
    TypeResolutionFailed { expr: String, reason: String },

    #[error("Failed to resolve target '{name}': {reason}")]
    TargetResolutionFailed { name: String, reason: String },

    #[error("Unsupported node kind: {0}")]
    UnsupportedNodeKind(String),

    #[error("Ambiguous implementation for interface {interface_name}.{method_name}, found implementations: {implementations:?}")]
    AmbiguousInterfaceImplementation {
        interface_name: String,
        method_name: String,
        implementations: Vec<String>,
    },

    #[error("Internal error: {0}")]
    Internal(String),
}

impl From<tree_sitter::QueryError> for TypeError {
    fn from(error: tree_sitter::QueryError) -> Self {
        // Convert the tree-sitter error to a string representation
        TypeError::QueryError(error.to_string())
    }
}

// --- Data Structures ---

/// Represents the resolved target of a specific call step.
/// Stores names, allowing cg.rs to perform the final node ID lookup.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ResolvedTarget {
    /// Resolved to a specific function/modifier/constructor.
    Function {
        contract_name: Option<String>, // None for free functions
        function_name: String,
        node_type: NodeType, // Function, Constructor, or Modifier
    },
    /// Resolved to an interface method. Implementation might be unique, ambiguous, or external.
    InterfaceMethod {
        interface_name: String,
        method_name: String,
        /// If a single, concrete implementation was found, provide its details.
        implementation: Option<Box<ResolvedTarget>>, // Box to avoid recursive type size issue
    },
    /// Represents a built-in function or property (e.g., .push, .balance).
    BuiltIn { object_type: String, name: String },
    /// Resolution failed or target is not callable (e.g., state variable access).
    NotCallable { reason: String },
    /// Target is external/unknown (e.g., call to an address).
    External { address_expr: String },
    /// Represents a type cast expression like `TypeName(...)`.
    TypeCast { type_name: String },
}

/// Represents a single step in a potentially chained call sequence.
// Added PartialEq, Eq, PartialOrd, Ord for sorting and deduplication
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ResolvedCallStep {
    /// The byte offsets representing the span of this specific call expression (e.g., `.method(...)`).
    pub call_expr_span: (usize, usize),
    /// The byte offsets representing the span of the function/member being called (e.g., `method` in `obj.method()`).
    pub function_span: (usize, usize),
    /// The resolved target for this call step.
    pub target: ResolvedTarget,
    /// Text representation of the arguments passed in this call step.
    pub arguments: Vec<String>,
    /// The Solidity type name resolved for the *result* of this call step.
    /// This becomes the object type for the *next* step in the chain.
    /// None if the call returns void or resolution failed.
    pub result_type: Option<String>,
    /// The Solidity type name resolved for the *object* being called upon in this step.
    /// None for simple function calls or `new` expressions.
    pub object_type: Option<String>,
    /// The source text of the object instance being called upon.
    /// e.g., "myVar" in "myVar.method()", or "getStruct().field" in "getStruct().field.method()".
    /// None for simple function calls or `new` expressions.
    pub object_instance_text: Option<String>,
    /// The start byte of the original expression that initiated this chain analysis.
    /// Used for sorting steps correctly.
    pub originating_span_start: usize,
    /// If the target is BuiltIn (like push/pop), this holds the name of the base variable identifier
    /// being acted upon (e.g., "allPairs" in "allPairs.push()").
    pub base_object_identifier_for_builtin: Option<String>,
}

// --- Main Analysis Function ---

/// Analyzes a potentially chained call expression node to resolve types and targets.
///
/// This function attempts to decompose expressions like `a.b(c).d(e)` into a sequence of resolved steps.
///
/// Args:
/// * `start_node`: The tree-sitter node representing the beginning of the expression
///   (e.g., an identifier, a type cast, a `new` expression, or the full chained call).
/// * `caller_node_id`: The ID of the node (in the eventual graph) containing this call.
/// * `caller_contract_name_opt`: The name of the contract containing the caller, if any.
/// * `ctx`: The context containing information about contracts, interfaces, state vars, etc.
/// * `graph`: The current call graph (needed for node lookups during resolution).
/// * `source`: The original source code string.
/// * `solidity_lang`: The tree-sitter language definition.
///
/// Returns:
/// A vector of `ResolvedCallStep` structs, one for each call in the chain,
/// or a `TypeError` if analysis fails at any point.
pub(crate) fn analyze_chained_call<'a>(
    start_node: TsNode<'a>,
    caller_node_id: usize,
    caller_contract_name_opt: &'a Option<String>,
    ctx: &CallGraphGeneratorContext,
    graph: &'a CallGraph, // Pass graph for lookups
    source: &'a str,
    solidity_lang: &'a tree_sitter::Language,
    input: &'a CallGraphGeneratorInput,
    original_start_node_for_err_reporting: Option<TsNode<'a>>, // Keep for error reporting if needed
    originating_span_start: usize,                             // Pass the originating span start
) -> std::result::Result<Vec<ResolvedCallStep>, TypeError> {
    // If original_start_node is None, this is the top-level call, so use start_node
    let _original_start_node_for_err_reporting =
        original_start_node_for_err_reporting.unwrap_or(start_node); // Keep if needed for errors
                                                                     // Use full path to Result
    let mut steps = Vec::new();

    eprintln!(
        "[Analyze Chained] Starting analysis for node kind: '{}', text: '{}'",
        start_node.kind(),
        get_node_text(&start_node, source).trim()
    );

    match start_node.kind() {
        // --- Base Cases (Start of a chain or simple expression) ---
        "identifier" | "primitive_type" | "string_literal" | "number_literal"
        | "boolean_literal" | "hex_literal" | "address_literal" => {
            // This is not a call itself, but the start of a potential chain. Resolve its type.
            // Resolve type, but no steps generated from here directly.
            // The caller using this identifier/literal will handle type resolution.
            let _resolved_type = resolve_expression_type_v2(
                start_node,
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
            )?;
            eprintln!(
                "[Analyze Chained] Base case node '{}' processed. Type resolved: {:?}",
                start_node.kind(),
                _resolved_type
            );
            // If it's just an identifier/literal, there are no call steps *from* it directly.
            // The chain must continue via member access or call expression wrapping it.
        }

        "new_expression" => {
            // Handle `new Contract(...)` as the start of a potential chain
            let type_name_node = start_node.named_child(0).ok_or_else(|| {
                TypeError::MissingChild(format!(
                    "new_expression missing type_name (named_child(0) failed for node: {})",
                    get_node_text(&start_node, source)
                ))
            })?;
            let contract_name = get_node_text(&type_name_node, source).to_string(); // Simplified

            // The "call" is to the constructor
            let target = ResolvedTarget::Function {
                contract_name: Some(contract_name.clone()),
                function_name: contract_name.clone(), // Constructor name is contract name
                node_type: NodeType::Constructor,
            };

            // Arguments for `new` are usually in the parent `call_expression`.
            // We need the parent node to extract them correctly.
            // For now, assume the caller (likely the `call_expression` handler below)
            // provides the arguments if `new` is wrapped. If `new` is standalone, args are empty.
            // Let's refine argument extraction later if needed.
            let arguments = if let Some(parent) = start_node.parent() {
                if parent.kind() == "call_expression" {
                    extract_arguments_v2(parent, source)
                } else {
                    vec![] // Standalone new expression
                }
            } else {
                vec![]
            };

            let step = ResolvedCallStep {
                call_expr_span: (start_node.start_byte(), start_node.end_byte()), // Use start/end_byte
                function_span: (
                    type_name_node.start_byte(), // Use start/end_byte
                    type_name_node.end_byte(),   // Use start/end_byte
                ), // Span of the type name
                target,
                arguments,
                result_type: Some(contract_name.clone()), // Result type is the contract itself
                object_type: None,                        // No object for `new`
                object_instance_text: None,               // No object instance for `new`
                originating_span_start,                   // Populate the new field
                base_object_identifier_for_builtin: None, // Added: No base object for 'new'
            };
            steps.push(step);
            eprintln!(
                "[Analyze Chained] 'new' expression step added. Result type: {:?}",
                steps.last().and_then(|s| s.result_type.as_ref())
            );
        }

        // --- Recursive / Chaining Cases ---
        "call_expression" => {
            let function_node = start_node // Use start_node
                .child_by_field_name("function")
                .ok_or_else(|| {
                    TypeError::MissingChild("call_expression missing function".to_string())
                })?;
            // Get argument expression nodes
            let argument_nodes = find_argument_expression_nodes(start_node); // Use start_node

            eprintln!(
                        "[Analyze Chained] Handling call_expression. Function node kind: '{}', text: '{}', Num args: {}",
                        function_node.kind(),
                        get_node_text(&function_node, source).trim(),
                        argument_nodes.len()
                    );

            // --- 1. Recursively analyze arguments FIRST ---
            let mut argument_steps = Vec::new();
            let mut argument_texts = Vec::with_capacity(argument_nodes.len());
            for arg_node in argument_nodes {
                eprintln!(
                    "[Analyze Chained]   Analyzing argument node kind: '{}', text: '{}'",
                    arg_node.kind(),
                    get_node_text(&arg_node, source).trim()
                );
                // Recursively call analyze_chained_call for each argument
                let inner_steps = analyze_chained_call(
                    arg_node,
                    caller_node_id,
                    caller_contract_name_opt,
                    ctx,
                    graph,
                    source,
                    solidity_lang,
                    input,
                    Some(_original_start_node_for_err_reporting),
                    originating_span_start, // Pass down the same originating span start
                )?;

                argument_steps.extend(inner_steps);
                // Store the original text of the argument for the outer call step
                argument_texts.push(get_node_text(&arg_node, source).to_string());
            }
            eprintln!(
                "[Analyze Chained]   Finished analyzing arguments. Collected {} inner steps.",
                argument_texts.len()
            );

            // --- 2. Process the outer call itself ---
            // let mut object_steps = Vec::new(); // Initialized later if needed
            // let mut outer_object_type: Option<String> = None; // Initialized later if needed

            match function_node.kind() {
                // Case A: Simple call `foo()` or type cast `Type(arg)`
                "expression"
                    if function_node
                        .child(0)
                        .map_or(false, |n| n.kind() == "identifier") =>
                {
                    let id_node = function_node.child(0).unwrap();
                    let name = get_node_text(&id_node, source).to_string();

                    // Check for Type Cast `TypeName(...)` - these don't generate a call step themselves
                    if name.chars().next().map_or(false, |c| c.is_uppercase())
                        && (ctx.all_contracts.contains_key(&name)
                            || ctx.all_interfaces.contains_key(&name)
                            || ctx.all_libraries.contains_key(&name))
                    // Check libraries too
                    {
                        eprintln!(
                                    "[Analyze Chained]   Outer call is Type Cast/Constructor '{}'. No outer step generated.", name
                                );
                        // The type resolution happens via the recursive call on the argument(s).
                        // The result type for the *overall* expression is the type name itself.
                        // However, analyze_chained_call returns steps, not just the final type.
                        // If this cast is the *outermost* node analyzed, we might need
                        // a way to signal the final type without a step.
                        // For now, if arguments generated steps, those are returned.
                        // If no arguments or simple args, steps might be empty.
                        // This case needs careful handling depending on how the result is used.
                        // Let's assume for now that if it's just a cast, the caller handles it.
                        // If it's `new Type()`, it's handled by the `new_expression` case.
                        // If it's `Type(arg).method()`, the `member_expression` handler deals with it.
                        // So, if we reach here, it's likely just `Type(arg)` used as a value.
                        // We return the steps generated by analyzing `arg`.
                    } else {
                        // Simple function call `foo()`
                        eprintln!(
                            "[Analyze Chained]   Outer call is Simple Function Call '{}'",
                            name
                        );
                        let target =
                            resolve_simple_call_v2(&name, caller_contract_name_opt, graph, ctx)?;
                        let result_type = resolve_call_return_type(
                            &target,
                            ctx,
                            graph,
                            source,
                            solidity_lang,
                            input,
                        )?;

                        let outer_step = ResolvedCallStep {
                            call_expr_span: (start_node.start_byte(), start_node.end_byte()),
                            function_span: (id_node.start_byte(), id_node.end_byte()),
                            target,
                            arguments: argument_texts, // Use collected texts
                            result_type: result_type.clone(),
                            object_type: None, // No object for simple calls
                            object_instance_text: None, // No object instance for simple calls
                            originating_span_start, // Populate the new field
                            base_object_identifier_for_builtin: None, // Added: No base object for simple calls
                        };
                        // Combine steps: argument_steps first, then the outer_step
                        steps = argument_steps; // Start with argument steps
                        steps.push(outer_step); // Add the simple call step
                        eprintln!(
                            "[Analyze Chained]   Simple call step added. Result type: {:?}. Total steps for this branch: {}",
                            result_type, steps.len()
                        );
                    }
                }

                // Case B: Member call `obj.method()` or `expr.method()`
                "expression"
                    if function_node
                        .child(0)
                        .map_or(false, |n| n.kind() == "member_expression") =>
                {
                    let member_expr_node = function_node.child(0).unwrap();
                    let object_node =
                        member_expr_node
                            .child_by_field_name("object")
                            .ok_or_else(|| {
                                TypeError::MissingChild(
                                    "member_expression missing object".to_string(),
                                )
                            })?;
                    let property_node = member_expr_node
                        .child_by_field_name("property")
                        .ok_or_else(|| {
                            TypeError::MissingChild(
                                "member_expression missing property".to_string(),
                            )
                        })?;

                    if property_node.kind() != "identifier" {
                        return Err(TypeError::UnsupportedNodeKind(format!(
                            "Member expression property kind '{}' not supported",
                            property_node.kind()
                        )));
                    }
                    let property_name = get_node_text(&property_node, source).to_string();
                    eprintln!(
                        "[Analyze Chained]   Outer call is Member Call '.{}'",
                        property_name
                    );
                    // Recursively analyze the object part first
                    let object_steps = analyze_chained_call(
                        // Assign to the outer object_steps
                        object_node,
                        caller_node_id,
                        caller_contract_name_opt,
                        ctx,
                        graph,
                        source,
                        solidity_lang,
                        input,
                        Some(_original_start_node_for_err_reporting),
                        originating_span_start, // Pass down the same originating span start
                    )?;
                    // --- Try to capture the base identifier if the object is simple ---
                    let base_identifier_name = if object_node.kind() == "identifier" {
                        Some(get_node_text(&object_node, source).to_string())
                    } else {
                        // TODO: Handle more complex base objects if needed, e.g., struct members `myStruct.arr.push()`
                        // For now, only capture direct identifiers like `allPairs`.
                        None
                    };

                    // Determine the type of the object for *this* call
                    let outer_object_type = if let Some(last_object_step) = object_steps.last() {
                        last_object_step.result_type.clone()
                    } else {
                        // If object analysis yielded no steps (e.g., simple identifier)
                        resolve_expression_type_v2(
                            object_node,
                            caller_node_id,
                            caller_contract_name_opt,
                            ctx,
                            graph,
                            source,
                            solidity_lang,
                            input,
                        )?
                    };
                    eprintln!(
                        "[Analyze Chained]     Resolved outer_object_type for member call: {:?}",
                        outer_object_type
                    );

                    if let Some(ref obj_type) = outer_object_type {
                        eprintln!(
                            "[Analyze Chained]     Object type for '.{}' call resolved to: '{}'",
                            property_name, obj_type
                        );
                        eprintln!("[Analyze Chained]     Calling resolve_member_or_library_call_v2 with obj_type='{}', property='{}'", obj_type, property_name);
                        let target = resolve_member_or_library_call_v2(
                            obj_type,
                            &property_name,
                            caller_contract_name_opt,
                            graph,
                            ctx,
                            source,
                            (member_expr_node.start_byte(), member_expr_node.end_byte()),
                        )?;
                        let result_type = resolve_call_return_type(
                            &target,
                            ctx,
                            graph,
                            source,
                            solidity_lang,
                            input,
                        )?;

                        let current_object_instance_text =
                            Some(get_node_text(&object_node, source).to_string());

                        let outer_step = ResolvedCallStep {
                            call_expr_span: (start_node.start_byte(), start_node.end_byte()),
                            function_span: (property_node.start_byte(), property_node.end_byte()),
                            target: target.clone(), // Clone target here to allow reuse below
                            arguments: argument_texts, // Use collected texts
                            result_type: result_type.clone(),
                            object_type: outer_object_type.clone(),
                            object_instance_text: current_object_instance_text, // Populate the instance text
                            originating_span_start, // Populate the new field
                            base_object_identifier_for_builtin: if matches!(
                                target,
                                ResolvedTarget::BuiltIn { .. }
                            ) {
                                base_identifier_name // Use the captured name
                            } else {
                                None
                            },
                        };
                        // Combine steps: object_steps first, then argument_steps, then the outer_step
                        steps = object_steps; // Start with object steps
                        steps.extend(argument_steps); // Add argument steps
                        steps.push(outer_step); // Add the final member call step
                        eprintln!(
                            "[Analyze Chained]     Member call step added. Result type: {:?}. Total steps for this branch: {}",
                            result_type, steps.len()
                        );
                    } else {
                        eprintln!("[Analyze Chained]     Failed to resolve object type for member call '.{}'", property_name);
                        return Err(TypeError::TypeResolutionFailed {
                            expr: get_node_text(&object_node, source).to_string(),
                            reason: "Could not determine type for member call.".to_string(),
                        });
                    }
                }

                // Case C: Constructor call `new Contract(...)` - Handled by `new_expression` case now?
                // Let's double-check the AST structure. `new Contract()` is often wrapped in `call_expression`.
                // The `new_expression` case above handles the `new` node itself.
                // If the `call_expression` handler finds a `new_expression` inside, it should delegate.
                "expression"
                    if function_node
                        .child(0)
                        .map_or(false, |n| n.kind() == "new_expression") =>
                {
                    // Recursively analyze the new_expression itself to get its step(s)
                    let _object_steps: Vec<ResolvedCallStep> = Vec::new(); // To satisfy structure, though new_steps is what's used
                    let _outer_object_type: Option<String>; // To satisfy structure

                    let new_expression_node = function_node.child(0).unwrap();
                    let mut new_constructor_steps = analyze_chained_call(
                        new_expression_node, // The new_expression node
                        caller_node_id,
                        caller_contract_name_opt,
                        ctx,
                        graph,
                        source,
                        solidity_lang,
                        input,
                        Some(_original_start_node_for_err_reporting),
                        originating_span_start,
                    )?;

                    // If the new_expression generated a step (it should for a constructor),
                    // update its arguments with those from the outer call_expression.
                    if let Some(constructor_step) = new_constructor_steps.get_mut(0) {
                        // argument_texts were collected earlier in the call_expression handler
                        constructor_step.arguments = argument_texts.clone();
                    }

                    // Combine steps: argument_steps first, then the (potentially modified) new_constructor_steps
                    steps = argument_steps; // Start with argument steps
                    steps.extend(new_constructor_steps); // Add steps from analyzing the 'new' expression
                    eprintln!(
                        "[Analyze Chained]   'new' expression wrapped in call processed. Total steps for this branch: {}",
                        steps.len()
                    );
                }
                _ => {
                    return Err(TypeError::UnsupportedNodeKind(format!(
                        "Unsupported function node kind in call_expression: '{}'",
                        function_node.kind()
                    )));
                }
            }
        }

        "member_expression" => {
            // Handle cases like `a.b` where `b` is accessed but not called.
            // This sets the stage for a potential *next* call in the chain.
            let object_node = start_node // Use start_node
                .child_by_field_name("object")
                .ok_or_else(|| {
                    TypeError::MissingChild("member_expression missing object".to_string())
                })?;
            let property_node = start_node // Use start_node
                .child_by_field_name("property")
                .ok_or_else(|| {
                    TypeError::MissingChild("member_expression missing property".to_string())
                })?;

            if property_node.kind() != "identifier" {
                return Err(TypeError::UnsupportedNodeKind(format!(
                    "Member expression property kind '{}' not supported",
                    property_node.kind()
                )));
            }
            let property_name = get_node_text(&property_node, source).to_string();

            eprintln!(
                "[Analyze Chained] Handling member_expression '.{}' (not a call)",
                property_name
            );

            // Recursively analyze the object part first
            let object_steps = analyze_chained_call(
                object_node,
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
                Some(_original_start_node_for_err_reporting), // Use the correct parameter name
                originating_span_start, // Pass down the same originating span start
            )?;
            steps.extend(object_steps); // Prepend object steps

            // This member access itself doesn't generate a call step.
            // We just needed to ensure the object part was analyzed.
            // The type resolution for the *next* step will handle this property.
            eprintln!(
                "[Analyze Chained]   Member access processed. {} inner steps added.",
                steps.len()
            );
        }

        "expression" => {
            // Delegate analysis to the first child of the expression node
            if let Some(child_node) = start_node.child(0) {
                // Use start_node
                eprintln!(
                    "[Analyze Chained] Delegating 'expression' analysis to child '{}'.",
                    child_node.kind()
                );
                // Recursively call analyze_chained_call on the child.
                // The result of this recursive call *is* the result for the expression node.
                return analyze_chained_call(
                    // Use return here
                    child_node,
                    caller_node_id,
                    caller_contract_name_opt,
                    ctx,
                    graph,
                    source,
                    solidity_lang,
                    input,
                    Some(_original_start_node_for_err_reporting),
                    originating_span_start, // Pass down the same originating span start
                );
            } else {
                eprintln!("[Analyze Chained] 'expression' node has no children.");
                // Fall through to return the current (likely empty) steps vector.
            }
        }
        "binary_expression" => {
            // Recursively analyze left and right operands to collect steps within them
            let left_node = start_node.child_by_field_name("left").ok_or_else(|| {
                TypeError::MissingChild("binary_expression missing left operand".to_string())
            })?;
            let right_node = start_node.child_by_field_name("right").ok_or_else(|| {
                TypeError::MissingChild("binary_expression missing right operand".to_string())
            })?;

            eprintln!(
                "[Analyze Chained]   Binary expression: Analyzing left operand kind: '{}'",
                left_node.kind()
            );
            let left_steps = analyze_chained_call(
                left_node,
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
                Some(_original_start_node_for_err_reporting), // Use the correct parameter name
                originating_span_start, // Pass down the same originating span start
            )?;
            steps.extend(left_steps); // Add steps from left operand

            eprintln!(
                "[Analyze Chained]   Binary expression: Analyzing right operand kind: '{}'",
                right_node.kind()
            );
            let right_steps = analyze_chained_call(
                right_node,
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
                Some(_original_start_node_for_err_reporting), // Use the correct parameter name
                originating_span_start, // Pass down the same originating span start
            )?;
            steps.extend(right_steps); // Add steps from right operand

            eprintln!(
                "[Analyze Chained]   Binary expression processed. Total steps for this branch now: {}",
                steps.len()
            );
            // The type resolution for the binary expression itself happens in resolve_expression_type_v2
        }
        // Default case for other unhandled node kinds
        _ => {
            // If the start node is an unhandled kind, try resolving its type.
            // This might be the start of a chain like `(a + b).call()`.
            // No steps generated directly from these.
            let _resolved_type = resolve_expression_type_v2(
                // Mark unused
                start_node, // Use start_node
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
            )?;
            // Only log if it's truly unhandled, not common cases like type casts or binary ops
            if !matches!(
                start_node.kind(),
                "type_cast_expression"
                    | "binary_expression"
                    | "unary_expression"
                    | "parenthesized_expression"
            ) {
                eprintln!(
                            "[Analyze Chained] Potentially unhandled start node kind '{}', resolved type: {:?}. No steps generated.",
                            start_node.kind(), // Use start_node
                            _resolved_type
                        );
            } else {
                eprintln!(
                            "[Analyze Chained] Handled base node kind '{}', resolved type: {:?}. No steps generated.",
                            start_node.kind(), // Use start_node
                            _resolved_type
                        );
            }
        }
    }
    // --- Order Preservation ---
    // The order is now determined by the sequence of analysis:
    // object -> arguments -> outer call.
    // Deduplication might need to be revisited if duplicate steps cause issues.

    eprintln!(
        "[Analyze Chained] Analysis finished. Total steps generated: {}",
        steps.len()
    );
    Ok(steps)
}

// --- Helper Functions ---

/// Finds the direct child expression nodes corresponding to arguments in a call.
fn find_argument_expression_nodes<'a>(call_expr_node: TsNode<'a>) -> Vec<TsNode<'a>> {
    let mut arg_nodes = Vec::new();
    let mut cursor = call_expr_node.walk();
    for child in call_expr_node.children(&mut cursor) {
        // Look for 'call_argument' nodes directly under the call_expression
        if child.kind() == "call_argument" {
            if let Some(expr_node) = child.child(0) {
                // Assuming expression is the first child
                arg_nodes.push(expr_node);
            }
        }
        // Also handle the case where arguments are wrapped in an 'arguments' node
        else if child.kind() == "arguments" {
            let mut arg_cursor = child.walk();
            for arg_child in child.children(&mut arg_cursor) {
                if arg_child.kind() == "call_argument" {
                    if let Some(expr_node) = arg_child.child(0) {
                        arg_nodes.push(expr_node);
                    }
                }
            }
        }
    }
    arg_nodes
}

/// Resolves the Solidity type name for a given expression node (V2).
/// Inspired by cg::resolve_expression_type but adapted for this module.
fn resolve_expression_type_v2<'a>(
    expr_node: TsNode<'a>,
    caller_node_id: usize,
    caller_contract_name_opt: &'a Option<String>,
    ctx: &CallGraphGeneratorContext,
    graph: &'a CallGraph,
    source: &'a str,
    solidity_lang: &'a tree_sitter::Language,
    input: &'a CallGraphGeneratorInput, // Pass input for potential use in future
) -> std::result::Result<Option<String>, TypeError> {
    // Use full path to Result
    let expr_text = get_node_text(&expr_node, source).trim().to_string();
    eprintln!(
        "[Resolve Type V2] Resolving type for node kind: '{}', text: '{}'",
        expr_node.kind(),
        expr_text
    );

    match expr_node.kind() {
        // --- Base Cases ---
        "identifier" => {
            let name = expr_text;
            if name == "Math" {
                eprintln!(
                    "[Resolve Type V2] Encountered identifier 'Math'. CallerScope='{:?}'",
                    caller_contract_name_opt
                );
            }
            // 1. Check state variables in current contract scope
            if let Some(contract_name) = caller_contract_name_opt {
                if let Some(type_name) = ctx
                    .state_var_types
                    .get(&(contract_name.clone(), name.clone()))
                {
                    eprintln!(
                        "[Resolve Type V2]   Identifier '{}' resolved to state var type '{}'",
                        name, type_name
                    );
                    return Ok(Some(type_name.clone()));
                }
            }
            // TODO: Check local variables/parameters within the caller_node_id's definition_ts_node
            // This requires parsing the function parameters and local variable declarations.
            // --- Start: Check local variables ---
            // Ensure we log attempt even if it fails to find node info later
            if let Some(caller_node_graph_info) = graph.nodes.get(caller_node_id) {
                let current_func_name = &caller_node_graph_info.name;
                let current_contract_name = caller_node_graph_info
                    .contract_name
                    .as_deref()
                    .unwrap_or("Global");

                eprintln!("[Resolve Type V2 - LocalVar] Attempting to resolve identifier '{}' as local variable in function '{}.{}' (Caller Node ID: {}, Expr Node Span: {:?})", name, current_contract_name, current_func_name, caller_node_id, (expr_node.start_byte(), expr_node.end_byte()));

                if let Some((_, caller_node_info_for_span, _)) =
                    ctx // Renamed for clarity
                        .definition_nodes_info
                        .iter()
                        .find(|(id, _, _)| *id == caller_node_id)
                {
                    if let Some(_definition_ts_node) =
                        input.tree.root_node().descendant_for_byte_range(
                            caller_node_info_for_span.span.0,
                            caller_node_info_for_span.span.1,
                        )
                    {
                        // --- Start: Check local variables (revised with upward search and correct boundary) ---
                        let local_var_query_str = r#"
                            (variable_declaration_statement
                              (variable_declaration
                                type: (_) @local_var_type
                                name: (identifier) @local_var_name
                              )
                            ) @local_var_decl_stmt
                        "#;
                        let local_var_query =
                            match Query::new(&input.solidity_lang, local_var_query_str) {
                                Ok(q) => q,
                                Err(e) => {
                                    return Err(TypeError::QueryError(format!(
                                        "Failed to create local var query: {}",
                                        e
                                    )))
                                }
                            };

                        // Determine the enclosing true function TsNode for boundary
                        let mut enclosing_true_function_ts_node: Option<TsNode> = None;
                        let mut temp_ancestor_node_opt = Some(expr_node);
                        while let Some(temp_ancestor_node) = temp_ancestor_node_opt {
                            match temp_ancestor_node.kind() {
                                "function_definition"
                                | "modifier_definition"
                                | "constructor_definition" => {
                                    enclosing_true_function_ts_node = Some(temp_ancestor_node);
                                    break;
                                }
                                _ => temp_ancestor_node_opt = temp_ancestor_node.parent(),
                            }
                        }

                        if let Some(true_function_boundary_node) = enclosing_true_function_ts_node {
                            eprintln!("[Resolve Type V2 - LocalVar Upward] Boundary for search: True function kind='{}', span=({},{})", true_function_boundary_node.kind(), true_function_boundary_node.start_byte(), true_function_boundary_node.end_byte());

                            let mut current_scope_node_opt = expr_node.parent();
                            eprintln!("[Resolve Type V2 - LocalVar Upward] Starting upward search for identifier '{}' (usage at byte {}), from parent kind: {:?}", name, expr_node.start_byte(), current_scope_node_opt.map(|n| n.kind()));

                            while let Some(current_scope_node) = current_scope_node_opt {
                                eprintln!("[Resolve Type V2 - LocalVar Upward]   Searching in scope: kind='{}', id={}, span=({},{})", current_scope_node.kind(), current_scope_node.id(), current_scope_node.start_byte(), current_scope_node.end_byte());

                                let mut cursor = QueryCursor::new();
                                let source_bytes = input.source.as_bytes();
                                let mut matches = cursor.matches(
                                    &local_var_query,
                                    current_scope_node,
                                    |n: TsNode| std::iter::once(&source_bytes[n.byte_range()]),
                                );

                                while let Some(match_) = matches.next() {
                                    let mut var_name_opt: Option<String> = None;
                                    let mut type_node_opt: Option<TsNode> = None;
                                    let mut decl_stmt_node_opt: Option<TsNode> = None;

                                    for capture in match_.captures {
                                        let capture_name = &local_var_query.capture_names()
                                            [capture.index as usize];
                                        match *capture_name {
                                            "local_var_name" => {
                                                var_name_opt = Some(
                                                    get_node_text(&capture.node, &input.source)
                                                        .to_string(),
                                                )
                                            }
                                            "local_var_type" => type_node_opt = Some(capture.node),
                                            "local_var_decl_stmt" => {
                                                decl_stmt_node_opt = Some(capture.node)
                                            }
                                            _ => {}
                                        }
                                    }

                                    if let (
                                        Some(var_name_found),
                                        Some(type_node_found),
                                        Some(decl_stmt_node),
                                    ) = (var_name_opt, type_node_opt, decl_stmt_node_opt)
                                    {
                                        // If the query found it within current_scope_node, it's in scope.
                                        // The main conditions are matching name and declaration appearing before usage.
                                        if var_name_found == name
                                            && decl_stmt_node.end_byte() <= expr_node.start_byte()
                                        {
                                            let type_name_found =
                                                get_node_text(&type_node_found, &input.source)
                                                    .to_string();
                                            eprintln!("[Resolve Type V2 - LocalVar Upward]     MATCH! In scope '{}' (ID: {}), found decl for '{}' (type '{}') ending at byte {}. Usage at byte {}.", current_scope_node.kind(), current_scope_node.id(), var_name_found, type_name_found, decl_stmt_node.end_byte(), expr_node.start_byte());
                                            return Ok(Some(type_name_found));
                                        } else if var_name_found == name {
                                            // This condition means name matched, but decl_stmt_node.end_byte() > expr_node.start_byte()
                                            // (i.e., usage is before or within the declaration span, or at least not strictly after)
                                            eprintln!("[Resolve Type V2 - LocalVar Upward]     Found decl for '{}' in scope '{}' (ID: {}), but usage (byte {}) is not strictly after declaration end (byte {}). Decl span: ({},{}).", name, current_scope_node.kind(), current_scope_node.id(), expr_node.start_byte(), decl_stmt_node.end_byte(), decl_stmt_node.start_byte(), decl_stmt_node.end_byte());
                                        }
                                        // Removed the 'else' block that logged "Skipping decl ... because its parent ... is not current_scope_node"
                                        // as that parent check was removed.
                                    }
                                }

                                if current_scope_node.id() == true_function_boundary_node.id() {
                                    eprintln!("[Resolve Type V2 - LocalVar Upward]   Reached true function boundary ('{}'). Stopping upward search for '{}'.", true_function_boundary_node.kind(), name);
                                    // current_scope_node_opt = None; // This assignment's value is reported as never read if loop breaks
                                    break; // Stop inner match loop for this scope, will also stop outer while via current_scope_node_opt becoming None effectively
                                }

                                current_scope_node_opt = current_scope_node.parent();
                                if current_scope_node_opt.is_none() {
                                    eprintln!("[Resolve Type V2 - LocalVar Upward]   Reached tree root. Stopping upward search for '{}'.", name);
                                    break;
                                }
                            } // End while current_scope_node_opt
                        } else {
                            eprintln!("[Resolve Type V2 - LocalVar Upward] Could not determine true function boundary for identifier '{}'. Local variable search might be incomplete.", name);
                        }
                        eprintln!("[Resolve Type V2 - LocalVar Upward] Finished upward search for '{}'. Not found as local variable in accessible scope.", name);
                        // --- End: Check local variables ---
                    } else {
                        // This 'else' corresponds to: if let Some(_definition_ts_node) = input.tree.root_node()...
                        eprintln!("[Resolve Type V2 - LocalVar]   Could not find definition TsNode for Caller Node ID: {}", caller_node_id);
                    }
                } else {
                    // This 'else' corresponds to: if let Some((_, caller_node_info_for_span, _)) = ctx.definition_nodes_info...
                    eprintln!("[Resolve Type V2 - LocalVar]   Could not find NodeInfo for Caller Node ID: {} in definition_nodes_info", caller_node_id);
                }
            } else {
                // This 'else' corresponds to: if let Some(caller_node_graph_info) = graph.nodes.get(caller_node_id)
                eprintln!("[Resolve Type V2 - LocalVar]   Could not find graph node info for Caller Node ID: {} (e.g. for function name/contract context)", caller_node_id);
            }
            // 2. Check if it's a known contract, library, or interface name (these are types)
            let is_contract = ctx.all_contracts.contains_key(&name);
            let is_library = ctx.all_libraries.contains_key(&name);
            let is_interface = ctx.all_interfaces.contains_key(&name);
            eprintln!("[Resolve Type V2]   Identifier '{}': is_contract={}, is_library={}, is_interface={}", name, is_contract, is_library, is_interface); // DEBUG LOG
            if is_contract || is_library || is_interface {
                eprintln!("[Resolve Type V2]   Identifier '{}' resolved to type (contract/library/interface) '{}'", name, name);
                return Ok(Some(name));
            }
            // 3. Check if it's the special 'this' keyword
            if name == "this" {
                if let Some(contract_name) = caller_contract_name_opt {
                    eprintln!(
                        "[Resolve Type V2]   Identifier 'this' resolved to contract type '{}'",
                        contract_name
                    );
                    return Ok(Some(contract_name.clone()));
                } else {
                    eprintln!("[Resolve Type V2]   Identifier 'this' used outside contract scope.");
                    return Ok(None); // 'this' outside contract is invalid
                }
            }
            // 4. Check if it's the special 'super' keyword
            // TODO: Handle 'super' keyword resolution (requires inheritance info)

            // Check if identifier resolves to a function in scope/inheritance ---
            // This handles cases like `totalSupply.mul()` where `totalSupply` is a function call.
            // We need its *return type* to resolve the subsequent `.mul`.
            match resolve_simple_call_v2(&name, caller_contract_name_opt, graph, ctx) {
                Ok(ResolvedTarget::Function {
                    contract_name,
                    function_name,
                    ..
                }) => {
                    eprintln!("[Resolve Type V2]   Identifier '{}' resolved to function '{:?}.{}'. Getting return type.", name, contract_name, function_name);
                    let func_key = (contract_name.clone(), function_name.clone());
                    if let Some(node_id) = graph.node_lookup.get(&func_key) {
                        let def_node_info_opt = ctx
                            .definition_nodes_info
                            .iter()
                            .find(|(id, _, _)| *id == *node_id)
                            .map(|(_, info, _)| info.clone());
                        if let Some(def_node_info) = def_node_info_opt {
                            // Use the function's return type as the expression's type
                            let return_type = get_function_return_type_v2(&def_node_info, input)?;
                            eprintln!(
                                "[Resolve Type V2]     Function return type: {:?}",
                                return_type
                            );
                            return Ok(return_type);
                        } else {
                            eprintln!("[Resolve Type V2]     Could not find definition NodeInfo for function ID {}", node_id);
                        }
                    } else {
                        eprintln!("[Resolve Type V2]     Could not find node ID in graph lookup for function key {:?}", func_key);
                    }
                }
                Ok(_) | Err(_) => {
                    // Didn't resolve to a function, or an error occurred during resolution.
                    // Continue with the original "not resolved" path.
                    eprintln!(
                        "[Resolve Type V2]   Identifier '{}' did not resolve to a function in scope.",
                        name
                    );
                }
            }

            eprintln!(
                "[Resolve Type V2]   Identifier '{}' type not resolved (state/type/local/function?).",
                name
            );
            Ok(None) // Identifier not resolved by any means
        }
        "primitive_type" => Ok(Some(expr_text)),
        "type_cast_expression" => {
            // Type is the type being cast to
            let type_node = expr_node.child(0).ok_or_else(|| {
                TypeError::MissingChild("type_cast_expression missing type node".to_string())
            })?;
            let type_name = get_node_text(&type_node, source).to_string();
            eprintln!(
                "[Resolve Type V2]   Type cast expression resolves to type '{}'",
                type_name
            );
            Ok(Some(type_name))
        }
        "string_literal" => Ok(Some("string".to_string())),
        "number_literal" => Ok(Some("uint256".to_string())),
        "boolean_literal" => Ok(Some("bool".to_string())),
        "hex_literal" => Ok(Some("bytes".to_string())),
        "address_literal" => Ok(Some("address".to_string())),

        // --- Recursive Cases ---
        "member_expression" => resolve_property_type_from_node(
            expr_node,
            caller_node_id,
            caller_contract_name_opt,
            ctx,
            graph,
            source,
            solidity_lang,
            input,
        ),
        "call_expression" => {
            // The type of a call expression is the return type of the function being called.
            // We need to analyze the call to find the target and then its return type.
            eprintln!(
                "[Resolve Type V2]   Call expression: Analyzing call to determine return type."
            );
            // Use analyze_chained_call on the call_expression itself.
            // This seems recursive, be careful. Maybe analyze_chained_call should return the final type?
            // Let's try calling analyze_chained_call and taking the result_type of the last step.
            let steps = analyze_chained_call(
                expr_node, // Analyze the call expression itself
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
                None,                   // original_start_node_for_err_reporting
                expr_node.start_byte(), // Pass the start byte of this call expression as the originating span start
            )?;

            if let Some(last_step) = steps.last() {
                eprintln!(
                    "[Resolve Type V2]   Call expression resolved. Last step result type: {:?}",
                    last_step.result_type
                );
                Ok(last_step.result_type.clone())
            } else {
                eprintln!("[Resolve Type V2]   Call expression analysis yielded no steps. Cannot determine type.");
                if let Some(func_node) = expr_node.child_by_field_name("function") {
                    if func_node.kind() == "expression"
                        && func_node
                            .child(0)
                            .map_or(false, |n| n.kind() == "identifier")
                    {
                        let id_node = func_node.child(0).unwrap();
                        let name = get_node_text(&id_node, source).to_string();
                        if ctx.all_interfaces.contains_key(&name) {
                            eprintln!("[Resolve Type V2]   Re-identified as interface type cast to '{}' (no steps generated)", name);
                            return Ok(Some(name)); // Return the interface type
                        }
                    }
                }
                Ok(None) // Truly unresolved
            }
        }
        "new_expression" => {
            // Type is the contract being constructed
            let type_name_node = expr_node.named_child(0).ok_or_else(|| {
                TypeError::MissingChild(format!(
                    "new_expression missing type_name (named_child(0) failed for expr_node: {})",
                    get_node_text(&expr_node, source)
                ))
            })?;
            let contract_name = get_node_text(&type_name_node, source).to_string(); // Simplified
            eprintln!(
                "[Resolve Type V2]   New expression resolves to type '{}'",
                contract_name
            );
            Ok(Some(contract_name))
        }
        "expression" => {
            // Delegate type resolution to the first child of the expression node
            if let Some(child_node) = expr_node.child(0) {
                eprintln!(
                    "[Resolve Type V2]   Delegating 'expression' type resolution to child kind: '{}'",
                    child_node.kind()
                );
                // Recursively call resolve_expression_type_v2 on the child
                resolve_expression_type_v2(
                    child_node,
                    caller_node_id,
                    caller_contract_name_opt,
                    ctx,
                    graph,
                    source,
                    solidity_lang,
                    input,
                )
            } else {
                eprintln!("[Resolve Type V2]   'expression' node has no children.");
                Ok(None) // Expression node with no children has no type
            }
        }
        // TODO: Handle tuple_expression, etc.
        "array_access" => {
            let base_node = expr_node.child_by_field_name("base").ok_or_else(|| {
                TypeError::MissingChild(format!(
                    "array_access missing base for node: {}",
                    get_node_text(&expr_node, source)
                ))
            })?;
            eprintln!(
                "[Resolve Type V2]   Array access: base kind '{}', text '{}'",
                base_node.kind(),
                get_node_text(&base_node, source)
            );

            let base_type_opt = resolve_expression_type_v2(
                base_node,
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
            )?;

            if let Some(ref base_type_of_base_expr_str) = base_type_opt {
                // Use `ref` to borrow
                eprintln!(
                    "[Resolve Type V2]     Base type of array_access resolved to: '{}'",
                    base_type_of_base_expr_str
                );

                // Priority 1: If base is an identifier, check ctx.contract_mappings for precise value_type
                if base_node.kind() == "identifier" {
                    if let Some(contract_name) = caller_contract_name_opt {
                        let mapping_var_name = get_node_text(&base_node, source);
                        if let Some(mapping_info) = ctx
                            .contract_mappings
                            .get(&(contract_name.clone(), mapping_var_name.to_string()))
                        {
                            eprintln!(
                                "[Resolve Type V2]       Array access on known mapping '{}.{}'. Resolved value type from MappingInfo: '{}'",
                                contract_name, mapping_var_name, mapping_info.value_type
                            );
                            return Ok(Some(mapping_info.value_type.clone()));
                        }
                    }
                }

                // Priority 2: Fallback to parsing the resolved base_type_of_base_expr_str
                if base_type_of_base_expr_str.starts_with("mapping(") {
                    match parse_mapping_final_value_type(base_type_of_base_expr_str) {
                        Ok(final_value_type) => {
                            eprintln!(
                                "[Resolve Type V2]       Parsed mapping type string '{}'. Final value type: '{}'",
                                base_type_of_base_expr_str, final_value_type
                            );
                            return Ok(Some(final_value_type));
                        }
                        Err(e) => {
                            eprintln!(
                                "[Resolve Type V2]       Error parsing mapping type string '{}': {}. Falling back.",
                                base_type_of_base_expr_str, e
                            );
                            // Fall through to other checks if parsing fails, or return error
                            // For now, let's try to be robust and let other checks proceed.
                        }
                    }
                }

                // Existing logic for arrays and bytes
                if base_type_of_base_expr_str.ends_with("[]") {
                    let element_type =
                        base_type_of_base_expr_str[0..base_type_of_base_expr_str.len() - 2].trim();
                    if !element_type.is_empty() {
                        eprintln!(
                            "[Resolve Type V2]       Array element type: '{}'",
                            element_type
                        );
                        return Ok(Some(element_type.to_string()));
                    }
                    eprintln!(
                        "[Resolve Type V2]       Could not parse array type: {}",
                        base_type_of_base_expr_str // Fix: Use new variable name
                    );
                    Ok(None)
                } else if base_type_of_base_expr_str == "bytes" {
                    // Fix: Use new variable name
                    // Indexing bytes returns bytes1
                    eprintln!("[Resolve Type V2]       Bytes indexed, returning 'bytes1'");
                    return Ok(Some("bytes1".to_string()));
                }
                // Add other special cases if needed (e.g., string)
                else {
                    eprintln!(
                            "[Resolve Type V2]     Base type '{}' is not a mapping, array, or bytes. Cannot determine indexed type.",
                            base_type_of_base_expr_str // Fix: Use new variable name
                        );
                    Ok(None)
                }
            } else {
                eprintln!("[Resolve Type V2]     Could not resolve base type of array_access.");
                Ok(None)
            }
        }
        "binary_expression" => {
            // --- Determine result type based on operator ---
            // We don't need to analyze operands recursively here, just determine the result type.
            let operator_node = expr_node.child_by_field_name("operator").ok_or_else(|| {
                TypeError::MissingChild("binary_expression missing operator".to_string())
            })?;
            let operator_text = get_node_text(&operator_node, source);

            let result_type = match &*operator_text {
                // Use &* to dereference String to &str
                ">" | "<" | ">=" | "<=" | "==" | "!=" => Some("bool".to_string()),
                "&&" | "||" => Some("bool".to_string()),
                "+" | "-" | "*" | "/" | "%" | "**" | "&" | "|" | "^" | "<<" | ">>" => {
                    // Assume uint256 for arithmetic/bitwise/shift for now
                    Some("uint256".to_string())
                }
                _ => {
                    eprintln!(
                        "[Resolve Type V2]   Unhandled binary operator: '{}'",
                        operator_text
                    );
                    None // Unknown operator type
                }
            };
            return Ok(result_type); // Return the determined type
        }
        "unary_expression" => {
            // Basic heuristic: Assume numeric ops return uint256, boolean ops return bool
            let operator = expr_node
                .child_by_field_name("operator")
                .map(|n| get_node_text(&n, source));
            let operand_node = expr_node.child_by_field_name("argument").ok_or_else(|| {
                TypeError::MissingChild("unary_expression missing argument".to_string())
            })?;
            match operator.as_deref() {
                Some("!") => Ok(Some("bool".to_string())),
                Some("-") | Some("+") | Some("++") | Some("--") => Ok(Some("uint256".to_string())), // Assume numeric
                _ => {
                    eprintln!(
                        "[Resolve Type V2]   Unhandled unary operator: {:?}",
                        operator
                    );
                    // Fallback to operand type?
                    resolve_expression_type_v2(
                        operand_node,
                        caller_node_id,
                        caller_contract_name_opt,
                        ctx,
                        graph,
                        source,
                        solidity_lang,
                        input,
                    )
                }
            }
        }
        _ => {
            eprintln!(
                "[Resolve Type V2]   Unhandled expression kind: {}",
                expr_node.kind()
            );
            Ok(None)
        }
    }
}

/// Resolves the type of a property accessed via member expression (`object.property`).
fn resolve_property_type<'a>(
    object_type_name: &str,
    property_name: &str,
    caller_contract_name_opt: &'a Option<String>, // Prefixed, seems unused
    graph: &'a CallGraph,
    ctx: &CallGraphGeneratorContext,
    _source: &'a str,                          // Prefixed, seems unused
    _solidity_lang: &'a tree_sitter::Language, // Prefixed, seems unused
    input: &'a CallGraphGeneratorInput,        // Pass input for potential use in future
) -> std::result::Result<Option<String>, TypeError> {
    // Use full path to Result
    eprintln!(
        "[Resolve Property Type] Resolving type for property '{}' on object type '{}'",
        property_name, object_type_name
    );

    // 1. Check if the object type is a contract/library/interface and the property is a state variable
    // Note: Interfaces don't have state variables.
    if !ctx.all_interfaces.contains_key(object_type_name) {
        if let Some(type_name) = ctx
            .state_var_types
            .get(&(object_type_name.to_string(), property_name.to_string()))
        {
            eprintln!(
                "[Resolve Property Type]   Property resolved to state var type '{}'",
                type_name
            );
            return Ok(Some(type_name.clone()));
        }
    }

    // 2. Check if the object type is a contract/library/interface and the property is a function/method
    // We need the *return type* of that function.
    let function_key = (
        Some(object_type_name.to_string()),
        property_name.to_string(),
    );
    if let Some(target_node_id) = graph.node_lookup.get(&function_key) {
        eprintln!("[Resolve Property Type]   Property resolved to function member. Node ID: {}. Getting return type.", target_node_id);
        // Use the existing helper from cg.rs (or move/adapt it later)
        // Need to get the definition TsNode for the target function
        let target_def_node_opt = ctx
            .definition_nodes_info
            .iter()
            .find(|(id, _, _)| *id == *target_node_id)
            .map(|(_, n, _)| n.clone());
        if let Some(target_def_node) = target_def_node_opt {
            let return_type = get_function_return_type_v2(&target_def_node, input)?; // Use V2 helper
            eprintln!(
                "[Resolve Property Type]     Return type from get_function_return_type_v2: {:?}",
                return_type
            );
            return Ok(return_type);
        } else {
            eprintln!("[Resolve Property Type]     Could not find definition node for target function ID {}", target_node_id);
            return Ok(None); // Cannot determine return type without definition node
        }
    }

    // 3. Check 'using for' libraries (if the property is a function in an attached library)
    // This requires finding the library function and getting its return type.
    if let Some(library_target) = find_using_for_target(
        object_type_name,
        property_name,
        caller_contract_name_opt,
        graph,
        ctx,
    )? {
        if let ResolvedTarget::Function {
            contract_name: Some(lib_name),
            function_name,
            ..
        } = library_target
        {
            let lib_func_key = (Some(lib_name.clone()), function_name.clone());
            if let Some(target_node_id) = graph.node_lookup.get(&lib_func_key) {
                eprintln!("[Resolve Property Type]   Property resolved to 'using for' library function '{}.{}'. Node ID: {}. Getting return type.", lib_name, function_name, target_node_id);
                let target_def_node_opt = ctx
                    .definition_nodes_info
                    .iter()
                    .find(|(id, _, _)| *id == *target_node_id)
                    .map(|(_, n, _)| n.clone());
                if let Some(target_def_node) = target_def_node_opt {
                    let return_type = get_function_return_type_v2(&target_def_node, input)?;
                    eprintln!("[Resolve Property Type]     Return type from get_function_return_type_v2: {:?}", return_type);
                    return Ok(return_type);
                } else {
                    eprintln!("[Resolve Property Type]     Could not find definition node for target library function ID {}", target_node_id);
                    return Ok(None);
                }
            }
        }
    }

    // 4. Check built-in properties (e.g., array.length -> uint256, address.balance -> uint256)
    // TODO: Implement built-in property type resolution
    if property_name == "length"
        && (object_type_name.ends_with("[]")
            || object_type_name == "bytes"
            || object_type_name == "string")
    {
        return Ok(Some("uint256".to_string()));
    }
    if property_name == "balance" && object_type_name == "address" {
        return Ok(Some("uint256".to_string()));
    }

    eprintln!(
        "[Resolve Property Type]   Property '{}' type not resolved within type '{}'.",
        property_name, object_type_name
    );
    Ok(None)
}

/// Helper to resolve property type directly from a member_expression node.
fn resolve_property_type_from_node<'a>(
    member_expr_node: TsNode<'a>,
    caller_node_id: usize,
    caller_contract_name_opt: &'a Option<String>,
    ctx: &CallGraphGeneratorContext,
    graph: &'a CallGraph,
    source: &'a str,
    solidity_lang: &'a tree_sitter::Language,
    input: &'a CallGraphGeneratorInput, // Pass input for potential use in future
) -> std::result::Result<Option<String>, TypeError> {
    // Use full path to Result
    let object_node = member_expr_node
        .child_by_field_name("object")
        .ok_or_else(|| TypeError::MissingChild("member_expression missing object".to_string()))?;
    let property_node = member_expr_node
        .child_by_field_name("property")
        .ok_or_else(|| TypeError::MissingChild("member_expression missing property".to_string()))?;

    if property_node.kind() != "identifier" {
        eprintln!("[Resolve Type V2]   Member expr: Property is not an identifier.");
        return Ok(None);
    }
    let property_name = get_node_text(&property_node, source).to_string();

    // Resolve the type of the object first
    let object_type_name_opt = resolve_expression_type_v2(
        object_node,
        caller_node_id,
        caller_contract_name_opt,
        ctx,
        graph,
        source,
        solidity_lang,
        input,
    )?;

    if let Some(object_type_name) = object_type_name_opt {
        // Now resolve the property type based on the object type
        resolve_property_type(
            &object_type_name,
            &property_name,
            caller_contract_name_opt,
            graph,
            ctx,
            source,
            solidity_lang,
            input,
        )
    } else {
        eprintln!("[Resolve Type V2]   Member expr: Could not resolve object type.");
        Ok(None) // Object type couldn't be resolved
    }
}

/// Resolves the target of a member access or library call (V2).
/// Returns a `ResolvedTarget` enum.
fn resolve_member_or_library_call_v2<'a>(
    object_type_name: &str,
    property_name: &str,
    caller_contract_name_opt: &'a Option<String>,
    graph: &'a CallGraph,
    ctx: &CallGraphGeneratorContext,
    source: &'a str, // Needed for interface resolution heuristic potentially
    call_span_bytes: (usize, usize), // For error reporting
) -> std::result::Result<ResolvedTarget, TypeError> {
    // Use full path to Result
    let call_span_text = &source[call_span_bytes.0..call_span_bytes.1];
    eprintln!(
        "[Resolve Member/Lib V2] Resolving: Type='{}', Property='{}', CallerScope='{:?}', Span='{}'",
        object_type_name, property_name, caller_contract_name_opt, call_span_text
    );

    // --- Priority 1: Interface Implementation Resolution (if object type is an interface) ---
    if ctx.all_interfaces.contains_key(object_type_name) {
        eprintln!(
            "[Resolve Member/Lib V2]   Object type '{}' is an interface. Looking for implementations.",
            object_type_name
        );
        let interface_name = object_type_name; // Alias for clarity
        let method_name = property_name; // Alias for clarity

        // Find contracts directly implementing this interface
        let mut implementing_contracts = Vec::new();
        for (contract, implemented_interfaces) in &ctx.contract_implements {
            if implemented_interfaces.contains(&interface_name.to_string()) {
                implementing_contracts.push(contract.clone());
            }
        }
        eprintln!(
            "[Resolve Member/Lib V2]     Directly implementing contracts found: {:?}",
            implementing_contracts
        );

        let mut potential_direct_targets = Vec::new();
        for contract_name_impl in &implementing_contracts {
            let target_key = (Some(contract_name_impl.clone()), method_name.to_string());
            if let Some(node_id) = graph.node_lookup.get(&target_key).copied() {
                if let Some(node) = graph.nodes.get(node_id) {
                    potential_direct_targets.push(ResolvedTarget::Function {
                        contract_name: Some(contract_name_impl.clone()),
                        function_name: method_name.to_string(),
                        node_type: node.node_type.clone(),
                    });
                }
            }
        }

        // Attempt Natspec-driven binding resolution
        let mut natspec_binding_provided_concrete_impl: Option<ResolvedTarget> = None;
        let mut natspec_key_found_but_binding_non_concrete = false;

        if let (Some(manifest_ref), Some(registry_ref)) =
            (ctx.manifest.as_ref(), ctx.binding_registry.as_ref())
        {
            let resolver =
                crate::interface_resolver::InterfaceResolver::new(manifest_ref, registry_ref);
            let interface_file_path_hint: Option<std::path::PathBuf> = manifest_ref
                .entries
                .iter()
                .find(|entry| {
                    entry.item_kind == crate::natspec::extract::SourceItemKind::Interface
                        && entry.item_name.as_deref() == Some(interface_name)
                })
                .map(|entry| entry.file_path.clone());

            eprintln!(
                "[Resolve Member/Lib V2]       Interface file hint for binding lookup: {:?}",
                interface_file_path_hint
            );

            match resolver.resolve_for_item_name_kind(
                interface_name,
                crate::natspec::extract::SourceItemKind::Interface,
                interface_file_path_hint.as_deref(),
            ) {
                Ok(Some(binding_config)) => {
                    eprintln!("[Resolve Member/Lib V2]       Natspec binding key '{}' found for interface '{}'.", binding_config.key, interface_name);
                    if let Some(concrete_contract_name) = &binding_config.contract_name {
                        eprintln!("[Resolve Member/Lib V2]         Binding config specifies concrete contract: '{}'", concrete_contract_name);
                        let bound_target_key = (
                            Some(concrete_contract_name.clone()),
                            method_name.to_string(),
                        );
                        if let Some(node_id) = graph.node_lookup.get(&bound_target_key) {
                            if let Some(node_info) = graph.nodes.get(*node_id) {
                                match node_info.node_type {
                                    NodeType::Function | NodeType::Modifier | NodeType::Constructor => {
                                        let bound_implementation = ResolvedTarget::Function {
                                            contract_name: Some(concrete_contract_name.clone()),
                                            function_name: method_name.to_string(),
                                            node_type: node_info.node_type.clone(),
                                        };
                                        eprintln!("[Resolve Member/Lib V2]           Successfully resolved to bound callable implementation: {:?}", bound_implementation);
                                        natspec_binding_provided_concrete_impl = Some(ResolvedTarget::InterfaceMethod {
                                            interface_name: interface_name.to_string(),
                                            method_name: method_name.to_string(),
                                            implementation: Some(Box::new(bound_implementation)),
                                        });
                                    }
                                    NodeType::StorageVariable => {
                                        if node_info.visibility == crate::cg::Visibility::Public {
                                            let getter_implementation = ResolvedTarget::Function {
                                                contract_name: Some(concrete_contract_name.clone()),
                                                function_name: method_name.to_string(), // Getter name is var name
                                                node_type: NodeType::Function, // The getter is conceptually a function
                                            };
                                            eprintln!("[Resolve Member/Lib V2]           Successfully resolved to getter for public storage variable: {:?}", getter_implementation);
                                            natspec_binding_provided_concrete_impl = Some(ResolvedTarget::InterfaceMethod {
                                                interface_name: interface_name.to_string(),
                                                method_name: method_name.to_string(),
                                                implementation: Some(Box::new(getter_implementation)),
                                            });
                                        } else {
                                            eprintln!("[Resolve Member/Lib V2]           Bound contract '{}' member '{}' is a non-public StorageVariable. Binding not used.", concrete_contract_name, method_name);
                                        }
                                    }
                                    _ => eprintln!("[Resolve Member/Lib V2]           Bound contract '{}' member '{}' is not callable or a public storage variable (Type: {:?}). Binding not used.", concrete_contract_name, method_name, node_info.node_type),
                                }
                            } else {
                                eprintln!("[Resolve Member/Lib V2]           Bound contract '{}' method '{}' (Node ID {}) not found in graph.nodes. Binding not used.", concrete_contract_name, method_name, node_id);
                            }
                        } else { // node_id not found in graph.node_lookup
                            eprintln!("[Resolve Member/Lib V2]           Method or Variable '{}' not found in bound contract '{}' via graph.node_lookup. Binding not used.", method_name, concrete_contract_name);
                        }
                    } else {
                        eprintln!("[Resolve Member/Lib V2]         Binding for interface '{}' (key '{}') has no concrete_contract_name.", interface_name, binding_config.key);
                        natspec_key_found_but_binding_non_concrete = true;
                    }
                }
                Ok(None) => eprintln!("[Resolve Member/Lib V2]       No Natspec binding key found for interface type '{}', or key not in registry.", interface_name),
                Err(e) => eprintln!("Warning: Error resolving Natspec binding for interface type '{}': {}. Proceeding as if no binding found.", interface_name, e),
            }
        } else {
            eprintln!("[Resolve Member/Lib V2]     Manifest or BindingRegistry not available for Natspec-driven interface binding resolution.");
        }

        // Decision logic based on binding and direct implementations
        if let Some(concrete_impl) = natspec_binding_provided_concrete_impl {
            return Ok(concrete_impl);
        }

        // Natspec binding did not provide a concrete implementation. Consider direct ones.
        if potential_direct_targets.len() == 1 {
            let direct_implementation = potential_direct_targets.remove(0);
            eprintln!("[Resolve Member/Lib V2]     Resolved to single direct implementation (Natspec binding was not concrete or not found): {:?}", direct_implementation);
            return Ok(ResolvedTarget::InterfaceMethod {
                interface_name: interface_name.to_string(),
                method_name: method_name.to_string(),
                implementation: Some(Box::new(direct_implementation)),
            });
        }

        if potential_direct_targets.len() > 1 {
            if natspec_key_found_but_binding_non_concrete {
                eprintln!("[Resolve Member/Lib V2]     Ambiguous direct implementations for '{}.{}', but Natspec binding key was found (though non-concrete). Falling back to abstract.", interface_name, method_name);
                return Ok(ResolvedTarget::InterfaceMethod {
                    interface_name: interface_name.to_string(),
                    method_name: method_name.to_string(),
                    implementation: None,
                });
            } else {
                // Multiple direct implementers AND (no Natspec binding key found OR Natspec key lookup failed entirely).
                eprintln!("[Resolve Member/Lib V2]     Ambiguous implementation for '{}.{}' ({} direct targets) and no overriding/clarifying Natspec binding. Implementations: {:?}", interface_name, method_name, potential_direct_targets.len(), implementing_contracts);
                return Err(TypeError::AmbiguousInterfaceImplementation {
                    interface_name: interface_name.to_string(),
                    method_name: method_name.to_string(),
                    implementations: implementing_contracts,
                });
            }
        }

        // potential_direct_targets.len() == 0 and Natspec binding did not provide a concrete implementation.
        eprintln!("[Resolve Member/Lib V2]     No specific implementation found for '{}.{}'. Assuming external or abstract.", interface_name, method_name);
        return Ok(ResolvedTarget::InterfaceMethod {
            interface_name: interface_name.to_string(),
            method_name: method_name.to_string(),
            implementation: None,
        });
    }

    // --- Priority 2: Direct Member Lookup (Contract/Library) ---
    let direct_lookup_key = (
        Some(object_type_name.to_string()),
        property_name.to_string(),
    );
    let lookup_result = graph.node_lookup.get(&direct_lookup_key);
    eprintln!(
        "[Resolve Member/Lib V2]   Attempting direct lookup with key: {:?}. Found: {}",
        direct_lookup_key,
        lookup_result.is_some()
    );
    if let Some(node_id) = lookup_result {
        // Use the captured result
        if let Some(node) = graph.nodes.get(*node_id) {
            // Ensure it's a callable type (Function, Modifier, Constructor)
            match node.node_type {
                NodeType::Function | NodeType::Modifier | NodeType::Constructor => {
                    eprintln!(
                        "[Resolve Member/Lib V2]   Found direct member: Node ID {}, Type: {:?}",
                        node_id, node.node_type
                    );
                    return Ok(ResolvedTarget::Function {
                        contract_name: node.contract_name.clone(),
                        function_name: node.name.clone(),
                        node_type: node.node_type.clone(),
                    });
                }
                _ => {
                    eprintln!("[Resolve Member/Lib V2]   Found direct member but it's not callable (Type: {:?}). Node ID {}", node.node_type, node_id);
                    // It's a state variable or other non-callable member.
                    return Ok(ResolvedTarget::NotCallable {
                        reason: format!(
                            "Member '{}' is a {:?}, not a function.",
                            property_name, node.node_type
                        ),
                    });
                }
            }
        }
    } else {
        eprintln!(
            "[Resolve Member/Lib V2]   Direct lookup failed for key: {:?}",
            direct_lookup_key
        );
    }

    // --- Priority 3: 'using for' Directives ---
    if let Some(library_target) = find_using_for_target(
        object_type_name,
        property_name,
        caller_contract_name_opt,
        graph,
        ctx,
    )? {
        eprintln!(
            "[Resolve Member/Lib V2]   Found via 'using for': {:?}",
            library_target
        );
        // library_target is already the ResolvedTarget due to `if let Some()`
        return Ok(library_target);
    }

    // --- Priority 4: Built-in Members ---
    if let Some(builtin) = builtin::get_builtin(property_name, object_type_name) {
        eprintln!(
            "[Resolve Member/Lib V2]   Resolved to built-in member '{}' for type '{}'",
            builtin.name, builtin.object_type
        );
        return Ok(ResolvedTarget::BuiltIn {
            object_type: object_type_name.to_string(), // Keep the specific type (e.g., uint[])
            name: builtin.name.to_string(),
        });
    }

    // --- Not Found ---
    eprintln!(
        "[Resolve Member/Lib V2]   Target '{}.{}' could not be resolved.",
        object_type_name, property_name
    );
    Ok(ResolvedTarget::NotCallable {
        reason: format!(
            "Member or method '{}.{}' not found.",
            object_type_name, property_name
        ),
    })
}

/// Helper to specifically find a target using 'using for' directives.
fn find_using_for_target<'a>(
    object_type_name: &str,
    property_name: &str,
    caller_contract_name_opt: &'a Option<String>,
    graph: &'a CallGraph,
    ctx: &CallGraphGeneratorContext,
) -> std::result::Result<Option<ResolvedTarget>, TypeError> {
    // Use full path to Result
    let mut potential_libraries = Vec::new();
    let mut types_to_check = vec![object_type_name.to_string()];

    if object_type_name == "uint" {
        types_to_check.push("uint256".to_string());
    } else if object_type_name == "uint256" {
        types_to_check.push("uint".to_string());
    }

    // Check contract-specific directives first
    if let Some(caller_contract_name) = caller_contract_name_opt {
        for type_name in &types_to_check {
            // Iterate through original type and alias
            // Check for specific type: (Some(Contract), Type)
            let specific_type_key = (
                Some(caller_contract_name.clone()),
                type_name.clone(), // Use type_name from loop
            );
            eprintln!(
                "[Find UsingFor] Checking specific key: {:?}",
                specific_type_key
            ); // DEBUG
            if let Some(libs) = ctx.using_for_directives.get(&specific_type_key) {
                eprintln!(
                    "[Find UsingFor]   Found specific directive for contract '{}', type '{}': {:?}",
                    caller_contract_name,
                    type_name,
                    libs // Use type_name
                );
                potential_libraries.extend(libs.iter().cloned());
            }
        }
        // Check for wildcard type: (Some(Contract), "*") - Only check once
        let wildcard_key = (Some(caller_contract_name.clone()), "*".to_string());
        eprintln!("[Find UsingFor] Checking wildcard key: {:?}", wildcard_key); // DEBUG
        if let Some(libs) = ctx.using_for_directives.get(&wildcard_key) {
            eprintln!(
                "[Find UsingFor]   Found wildcard directive for contract '{}': {:?}",
                caller_contract_name, libs
            );
            potential_libraries.extend(libs.iter().cloned());
        }
    } else {
        eprintln!(
            "[Find UsingFor]   No caller contract name provided. Skipping contract-specific directives."
        );
    }
    // TODO: Check global 'using for' directives (key: (None, Type) and (None, "*"))

    eprintln!(
        "[Find UsingFor]   All potential libraries before dedup: {:?}",
        potential_libraries
    );

    // Remove duplicates and check libraries
    potential_libraries.sort_unstable();
    potential_libraries.dedup();

    eprintln!(
        "[Find UsingFor]   Potential libraries after dedup: {:?}", // DEBUG
        potential_libraries
    );

    if !potential_libraries.is_empty() {
        eprintln!(
            "[Find UsingFor] Checking libraries for '{}.{}': {:?}",
            object_type_name,
            property_name,
            potential_libraries // Log original type name here
        );
        for library_name in potential_libraries {
            let library_method_key = (Some(library_name.clone()), property_name.to_string());
            eprintln!(
                "[Find UsingFor]   Checking library key: {:?}",
                library_method_key
            ); // DEBUG
            if let Some(id) = graph.node_lookup.get(&library_method_key) {
                if let Some(node) = graph.nodes.get(*id) {
                    // Found a match in a library
                    eprintln!(
                        "[Find UsingFor]   Found match in library '{}': Node ID {}",
                        library_name, id
                    );
                    // Solidity ambiguity rules usually prevent multiple libs defining the same function for the same type.
                    // Return the first one found.
                    return Ok(Some(ResolvedTarget::Function {
                        contract_name: Some(library_name.clone()), // Library name acts as scope
                        function_name: property_name.to_string(),
                        node_type: node.node_type.clone(), // Should be Function
                    }));
                }
            } else {
                eprintln!(
                    "[Find UsingFor]     Lookup failed for key: {:?}",
                    library_method_key
                ); // DEBUG
            }
        }
    }
    eprintln!("[Find UsingFor]   No match found via 'using for'."); // DEBUG
    Ok(None) // Not found via using for
}

/// Parses a mapping type string to extract its final (potentially nested) value type.
/// e.g., "mapping(address => mapping(uint => IMyInterface))" -> "IMyInterface"
/// e.g., "mapping(address => uint)" -> "uint"
/// e.g., "uint" -> "uint"
fn parse_mapping_final_value_type(type_str: &str) -> Result<String, String> {
    let trimmed_type_str = type_str.trim();
    if trimmed_type_str.starts_with("mapping(") && trimmed_type_str.ends_with(')') {
        // Find the main "=>" separating key type from value type.
        // We need to balance parentheses to correctly find the top-level arrow.
        let mut balance = 0;
        let mut arrow_index_opt: Option<usize> = None;

        // Iterate from after "mapping(" up to before the final ")"
        let content_start = "mapping(".len();
        let content_end = trimmed_type_str.len() - ")".len();

        if content_start >= content_end {
            return Err(format!("Invalid mapping string (too short): {}", type_str));
        }

        for (i, char_code) in trimmed_type_str
            .char_indices()
            .skip(content_start)
            .take(content_end - content_start)
        {
            if char_code == '(' {
                balance += 1;
            } else if char_code == ')' {
                balance -= 1;
                if balance < 0 {
                    // Unbalanced parentheses before finding arrow
                    return Err(format!(
                        "Malformed mapping string (unbalanced parentheses): {}",
                        type_str
                    ));
                }
            } else if char_code == '=' && balance == 0 {
                // Check if next char is '>'
                if trimmed_type_str.chars().nth(i + 1) == Some('>') {
                    arrow_index_opt = Some(i);
                    break;
                }
            }
        }

        if let Some(arrow_idx) = arrow_index_opt {
            // Ensure "=>" is followed by something.
            if arrow_idx + 2 >= content_end {
                return Err(format!(
                    "Malformed mapping string (no value type after '=>'): {}",
                    type_str
                ));
            }
            let value_type_part = trimmed_type_str[arrow_idx + 2..content_end].trim();
            // Recursively parse the value part, as it could be another mapping
            parse_mapping_final_value_type(value_type_part)
        } else {
            Err(format!(
                "Malformed mapping string (could not find top-level '=>'): {}",
                type_str
            ))
        }
    } else {
        // Base case: not a mapping, so this is the final value type
        Ok(trimmed_type_str.to_string())
    }
}

/// Resolves the target of a simple call (identifier only).
fn resolve_simple_call_v2<'a>(
    name: &str,
    caller_contract_name_opt: &'a Option<String>,
    graph: &'a CallGraph,
    ctx: &CallGraphGeneratorContext,
) -> std::result::Result<ResolvedTarget, TypeError> {
    // Use full path to Result
    eprintln!(
        "[Resolve Simple V2] Resolving simple call '{}', CallerScope='{:?}'",
        name, caller_contract_name_opt
    );

    // 1. Look within the current contract scope (if any)
    if let Some(contract_name) = caller_contract_name_opt {
        let key = (Some(contract_name.clone()), name.to_string());
        if let Some(id) = graph.node_lookup.get(&key) {
            if let Some(node) = graph.nodes.get(*id) {
                eprintln!(
                    "[Resolve Simple V2]   Found in contract scope: Node ID {}, Type: {:?}",
                    id, node.node_type
                );
                return Ok(ResolvedTarget::Function {
                    contract_name: node.contract_name.clone(),
                    function_name: node.name.clone(),
                    node_type: node.node_type.clone(),
                });
            }
        }
        // --- Start: Check inherited contracts ---
        if let Some(inherited_names) = ctx.contract_inherits.get(contract_name) {
            eprintln!(
                "[Resolve Simple V2]   Checking inheritance for '{}': {:?}",
                contract_name, inherited_names
            );
            for base_name in inherited_names {
                let base_key = (Some(base_name.clone()), name.to_string());
                if let Some(id) = graph.node_lookup.get(&base_key) {
                    if let Some(node) = graph.nodes.get(*id) {
                        eprintln!("[Resolve Simple V2]     Found in base contract '{}': Node ID {}, Type: {:?}", base_name, id, node.node_type);
                        // TODO: Handle potential ambiguity if found in multiple bases? For now, return first found.
                        // TODO: Handle visibility checks for inherited functions.
                        return Ok(ResolvedTarget::Function {
                            contract_name: node.contract_name.clone(), // Use the base contract's name
                            function_name: node.name.clone(),
                            node_type: node.node_type.clone(),
                        });
                    }
                }
            }
        }
    }

    // 2. Look for a free function (no contract scope)
    let free_function_key = (None, name.to_string());
    if let Some(id) = graph.node_lookup.get(&free_function_key) {
        if let Some(node) = graph.nodes.get(*id) {
            eprintln!(
                "[Resolve Simple V2]   Found free function: Node ID {}, Type: {:?}",
                id, node.node_type
            );
            return Ok(ResolvedTarget::Function {
                contract_name: None,
                function_name: node.name.clone(),
                node_type: node.node_type.clone(),
            });
        }
    }

    // 3. Check if it's a constructor call (matches a contract name)
    if ctx.all_contracts.contains_key(name) {
        let constructor_key = (Some(name.to_string()), name.to_string());
        if graph.node_lookup.contains_key(&constructor_key) {
            eprintln!(
                "[Resolve Simple V2]   Found constructor for contract '{}'",
                name
            );
            return Ok(ResolvedTarget::Function {
                contract_name: Some(name.to_string()),
                function_name: name.to_string(),
                node_type: NodeType::Constructor,
            });
        }
    }

    // Not found
    eprintln!(
        "[Resolve Simple V2]   Simple call target '{}' not found.",
        name
    );
    Err(TypeError::TargetResolutionFailed {
        name: name.to_string(),
        reason: "Function or constructor not found in current scope or globally.".to_string(),
    })
}

/// Extracts argument text from a call expression or emit statement node.
fn extract_arguments_v2<'a>(node: TsNode<'a>, source: &'a str) -> Vec<String> {
    let mut argument_texts: Vec<String> = Vec::new();
    let mut cursor = node.walk();
    for child_node in node.children(&mut cursor) {
        // Arguments are typically within a node like 'arguments' or directly as 'call_argument'
        let mut search_queue = VecDeque::new();
        search_queue.push_back(child_node);

        while let Some(current) = search_queue.pop_front() {
            if current.kind() == "call_argument" {
                if let Some(expression_node) = current.child(0) {
                    argument_texts.push(get_node_text(&expression_node, source).to_string());
                }
            } else {
                // Recursively check children if not a direct argument node
                let mut inner_cursor = current.walk();
                for inner_child in current.children(&mut inner_cursor) {
                    // Avoid infinite loops, maybe limit depth or check node kind
                    if inner_child.kind() != node.kind() {
                        // Simple cycle check
                        search_queue.push_back(inner_child);
                    }
                }
            }
        }
    }
    argument_texts
}

/// Attempts to parse the return type string from a function definition node (V2).
/// Handles single types, basic tuples, and named return parameters.
fn get_function_return_type_v2(
    definition_node_info: &NodeInfo,
    input: &CallGraphGeneratorInput, // Add input
) -> std::result::Result<Option<String>, TypeError> {
    // Use full path to Result
    // Query to find the actual type name node within the standard return structure.
    // Handles simple cases like `returns (uint)`. Does not handle named returns yet.
    // Added pattern for interface function definitions.
    let query_str = r#"
        (function_definition
          return_type: (return_type_definition
            (parameter type: (type_name) @return_type_name_node)
          )
        )

        ; Interface function definitions might have a slightly different structure
        ; but the return type part should be similar.
        (interface_declaration (contract_body (function_definition
          return_type: (return_type_definition
            (parameter type: (type_name) @return_type_name_node))
        )))
    "#;
    // Use lang and source from input
    let query = Query::new(&input.solidity_lang, query_str)?;
    let mut cursor = QueryCursor::new();
    let source_bytes = input.source.as_bytes();

    // Retrieve the TsNode using the span from NodeInfo and the tree from input
    let definition_ts_node = input
        .tree
        .root_node()
        .descendant_for_byte_range(definition_node_info.span.0, definition_node_info.span.1)
        .ok_or_else(|| {
            // Use TypeError::Internal or a more specific error
            TypeError::Internal(format!(
                "Failed to find definition TsNode for span {:?}",
                definition_node_info.span
            ))
        })?;

    let mut matches = cursor.matches(
        &query,
        definition_ts_node, // Query only within this node
        |node: TsNode| std::iter::once(&source_bytes[node.byte_range()]),
    );
    // Use while let Some() for streaming iterator
    while let Some(match_) = matches.next() {
        // Find the @return_type_name_node capture
        for capture in match_.captures {
            // Use capture_names() slice to get the name by index
            if query.capture_names()[capture.index as usize] == "return_type_name_node" {
                let type_name_node = capture.node;
                let type_name_text = get_node_text(&type_name_node, &input.source).to_string();

                if !type_name_text.is_empty() {
                    eprintln!(
                        "[Get Return Type V2] Found single return type name: '{}'",
                        type_name_text
                    );
                    // TODO: Handle multiple return types if the query is extended later
                    return Ok(Some(type_name_text)); // Return the captured type name text
                } else {
                    eprintln!("[Get Return Type V2] Found empty return type name node.");
                    // Fall through to return None if text is empty
                }
            }
        }
    }

    eprintln!("[Get Return Type V2] No return type name node captured or parsed.");
    Ok(None) // No return type found or parsed
}

/// Resolves the return type of a call based on its ResolvedTarget.
fn resolve_call_return_type<'a>(
    target: &ResolvedTarget,
    ctx: &CallGraphGeneratorContext,
    graph: &'a CallGraph,
    source: &'a str,
    solidity_lang: &'a tree_sitter::Language,
    input: &'a CallGraphGeneratorInput, // Pass input for potential use in future
) -> std::result::Result<Option<String>, TypeError> {
    // Use full path to Result
    match target {
        ResolvedTarget::Function {
            contract_name,
            function_name,
            node_type: _,
        } => {
            // Use _ for node_type as we check graph
            let lookup_key = (contract_name.clone(), function_name.clone());
            if let Some(node_id) = graph.node_lookup.get(&lookup_key) {
                let graph_node = &graph.nodes[*node_id]; // This is the node found by (Scope, Name)

                match graph_node.node_type {
                    NodeType::Function | NodeType::Modifier | NodeType::Constructor => {
                        // Prioritize graph_node.declared_return_type if available
                        if let Some(declared_type) = &graph_node.declared_return_type {
                            eprintln!("[Resolve Call Return Type] Using declared_return_type '{}' for Node ID {}", declared_type, *node_id);
                            return Ok(Some(declared_type.clone()));
                        }
                        // Fallback to parsing TsNode via definition_nodes_info
                        let def_node_info_opt = ctx
                            .definition_nodes_info
                            .iter()
                            .find(|(id, _, _)| *id == *node_id)
                            .map(|(_, info, _)| info.clone());
                        if let Some(def_node_info) = def_node_info_opt {
                            eprintln!("[Resolve Call Return Type] Falling back to get_function_return_type_v2 for Node ID {}", *node_id);
                            return get_function_return_type_v2(&def_node_info, input);
                        }
                        eprintln!("[Resolve Call Return Type] No declared_return_type and no NodeInfo for Node ID {}", *node_id);
                        return Ok(None);
                    }
                    NodeType::StorageVariable => {
                        if graph_node.visibility == crate::cg::Visibility::Public {
                            let actual_contract_name =
                                graph_node.contract_name.as_ref().ok_or_else(|| {
                                    TypeError::Internal(
                                        "StorageVariable node missing contract name".to_string(),
                                    )
                                })?;
                            let var_name = &graph_node.name;
                            eprintln!("[Resolve Call Return Type] ResolvedTarget::Function points to public StorageVariable '{}.{}'. Determining getter return type.", actual_contract_name, var_name);

                            // For mappings, the getter returns the final value type.
                            if let Some(mapping_info) = ctx
                                .contract_mappings
                                .get(&(actual_contract_name.clone(), var_name.clone()))
                            {
                                eprintln!("[Resolve Call Return Type]   Found mapping info. Value type: '{}'", mapping_info.value_type);
                                return Ok(Some(mapping_info.value_type.clone()));
                            }
                            // For other public state variables, the getter returns the variable's type.
                            if let Some(type_str) = ctx
                                .state_var_types
                                .get(&(actual_contract_name.clone(), var_name.clone()))
                            {
                                eprintln!("[Resolve Call Return Type]   Found state_var_types info. Type: '{}'", type_str);
                                return Ok(Some(type_str.clone()));
                            }
                            eprintln!("[Resolve Call Return Type]   Public StorageVariable {}.{} type info not found in contract_mappings or state_var_types.", actual_contract_name, var_name);
                            return Err(TypeError::Internal(format!(
                                "Public StorageVariable {}.{} type info not found.",
                                actual_contract_name, var_name
                            )));
                        } else {
                            eprintln!("[Resolve Call Return Type] ResolvedTarget::Function points to non-public StorageVariable '{}.{}'. No getter.", graph_node.contract_name.as_deref().unwrap_or("?"), graph_node.name);
                            return Ok(None);
                        }
                    }
                    _ => {
                        eprintln!("[Resolve Call Return Type] Warning: Node for '{:?}' (ID {}) is of unexpected type {:?}.", lookup_key, *node_id, graph_node.node_type);
                        return Ok(None);
                    }
                }
            } else {
                eprintln!("[Resolve Call Return Type] Warning: Node for '{:?}' not found in graph.node_lookup.", lookup_key);
                return Ok(None);
            }
        }
        ResolvedTarget::InterfaceMethod {
            interface_name,
            method_name,
            implementation,
        } => {
            // If a concrete implementation was found, use its return type.
            if let Some(impl_target) = implementation {
                resolve_call_return_type(impl_target, ctx, graph, source, solidity_lang, input)
            } else {
                // Find the interface method definition node and parse its return type
                let _iface_key = (Some(interface_name.clone()), interface_name.clone()); // Key for interface node itself (Mark as unused for now)
                let method_key = (Some(interface_name.clone()), method_name.clone()); // Key for method node
                if let Some(method_node_id) = graph.node_lookup.get(&method_key) {
                    let def_node_opt = ctx
                        .definition_nodes_info
                        .iter()
                        .find(|(id, _, _)| *id == *method_node_id)
                        .map(|(_, n, _)| n.clone());
                    if let Some(def_node) = def_node_opt {
                        get_function_return_type_v2(&def_node, input)
                    } else {
                        Ok(None) // Cannot find definition node for interface method
                    }
                } else {
                    Ok(None) // Interface method not found in graph
                }
            }
        }
        ResolvedTarget::BuiltIn { object_type, name } => {
            // Use the builtin module to get the return type string
            if let Some(return_type_str) = builtin::get_builtin_return_type(name, object_type) {
                if return_type_str == "void" {
                    Ok(None) // Map "void" to None
                } else {
                    Ok(Some(return_type_str.to_string()))
                }
            } else {
                eprintln!(
                    "[Resolve Return Type] Warning: Could not find return type for known built-in {}.{}",
                    object_type, name
                );
                Ok(None) // Fallback if lookup fails for some reason
            }
        }
        ResolvedTarget::NotCallable { .. } => Ok(None), // Not callable, no return type
        ResolvedTarget::External { .. } => Ok(None), // Cannot determine return type of external call
        ResolvedTarget::TypeCast { type_name } => {
            // The result type of a type cast is the type name itself.
            Ok(Some(type_name.clone()))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cg::{
        CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorPipeline,
    }; // Import necessary items
    use crate::steps::CallsHandling;
    use crate::steps::ContractHandling;
    use anyhow::{Context, Result};
    use language::{Language, Solidity};
    use std::collections::HashMap;
    // use std::sync::Arc; // Removed unused import
    use tree_sitter::{Parser, Tree};

    // Test-specific imports for manifest and binding
    use crate::interface_resolver::{BindingConfig, BindingRegistry}; // Removed BindingFile
    use crate::manifest::{Manifest, ManifestEntry};
    use crate::natspec::extract::SourceItemKind as NatspecSourceItemKind;
    use crate::natspec::{TextIndex as NatspecTextIndex, TextRange as NatspecTextRange};

    // Helper to create a basic context and graph for testing
    fn setup_test_environment(
        source: &str,
    ) -> Result<(
        CallGraphGeneratorContext,
        CallGraph,
        Tree,
        tree_sitter::Language,
        CallGraphGeneratorInput,
    )> {
        let solidity_lang = Solidity.get_tree_sitter_language();
        let mut parser = Parser::new();
        parser
            .set_language(&solidity_lang)
            .context("Failed to set language")?;
        let tree = parser
            .parse(source, None)
            .context("Failed to parse source code")?;

        let mut ctx = CallGraphGeneratorContext::default();
        let mut graph = CallGraph::new();

        // --- Populate context and graph using the pipeline ---

        let input = CallGraphGeneratorInput {
            source: source.to_string(),
            tree: tree.clone(),
            solidity_lang: solidity_lang.clone(),
        };
        let input_clone = input.clone();
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default()));
        pipeline.add_step(Box::new(CallsHandling::default()));
        let config: HashMap<String, String> = HashMap::new(); // Empty config for tests
        pipeline
            .run(input, &mut ctx, &mut graph, &config)
            .context("Pipeline execution failed")?;
        // --- End population ---

        Ok((
            ctx.clone(),
            graph.clone(),
            tree.clone(),
            solidity_lang.clone(),
            input_clone, // Return the input for use in tests
        ))
    }

    // More controlled setup for specific manifest/binding tests
    fn setup_test_environment_customized(
        source: &str,
        custom_manifest: Option<Manifest>,
        custom_binding_registry: Option<BindingRegistry>,
        // We still run parts of the pipeline to populate basic graph structure
        // like nodes for contracts and functions.
    ) -> Result<(
        CallGraphGeneratorContext,
        CallGraph,
        Tree,
        tree_sitter::Language,
        CallGraphGeneratorInput,
    )> {
        let solidity_lang = Solidity.get_tree_sitter_language();
        let mut parser = Parser::new();
        parser
            .set_language(&solidity_lang)
            .context("Failed to set language")?;
        let tree = parser
            .parse(source, None)
            .context("Failed to parse source code")?;

        let mut ctx = CallGraphGeneratorContext::default();
        let mut graph = CallGraph::new();

        // Populate from custom arguments
        if let Some(manifest) = custom_manifest {
            ctx.manifest = Some(manifest);
        }
        if let Some(registry) = custom_binding_registry {
            ctx.binding_registry = Some(registry);
        }

        // Run a minimal pipeline to get basic graph structure (contracts, functions)
        // This will also populate ctx.all_contracts, ctx.all_interfaces, etc.
        // and graph.node_lookup, graph.nodes.
        let input = CallGraphGeneratorInput {
            source: source.to_string(),
            tree: tree.clone(),
            solidity_lang: solidity_lang.clone(),
        };
        let input_clone = input.clone();
        let mut pipeline = CallGraphGeneratorPipeline::new();
        pipeline.add_step(Box::new(ContractHandling::default())); // Gets contract/interface/function nodes
                                                                  // We might not need CallsHandling if we are testing resolution logic directly
                                                                  // and not relying on its graph modifications.
                                                                  // pipeline.add_step(Box::new(CallsHandling::default()));
        let config: HashMap<String, String> = HashMap::new();
        pipeline
            .run(input, &mut ctx, &mut graph, &config)
            .context("Minimal pipeline execution failed for custom setup")?;

        // Crucially, after the pipeline runs and populates ctx.all_contracts etc.,
        // we might need to re-apply our custom manifest and registry if the pipeline
        // overwrote or modified them in an undesired way for the test's specific scenario.
        // For this test, ContractHandling should populate all_contracts/interfaces correctly.
        // The binding registry and manifest are primarily *inputs* to the chains.rs logic.

        Ok((
            ctx.clone(),
            graph.clone(),
            tree.clone(),
            solidity_lang.clone(),
            input_clone,
        ))
    }
    // Helper to find a specific node within the source code for testing
    fn find_node_by_kind_and_text<'a>(
        tree: &'a Tree,
        source: &'a str,
        kind: &str,
        text: &str,
    ) -> Option<TsNode<'a>> {
        let mut cursor = tree.walk();
        loop {
            let node = cursor.node();
            if node.kind() == kind && get_node_text(&node, source) == text {
                return Some(node);
            }
            if !cursor.goto_first_child() {
                while !cursor.goto_next_sibling() {
                    if !cursor.goto_parent() {
                        return None; // Reached root without finding
                    }
                }
            }
        }
    }

    // Helper to find a function definition node by its name identifier using a query
    fn find_function_definition_node_by_name<'a>(
        tree: &'a Tree,
        source: &'a str,
        lang: &'a tree_sitter::Language,
        function_name: &str,
    ) -> Result<TsNode<'a>> {
        // Use anyhow::Result
        let query_str = r#"
            (function_definition
              name: (identifier) @function_name
            )
        "#;
        let query = Query::new(lang, query_str).context("Failed to create function name query")?;
        let mut cursor = QueryCursor::new();
        let source_bytes = source.as_bytes();

        let mut matches = cursor.matches(&query, tree.root_node(), |node: TsNode| {
            std::iter::once(&source_bytes[node.byte_range()])
        });

        // Use while let Some() for streaming iterator
        while let Some(match_) = matches.next() {
            for capture in match_.captures {
                // Use capture_names() slice to get the name by index
                if query.capture_names()[capture.index as usize] == "function_name" {
                    let name_node = capture.node;
                    if get_node_text(&name_node, source) == function_name {
                        // Found the name node, now get its parent (the function_definition)
                        return name_node.parent().ok_or_else(|| {
                            anyhow::anyhow!(
                                // Use anyhow::anyhow!
                                "Failed to get parent of function name identifier '{}'",
                                function_name
                            )
                        });
                    }
                }
            }
        }

        Err(anyhow::anyhow!(
            // Use anyhow::anyhow!
            "Function definition node for '{}' not found",
            function_name
        ))
    }

    // Helper to find the Nth node of a specific kind
    fn find_nth_node_of_kind<'a>(tree: &'a Tree, kind: &str, n: usize) -> Option<TsNode<'a>> {
        let mut cursor = tree.walk();
        let mut count = 0;
        loop {
            let node = cursor.node();
            if node.kind() == kind {
                if count == n {
                    return Some(node);
                }
                count += 1;
            }
            if !cursor.goto_first_child() {
                while !cursor.goto_next_sibling() {
                    if !cursor.goto_parent() {
                        return None; // Reached root without finding
                    }
                }
            }
        }
    }

    // Helper to find the Nth descendant node of a specific kind using a query (Revised with captures)
    fn find_nth_descendant_node_of_kind<'a>(
        start_node: &TsNode<'a>, // Start search from this node
        source: &'a str,
        lang: &'a tree_sitter::Language,
        kind: &str,
        n: usize,
    ) -> Result<Option<TsNode<'a>>> {
        // Return Result<Option<...>>
        let query_str = format!("({}) @target_kind", kind);
        let query = Query::new(lang, &query_str)
            .with_context(|| format!("Failed to create query for kind '{}'", kind))?;
        let mut cursor = QueryCursor::new();
        let source_bytes = source.as_bytes();

        // Use captures which iterates through all captures for all matches within the start_node
        let mut captures = cursor.captures(
            &query,
            start_node.clone(), // Query within the start_node
            |node: TsNode| std::iter::once(&source_bytes[node.byte_range()]),
        );

        let mut count = 0;
        // Use while let Some to iterate over QueryCaptures
        while let Some((match_, capture_index)) = captures.next() {
            // We only have one capture name "@target_kind", index 0
            // Dereference capture_index for comparison
            if *capture_index == 0 {
                // Dereference capture_index for indexing
                let node = match_.captures[*capture_index].node; // Get the node from the capture
                eprintln!(
                    // DEBUG
                    "[find_nth_descendant] Found node: Kind='{}', Span=({},{}), Count={}",
                    node.kind(),
                    node.start_byte(),
                    node.end_byte(),
                    count
                );
                if count == n {
                    eprintln!("[find_nth_descendant] Returning node at index {}", n); // DEBUG
                    return Ok(Some(node)); // Found the nth descendant
                }
                count += 1;
            }
        }
        eprintln!(
            "[find_nth_descendant] Node of kind '{}' at index {} not found.",
            kind, n
        ); // DEBUG
        Ok(None) // Nth descendant not found
    }

    #[test]
    fn test_resolve_simple_identifier_type() -> Result<()> {
        // Use our own Result type
        let source = r#"
            contract Test {
                uint256 stateVar;
                bool flag;
                function testFunc(uint256 local) public {
                    stateVar = 1; // Access state var
                    flag = true;  // Access state var
                    uint256 x = local; // Access local var
                }
            }
        "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;
        // Find the first function definition node (testFunc)
        let _func_def_node = find_nth_node_of_kind(&tree, "function_definition", 0)
            .expect("Could not find function definition node for testFunc");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "testFunc".to_string()))
            .copied()
            .unwrap();

        // Find the identifier nodes within the function body
        let state_var_node =
            find_node_by_kind_and_text(&tree, source, "identifier", "stateVar").unwrap();
        let flag_node = find_node_by_kind_and_text(&tree, source, "identifier", "flag").unwrap();
        // let local_node = find_node_by_kind_and_text(&tree, source, "identifier", "local").unwrap(); // Need 2nd instance

        // Resolve state variables
        let type1 = resolve_expression_type_v2(
            state_var_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
        )?;
        assert_eq!(type1, Some("uint256".to_string()));

        let type2 = resolve_expression_type_v2(
            flag_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
        )?;
        assert_eq!(type2, Some("bool".to_string()));

        // TODO: Test local variable resolution once implemented
        // let type3 = resolve_expression_type_v2(local_node, caller_node_id, &Some("Test".to_string()), &ctx, &graph, source, &lang)?;
        // assert_eq!(type3, Some("uint256".to_string()));

        Ok(())
    }

    #[test]
    fn test_resolve_member_access_type() -> Result<()> {
        // Use our own Result type
        let source = r#"
            contract C { uint public x; }
            contract Test {
                C c_instance;
                function getX() public view returns (uint) {
                    return c_instance.x; // Member access
                }
            }
        "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;
        let _func_node = find_nth_node_of_kind(&tree, "function_definition", 0)
            .expect("Could not find function definition node for getX");
        // Find the member_expression node for c_instance.x
        let member_expr_node = find_nth_node_of_kind(&tree, "member_expression", 0)
            .expect("Could not find the first member_expression node (c_instance.x)"); // Add expect
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "getX".to_string()))
            .copied()
            .unwrap();

        // Find the member_expression node for c_instance.x
        let member_expr_node = find_nth_node_of_kind(&tree, "member_expression", 0)
            .expect("Could not find the first member_expression node (c_instance.x)"); // Add expect

        let resolved_type = resolve_expression_type_v2(
            member_expr_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
        )?;
        assert_eq!(resolved_type, Some("uint".to_string()));

        Ok(())
    }

    #[test]
    fn test_analyze_simple_call() -> Result<()> {
        // Use our own Result type
        let source = r#"
            contract Test {
                function target() internal pure returns (uint) { return 1; }
                function caller() public pure {
                    target(); // Simple call
                }
            }
        "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;
        // Use the new helper function to find the function definition node
        let _caller_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "caller")
                .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the call_expression node for target()
        let call_expr_node = find_nth_node_of_kind(&tree, "call_expression", 0)
            .expect("Could not find the call_expression node for target()"); // Add expect

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,                        // This is the top-level call
            call_expr_node.start_byte(), // Add originating_span_start
        )?;

        assert_eq!(steps.len(), 1);
        let step = &steps[0];
        assert_eq!(step.object_type, None);
        assert_eq!(step.result_type, Some("uint".to_string())); // target returns uint
        assert!(step.arguments.is_empty());
        match &step.target {
            ResolvedTarget::Function {
                contract_name,
                function_name,
                node_type,
            } => {
                assert_eq!(contract_name.as_deref(), Some("Test"));
                assert_eq!(function_name, "target");
                assert_eq!(*node_type, NodeType::Function);
            }
            _ => panic!("Expected ResolvedTarget::Function, got {:?}", step.target),
        }

        Ok(())
    }

    #[test]
    fn test_analyze_member_call() -> Result<()> {
        // Use our own Result type
        let source = r#"
             contract C {
                 function method() public pure returns (bool) { return true; }
             }
             contract Test {
                 C c_instance;
                 function caller() public {
                     c_instance.method(); // Member call
                 }
             }
         "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;
        // Use the correct helper to find the function definition node by its name
        let caller_def_node = find_function_definition_node_by_name(&tree, source, &lang, "caller")
            .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the call_expression node for c_instance.method() within the caller function
        // Use the new helper function starting from the caller's definition node
        let call_expr_node_opt = find_nth_descendant_node_of_kind(
            // Changed to Option
            &caller_def_node, // Start search within the caller function node
            source,
            &lang,
            "call_expression", // Kind to find
            0,                 // Find the first one (index 0)
        )?; // Propagate Result error

        let call_expr_node = call_expr_node_opt.ok_or_else(|| { // Convert Option to Result
                TypeError::Internal(format!(
                    "Could not find the call_expression node for c_instance.method() within caller function node: {:?}",
                    caller_def_node.byte_range()
                ))
            })?; // Propagate Option error

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(), // Add originating_span_start
        )?;

        assert_eq!(steps.len(), 1);
        let step = &steps[0];
        assert_eq!(step.object_type, Some("C".to_string())); // Type of c_instance is C
        assert_eq!(step.result_type, Some("bool".to_string())); // method returns bool
        assert!(step.arguments.is_empty());
        match &step.target {
            ResolvedTarget::Function {
                contract_name,
                function_name,
                node_type,
            } => {
                assert_eq!(contract_name.as_deref(), Some("C"));
                assert_eq!(function_name, "method");
                assert_eq!(*node_type, NodeType::Function);
            }
            _ => panic!("Expected ResolvedTarget::Function, got {:?}", step.target),
        }

        Ok(())
    }

    #[test]
    fn test_analyze_using_for_call() -> Result<()> {
        // Use our own Result type
        let source = r#"
            library SafeMath {
                function add(uint a, uint b) internal pure returns (uint) { return a+b; }
            }
            contract Test {
                using SafeMath for uint;
                uint value;
                function caller(uint y) public {
                    value = value.add(y); // Using for call
                }
             }
         "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;
        let _caller_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "caller")
                .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the call_expression node for value.add(y)
        let call_expr_node = find_nth_node_of_kind(&tree, "call_expression", 0).unwrap();

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(), // Add originating_span_start
        )?;

        assert_eq!(steps.len(), 1);
        let step = &steps[0];
        // The state var 'value' is declared as 'uint', so the resolved object type is "uint"
        assert_eq!(step.object_type, Some("uint".to_string()));
        assert_eq!(step.result_type, Some("uint".to_string())); // add returns uint
        assert_eq!(step.arguments, vec!["y".to_string()]);
        match &step.target {
            ResolvedTarget::Function {
                contract_name,
                function_name,
                node_type,
            } => {
                assert_eq!(contract_name.as_deref(), Some("SafeMath")); // Resolved to library
                assert_eq!(function_name, "add");
                assert_eq!(*node_type, NodeType::Function);
            }
            _ => panic!(
                "Expected ResolvedTarget::Function (Library), got {:?}",
                step.target
            ),
        }

        Ok(())
    }

    #[test]
    fn test_analyze_interface_call_single_impl() -> Result<()> {
        // Use our own Result type
        let source = r#"
            interface ICounter { function increment() external; }
            contract Counter is ICounter { function increment() external override {} }
            contract Test {
                ICounter c;
                function caller() public {
                    c.increment(); // Interface call
                }
            }
        "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;
        let caller_def_node = find_function_definition_node_by_name(&tree, source, &lang, "caller")
            .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the call_expression node for c.increment()
        let call_expr_node = find_nth_node_of_kind(&tree, "call_expression", 0).unwrap();

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(), // Add originating_span_start
        )?;

        assert_eq!(steps.len(), 1);
        let step = &steps[0];
        assert_eq!(step.object_type, Some("ICounter".to_string())); // Type of c is ICounter
        assert_eq!(step.result_type, None); // increment returns nothing
        assert!(step.arguments.is_empty());
        match &step.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(interface_name, "ICounter");
                assert_eq!(method_name, "increment");
                assert!(implementation.is_some());
                // Check the implementation details
                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name,
                        function_name,
                        node_type,
                    }) => {
                        assert_eq!(contract_name.as_deref(), Some("Counter")); // Resolved to implementation
                        assert_eq!(function_name, "increment");
                        assert_eq!(*node_type, NodeType::Function);
                    }
                    _ => panic!(
                        "Expected implementation to be ResolvedTarget::Function, got {:?}",
                        implementation
                    ),
                }
            }
            _ => panic!(
                "Expected ResolvedTarget::InterfaceMethod, got {:?}",
                step.target
            ),
        }

        Ok(())
    }

    #[test]
    fn test_analyze_chained_call_using_for() -> Result<()> {
        // Use our own Result type
        let source = r#"
            library SafeMath {
                function add(uint a, uint b) internal pure returns (uint) { return a+b; }
                function sub(uint a, uint b) internal pure returns (uint) { return a-b; }
            }
            contract Test {
                using SafeMath for uint;
                uint value;
                function caller(uint x, uint y) public {
                     value = value.add(x).sub(y); // Chained using for call
                 }
            }
        "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;

        let _caller_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "caller")
                .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the outer call_expression node for value.add(x).sub(y) by navigating from the assignment
        // Re-fetch caller_def_node as it was prefixed with _
        let caller_def_node_for_search =
            find_function_definition_node_by_name(&tree, source, &lang, "caller")
                .expect("Could not find function definition node for caller (for search)");
        let assignment_node = find_nth_descendant_node_of_kind(
            &caller_def_node_for_search, // Search within the caller function
            source,
            &lang,
            "assignment_expression", // Kind to find
            0,                       // Find the first one
        )
        .expect("Failed to query for assignment node")
        .expect("Could not find the assignment_expression node within caller");

        // Navigate: assignment -> right: expression -> child(0): call_expression
        let outer_call_expr_node = assignment_node
            .child_by_field_name("right")
            .expect("Assignment node missing 'right' child")
            .child(0)
            .expect("Assignment 'right' expression missing child(0)");

        assert_eq!(
            outer_call_expr_node.kind(),
            "call_expression",
            "Navigated node is not a call_expression"
        );
        eprintln!(
            "DEBUG [Test]: Analyzing node kind='{}', text='{}'",
            outer_call_expr_node.kind(),
            get_node_text(&outer_call_expr_node, source)
        );

        let steps = analyze_chained_call(
            outer_call_expr_node, // Use the navigated node
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            outer_call_expr_node.start_byte(), // Add originating_span_start
        )?;

        // Should have two steps: add, then sub
        assert_eq!(steps.len(), 2);

        // Step 1: add
        let step1 = &steps[0];
        assert_eq!(step1.object_type, Some("uint".to_string())); // value is declared as uint
        assert_eq!(step1.result_type, Some("uint".to_string())); // add returns uint
        assert_eq!(step1.arguments, vec!["x".to_string()]);
        match &step1.target {
            ResolvedTarget::Function {
                contract_name,
                function_name,
                ..
            } => {
                assert_eq!(contract_name.as_deref(), Some("SafeMath"));
                assert_eq!(function_name, "add");
            }
            _ => panic!("Step 1: Expected Library Function, got {:?}", step1.target),
        }

        // Step 2: sub
        let step2 = &steps[1];
        assert_eq!(step2.object_type, Some("uint".to_string())); // Object is the result of add (uint)
        assert_eq!(step2.result_type, Some("uint".to_string())); // sub returns uint
        assert_eq!(step2.arguments, vec!["y".to_string()]);
        match &step2.target {
            ResolvedTarget::Function {
                contract_name,
                function_name,
                ..
            } => {
                assert_eq!(contract_name.as_deref(), Some("SafeMath"));
                assert_eq!(function_name, "sub");
            }
            _ => panic!("Step 2: Expected Library Function, got {:?}", step2.target),
        }

        Ok(())
    }

    #[test]
    fn test_analyze_chained_call_interface_factory() -> Result<()> {
        // Use our own Result type
        let source = r#"
             interface IAction { function perform() external returns (bool); }
             contract Action is IAction { function perform() external override returns (bool) { return true; } }
             interface IFactory { function create() external returns (IAction); }
             contract Factory is IFactory { function create() external override returns (IAction) { return new Action(); } }
             contract Test {
                 IFactory factory;
                 function caller() public returns (bool) {
                     // factory.create().perform()
                     return factory.create().perform();
                 }
             }
         "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;

        let caller_def_node = find_function_definition_node_by_name(&tree, source, &lang, "caller")
            .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the outer call_expression node for (...).perform()
        let call_expr_node = find_nth_node_of_kind(&tree, "call_expression", 1).unwrap(); // Assuming perform is the second call

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(), // Add originating_span_start
        )?;

        // Should have two steps: create, then perform
        assert_eq!(steps.len(), 2);

        // Step 1: create
        let step1 = &steps[0];
        assert_eq!(step1.object_type, Some("IFactory".to_string())); // factory is IFactory
        assert_eq!(step1.result_type, Some("IAction".to_string())); // create returns IAction
        assert!(step1.arguments.is_empty());
        match &step1.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(interface_name, "IFactory");
                assert_eq!(method_name, "create");
                assert!(implementation.is_some()); // Should resolve to Factory.create
                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name,
                        function_name,
                        ..
                    }) => {
                        assert_eq!(contract_name.as_deref(), Some("Factory"));
                        assert_eq!(function_name, "create");
                    }
                    _ => panic!(
                        "Step 1: Expected implementation Function, got {:?}",
                        implementation
                    ),
                }
            }
            _ => panic!("Step 1: Expected InterfaceMethod, got {:?}", step1.target),
        }

        // Step 2: perform
        let step2 = &steps[1];
        assert_eq!(step2.object_type, Some("IAction".to_string())); // Object is the result of create (IAction)
        assert_eq!(step2.result_type, Some("bool".to_string())); // perform returns bool
        assert!(step2.arguments.is_empty());
        match &step2.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(interface_name, "IAction");
                assert_eq!(method_name, "perform");
                assert!(implementation.is_some()); // Should resolve to Action.perform
                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name,
                        function_name,
                        ..
                    }) => {
                        assert_eq!(contract_name.as_deref(), Some("Action"));
                        assert_eq!(function_name, "perform");
                    }
                    _ => panic!(
                        "Step 2: Expected implementation Function, got {:?}",
                        implementation
                    ),
                }
            }
            _ => panic!("Step 2: Expected InterfaceMethod, got {:?}", step2.target),
        }

        Ok(())
    }

    #[test]
    fn test_analyze_chained_call_inline_factory() -> Result<()> {
        // Variant of test_analyze_chained_call_interface_factory
        // Here, the factory is instantiated inline: `new Factory().create().perform()`
        let source = r#"
             interface IAction { function perform() external returns (bool); }
             contract Action is IAction { function perform() external override returns (bool) { return true; } }
             interface IFactory { function create() external returns (IAction); }
             contract Factory is IFactory { function create() external override returns (IAction) { return new Action(); } }
             contract Test {
                 // No factory state variable
                 function caller() public returns (bool) {
                     // Instantiate factory inline, then call create, then perform
                     return new Factory().create().perform();
                 }
             }
         "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;

        let caller_def_node = find_function_definition_node_by_name(&tree, source, &lang, "caller")
            .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the outermost call_expression node for (...).perform()
        // It should be the second call expression overall (first is create, second is perform)
        let outer_call_expr_node =
            find_nth_node_of_kind(&tree, "call_expression", 1) // Find the second call_expression
                .expect("Could not find the outer call_expression node for .perform()");

        // Debug: Verify we found the correct node
        eprintln!(
            "[Test Inline Factory] Analyzing node kind='{}', text='{}'",
            outer_call_expr_node.kind(),
            get_node_text(&outer_call_expr_node, source)
        );
        assert!(get_node_text(&outer_call_expr_node, source).contains(".perform()"));

        let steps = analyze_chained_call(
            outer_call_expr_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            outer_call_expr_node.start_byte(), // Add originating_span_start
        )?;

        // Should have three steps: new Factory(), then .create(), then .perform()
        assert_eq!(steps.len(), 3, "Expected 3 steps for inline factory chain");

        // Step 1: new Factory()
        let step1 = &steps[0];
        assert_eq!(
            step1.object_type, None,
            "Step 1 (new): object_type should be None"
        );
        assert_eq!(
            step1.result_type,
            Some("Factory".to_string()),
            "Step 1 (new): result_type should be Factory"
        );
        assert!(
            step1.arguments.is_empty(),
            "Step 1 (new): arguments should be empty"
        );
        match &step1.target {
            ResolvedTarget::Function {
                contract_name,
                function_name,
                node_type,
            } => {
                assert_eq!(
                    contract_name.as_deref(),
                    Some("Factory"),
                    "Step 1 (new): target contract"
                );
                assert_eq!(
                    function_name, "Factory",
                    "Step 1 (new): target function name (constructor)"
                );
                assert_eq!(
                    *node_type,
                    NodeType::Constructor,
                    "Step 1 (new): target node type"
                );
            }
            _ => panic!(
                "Step 1 (new): Expected Function (Constructor), got {:?}",
                step1.target
            ),
        }

        // Step 2: .create()
        let step2 = &steps[1];
        assert_eq!(
            step2.object_type,
            Some("Factory".to_string()),
            "Step 2 (create): object_type should be Factory"
        );
        assert_eq!(
            step2.result_type,
            Some("IAction".to_string()),
            "Step 2 (create): result_type should be IAction"
        );
        assert!(
            step2.arguments.is_empty(),
            "Step 2 (create): arguments should be empty"
        );
        match &step2.target {
            // Assuming resolution finds the concrete implementation directly or via InterfaceMethod
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(
                    interface_name, "IFactory",
                    "Step 2 (create): target interface name"
                );
                assert_eq!(method_name, "create", "Step 2 (create): target method name");
                assert!(
                    implementation.is_some(),
                    "Step 2 (create): implementation should be resolved"
                );
                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name,
                        function_name,
                        node_type,
                    }) => {
                        assert_eq!(
                            contract_name.as_deref(),
                            Some("Factory"),
                            "Step 2 (create): impl contract"
                        );
                        assert_eq!(
                            function_name, "create",
                            "Step 2 (create): impl function name"
                        );
                        assert_eq!(
                            *node_type,
                            NodeType::Function,
                            "Step 2 (create): impl node type"
                        );
                    }
                    _ => panic!(
                        "Step 2 (create): Expected implementation Function, got {:?}",
                        implementation
                    ),
                }
            }
            ResolvedTarget::Function {
                contract_name,
                function_name,
                node_type,
            } => {
                // Allow direct resolution to function if InterfaceMethod is skipped
                assert_eq!(
                    contract_name.as_deref(),
                    Some("Factory"),
                    "Step 2 (create): target contract (direct)"
                );
                assert_eq!(
                    function_name, "create",
                    "Step 2 (create): target function name (direct)"
                );
                assert_eq!(
                    *node_type,
                    NodeType::Function,
                    "Step 2 (create): target node type (direct)"
                );
            }
            _ => panic!(
                "Step 2 (create): Expected InterfaceMethod or Function, got {:?}",
                step2.target
            ),
        }

        // Step 3: .perform()
        let step3 = &steps[2];
        assert_eq!(
            step3.object_type,
            Some("IAction".to_string()),
            "Step 3 (perform): object_type should be IAction"
        );
        assert_eq!(
            step3.result_type,
            Some("bool".to_string()),
            "Step 3 (perform): result_type should be bool"
        );
        assert!(
            step3.arguments.is_empty(),
            "Step 3 (perform): arguments should be empty"
        );
        match &step3.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(
                    interface_name, "IAction",
                    "Step 3 (perform): target interface name"
                );
                assert_eq!(
                    method_name, "perform",
                    "Step 3 (perform): target method name"
                );
                assert!(
                    implementation.is_some(),
                    "Step 3 (perform): implementation should be resolved"
                );
                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name,
                        function_name,
                        node_type,
                    }) => {
                        assert_eq!(
                            contract_name.as_deref(),
                            Some("Action"),
                            "Step 3 (perform): impl contract"
                        );
                        assert_eq!(
                            function_name, "perform",
                            "Step 3 (perform): impl function name"
                        );
                        assert_eq!(
                            *node_type,
                            NodeType::Function,
                            "Step 3 (perform): impl node type"
                        );
                    }
                    _ => panic!(
                        "Step 3 (perform): Expected implementation Function, got {:?}",
                        implementation
                    ),
                }
            }
            _ => panic!(
                "Step 3 (perform): Expected InterfaceMethod, got {:?}",
                step3.target
            ),
        }

        Ok(())
    }

    #[test]
    fn test_analyze_constructor_call() -> Result<()> {
        // Use our own Result type
        let source = r#"
             contract Target { constructor(uint x) {} }
             contract Test {
                 function caller() public {
                     new Target(123); // Constructor call
                 }
             }
         "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;

        let caller_def_node = // Prefixed
            find_function_definition_node_by_name(&tree, source, &lang, "caller")?; // Use ?
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Caller node ID not found in graph"))?; // Use ok_or_else and ?

        // Find the call_expression node wrapping the new_expression within the caller function
        // Re-fetch caller_def_node as it was prefixed with _
        let caller_def_node_for_search =
            find_function_definition_node_by_name(&tree, source, &lang, "caller")?;
        let call_expr_node = find_nth_descendant_node_of_kind(
                &caller_def_node_for_search, // Start search within the caller function node
                source,
                &lang,
                "call_expression", // Kind to find
                0,                 // Find the first one (index 0)
            )? // Propagate Result error
            .ok_or_else(|| { // Convert Option to Result
                // Use our own error type for better context if node not found
                TypeError::Internal(format!(
                    "Could not find the call_expression node for new Target(123) within caller function node: {:?}",
                    caller_def_node.byte_range()
                ))
            })?; // Propagate Option error

        // Optional: Add a debug print to verify the found node
        // eprintln!("DEBUG [Test Constructor Call]: Found call_expr_node: kind='{}', text='{}'",
        //           call_expr_node.kind(), get_node_text(&call_expr_node, source));

        let steps = analyze_chained_call(
            call_expr_node, // Use the node found within the caller function
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(), // Add originating_span_start
        )?;

        assert_eq!(steps.len(), 1);
        let step = &steps[0];
        assert_eq!(step.object_type, None);
        assert_eq!(step.result_type, Some("Target".to_string())); // Result is the contract type
        assert_eq!(step.arguments, vec!["123".to_string()]);
        match &step.target {
            ResolvedTarget::Function {
                contract_name,
                function_name,
                node_type,
            } => {
                assert_eq!(contract_name.as_deref(), Some("Target"));
                assert_eq!(function_name, "Target"); // Constructor name
                assert_eq!(*node_type, NodeType::Constructor);
            }
            _ => panic!(
                "Expected ResolvedTarget::Function (Constructor), got {:?}",
                step.target
            ),
        }

        Ok(())
    }

    #[test]
    fn test_analyze_type_cast_call() -> Result<()> {
        // Use our own Result type
        let source = r#"
             interface IAction { function perform() external; }
             contract Test {
                 address actionAddr;
                 function caller() public {
                     IAction(actionAddr).perform(); // Type cast then call
                 }
             }
         "#;
        let (ctx, graph, tree, lang, input) = setup_test_environment(source)?;
        let caller_def_node = find_function_definition_node_by_name(&tree, source, &lang, "caller")
            .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the outer call_expression node for (...).perform()
        let call_expr_node = find_nth_node_of_kind(&tree, "call_expression", 0).unwrap(); // Only one call expr

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("Test".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(), // Add originating_span_start
        )?;

        // Should have one step: perform
        // The type cast `IAction(actionAddr)` itself doesn't create a step, but resolves the type.
        assert_eq!(steps.len(), 1);

        // Step 1: perform
        let step1 = &steps[0];
        assert_eq!(step1.object_type, Some("IAction".to_string())); // Object type resolved via cast
        assert_eq!(step1.result_type, None); // perform returns nothing
        assert!(step1.arguments.is_empty());
        match &step1.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(interface_name, "IAction");
                assert_eq!(method_name, "perform");
                // Implementation is None because we don't have a concrete contract provided here
                assert!(implementation.is_none());
            }
            _ => panic!("Step 1: Expected InterfaceMethod, got {:?}", step1.target),
        }

        Ok(())
    }

    fn default_natspec_text_range() -> NatspecTextRange {
        NatspecTextRange {
            start: NatspecTextIndex {
                utf8: 0,
                line: 0,
                column: 0,
            },
            end: NatspecTextIndex {
                utf8: 0,
                line: 0,
                column: 0,
            },
        }
    }

    #[test]
    fn test_analyze_interface_call_fallback_to_binding_multiple_implementers() -> Result<()> {
        let source = r#"
            interface IMyService { function doThing() external returns (uint); }
            contract MyServiceImplA is IMyService { function doThing() external override returns (uint) { return 1; } }
            contract MyServiceImplB is IMyService { function doThing() external override returns (uint) { return 2; } }

            contract Caller {
                IMyService serviceInstance;
                function callIt() public returns (uint) {
                    return serviceInstance.doThing();
                }
            }
        "#;
        let (mut ctx, graph, tree, lang, input) = setup_test_environment(source)?;

        // Create Manifest
        let manifest_entry = ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "/// @custom:binds-to MyService_BoundTo_A".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Interface,
            item_name: Some("IMyService".to_string()),
            item_span: default_natspec_text_range(), // Actual span not critical for this test's logic path
            is_natspec: true,
        };
        let manifest = Manifest {
            entries: vec![manifest_entry],
        };
        ctx.manifest = Some(manifest);

        // Create BindingRegistry
        let binding_config = BindingConfig {
            key: "MyService_BoundTo_A".to_string(),
            contract_name: Some("MyServiceImplA".to_string()),
            address: None,
            chain_id: None,
            notes: None,
        };
        let mut bindings_map = HashMap::new();
        bindings_map.insert("MyService_BoundTo_A".to_string(), binding_config);
        let binding_registry = BindingRegistry {
            bindings: bindings_map,
        };
        ctx.binding_registry = Some(binding_registry);

        // Find the 'caller' function definition node
        let caller_func_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "callIt")
                .expect("Could not find function definition node for callIt");

        // Get the Node ID for the caller function from the graph
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Caller".to_string()), "callIt".to_string()))
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Node ID for Caller.callIt not found in graph"))?;

        // Find the call_expression node for serviceInstance.doThing()
        let call_expr_node = find_nth_descendant_node_of_kind(
            &caller_func_def_node,
            source,
            &lang,
            "call_expression",
            0, // First call expression in callIt
        )?
        .ok_or_else(|| {
            TypeError::Internal(
                "call_expression node for serviceInstance.doThing() not found".to_string(),
            )
        })?;

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("Caller".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(),
        )?;

        assert_eq!(steps.len(), 1, "Expected one step for the call");
        let step = &steps[0];

        assert_eq!(step.object_type, Some("IMyService".to_string()));
        assert_eq!(step.result_type, Some("uint".to_string())); // MyServiceImplA.doThing returns uint

        match &step.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(interface_name, "IMyService");
                assert_eq!(method_name, "doThing");
                assert!(
                    implementation.is_some(),
                    "Implementation should be resolved via binding"
                );

                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name,
                        function_name,
                        node_type,
                    }) => {
                        assert_eq!(contract_name.as_deref(), Some("MyServiceImplA"));
                        assert_eq!(function_name, "doThing");
                        assert_eq!(*node_type, NodeType::Function);
                    }
                    _ => panic!(
                        "Expected implementation to be ResolvedTarget::Function, got {:?}",
                        implementation
                    ),
                }
            }
            _ => panic!(
                "Expected ResolvedTarget::InterfaceMethod, got {:?}",
                step.target
            ),
        }
        Ok(())
    }

    #[test]
    fn test_analyze_interface_call_fallback_to_binding_zero_implementers() -> Result<()> {
        let source = r#"
            interface IExternalService { function performRemoteAction() external returns (bool); }
            // No direct implementers of IExternalService in this source

            // This contract will be specified in bindings
            contract BoundRemoteImpl {
                function performRemoteAction() public pure returns (bool) { return true; }
            }

            contract UserContract {
                IExternalService remoteService;
                function executeRemote() public returns (bool) {
                    return remoteService.performRemoteAction();
                }
            }
        "#;
        let (mut ctx, graph, tree, lang, input) = setup_test_environment(source)?;

        // Create Manifest
        let manifest_entry = ManifestEntry {
            file_path: std::path::PathBuf::from("interfaces.sol"), // Dummy path
            text: "/// @custom:binds-to RemoteServiceKey".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Interface,
            item_name: Some("IExternalService".to_string()),
            item_span: default_natspec_text_range(),
            is_natspec: true,
        };
        let manifest = Manifest {
            entries: vec![manifest_entry],
        };
        ctx.manifest = Some(manifest);

        // Create BindingRegistry
        let binding_config = BindingConfig {
            key: "RemoteServiceKey".to_string(),
            contract_name: Some("BoundRemoteImpl".to_string()),
            address: None,
            chain_id: None,
            notes: None,
        };
        let mut bindings_map = HashMap::new();
        bindings_map.insert("RemoteServiceKey".to_string(), binding_config);
        let binding_registry = BindingRegistry {
            bindings: bindings_map,
        };
        ctx.binding_registry = Some(binding_registry);

        let caller_func_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "executeRemote")?;
        let caller_node_id = graph
            .node_lookup
            .get(&(
                Some("UserContract".to_string()),
                "executeRemote".to_string(),
            ))
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Node ID for UserContract.executeRemote not found"))?;

        let call_expr_node = find_nth_descendant_node_of_kind(
            &caller_func_def_node,
            source,
            &lang,
            "call_expression",
            0,
        )?
        .ok_or_else(|| {
            TypeError::Internal(
                "call_expression node for remoteService.performRemoteAction() not found"
                    .to_string(),
            )
        })?;

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("UserContract".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(),
        )?;

        assert_eq!(steps.len(), 1);
        let step = &steps[0];
        assert_eq!(step.object_type, Some("IExternalService".to_string()));
        assert_eq!(step.result_type, Some("bool".to_string())); // BoundRemoteImpl.performRemoteAction returns bool

        match &step.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(interface_name, "IExternalService");
                assert_eq!(method_name, "performRemoteAction");
                assert!(
                    implementation.is_some(),
                    "Implementation should be resolved via binding"
                );

                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name,
                        function_name,
                        node_type,
                    }) => {
                        assert_eq!(contract_name.as_deref(), Some("BoundRemoteImpl"));
                        assert_eq!(function_name, "performRemoteAction");
                        assert_eq!(*node_type, NodeType::Function);
                    }
                    _ => panic!(
                        "Expected implementation to be ResolvedTarget::Function, got {:?}",
                        implementation
                    ),
                }
            }
            _ => panic!(
                "Expected ResolvedTarget::InterfaceMethod, got {:?}",
                step.target
            ),
        }
        Ok(())
    }

    #[test]
    fn test_analyze_interface_call_structural_typing_via_shared_key_no_inheritance() -> Result<()> {
        let source = r#"
            interface IExternal {
                /// @custom:binds-to ExternalKey
                function doExternalWork() external returns (string);
            }

            /// @custom:binds-to ExternalKey
            contract ExternalContractImpl {
                // This contract does NOT explicitly implement IExternal
                function doExternalWork() public pure returns (string) {
                    return "ExternalContractImpl.doExternalWork";
                }
            }

            contract CallerContract {
                IExternal externalService;

                function setService(address _serviceAddr) public {
                    externalService = IExternal(_serviceAddr);
                }

                function callExternalWork() public returns (string) {
                    return externalService.doExternalWork(); // This should resolve to ExternalContractImpl.doExternalWork
                }
            }
        "#;

        // --- Setup Manifest ---
        let mut manifest_entries = Vec::new();

        // ManifestEntry for IExternal
        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "/// @custom:binds-to ExternalKey".to_string(),
            raw_comment_span: default_natspec_text_range(), // Dummy span
            item_kind: NatspecSourceItemKind::Interface,
            item_name: Some("IExternal".to_string()),
            item_span: NatspecTextRange {
                // Approx span for "interface IExternal"
                start: NatspecTextIndex {
                    utf8: 13,
                    line: 1,
                    column: 12,
                },
                end: NatspecTextIndex {
                    utf8: 108,
                    line: 4,
                    column: 13,
                },
            },
            is_natspec: true,
        });

        // ManifestEntry for ExternalContractImpl
        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "/// @custom:binds-to ExternalKey".to_string(),
            raw_comment_span: default_natspec_text_range(), // Dummy span
            item_kind: NatspecSourceItemKind::Contract,
            item_name: Some("ExternalContractImpl".to_string()),
            item_span: NatspecTextRange {
                // Approx span for "contract ExternalContractImpl"
                start: NatspecTextIndex {
                    utf8: 123,
                    line: 6,
                    column: 0,
                },
                end: NatspecTextIndex {
                    utf8: 280,
                    line: 11,
                    column: 1,
                },
            },
            is_natspec: true,
        });

        // ManifestEntry for CallerContract.callExternalWork (to ensure it's in the graph)
        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "".to_string(), // No natspec needed for the caller function itself for this test
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Function,
            item_name: Some("callExternalWork".to_string()),
            item_span: NatspecTextRange {
                // Approx span for "function callExternalWork"
                start: NatspecTextIndex {
                    utf8: 408,
                    line: 19,
                    column: 4,
                },
                end: NatspecTextIndex {
                    utf8: 504,
                    line: 21,
                    column: 5,
                },
            },
            is_natspec: false,
        });

        let manifest = Manifest {
            entries: manifest_entries,
        };
        let mut binding_registry = BindingRegistry::default();

        // Simulate sol2cg's population of the registry from contract Natspec
        binding_registry.populate_from_manifest(&manifest);

        // Verify that populate_from_manifest correctly set the contract_name for ExternalKey
        let binding_conf_after_populate = binding_registry.get_binding("ExternalKey");
        assert!(
            binding_conf_after_populate.is_some(),
            "BindingConfig for ExternalKey should exist after populate"
        );
        assert_eq!(
            binding_conf_after_populate.unwrap().contract_name,
            Some("ExternalContractImpl".to_string()),
            "BindingConfig for ExternalKey should have ExternalContractImpl as contract_name"
        );

        let (ctx, graph, tree, lang, input) =
            setup_test_environment_customized(source, Some(manifest), Some(binding_registry))?;

        // Find the 'callExternalWork' function definition node in CallerContract
        let caller_func_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "callExternalWork")?;

        // Get the Node ID for the caller function from the graph
        let caller_node_id = graph
            .node_lookup
            .get(&(
                Some("CallerContract".to_string()),
                "callExternalWork".to_string(),
            ))
            .copied()
            .ok_or_else(|| {
                anyhow::anyhow!("Node ID for CallerContract.callExternalWork not found")
            })?;

        // Find the call_expression node for externalService.doExternalWork()
        let call_expr_node = find_nth_descendant_node_of_kind(
            &caller_func_def_node,
            source,
            &lang,
            "call_expression",
            0, // First call expression in callExternalWork
        )?
        .ok_or_else(|| {
            TypeError::Internal(
                "call_expression node for externalService.doExternalWork() not found".to_string(),
            )
        })?;

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("CallerContract".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None, // This is the top-level call being analyzed
            call_expr_node.start_byte(),
        )?;

        assert_eq!(steps.len(), 1, "Expected one step for the call");
        let step = &steps[0];

        assert_eq!(
            step.object_type,
            Some("IExternal".to_string()),
            "Object type should be IExternal"
        );
        assert_eq!(
            step.result_type,
            Some("string".to_string()),
            "Result type should be string (from ExternalContractImpl.doExternalWork)"
        );

        match &step.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(interface_name, "IExternal");
                assert_eq!(method_name, "doExternalWork");
                assert!(
                    implementation.is_some(),
                    "Implementation should be resolved via shared Natspec binding key"
                );

                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name: impl_contract_name,
                        function_name: impl_function_name,
                        node_type,
                    }) => {
                        assert_eq!(
                            impl_contract_name.as_deref(),
                            Some("ExternalContractImpl"),
                            "Implementation should be ExternalContractImpl"
                        );
                        assert_eq!(impl_function_name, "doExternalWork");
                        assert_eq!(*node_type, NodeType::Function);
                    }
                    _ => panic!(
                        "Expected implementation to be ResolvedTarget::Function, got {:?}",
                        implementation
                    ),
                }
            }
            _ => panic!(
                "Expected ResolvedTarget::InterfaceMethod, got {:?}",
                step.target
            ),
        }
        Ok(())
    }

    #[test]
    fn test_analyze_interface_cast_call_structural_typing_shared_key() -> Result<()> {
        let source = r#"
            interface IERC20 {
                /// @custom:binds-to ERC20Key
                function balanceOf(address account) external view returns (uint256);
            }

            /// @custom:binds-to ERC20Key
            contract MyToken {
                // This contract does NOT explicitly implement IERC20
                mapping(address => uint256) private _balances;

                function balanceOf(address account) public view returns (uint256) {
                    return _balances[account];
                }
            }

            contract CallerContract {
                address token0; // Assume this is set to MyToken's address

                constructor(address _token0) {
                    token0 = _token0;
                }

                function getMyBalance() public view returns (uint256) {
                    // Call via type cast: InterfaceName(variable).method()
                    return IERC20(token0).balanceOf(address(this));
                }
            }
        "#;

        // --- Setup Manifest ---
        let mut manifest_entries = Vec::new();

        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "/// @custom:binds-to ERC20Key".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Interface,
            item_name: Some("IERC20".to_string()),
            item_span: NatspecTextRange {
                // Approx span for "interface IERC20"
                start: NatspecTextIndex {
                    utf8: 13,
                    line: 1,
                    column: 12,
                },
                end: NatspecTextIndex {
                    utf8: 123,
                    line: 4,
                    column: 13,
                },
            },
            is_natspec: true,
        });

        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "/// @custom:binds-to ERC20Key".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Contract,
            item_name: Some("MyToken".to_string()),
            item_span: NatspecTextRange {
                // Approx span for "contract MyToken"
                start: NatspecTextIndex {
                    utf8: 138,
                    line: 6,
                    column: 0,
                },
                end: NatspecTextIndex {
                    utf8: 330,
                    line: 13,
                    column: 1,
                },
            },
            is_natspec: true,
        });

        manifest_entries.push(ManifestEntry {
            // For CallerContract.getMyBalance
            file_path: std::path::PathBuf::from("test.sol"),
            text: "".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Function,
            item_name: Some("getMyBalance".to_string()),
            item_span: NatspecTextRange {
                // Approx span for "function getMyBalance"
                start: NatspecTextIndex {
                    utf8: 450,
                    line: 20,
                    column: 4,
                },
                end: NatspecTextIndex {
                    utf8: 590,
                    line: 24,
                    column: 5,
                },
            },
            is_natspec: false,
        });

        let manifest = Manifest {
            entries: manifest_entries,
        };
        let mut binding_registry = BindingRegistry::default();
        binding_registry.populate_from_manifest(&manifest);

        // Verify binding population
        let binding_conf = binding_registry.get_binding("ERC20Key");
        assert!(
            binding_conf.is_some(),
            "BindingConfig for ERC20Key should exist"
        );
        assert_eq!(
            binding_conf.unwrap().contract_name,
            Some("MyToken".to_string()),
            "ERC20Key should bind to MyToken contract"
        );

        let (ctx, graph, tree, lang, input) =
            setup_test_environment_customized(source, Some(manifest), Some(binding_registry))?;

        let caller_func_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "getMyBalance")?;
        let caller_node_id = graph
            .node_lookup
            .get(&(
                Some("CallerContract".to_string()),
                "getMyBalance".to_string(),
            ))
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Node ID for CallerContract.getMyBalance not found"))?;

        // Find the call_expression node for IERC20(token0).balanceOf(address(this))
        // This is the only call_expression within getMyBalance.
        let call_expr_node = find_nth_descendant_node_of_kind(
            &caller_func_def_node,
            source,
            &lang,
            "call_expression",
            0,
        )?
        .ok_or_else(|| {
            TypeError::Internal(
                "call_expression node for IERC20(token0).balanceOf(...) not found".to_string(),
            )
        })?;

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("CallerContract".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(),
        )?;

        assert_eq!(steps.len(), 1, "Expected one step for the call");
        let step = &steps[0];

        assert_eq!(
            step.object_type,
            Some("IERC20".to_string()),
            "Object type should be IERC20 (from type cast)"
        );
        assert_eq!(
            step.result_type,
            Some("uint256".to_string()),
            "Result type should be uint256 (from MyToken.balanceOf)"
        );
        assert_eq!(step.arguments.len(), 1, "Expected one argument");
        assert_eq!(
            step.arguments[0], "address(this)",
            "Argument should be address(this)"
        );

        match &step.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(interface_name, "IERC20");
                assert_eq!(method_name, "balanceOf");
                assert!(
                    implementation.is_some(),
                    "Implementation should be resolved via shared Natspec binding key"
                );

                match implementation.as_deref() {
                    Some(ResolvedTarget::Function {
                        contract_name: impl_contract_name,
                        function_name: impl_function_name,
                        node_type,
                    }) => {
                        assert_eq!(
                            impl_contract_name.as_deref(),
                            Some("MyToken"),
                            "Implementation should be MyToken"
                        );
                        assert_eq!(impl_function_name, "balanceOf");
                        assert_eq!(*node_type, NodeType::Function);
                    }
                    _ => panic!(
                        "Expected implementation to be ResolvedTarget::Function, got {:?}",
                        implementation
                    ),
                }
            }
            _ => panic!(
                "Expected ResolvedTarget::InterfaceMethod, got {:?}",
                step.target
            ),
        }
        Ok(())
    }

    #[test]
    fn test_analyze_call_on_interface_method_to_mapping_getter_with_binding() -> Result<()> {
        let source = r#"
            interface IUniswapV2ERC20 {
                /// @custom:binds-to ERC20Impl
                function balanceOf(address account) external view returns (uint);
            }

            /// @custom:binds-to ERC20Impl
            contract UniswapV2ERC20 { // Does not explicitly inherit
                mapping(address => uint) public balanceOf;
                // Other UniswapV2ERC20 members not needed for this specific test
            }

            contract CallerContract {
                address token; // Changed from IUniswapV2ERC20 to address

                constructor(address _tokenAddress) {
                    token = _tokenAddress; // Store the address directly
                }

                function getBalance(address who) public view returns (uint) {
                    // Instantiate interface and call in one line
                    return IUniswapV2ERC20(token).balanceOf(who);
                }
            }
        "#;

        // --- Setup Manifest & BindingRegistry ---
        let mut manifest_entries = Vec::new();
        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "/// @custom:binds-to ERC20Impl".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Interface,
            item_name: Some("IUniswapV2ERC20".to_string()),
            item_span: default_natspec_text_range(),
            is_natspec: true,
        });
        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "/// @custom:binds-to ERC20Impl".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Contract,
            item_name: Some("UniswapV2ERC20".to_string()),
            item_span: default_natspec_text_range(),
            is_natspec: true,
        });
        // Add entries for other contracts/functions if needed for graph construction
        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Contract,
            item_name: Some("CallerContract".to_string()),
            item_span: default_natspec_text_range(),
            is_natspec: false,
        });
        manifest_entries.push(ManifestEntry {
            file_path: std::path::PathBuf::from("test.sol"),
            text: "".to_string(),
            raw_comment_span: default_natspec_text_range(),
            item_kind: NatspecSourceItemKind::Function,
            item_name: Some("getBalance".to_string()),
            item_span: default_natspec_text_range(),
            is_natspec: false,
        });

        let manifest = Manifest {
            entries: manifest_entries,
        };
        let mut binding_registry = BindingRegistry::default();
        binding_registry.populate_from_manifest(&manifest);

        let (ctx, graph, tree, lang, input) =
            setup_test_environment_customized(source, Some(manifest), Some(binding_registry))?;

        let caller_func_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "getBalance")?;
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("CallerContract".to_string()), "getBalance".to_string()))
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Node ID for CallerContract.getBalance not found"))?;

        // Find the call_expression node for `token.balanceOf(who)`
        let call_expr_node = find_nth_descendant_node_of_kind(
            &caller_func_def_node,
            source,
            &lang,
            "call_expression",
            0, // The first call in getBalance is token.balanceOf(who)
        )?
        .ok_or_else(|| {
            TypeError::Internal(
                "Could not find the call_expression for token.balanceOf(who)".to_string(),
            )
        })?;

        let steps = analyze_chained_call(
            call_expr_node,
            caller_node_id,
            &Some("CallerContract".to_string()),
            &ctx,
            &graph,
            source,
            &lang,
            &input,
            None,
            call_expr_node.start_byte(),
        )?;

        assert_eq!(steps.len(), 1, "Expected one step in the chain");

        let step1 = &steps[0];
        eprintln!("Step 1: {:?}", step1);
        assert_eq!(
            step1.object_type,
            Some("IUniswapV2ERC20".to_string()),
            "Step 1 object_type"
        );
        assert_eq!(
            step1.object_instance_text,
            Some("IUniswapV2ERC20(token)".to_string()), // Updated expected object instance text
            "Step 1 object_instance_text"
        );
        assert_eq!(step1.arguments, vec!["who".to_string()], "Step 1 arguments");
        assert_eq!(
            step1.result_type,
            Some("uint".to_string()),
            "Step 1 result_type (must be uint from mapping)"
        );

        match &step1.target {
            ResolvedTarget::InterfaceMethod {
                interface_name,
                method_name,
                implementation,
            } => {
                assert_eq!(
                    interface_name, "IUniswapV2ERC20",
                    "Step 1 target interface_name"
                );
                assert_eq!(method_name, "balanceOf", "Step 1 target method_name");
                assert!(
                    implementation.is_some(),
                    "Step 1 implementation should be resolved"
                );

                match implementation.as_deref() {
                    Some(ResolvedTarget::Function { contract_name, function_name, node_type }) => {
                        assert_eq!(contract_name.as_deref(), Some("UniswapV2ERC20"), "Step 1 impl contract (concrete contract)");
                        assert_eq!(function_name, "balanceOf", "Step 1 impl function (getter for mapping)");
                        assert_eq!(*node_type, NodeType::Function, "Step 1 impl type (getter is a function)");
                    }
                    _ => panic!("Step 1: Expected implementation to be ResolvedTarget::Function for getter, got {:?}", implementation),
                }
            }
            _ => panic!(
                "Step 1: Expected ResolvedTarget::InterfaceMethod, got {:?}",
                step1.target
            ),
        }

        Ok(())
    }
}
