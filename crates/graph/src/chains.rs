//! Module for resolving types and analyzing chained calls in Solidity expressions.

use crate::cg::{
    CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, NodeInfo, NodeType,
}; // Keep only used items from cg
use crate::parser::get_node_text;
// Removed anyhow imports
use std::collections::VecDeque; // Keep only VecDeque
                                // Removed unused std::error::Error import
use streaming_iterator::StreamingIterator; // Import the trait for .next()
use tree_sitter::{Node as TsNode, Query, QueryCursor}; // Remove unused Point and Tree
                                                       //

// --- Error Handling ---

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TypeError {
    #[error("Query error: {0}")]
    QueryError(String), // Store String to avoid tree-sitter dep in error type if possible

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
        implementations: Vec<String>, // List contract names
    },

    #[error("Internal error: {0}")]
    Internal(String),
    // Removed Other(String) variant used for anyhow
}

// Convert tree-sitter QueryError to our TypeError
impl From<tree_sitter::QueryError> for TypeError {
    fn from(error: tree_sitter::QueryError) -> Self {
        // Convert the tree-sitter error to a string representation
        TypeError::QueryError(error.to_string())
    }
}

// Note: `thiserror` automatically implements `std::error::Error` and `Display`

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
    original_start_node: Option<TsNode<'a>>, // Track the original start node
) -> std::result::Result<Vec<ResolvedCallStep>, TypeError> {
    // If original_start_node is None, this is the top-level call, so use start_node
    let original_start_node = original_start_node.unwrap_or(start_node);
    // Use full path to Result
    let mut steps = Vec::new();
    let current_node = start_node; // Removed unused mut
    let mut current_object_type: Option<String> = None; // Type of the result of the previous step

    eprintln!(
        "[Analyze Chained] Starting analysis for node kind: '{}', text: '{}'",
        start_node.kind(),
        get_node_text(&start_node, source).trim()
    );

    eprintln!(
            "[Analyze Chained] Loop iteration. Current node kind: '{}', text: '{}', current object type: {:?}",
            current_node.kind(),
            get_node_text(&current_node, source).trim(),
            current_object_type
        );

    match current_node.kind() {
        // --- Base Cases (Start of a chain or simple expression) ---
        "identifier" | "primitive_type" | "string_literal" | "number_literal"
        | "boolean_literal" | "hex_literal" | "address_literal" => {
            // This is not a call itself, but the start of a potential chain. Resolve its type.
            current_object_type = resolve_expression_type_v2(
                current_node,
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
            )?;
            eprintln!(
                "[Analyze Chained] Base case resolved type: {:?}",
                current_object_type
            );
            // If it's just an identifier/literal, there are no call steps *from* it directly.
            // The chain must continue via member access or call expression wrapping it.
        }

        "new_expression" => {
            // Handle `new Contract(...)` as the start of a potential chain
            let type_name_node =
                current_node
                    .child_by_field_name("type_name")
                    .ok_or_else(|| {
                        TypeError::MissingChild("new_expression missing type_name".to_string())
                    })?;
            let contract_name = get_node_text(&type_name_node, source).to_string(); // Simplified

            // The "call" is to the constructor
            let target = ResolvedTarget::Function {
                contract_name: Some(contract_name.clone()),
                function_name: contract_name.clone(), // Constructor name is contract name
                node_type: NodeType::Constructor,
            };

            // Arguments are tricky for `new`, they are siblings within the parent `call_expression` usually.
            // Let's assume the caller handles argument extraction for `new` based on the parent `call_expression`.
            // For now, pass empty args.
            let arguments = vec![]; // Placeholder

            let step = ResolvedCallStep {
                call_expr_span: (current_node.start_byte(), current_node.end_byte()), // Use start/end_byte
                function_span: (
                    type_name_node.start_byte(), // Use start/end_byte
                    type_name_node.end_byte(),   // Use start/end_byte
                ), // Span of the type name
                target,
                arguments,
                result_type: Some(contract_name.clone()), // Result type is the contract itself
                object_type: None,                        // No object for `new`
            };
            steps.push(step);
            current_object_type = Some(contract_name);
            eprintln!(
                "[Analyze Chained] 'new' expression processed. Result type: {:?}",
                current_object_type
            );
            // A `new` expression itself doesn't chain further directly.
        }

        // --- Recursive / Chaining Cases ---
        "call_expression" => {
            let function_node = current_node
                .child_by_field_name("function")
                .ok_or_else(|| {
                    TypeError::MissingChild("call_expression missing function".to_string())
                })?;
            let arguments = extract_arguments_v2(current_node, source);

            eprintln!(
                "[Analyze Chained] Handling call_expression. Function node kind: '{}', text: '{}'",
                function_node.kind(),
                get_node_text(&function_node, source).trim()
            );

            match function_node.kind() {
                // Case 1: Simple call `foo()` or type cast `Type(arg)`
                "expression"
                    if function_node
                        .child(0)
                        .map_or(false, |n| n.kind() == "identifier") =>
                {
                    let id_node = function_node.child(0).unwrap();
                    let name = get_node_text(&id_node, source).to_string();

                    // Is it a constructor call `ContractName(...)` or an interface type cast `InterfaceName(...)`?
                    if name.chars().next().map_or(false, |c| c.is_uppercase()) {
                        if ctx.all_contracts.contains_key(&name) {
                            // Constructor Call `ContractName(...)`
                            eprintln!(
                                "[Analyze Chained]   Call identified as constructor call to '{}'",
                                name
                            );
                            let target = ResolvedTarget::Function {
                                contract_name: Some(name.clone()),
                                function_name: name.clone(), // Constructor name is contract name
                                node_type: NodeType::Constructor,
                            };
                            let result_type = Some(name.clone()); // Result type is the contract itself

                            let step = ResolvedCallStep {
                                call_expr_span: (
                                    current_node.start_byte(),
                                    current_node.end_byte(),
                                ),
                                function_span: (id_node.start_byte(), id_node.end_byte()),
                                target,
                                arguments, // Use extracted arguments
                                result_type: result_type.clone(),
                                object_type: None, // No object for constructor call
                            };
                            steps.push(step);
                            current_object_type = result_type;
                            eprintln!(
                                "[Analyze Chained]   Constructor call step added for '{}'. Result type: {:?}",
                                name, current_object_type
                            );
                        } else if ctx.all_interfaces.contains_key(&name) {
                            // Interface Type Cast `InterfaceName(...)`
                            eprintln!(
                                "[Analyze Chained]   Expression identified as interface type cast to '{}'",
                                name
                            );
                            // Resolve the type for the next step, but DO NOT create a call step for the cast itself.
                            current_object_type = Some(name.clone());
                            eprintln!(
                                "[Analyze Chained]   Interface type cast processed. Result type for next step: {:?}",
                                current_object_type
                            );
                            // No step is pushed for the type cast itself.
                        } else {
                            // Potentially a struct constructor or simple function call starting with uppercase?
                            // Treat as simple function call for now.
                            eprintln!(
                                "[Analyze Chained]   Call identified as simple function call '{}' (uppercase start)",
                                name
                            );
                            // Resolve target (could be local, contract func, free func)
                            let target = resolve_simple_call_v2(
                                &name,
                                caller_contract_name_opt,
                                graph,
                                ctx,
                            )?;
                            let result_type = resolve_call_return_type(
                                &target,
                                ctx,
                                graph,
                                source,
                                solidity_lang,
                                input,
                            )?;

                            let step = ResolvedCallStep {
                                call_expr_span: (
                                    current_node.start_byte(),
                                    current_node.end_byte(),
                                ),
                                function_span: (id_node.start_byte(), id_node.end_byte()),
                                target,
                                arguments,
                                result_type: result_type.clone(),
                                object_type: None, // No object for simple calls
                            };
                            steps.push(step);
                            current_object_type = result_type;
                            eprintln!(
                                "[Analyze Chained]   Simple call (uppercase) step added. Result type: {:?}",
                                current_object_type
                            );
                        }
                    } else {
                        // Simple function call `foo()` (lowercase start)
                        eprintln!(
                            "[Analyze Chained]   Call identified as simple function call '{}'",
                            name
                        );
                        // Resolve target (could be local, contract func, free func)
                        // Use a simplified resolution for now, assuming direct lookup works.
                        // TODO: Enhance simple call resolution (inheritance, etc.)
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

                        let step = ResolvedCallStep {
                            call_expr_span: (current_node.start_byte(), current_node.end_byte()), // Use start/end_byte
                            function_span: (id_node.start_byte(), id_node.end_byte()), // Use start/end_byte
                            target,
                            arguments,
                            result_type: result_type.clone(),
                            object_type: None, // No object for simple calls
                        };
                        steps.push(step);
                        current_object_type = result_type;
                        eprintln!(
                            "[Analyze Chained]   Simple call step added. Result type: {:?}",
                            current_object_type
                        );
                        // Simple call is usually the end of a chain.
                    }
                }

                // Case 2: Member call `obj.method()` or `expr.method()`
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
                        "[Analyze Chained]   Call identified as member call '.{}'",
                        property_name
                    );

                    // Resolve the type of the object *recursively* if needed
                    // This is where the chaining happens: the object_node becomes the new 'start_node'
                    // for the inner part of the chain.
                    let inner_steps = analyze_chained_call(
                        object_node, // Analyze the object part first
                        caller_node_id,
                        caller_contract_name_opt,
                        ctx,
                        graph,
                        source,
                        solidity_lang,
                        input,
                        Some(original_start_node), // Pass the original start node
                    )?;

                    // The type of the object for *this* call step is the result type of the *last* inner step,
                    // or the directly resolved type if the object_node was a base case.
                    let object_type_for_this_call =
                        if let Some(last_inner_step) = inner_steps.last() {
                            last_inner_step.result_type.clone()
                        } else {
                            // If no inner steps, resolve the object_node directly
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

                    // Prepend inner steps to our main steps list
                    // We process from outside in, so inner steps come first.
                    steps.splice(0..0, inner_steps); // Insert inner_steps at the beginning

                    if let Some(ref obj_type) = object_type_for_this_call {
                        eprintln!(
                            "[Analyze Chained]     Object type for '.{}' call resolved to: '{}'",
                            property_name, obj_type
                        );
                        // Resolve the target method based on the object type and property name
                        let target = resolve_member_or_library_call_v2(
                            obj_type,
                            &property_name,
                            caller_contract_name_opt,
                            graph,
                            ctx,
                            source,
                            (member_expr_node.start_byte(), member_expr_node.end_byte()), // Span for error reporting
                        )?;

                        let result_type = resolve_call_return_type(
                            &target,
                            ctx,
                            graph,
                            source,
                            solidity_lang,
                            input,
                        )?;

                        let step = ResolvedCallStep {
                            call_expr_span: (current_node.start_byte(), current_node.end_byte()), // Use start/end_byte
                            function_span: (property_node.start_byte(), property_node.end_byte()), // Use start/end_byte
                            target,
                            arguments,
                            result_type: result_type.clone(),
                            object_type: Some(obj_type.clone()),
                        };
                        steps.push(step); // Add the current call step
                        current_object_type = result_type;
                        eprintln!(
                            "[Analyze Chained]     Member call step added. Result type: {:?}",
                            current_object_type
                        );
                    } else {
                        eprintln!("[Analyze Chained]     Failed to resolve object type for member call '.{}'", property_name);
                        return Err(TypeError::TypeResolutionFailed {
                            expr: get_node_text(&object_node, source).to_string(),
                            reason: "Could not determine type for member call.".to_string(),
                        });
                    }
                }

                // Case 3: Constructor call `new Contract(...)`
                "expression"
                    if function_node
                        .child(0)
                        .map_or(false, |n| n.kind() == "new_expression") =>
                {
                    // Use ok_or_else instead of unwrap for better error context
                    let new_expr_node = function_node.child(0).ok_or_else(|| {
                        TypeError::Internal(format!(
                            "Function node '{}' in constructor call pattern expected a child (new_expression) but has none.",
                            get_node_text(&function_node, source)
                        ))
                    })?;

                    // Add debug print for the identified new_expression node
                    eprintln!(
                        "[Analyze Chained]     Identified potential new_expression node: kind='{}', text='{}'",
                        new_expr_node.kind(),
                        get_node_text(&new_expr_node, source).trim()
                    );

                    // Ensure the node is actually a new_expression before proceeding
                    if new_expr_node.kind() != "new_expression" {
                        return Err(TypeError::Internal(format!(
                            "Expected new_expression node within constructor call pattern, but got kind '{}' with text '{}'",
                            new_expr_node.kind(), get_node_text(&new_expr_node, source).trim()
                        )));
                    }

                    // --- Robust Type Name Extraction ---
                    let mut type_name_node_opt: Option<TsNode> = None;
                    let mut cursor = new_expr_node.walk();
                    eprintln!(
                        "[Analyze Chained]     Iterating children of new_expression '{}':",
                        get_node_text(&new_expr_node, source).trim()
                    );
                    for child in new_expr_node.children(&mut cursor) {
                        eprintln!(
                            "[Analyze Chained]       Child kind: '{}', text: '{}'",
                            child.kind(),
                            get_node_text(&child, source).trim()
                        );
                        // Look for the node representing the type name
                        // Common kinds are 'identifier' or 'type_name' itself
                        if child.kind() == "identifier" || child.kind() == "type_name" {
                            // Check if it's not the 'new' keyword itself if grammar is ambiguous
                            if get_node_text(&child, source).trim() != "new" {
                                type_name_node_opt = Some(child);
                                break; // Found it
                            }
                        }
                        // Add more kinds here if needed based on grammar inspection
                    }

                    let type_name_node = type_name_node_opt
                        .ok_or_else(|| {
                            // Error if no suitable child node was found
                            TypeError::MissingChild(format!(
                                "Could not find a type name node (identifier or type_name) as a child of the new_expression node '{}' (kind: {}). Node details: {:?}",
                                get_node_text(&new_expr_node, source).trim(),
                                new_expr_node.kind(),
                                new_expr_node // Consider printing node.to_sexp() if more detail needed
                            ))
                        })?;
                    // --- End Robust Type Name Extraction ---

                    let contract_name = get_node_text(&type_name_node, source).to_string(); // Simplified

                    eprintln!(
                        "[Analyze Chained]   Call identified as constructor call 'new {}' (using type node kind '{}')",
                        contract_name, type_name_node.kind() // Log the kind of node we used
                    );

                    let target = ResolvedTarget::Function {
                        contract_name: Some(contract_name.clone()),
                        function_name: contract_name.clone(),
                        node_type: NodeType::Constructor,
                    };

                    let step = ResolvedCallStep {
                        call_expr_span: (current_node.start_byte(), current_node.end_byte()), // Use start/end_byte
                        function_span: (type_name_node.start_byte(), type_name_node.end_byte()), // Use start/end_byte
                        target,
                        arguments,
                        result_type: Some(contract_name.clone()), // Result is the contract type
                        object_type: None,
                    };
                    steps.push(step);
                    current_object_type = Some(contract_name);
                    eprintln!(
                        "[Analyze Chained]   Constructor call step added. Result type: {:?}",
                        current_object_type
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
            let object_node = current_node.child_by_field_name("object").ok_or_else(|| {
                TypeError::MissingChild("member_expression missing object".to_string())
            })?;
            let property_node = current_node
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

            // Resolve the type of the object recursively
            let inner_steps = analyze_chained_call(
                object_node,
                caller_node_id,
                caller_contract_name_opt,
                ctx,
                graph,
                source,
                solidity_lang,
                input,
                None,
            )?;
            steps.splice(0..0, inner_steps.clone()); // Prepend inner steps

            let object_type_for_this_access = if let Some(last_inner_step) = inner_steps.last() {
                last_inner_step.result_type.clone()
            } else {
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

            if let Some(ref obj_type) = object_type_for_this_access {
                eprintln!(
                    "[Analyze Chained]   Object type for '.{}' access resolved to: '{}'",
                    property_name, obj_type
                );
                // Resolve the type of the *property itself*
                current_object_type = resolve_property_type(
                    obj_type,
                    &property_name,
                    caller_contract_name_opt,
                    graph,
                    ctx,
                    source,
                    solidity_lang,
                    input,
                )?;
                eprintln!(
                    "[Analyze Chained]   Member access resolved. Result type for next step: {:?}",
                    current_object_type
                );
                // This member access itself doesn't generate a call step.
                // We've updated current_object_type for the *next* potential call.
            } else {
                eprintln!(
                    "[Analyze Chained]   Failed to resolve object type for member access '.{}'",
                    property_name
                );
                return Err(TypeError::TypeResolutionFailed {
                    expr: get_node_text(&object_node, source).to_string(),
                    reason: "Could not determine type for member access.".to_string(),
                });
            }
        }

        "expression" => {
            // Delegate analysis to the first child of the expression node
            if let Some(child_node) = current_node.child(0) {
                eprintln!(
                    "[Analyze Chained] Delegating 'expression' analysis to child '{}'.", // New log
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
                    Some(original_start_node), // Pass the original start node
                );
                // Note: No steps are added for the 'expression' node itself.
            } else {
                // Expression node with no children? Unlikely, but handle defensively.
                eprintln!("[Analyze Chained] 'expression' node has no children.");
                // Fall through to return the current (likely empty) steps vector.
            }
        }
        // Default case for other unhandled node kinds
        _ => {
            // If the start node is an unhandled kind, try resolving its type.
            // This might be the start of a chain like `(a + b).call()`.
            if current_node == start_node {
                current_object_type = resolve_expression_type_v2(
                    current_node,
                    caller_node_id,
                    caller_contract_name_opt,
                    ctx,
                    graph,
                    source,
                    solidity_lang,
                    input,
                )?;
                eprintln!(
                    "[Analyze Chained] Unhandled start node kind '{}', resolved type: {:?}. No steps generated.",
                    current_node.kind(),
                    current_object_type
                );
                // No call steps generated from this node itself, steps remains empty.
            } else {
                // If we encounter an unhandled node kind *during* chain analysis (not start node), it's an error.
                return Err(TypeError::UnsupportedNodeKind(format!(
                    "Unexpected node kind '{}' encountered during chained call analysis",
                    current_node.kind()
                )));
            }
        }
    }
 // --- Deduplication ---
    // Sort steps primarily by call expression start byte, then end byte.
    // This allows dedup to remove identical steps originating from the same source location.
    // The Ord trait derived uses all fields, but sorting by span is the main goal here.
    steps.sort_unstable(); // Relies on the derived Ord implementation
    steps.dedup(); // Removes consecutive duplicates


    eprintln!(
        "[Analyze Chained] Analysis finished. Total steps generated: {}",
        steps.len()
    );
    Ok(steps)
}

// --- Helper Functions ---

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
            // TODO: Check local variables/parameters within the caller_node_id's definition_ts_node
            // This requires parsing the function parameters and local variable declarations.
            // --- Start: Check local variables ---
            if let Some((_, caller_node_info, _)) = ctx
                .definition_nodes_info
                .iter()
                .find(|(id, _, _)| *id == caller_node_id)
            {
                if let Some(definition_ts_node) = input
                    .tree
                    .root_node()
                    .descendant_for_byte_range(caller_node_info.span.0, caller_node_info.span.1)
                {
                    let local_var_query_str = r#"
                        ; Local variable declaration statement
                        (variable_declaration_statement
                          (variable_declaration
                            type: (_) @local_var_type
                            name: (identifier) @local_var_name
                          )
                        ) @local_var_decl_stmt

                    "#;
                    // Use try_new for query creation with error handling
                    let local_var_query = match Query::new(&input.solidity_lang, local_var_query_str) {
                        Ok(q) => q,
                        Err(e) => return Err(TypeError::QueryError(format!("Failed to create local var query: {}", e))),
                    };

                    let mut cursor = QueryCursor::new();
                    let source_bytes = input.source.as_bytes(); // Needed for closure
                    let mut matches = cursor.matches(
                        &local_var_query,
                        definition_ts_node, // Query within the function definition
                        |node: TsNode| std::iter::once(&source_bytes[node.byte_range()]),
                    );

                    while let Some(match_) = matches.next() {
                        let mut var_name_opt: Option<String> = None;
                        let mut type_node_opt: Option<TsNode> = None;
                        let mut decl_end_byte: usize = 0;

                        for capture in match_.captures {
                            let capture_name = &local_var_query.capture_names()[capture.index as usize];
                            match *capture_name { // Dereference capture_name
                                "local_var_name" | "param_name" => {
                                    var_name_opt = Some(get_node_text(&capture.node, &input.source).to_string());
                                    // Use the end byte of the name node for scope check
                                    decl_end_byte = capture.node.end_byte();
                                }
                                "local_var_type" | "param_type" => {
                                    type_node_opt = Some(capture.node);
                                }
                                "local_var_decl_stmt" => {
                                    // Use the end byte of the whole statement for scope check
                                    decl_end_byte = capture.node.end_byte();
                                }
                                _ => {}
                            }
                        }

                        if let (Some(var_name), Some(type_node)) = (var_name_opt, type_node_opt) {
                            // Check if the declared variable name matches the identifier we are resolving
                            // AND if the identifier usage occurs *after* the declaration
                            if var_name == name && expr_node.start_byte() >= decl_end_byte {
                                let type_name = get_node_text(&type_node, &input.source).to_string();
                                eprintln!(
                                    "[Resolve Type V2]   Identifier '{}' resolved to local var/param type '{}'",
                                    name, type_name
                                );
                                return Ok(Some(type_name));
                            }
                        }
                    }
                }
            }
            // --- End: Check local variables ---
        }
        // 2. Check if it's a known contract, library, or interface name (these are types)
        if ctx.all_contracts.contains_key(&name)
                || ctx.all_libraries.contains_key(&name)
                || ctx.all_interfaces.contains_key(&name)
            {
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

            eprintln!(
                "[Resolve Type V2]   Identifier '{}' type not resolved (state/type/local?).",
                name
            );
            Ok(None) // Identifier not resolved
        }
        "primitive_type" => Ok(Some(expr_text)),
        "string_literal" => Ok(Some("string".to_string())), // Assume string memory
        "number_literal" => Ok(Some("uint256".to_string())), // Assume uint256
        "boolean_literal" => Ok(Some("bool".to_string())),
        "hex_literal" => Ok(Some("bytes".to_string())), // Assume bytes
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
                None,
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
            let type_name_node = expr_node.child_by_field_name("type_name").ok_or_else(|| {
                TypeError::MissingChild("new_expression missing type_name".to_string())
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
        // TODO: Handle binary_expression, unary_expression, tuple_expression, array_access, etc.
        "binary_expression" => {
            // Basic heuristic: Assume numeric ops return uint256, boolean ops return bool
            let operator = expr_node
                .child_by_field_name("operator")
                .map(|n| get_node_text(&n, source));
            match operator.as_deref() {
                Some("&&") | Some("||") | Some("<") | Some("<=") | Some("==") | Some("!=")
                | Some(">=") | Some(">") => Ok(Some("bool".to_string())),
                Some("+") | Some("-") | Some("*") | Some("/") | Some("%") | Some("**") => {
                    Ok(Some("uint256".to_string()))
                } // Assume numeric
                _ => {
                    eprintln!(
                        "[Resolve Type V2]   Unhandled binary operator: {:?}",
                        operator
                    );
                    Ok(None)
                }
            }
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
    caller_contract_name_opt: &'a Option<String>,
    graph: &'a CallGraph,
    ctx: &CallGraphGeneratorContext,
    source: &'a str,
    solidity_lang: &'a tree_sitter::Language,
    input: &'a CallGraphGeneratorInput, // Pass input for potential use in future
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

    // --- Priority 1: Interface Implementation Resolution ---
    if ctx.all_interfaces.contains_key(object_type_name) {
        eprintln!(
            "[Resolve Member/Lib V2]   Object type is Interface '{}'.",
            object_type_name
        );
        let interface_name = object_type_name;
        let method_name = property_name;

        // Check if the method actually exists on the interface (or inherited interfaces)
        // TODO: Implement recursive check for inherited interface methods
        if !ctx
            .interface_functions
            .get(interface_name)
            .map_or(false, |funcs| funcs.contains(&method_name.to_string()))
        {
            eprintln!(
                "[Resolve Member/Lib V2]   Method '{}' not found in interface '{}' definition.",
                method_name, interface_name
            );
            // Fall through to check 'using for' just in case, though unlikely for interfaces
        } else {
            // Find contracts implementing this interface (TODO: handle inheritance)
            let implementing_contracts: Vec<_> = ctx
                .contract_implements
                .iter()
                .filter(|(_, implemented)| implemented.contains(&interface_name.to_string()))
                .map(|(contract_name, _)| contract_name.clone())
                .collect();

            eprintln!(
                "[Resolve Member/Lib V2]     Implementing contracts: {:?}",
                implementing_contracts
            );

            let mut potential_targets = Vec::new();
            for contract_name in &implementing_contracts {
                let target_key = (Some(contract_name.clone()), method_name.to_string());
                if let Some(node_id) = graph.node_lookup.get(&target_key) {
                    if let Some(node) = graph.nodes.get(*node_id) {
                        potential_targets.push(ResolvedTarget::Function {
                            contract_name: Some(contract_name.clone()),
                            function_name: method_name.to_string(),
                            node_type: node.node_type.clone(), // Assuming Function
                        });
                    }
                }
            }

            match potential_targets.len() {
                0 => {
                    eprintln!("[Resolve Member/Lib V2]     No implementations found for '{}.{}'. Assuming external or abstract.", interface_name, method_name);
                    // Return InterfaceMethod without implementation
                    return Ok(ResolvedTarget::InterfaceMethod {
                        interface_name: interface_name.to_string(),
                        method_name: method_name.to_string(),
                        implementation: None,
                    });
                }
                1 => {
                    let implementation = potential_targets.remove(0);
                    eprintln!(
                        "[Resolve Member/Lib V2]     Resolved to single implementation: {:?}",
                        implementation
                    );
                    // Return InterfaceMethod with the concrete implementation
                    return Ok(ResolvedTarget::InterfaceMethod {
                        interface_name: interface_name.to_string(),
                        method_name: method_name.to_string(),
                        implementation: Some(Box::new(implementation)),
                    });
                }
                _ => {
                    // Ambiguous - return InterfaceMethod without implementation for now.
                    // TODO: Could potentially apply heuristics later in cg.rs if needed.
                    eprintln!("[Resolve Member/Lib V2]     Ambiguous implementation for '{}.{}'. Found {} targets.", interface_name, method_name, potential_targets.len());
                    return Err(TypeError::AmbiguousInterfaceImplementation {
                        interface_name: interface_name.to_string(),
                        method_name: method_name.to_string(),
                        implementations: implementing_contracts,
                    });
                    // Or return unresolved:
                    // return Ok(ResolvedTarget::NotCallable {
                    //     reason: format!("Ambiguous implementation: {} contracts implement {}.{}", potential_targets.len(), interface_name, method_name)
                    // });
                }
            }
        }
    }

    // --- Priority 2: Direct Member Lookup (Contract/Library) ---
    let direct_lookup_key = (
        Some(object_type_name.to_string()),
        property_name.to_string(),
    );
    if let Some(node_id) = graph.node_lookup.get(&direct_lookup_key) {
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
        return Ok(library_target);
    }

    // --- Priority 4: Built-in Members ---
    // TODO: Check for built-ins like .push, .pop, .length, .balance etc.
    if (object_type_name.ends_with("[]")
        || object_type_name == "bytes"
        || object_type_name == "string")
        && (property_name == "push" || property_name == "pop" || property_name == "length")
    {
        eprintln!(
            "[Resolve Member/Lib V2]   Resolved to built-in array/bytes/string member '{}'",
            property_name
        );
        return Ok(ResolvedTarget::BuiltIn {
            object_type: object_type_name.to_string(),
            name: property_name.to_string(),
        });
    }
    if object_type_name == "address" && property_name == "balance" {
        eprintln!("[Resolve Member/Lib V2]   Resolved to built-in address member 'balance'");
        return Ok(ResolvedTarget::BuiltIn {
            object_type: object_type_name.to_string(),
            name: property_name.to_string(),
        });
    }
    if object_type_name == "address"
        && (property_name == "transfer"
            || property_name == "send"
            || property_name == "call"
            || property_name == "delegatecall"
            || property_name == "staticcall")
    {
        eprintln!(
            "[Resolve Member/Lib V2]   Resolved to built-in address member '{}'",
            property_name
        );
        return Ok(ResolvedTarget::BuiltIn {
            object_type: object_type_name.to_string(),
            name: property_name.to_string(),
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
    // Check contract-specific directives first
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
    }
    // TODO: Check global 'using for' directives (key: (None, Type) and (None, "*"))

    // Remove duplicates and check libraries
    potential_libraries.sort_unstable();
    potential_libraries.dedup();

    if !potential_libraries.is_empty() {
        eprintln!(
            "[Find UsingFor] Checking libraries for '{}.{}': {:?}",
            object_type_name, property_name, potential_libraries
        );
        for library_name in potential_libraries {
            let library_method_key = (Some(library_name.clone()), property_name.to_string());
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
            }
        }
    }
    Ok(None) // Not found via using for
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
        // --- End: Check inherited contracts ---
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

// crates/graph/src/chains.rs

/// Attempts to parse the return type string from a function definition node (V2).
/// Handles single types, basic tuples, and named return parameters.
// Remove 'a, change first param to NodeInfo, add input param
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
            ..
        } => {
            let key = (contract_name.clone(), function_name.clone());
            if let Some(node_id) = graph.node_lookup.get(&key) {
                let def_node_opt = ctx
                    .definition_nodes_info
                    .iter()
                    .find(|(id, _, _)| *id == *node_id)
                    .map(|(_, n, _)| n.clone());
                if let Some(def_node) = def_node_opt {
                    get_function_return_type_v2(&def_node, input)
                } else {
                    Ok(None) // Cannot find definition node
                }
            } else {
                Ok(None) // Function not found in graph (shouldn't happen if target resolved)
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
            // Determine return type of built-ins
            match (object_type.as_str(), name.as_str()) {
                (_, "length") => Ok(Some("uint256".to_string())),
                ("address", "balance") => Ok(Some("uint256".to_string())),
                ("address", "transfer") => Ok(None), // transfer returns nothing
                ("address", "send") => Ok(Some("bool".to_string())),
                ("address", "call") | ("address", "delegatecall") | ("address", "staticcall") => {
                    Ok(Some("(bool,bytes)".to_string()))
                } // Returns tuple
                (_, "push") => Ok(None), // push returns nothing (in older versions) or new length (newer)? Assume None for simplicity.
                (_, "pop") => Ok(None),  // pop returns nothing
                _ => Ok(None),           // Unknown built-in
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

// --- Unit Tests ---
#[cfg(test)]
mod tests {
    use super::*;
    use crate::cg::{
        CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorPipeline,
    }; // Import necessary items
    use crate::steps::CallsHandling;
    use crate::steps::ContractHandling;
    use anyhow::{Context, Result}; // Add anyhow imports
    use language::{Language, Solidity}; // Assuming Language trait and Solidity struct exist
    use std::collections::{HashMap, HashSet};
    use tree_sitter::{Parser, Tree};

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
        let func_def_node = find_nth_node_of_kind(&tree, "function_definition", 0)
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
        let func_node = find_nth_node_of_kind(&tree, "function_definition", 0)
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
        let caller_def_node = find_function_definition_node_by_name(&tree, source, &lang, "caller")
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
            None, // This is the top-level call
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
        let caller_def_node = find_function_definition_node_by_name(&tree, source, &lang, "caller")
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

        let caller_def_node = find_function_definition_node_by_name(&tree, source, &lang, "caller")
            .expect("Could not find function definition node for caller");
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .unwrap();

        // Find the outer call_expression node for value.add(x).sub(y) by navigating from the assignment
        let assignment_node = find_nth_descendant_node_of_kind(
            &caller_def_node, // Search within the caller function
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

        let caller_def_node =
            find_function_definition_node_by_name(&tree, source, &lang, "caller")?; // Use ?
        let caller_node_id = graph
            .node_lookup
            .get(&(Some("Test".to_string()), "caller".to_string()))
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Caller node ID not found in graph"))?; // Use ok_or_else and ?

        // Find the call_expression node wrapping the new_expression within the caller function
        let call_expr_node = find_nth_descendant_node_of_kind(
                &caller_def_node, // Start search within the caller function node
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
}
