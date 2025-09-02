//! Enhanced revert test generation using proper Solidity AST
//!
//! This module generates revert tests using the new AST-based approach with
//! SolidityTestContractBuilder and proper type-safe Solidity code generation.

use tracing::debug;

use crate::teststubs::{
    sanitize_identifier, to_pascal_case, ContractInfo, FunctionInfo,
    SolidityTestContract, SolidityTestContractBuilder,
};
use crate::invariant_breaker::{break_invariant, InvariantBreakerValue};
use anyhow::Result;
use graph::cg::{CallGraph, EdgeType, NodeType, ParameterInfo};
use std::collections::HashMap;

use solidity::ast::*;
use solidity::builder::*;

pub fn generate_revert_tests_from_cfg(
    graph: &CallGraph,
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
) -> Result<Vec<SolidityTestContract>> {
    debug!(
        "[REVERT DEBUG] Starting enhanced revert test generation for {}.{}",
        contract_name, function_name
    );
    debug!(
        "[REVERT DEBUG] Function has {} parameters",
        function_params.len()
    );
    for (i, param) in function_params.iter().enumerate() {
        debug!(
            "[REVERT DEBUG] Param {}: {} {}",
            i, param.param_type, param.name
        );
    }

    let mut test_contracts = Vec::new();

    let func_node = graph.nodes.iter().find(|n| {
        n.contract_name.as_deref() == Some(contract_name)
            && n.name == function_name
            && n.node_type == NodeType::Function
    });

    if let Some(func_node) = func_node {
        debug!(
            "[REVERT DEBUG] Found function node with ID: {}",
            func_node.id
        );

        let require_edges: Vec<_> = graph
            .edges
            .iter()
            .filter(|edge| {
                edge.source_node_id == func_node.id && edge.edge_type == EdgeType::Require
            })
            .collect();

        debug!(
            "[REVERT DEBUG] Found {} require edges for function",
            require_edges.len()
        );

        for (edge_idx, edge) in require_edges.iter().enumerate() {
            debug!(
                "[REVERT DEBUG] Processing require edge {} of {}",
                edge_idx + 1,
                require_edges.len()
            );

            let condition_node = &graph.nodes[edge.target_node_id];
            let original_condition_name = condition_node.name.clone();

            let descriptive_condition_text = condition_node
                .condition_expression
                .as_ref()
                .filter(|s| !s.is_empty())
                .cloned()
                .unwrap_or_else(|| original_condition_name.clone());

            let error_message = condition_node.revert_message.clone().unwrap_or_default();

            debug!(
                "[REVERT DEBUG] Processing condition: '{}'",
                descriptive_condition_text
            );

            let rt = tokio::runtime::Runtime::new().unwrap();
            let invariant_result = match rt.block_on(break_invariant(&descriptive_condition_text)) {
                Ok(result) => {
                    debug!(
                        "[REVERT DEBUG] Invariant breaker for '{}': success={}, entries={}",
                        descriptive_condition_text, result.success, result.entries.len()
                    );
                    if result.success && !result.entries.is_empty() {
                        Some(result)
                    } else {
                        debug!(
                            "[REVERT DEBUG] Invariant breaker did not find a counterexample for '{}'",
                            descriptive_condition_text
                        );
                        None
                    }
                }
                Err(e) => {
                    debug!(
                        "[REVERT DEBUG] Error calling invariant breaker for '{}': {}",
                        descriptive_condition_text, e
                    );
                    None
                }
            };

            if let Some(invariant_result) = invariant_result {
                let test_contract = create_revert_test_contract(
                    graph,
                    contract_name,
                    function_name,
                    function_params,
                    &descriptive_condition_text,
                    &error_message,
                    &invariant_result,
                    edge_idx,
                )?;

                test_contracts.push(test_contract);
            } else {
                debug!(
                    "[REVERT DEBUG] Skipping test for condition '{}' - no counterexample found",
                    descriptive_condition_text
                );
            }
        }
    } else {
        debug!(
            "[REVERT DEBUG] Function node not found in graph for {}.{}",
            contract_name, function_name
        );
    }

    debug!(
        "[REVERT DEBUG] Generated {} enhanced revert test contracts",
        test_contracts.len()
    );
    Ok(test_contracts)
}

fn create_revert_test_contract(
    graph: &CallGraph,
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
    condition: &str,
    error_message: &str,
    invariant_result: &crate::invariant_breaker::InvariantBreakerResult,
    edge_idx: usize,
) -> Result<SolidityTestContract> {
    let mut test_name_condition_identifier = sanitize_identifier(condition);
    if test_name_condition_identifier.len() > 40 {
        test_name_condition_identifier.truncate(40);
        while test_name_condition_identifier.ends_with('_') {
            test_name_condition_identifier.pop();
        }
    }
    if test_name_condition_identifier.is_empty() {
        test_name_condition_identifier = "condition".to_string();
    }

    let test_contract_name = format!(
        "{}{}RevertTest{}",
        contract_name,
        to_pascal_case(function_name),
        edge_idx
    );

    let test_function_name = format!(
        "test_{}_reverts_{}",
        function_name, test_name_condition_identifier
    );

    debug!(
        "[REVERT DEBUG] Creating test contract '{}' with function '{}'",
        test_contract_name, test_function_name
    );

    // Extract counterexample variables
    let first_entry = &invariant_result.entries[0];
    let (needs_prank, prank_address) = extract_prank_info(&first_entry.variables, condition);

    // Build the test contract using SolidityTestContractBuilder
    let test_contract = SolidityTestContractBuilder::new(test_contract_name.clone())
        .add_import(format!("../src/{}.sol", contract_name))
        .build_with_contract(|contract| {
            // Add state variable for contract instance
            contract.state_variable(
                user_type(contract_name),
                "contractInstance",
                Some(Visibility::Private),
                None,
            );

            // Add setUp function
            contract.function("setUp", |func| {
                func.visibility(Visibility::Public).body(|body| {
                    // Deploy contract instance - use constructor parameters from call graph
                    let constructor_args = if let Some(constructor_node) = graph.nodes.iter().find(|n| {
                        n.contract_name.as_deref() == Some(contract_name) && 
                        n.node_type == NodeType::Constructor
                    }) {
                        generate_constructor_args_as_expressions(&constructor_node.parameters)
                    } else {
                        // Fallback to empty args if no constructor found
                        vec![]
                    };
                    
                    body.expression(Expression::Assignment(AssignmentExpression {
                        left: Box::new(identifier("contractInstance")),
                        operator: AssignmentOperator::Assign,
                        right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(identifier(format!("new {}", contract_name))),
                            arguments: constructor_args,
                        })),
                    }));
                });
            });

            // Add the revert test function
            contract.function(&test_function_name, |func| {
                func.visibility(Visibility::Public).body(|body| {
                    // Add prank if needed
                    if needs_prank {
                        if let Some(prank_addr) = prank_address {
                            body.expression(Expression::FunctionCall(FunctionCallExpression {
                                function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                                    object: Box::new(identifier("vm")),
                                    member: "prank".to_string(),
                                })),
                                arguments: vec![string_literal(&prank_addr)],
                            }));
                        }
                    }

                    // Add expectRevert - convert statement to expression
                    if !error_message.is_empty() {
                        body.expression(Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                                object: Box::new(identifier("vm")),
                                member: "expectRevert".to_string(),
                            })),
                            arguments: vec![Expression::FunctionCall(FunctionCallExpression {
                                function: Box::new(identifier("bytes")),
                                arguments: vec![string_literal(error_message)],
                            })],
                        }));
                    } else {
                        body.expression(Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                                object: Box::new(identifier("vm")),
                                member: "expectRevert".to_string(),
                            })),
                            arguments: vec![],
                        }));
                    }

                    // Generate function call with counterexample arguments
                    match generate_function_args_from_invariant(
                        function_params,
                        &first_entry.variables,
                    ) {
                        Ok(function_args) => {
                            body.expression(Expression::FunctionCall(FunctionCallExpression {
                                function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                                    object: Box::new(identifier("contractInstance")),
                                    member: function_name.to_string(),
                                })),
                                arguments: function_args,
                            }));
                        }
                        Err(e) => {
                            debug!("[REVERT DEBUG] Failed to generate function arguments: {}", e);
                            // Add a comment about the error
                            body.expression(Expression::FunctionCall(FunctionCallExpression {
                                function: Box::new(identifier("// Failed to generate arguments")),
                                arguments: vec![],
                            }));
                        }
                    }

                    // Stop prank if needed
                    if needs_prank {
                        body.expression(Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                                object: Box::new(identifier("vm")),
                                member: "stopPrank".to_string(),
                            })),
                            arguments: vec![],
                        }));
                    }
                });
            });
        });

    Ok(test_contract)
}

fn extract_prank_info(
    variables: &HashMap<String, InvariantBreakerValue>,
    condition: &str,
) -> (bool, Option<String>) {
    let lower_condition = condition.to_lowercase();
    let involves_sender = lower_condition.contains("msg.sender") || lower_condition.contains("caller");
    let involves_access_control = lower_condition.contains("owner")
        || lower_condition.contains("admin")
        || lower_condition.contains("role");

    if involves_sender && involves_access_control {
        // Look for address variables in the counterexample
        for (var_name, var_value) in variables {
            if let InvariantBreakerValue::Address(addr) = var_value {
                debug!(
                    "[REVERT DEBUG] Found address variable '{}' = '{}' for prank",
                    var_name, addr
                );
                return (true, Some(addr.clone()));
            }
        }

        debug!(
            "[REVERT DEBUG] No address variable found in invariant breaker results for condition: {}",
            condition
        );
        (true, None) // We need a prank but don't have an address
    } else {
        (false, None)
    }
}

fn generate_constructor_args_as_expressions(params: &[ParameterInfo]) -> Vec<Expression> {
    params
        .iter()
        .map(|param| {
            match param.param_type.as_str() {
                "string" => string_literal("test"),
                "address" => Expression::FunctionCall(FunctionCallExpression {
                    function: Box::new(identifier("address")),
                    arguments: vec![number("1")],
                }),
                "bool" => boolean(true),
                t if t.starts_with("uint") => number("42"),
                t if t.starts_with("int") => number("42"),
                _ => number("0"), // Default fallback
            }
        })
        .collect()
}

fn generate_function_args_from_invariant(
    function_params: &[ParameterInfo],
    variables: &HashMap<String, InvariantBreakerValue>,
) -> Result<Vec<Expression>> {
    let mut args = Vec::new();
    let mut missing_params = Vec::new();

    for param in function_params {
        if let Some(var_value) = variables.get(&param.name) {
            debug!(
                "[REVERT DEBUG] Using invariant value for param '{}': {:?}",
                param.name, var_value
            );
            // Convert the invariant value to match the parameter's Solidity type
            args.push(invariant_value_to_expression_with_type(var_value, &param.param_type));
        } else {
            debug!(
                "[REVERT DEBUG] No invariant value found for param '{}'",
                param.name
            );
            missing_params.push(param.name.clone());
        }
    }

    if !missing_params.is_empty() {
        return Err(anyhow::anyhow!(
            "Missing invariant values for parameters: {}",
            missing_params.join(", ")
        ));
    }

    Ok(args)
}

fn invariant_value_to_expression_with_type(value: &InvariantBreakerValue, solidity_type: &str) -> Expression {
    match value {
        InvariantBreakerValue::Bool(b) => boolean(*b),
        InvariantBreakerValue::UInt(n) => {
            // For uint types, ensure the value is non-negative
            // UInt values are already non-negative
            number(n.to_string())
        },
        InvariantBreakerValue::Int(n) => {
            // Check if the target type is uint - if so, convert to positive
            if solidity_type.starts_with("uint") {
                // UInt values are already non-negative
            number(n.to_string())
            } else {
                number(n.to_string())
            }
        },
        InvariantBreakerValue::String(s) => string_literal(s),
        InvariantBreakerValue::Address(addr) => {
            // For addresses, we might want to use address() cast or just the literal
            if addr.starts_with("0x") {
                Expression::FunctionCall(FunctionCallExpression {
                    function: Box::new(identifier("address")),
                    arguments: vec![Expression::Literal(Literal::HexString(HexStringLiteral {
                        value: addr.strip_prefix("0x").unwrap_or(addr).to_string(),
                    }))],
                })
            } else {
                string_literal(addr)
            }
        }
        InvariantBreakerValue::Bytes(b) => {
            Expression::Literal(Literal::HexString(HexStringLiteral {
                value: hex::encode(b),
            }))
        }
    }
}

#[allow(dead_code)]
fn invariant_value_to_expression(value: &InvariantBreakerValue) -> Expression {
    match value {
        InvariantBreakerValue::Bool(b) => boolean(*b),
        InvariantBreakerValue::UInt(n) => {
            // Ensure uint values are non-negative
            // UInt values are already non-negative
            number(n.to_string())
        },
        InvariantBreakerValue::Int(n) => number(n.to_string()),
        InvariantBreakerValue::String(s) => string_literal(s),
        InvariantBreakerValue::Address(addr) => {
            // For addresses, we might want to use address() cast or just the literal
            if addr.starts_with("0x") {
                Expression::FunctionCall(FunctionCallExpression {
                    function: Box::new(identifier("address")),
                    arguments: vec![Expression::Literal(Literal::HexString(HexStringLiteral {
                        value: addr.strip_prefix("0x").unwrap_or(addr).to_string(),
                    }))],
                })
            } else {
                string_literal(addr)
            }
        }
        InvariantBreakerValue::Bytes(b) => {
            Expression::Literal(Literal::HexString(HexStringLiteral {
                value: hex::encode(b),
            }))
        }
    }
}

pub fn create_comprehensive_revert_test_contract(
    contract_info: &ContractInfo,
    function_info: &FunctionInfo,
    graph: &CallGraph,
) -> Result<SolidityTestContract> {
    debug!(
        "[REVERT DEBUG] Creating comprehensive revert test contract for {}.{}",
        contract_info.name, function_info.name
    );

    let test_contracts = generate_revert_tests_from_cfg(
        graph,
        &contract_info.name,
        &function_info.name,
        &function_info.parameters,
    )?;

    if test_contracts.is_empty() {
        return Err(anyhow::anyhow!(
            "No revert tests could be generated for function {}",
            function_info.name
        ));
    }

    // For now, return the first test contract
    // In a more sophisticated implementation, we might combine multiple test contracts
    Ok(test_contracts.into_iter().next().unwrap())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invariant_value_to_expression() {
        let bool_val = InvariantBreakerValue::Bool(true);
        let expr = invariant_value_to_expression(&bool_val);
        assert!(matches!(expr, Expression::Literal(Literal::Boolean(true))));

        let uint_val = InvariantBreakerValue::UInt(42);
        let expr = invariant_value_to_expression(&uint_val);
        if let Expression::Literal(Literal::Number(num)) = expr {
            assert_eq!(num.value, "42");
        } else {
            panic!("Expected number literal");
        }

        let string_val = InvariantBreakerValue::String("test".to_string());
        let expr = invariant_value_to_expression(&string_val);
        if let Expression::Literal(Literal::String(s)) = expr {
            assert_eq!(s.value, "test");
        } else {
            panic!("Expected string literal");
        }
    }

    #[test]
    fn test_extract_prank_info() {
        let mut variables = HashMap::new();
        variables.insert(
            "caller".to_string(),
            InvariantBreakerValue::Address("0x1234567890123456789012345678901234567890".to_string()),
        );

        let condition = "msg.sender == owner";
        let (needs_prank, prank_address) = extract_prank_info(&variables, condition);

        assert!(needs_prank);
        assert_eq!(
            prank_address,
            Some("0x1234567890123456789012345678901234567890".to_string())
        );
    }
}
