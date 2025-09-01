//! Enhanced access control test generation using proper Solidity AST
//!
//! This module generates access control tests using the new AST-based approach with
//! SolidityTestContractBuilder and proper type-safe Solidity code generation.

use crate::teststubs::{
    generate_valid_args_for_function, to_pascal_case, ContractInfo, FunctionInfo,
    SolidityTestContract, SolidityTestContractBuilder,
};
use anyhow::Result;
use graph::cg::{CallGraph, EdgeType, NodeType, ParameterInfo};
use solidity::ast::*;
use solidity::builder::*;

pub fn generate_access_control_tests_from_cfg(
    graph: &CallGraph,
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
    constructor_params: &[ParameterInfo],
) -> Result<Vec<SolidityTestContract>> {
    let mut test_contracts = Vec::new();

    let func_node = graph.nodes.iter().find(|n| {
        n.contract_name.as_deref() == Some(contract_name)
            && n.name == function_name
            && n.node_type == NodeType::Function
    });

    if let Some(func_node) = func_node {
        let require_edges: Vec<_> = graph
            .edges
            .iter()
            .filter(|edge| {
                edge.source_node_id == func_node.id && edge.edge_type == EdgeType::Require
            })
            .collect();

        for (edge_idx, edge) in require_edges.iter().enumerate() {
            let condition_node = &graph.nodes[edge.target_node_id];
            let condition_text = condition_node
                .condition_expression
                .as_ref()
                .filter(|s| !s.is_empty())
                .cloned()
                .unwrap_or_else(|| condition_node.name.clone());

            if is_access_control_condition(&condition_text) {
                let test_contract = create_access_control_test_contract(
                    contract_name,
                    function_name,
                    function_params,
                    constructor_params,
                    &condition_text,
                    &condition_node.revert_message.clone().unwrap_or_default(),
                    edge_idx,
                )?;

                test_contracts.push(test_contract);
            }
        }
    }

    Ok(test_contracts)
}

fn create_access_control_test_contract(
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
    constructor_params: &[ParameterInfo],
    _condition: &str,
    error_message: &str,
    edge_idx: usize,
) -> Result<SolidityTestContract> {
    let test_contract_name = format!(
        "{}{}AccessControlTest{}",
        contract_name,
        to_pascal_case(function_name),
        edge_idx
    );

    let test_function_name = format!("test_{}_access_control_{}", function_name, edge_idx);

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

            contract.function("setUp", |func| {
                func.visibility(Visibility::Public).body(|body| {
                    // Deploy contract instance
                    let constructor_args =
                        generate_constructor_args_as_expressions(constructor_params);

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

            contract.function(&test_function_name, |func| {
                func.visibility(Visibility::Public).body(|body| {
                    body.expression(Expression::FunctionCall(FunctionCallExpression {
                        function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                            object: Box::new(identifier("vm")),
                            member: "prank".to_string(),
                        })),
                        arguments: vec![Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(identifier("address")),
                            arguments: vec![number("0x1")],
                        })],
                    }));

                    // Add expectRevert
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

                    match generate_valid_args_for_function(function_params, None) {
                        Ok(function_args) => {
                            body.expression(Expression::FunctionCall(FunctionCallExpression {
                                function: Box::new(Expression::MemberAccess(
                                    MemberAccessExpression {
                                        object: Box::new(identifier("contractInstance")),
                                        member: function_name.to_string(),
                                    },
                                )),
                                arguments: function_args,
                            }));
                        }
                        Err(e) => {
                            eprintln!("Failed to generate function arguments: {}", e);
                            // Add a comment about the error
                            body.expression(Expression::FunctionCall(FunctionCallExpression {
                                function: Box::new(identifier("// Failed to generate arguments")),
                                arguments: vec![],
                            }));
                        }
                    }

                    body.expression(Expression::FunctionCall(FunctionCallExpression {
                        function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                            object: Box::new(identifier("vm")),
                            member: "stopPrank".to_string(),
                        })),
                        arguments: vec![],
                    }));
                });
            });
        });

    Ok(test_contract)
}

fn is_access_control_condition(condition: &str) -> bool {
    let lower_condition = condition.to_lowercase();
    lower_condition.contains("msg.sender")
        || lower_condition.contains("owner")
        || lower_condition.contains("admin")
        || lower_condition.contains("role")
        || lower_condition.contains("authorized")
        || lower_condition.contains("permission")
}

fn generate_constructor_args_as_expressions(params: &[ParameterInfo]) -> Vec<Expression> {
    params
        .iter()
        .map(|param| match param.param_type.as_str() {
            "string" => string_literal("test"),
            "address" => Expression::FunctionCall(FunctionCallExpression {
                function: Box::new(identifier("address")),
                arguments: vec![number("1")],
            }),
            "bool" => boolean(true),
            t if t.starts_with("uint") => number("42"),
            t if t.starts_with("int") => number("42"),
            _ => number("0"), // Default fallback
        })
        .collect()
}

pub fn create_comprehensive_access_control_test_contract(
    contract_info: &ContractInfo,
    function_info: &FunctionInfo,
    graph: &CallGraph,
) -> Result<SolidityTestContract> {
    let test_contracts = generate_access_control_tests_from_cfg(
        graph,
        &contract_info.name,
        &function_info.name,
        &function_info.parameters,
        &contract_info.constructor_params,
    )?;

    if test_contracts.is_empty() {
        return Err(anyhow::anyhow!(
            "No access control tests could be generated for function {}",
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
    fn test_is_access_control_condition() {
        assert!(is_access_control_condition("msg.sender == owner"));
        assert!(is_access_control_condition("onlyOwner"));
        assert!(is_access_control_condition(
            "hasRole(ADMIN_ROLE, msg.sender)"
        ));
        assert!(!is_access_control_condition("balance > 0"));
        assert!(!is_access_control_condition("amount < 1000"));
    }
}
