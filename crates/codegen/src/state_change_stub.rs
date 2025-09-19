//! Enhanced state change test generation using proper Solidity AST
//!
//! This module generates state change tests using the new AST-based approach with
//! SolidityTestContractBuilder and proper type-safe Solidity code generation.

use tracing::debug;

use crate::teststubs::{
    capitalize_first_letter, to_pascal_case, ContractInfo, FunctionInfo, SolidityTestContract,
    SolidityTestContractBuilder,
};
use anyhow::Result;
use traverse_graph::cg::{CallGraph, EdgeType, NodeType, ParameterInfo};
use traverse_solidity::ast::*;
use traverse_solidity::builder::*;

pub fn generate_state_change_tests_from_cfg(
    graph: &CallGraph,
    ctx: &traverse_graph::cg::CallGraphGeneratorContext,
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
) -> Result<Vec<SolidityTestContract>> {
    let mut test_contracts = Vec::new();

    let func_node = graph.nodes.iter().find(|n| {
        n.contract_name.as_deref() == Some(contract_name)
            && n.name == function_name
            && n.node_type == NodeType::Function
    });

    if let Some(func_node) = func_node {
        for edge in &graph.edges {
            if edge.source_node_id == func_node.id && edge.edge_type == EdgeType::StorageWrite {
                if let Some(var_node) = graph.nodes.get(edge.target_node_id) {
                    if var_node.node_type == NodeType::StorageVariable {
                        let test_contract = create_state_change_test_contract(
                            contract_name,
                            function_name,
                            function_params,
                            var_node,
                            ctx,
                            graph,
                        )?;
                        test_contracts.push(test_contract);
                    }
                }
            }
        }
    }

    Ok(test_contracts)
}

fn create_state_change_test_contract(
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
    var_node: &traverse_graph::cg::Node,
    ctx: &traverse_graph::cg::CallGraphGeneratorContext,
    graph: &CallGraph,
) -> Result<SolidityTestContract> {
    let var_name = &var_node.name;
    let test_contract_name = format!(
        "{}{}StateChangeTest",
        to_pascal_case(contract_name),
        to_pascal_case(function_name)
    );

    let test_function_name = format!("test_{}_changes_{}", function_name, var_name);

    // Get variable type information
    let var_contract_scope = var_node
        .contract_name
        .as_ref()
        .ok_or_else(|| anyhow::anyhow!("Storage variable {} missing contract scope", var_name))?;

    let actual_var_type = ctx
        .state_var_types
        .get(&(var_contract_scope.clone(), var_name.clone()))
        .cloned()
        .unwrap_or_else(|| {
            debug!(
                "Warning: Type for state variable {}.{} not found in ctx.state_var_types. Defaulting to uint256.",
                var_contract_scope, var_name
            );
            "uint256".to_string()
        });

    // Determine getter function name
    let getter_name = if var_node.visibility == traverse_graph::cg::Visibility::Public {
        var_name.clone()
    } else {
        format!("get{}", capitalize_first_letter(var_name))
    };

    let test_contract = SolidityTestContractBuilder::new(test_contract_name.clone())
        .add_import(format!("../src/{}.sol", contract_name))
        .build_with_contract(|contract| {
            // Add state variable for contract instance
            contract.state_variable(
                user_type(contract_name),
                "contractInstance",
                Some(traverse_solidity::ast::Visibility::Private),
                None,
            );

            contract.function("setUp", |func| {
                func.visibility(traverse_solidity::ast::Visibility::Public)
                    .body(|body| {
                        // Deploy contract instance - use constructor parameters from context
                        let constructor_args = if let Some(constructor_node) =
                            graph.nodes.iter().find(|n| {
                                n.contract_name.as_deref() == Some(contract_name)
                                    && n.node_type == NodeType::Constructor
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

            contract.function(&test_function_name, |func| {
                func.visibility(traverse_solidity::ast::Visibility::Public)
                    .body(|body| {
                        // Get initial value
                        let type_name = get_type_name_for_variable(&actual_var_type);
                        let initial_value_expr = Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                                object: Box::new(identifier("contractInstance")),
                                member: getter_name.clone(),
                            })),
                            arguments: vec![],
                        });

                        // Use variable_declaration_with_location for types that require data location
                        if traverse_solidity::builder::requires_data_location(&type_name) {
                            let data_location =
                                traverse_solidity::builder::get_default_data_location(&type_name);
                            body.variable_declaration_with_location(
                                type_name,
                                "initialValue",
                                data_location,
                                Some(initial_value_expr),
                            );
                        } else {
                            body.variable_declaration(
                                type_name,
                                "initialValue",
                                Some(initial_value_expr),
                            );
                        }

                        // Call the function that should change state
                        // Use different values than constructor to ensure state actually changes
                        match generate_different_args_for_function(function_params) {
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
                                debug!("Failed to generate function arguments: {}", e);
                                // Add a comment about the error
                                body.expression(Expression::FunctionCall(FunctionCallExpression {
                                    function: Box::new(identifier(
                                        "// Failed to generate arguments",
                                    )),
                                    arguments: vec![],
                                }));
                            }
                        }

                        // Assert that the value has changed
                        let assert_condition = if actual_var_type == "string" {
                            // For strings, compare keccak256 hashes
                            Expression::Binary(BinaryExpression {
                                left: Box::new(Expression::FunctionCall(FunctionCallExpression {
                                    function: Box::new(identifier("keccak256")),
                                    arguments: vec![Expression::FunctionCall(
                                        FunctionCallExpression {
                                            function: Box::new(identifier("abi.encodePacked")),
                                            arguments: vec![Expression::FunctionCall(
                                                FunctionCallExpression {
                                                    function: Box::new(Expression::MemberAccess(
                                                        MemberAccessExpression {
                                                            object: Box::new(identifier(
                                                                "contractInstance",
                                                            )),
                                                            member: getter_name.clone(),
                                                        },
                                                    )),
                                                    arguments: vec![],
                                                },
                                            )],
                                        },
                                    )],
                                })),
                                operator: BinaryOperator::NotEqual,
                                right: Box::new(Expression::FunctionCall(FunctionCallExpression {
                                    function: Box::new(identifier("keccak256")),
                                    arguments: vec![Expression::FunctionCall(
                                        FunctionCallExpression {
                                            function: Box::new(identifier("abi.encodePacked")),
                                            arguments: vec![identifier("initialValue")],
                                        },
                                    )],
                                })),
                            })
                        } else {
                            // For other types, direct comparison
                            Expression::Binary(BinaryExpression {
                                left: Box::new(Expression::FunctionCall(FunctionCallExpression {
                                    function: Box::new(Expression::MemberAccess(
                                        MemberAccessExpression {
                                            object: Box::new(identifier("contractInstance")),
                                            member: getter_name.clone(),
                                        },
                                    )),
                                    arguments: vec![],
                                })),
                                operator: BinaryOperator::NotEqual,
                                right: Box::new(identifier("initialValue")),
                            })
                        };

                        body.expression(Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(identifier("assertTrue")),
                            arguments: vec![assert_condition],
                        }));
                    });
            });
        });

    Ok(test_contract)
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

fn generate_different_args_for_function(params: &[ParameterInfo]) -> Result<Vec<Expression>> {
    let args = params
        .iter()
        .map(|param| match param.param_type.as_str() {
            "string" => string_literal("updated value"),
            "address" => Expression::FunctionCall(FunctionCallExpression {
                function: Box::new(identifier("address")),
                arguments: vec![number("2")],
            }),
            "bool" => boolean(false), // Different from constructor's true
            t if t.starts_with("uint") => number("100"), // Different from constructor's 42
            t if t.starts_with("int") => number("100"), // Different from constructor's 42
            _ => number("1"),         // Different from constructor's 0
        })
        .collect();

    Ok(args)
}

fn get_type_name_for_variable(type_str: &str) -> TypeName {
    let is_value_type = type_str == "bool"
        || type_str == "address"
        || type_str.starts_with("uint")
        || type_str.starts_with("int")
        || (type_str.starts_with("bytes")
            && type_str.len() > 5
            && type_str.chars().skip(5).all(|c| c.is_ascii_digit()));

    if is_value_type {
        match type_str {
            "bool" => bool(),
            "address" => address(),
            t if t.starts_with("uint") => {
                if let Some(size_str) = t.strip_prefix("uint") {
                    if size_str.is_empty() {
                        uint256()
                    } else if let Ok(size) = size_str.parse::<u16>() {
                        uint(size)
                    } else {
                        uint256()
                    }
                } else {
                    uint256()
                }
            }
            t if t.starts_with("int") => {
                if let Some(size_str) = t.strip_prefix("int") {
                    if size_str.is_empty() {
                        int256()
                    } else if let Ok(size) = size_str.parse::<u16>() {
                        int(size)
                    } else {
                        int256()
                    }
                } else {
                    int256()
                }
            }
            _ => user_type(type_str),
        }
    } else {
        // For reference types, we need to specify memory location
        user_type(type_str)
    }
}

pub fn create_comprehensive_state_change_test_contract(
    contract_info: &ContractInfo,
    function_info: &FunctionInfo,
    graph: &CallGraph,
    ctx: &traverse_graph::cg::CallGraphGeneratorContext,
) -> Result<SolidityTestContract> {
    let test_contracts = generate_state_change_tests_from_cfg(
        graph,
        ctx,
        &contract_info.name,
        &function_info.name,
        &function_info.parameters,
    )?;

    if test_contracts.is_empty() {
        return Err(anyhow::anyhow!(
            "No state change tests could be generated for function {}",
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
    fn test_get_type_name_for_variable() {
        let uint_type = get_type_name_for_variable("uint256");
        assert!(matches!(
            uint_type,
            TypeName::Elementary(ElementaryTypeName::UnsignedInteger(Some(256)))
        ));

        let bool_type = get_type_name_for_variable("bool");
        assert!(matches!(
            bool_type,
            TypeName::Elementary(ElementaryTypeName::Bool)
        ));

        let string_type = get_type_name_for_variable("string");
        assert!(matches!(string_type, TypeName::UserDefined(_)));
    }
}
