use crate::teststubs::{
    capitalize_first_letter, generate_constructor_args, generate_valid_args_for_function,
    to_pascal_case, ContractInfo, Expression, FunctionInfo, SolidityTestBuilder,
    SolidityTestContract, StateVariable, Statement, TestFunction, TestType,
};
use anyhow::Result;
use graph::cg::{CallGraph, EdgeType, NodeType, ParameterInfo, Visibility};

pub fn generate_state_change_tests_from_cfg(
    graph: &CallGraph,
    ctx: &graph::cg::CallGraphGeneratorContext, 
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
) -> Result<Vec<TestFunction>> {
    let mut test_functions = Vec::new();

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
                        let var_name = &var_node.name;
                        let test_name = format!("test_{}_changes_{}", function_name, var_name);

                        let mut body = Vec::new();

                        body.push(Statement::Comment {
                            text: format!("Test that {} modifies {}", function_name, var_name),
                        });

                        let getter_name = if var_node.visibility == Visibility::Public {
                            var_name.clone()
                        } else {
                            format!("get{}", capitalize_first_letter(var_name))
                        };

                        let var_contract_scope =
                            var_node.contract_name.as_ref().ok_or_else(|| {
                                anyhow::anyhow!(
                                    "Storage variable {} missing contract scope",
                                    var_name
                                )
                            })?;

                        let actual_var_type = ctx.state_var_types
                            .get(&(var_contract_scope.clone(), var_name.clone()))
                            .cloned()
                            .unwrap_or_else(|| {
                                eprintln!(
                                    "Warning: Type for state variable {}.{} not found in ctx.state_var_types. Defaulting to uint256.",
                                    var_contract_scope, var_name
                                );
                                "uint256".to_string() 
                            });

                        let type_for_local_var = {
                            let original_type = actual_var_type.as_str();
                            let is_value_type = original_type == "bool" ||
                                                original_type == "address" ||
                                                original_type.starts_with("uint") || 
                                                original_type.starts_with("int") ||  
                                                (original_type.starts_with("bytes") && 
                                                 original_type.len() > 5 && 
                                                 original_type.chars().skip(5).all(|c| c.is_ascii_digit())) ||
                                                // TODO: Add enum check if enum info becomes available in context
                                                original_type.contains("=>"); // Mappings are not assigned this way, but good to exclude

                            if is_value_type {
                                original_type.to_string()
                            } else {
                                // Assume reference types (string, bytes, arrays, structs) need "memory"
                                format!("{} memory", original_type)
                            }
                        };

                        body.push(Statement::VariableDeclaration {
                            var_type: type_for_local_var, // Use modified type
                            name: "initialValue".to_string(),
                            value: Some(Expression::FunctionCall {
                                target: Some("contractInstance".to_string()),
                                function: getter_name.clone(),
                                args: vec![],
                            }),
                        });

                        body.push(Statement::FunctionCall {
                            target: Some("contractInstance".to_string()),
                            function: function_name.to_string(),
                            args: generate_valid_args_for_function(function_params, None)?,
                        });

                        let assert_condition = if actual_var_type == "string" {
                            Expression::BinaryOp {
                                left: Box::new(Expression::FunctionCall {
                                    target: None,
                                    function: "keccak256".to_string(),
                                    args: vec![Expression::FunctionCall {
                                        target: None,
                                        function: "abi.encodePacked".to_string(),
                                        args: vec![Expression::FunctionCall {
                                            target: Some("contractInstance".to_string()),
                                            function: getter_name.clone(),
                                            args: vec![],
                                        }],
                                    }],
                                }),
                                operator: "!=".to_string(),
                                right: Box::new(Expression::FunctionCall {
                                    target: None,
                                    function: "keccak256".to_string(),
                                    args: vec![Expression::FunctionCall {
                                        target: None,
                                        function: "abi.encodePacked".to_string(),
                                        args: vec![Expression::Identifier(
                                            "initialValue".to_string(),
                                        )],
                                    }],
                                }),
                            }
                        } else {
                            Expression::BinaryOp {
                                left: Box::new(Expression::FunctionCall {
                                    target: Some("contractInstance".to_string()),
                                    function: getter_name.clone(),
                                    args: vec![],
                                }),
                                operator: "!=".to_string(),
                                right: Box::new(Expression::Identifier("initialValue".to_string())),
                            }
                        };

                        body.push(Statement::Assert {
                            condition: assert_condition,
                        });

                        test_functions.push(TestFunction {
                            name: test_name,
                            visibility: "public".to_string(),
                            body,
                            test_type: TestType::StateChangeTest {
                                variable_name: var_name.clone(),
                                expected_change: "value should change".to_string(),
                            },
                        });
                    }
                }
            }
        }
    }

    Ok(test_functions)
}

pub fn create_state_test_contract(
    contract_info: &ContractInfo,
    function_info: &FunctionInfo,
    state_tests: Vec<TestFunction>,
) -> SolidityTestContract {
    let mut builder = SolidityTestBuilder::new(format!(
        "{}{}StateTest",
        to_pascal_case(&contract_info.name),
        to_pascal_case(&function_info.name)
    ))
    .add_import(format!("../src/{}.sol", contract_info.name))
    .add_state_variable(StateVariable {
        name: "contractInstance".to_string(),
        var_type: contract_info.name.clone(),
        visibility: "private".to_string(),
        initial_value: None,
    })
    .set_setup_function(crate::teststubs::SetupFunction {
        body: vec![
            Statement::Comment {
                text: format!("Deploy {} for testing", contract_info.name),
            },
            Statement::Assignment {
                target: "contractInstance".to_string(),
                value: Expression::FunctionCall {
                    target: None,
                    function: format!("new {}", contract_info.name),
                    args: generate_constructor_args(&contract_info.constructor_params, None),
                },
            },
        ],
    });

    for test_func in state_tests {
        builder = builder.add_test_function(test_func);
    }

    builder.build()
}

fn get_type_for_local_variable_declaration(original_type: &str) -> String {
    let is_value_type = original_type == "bool" ||
                        original_type == "address" ||
                        original_type.starts_with("uint") || 
                        original_type.starts_with("int") ||  
                        (original_type.starts_with("bytes") && 
                         original_type.len() > 5 && 
                         original_type.chars().skip(5).all(|c| c.is_ascii_digit())) ||
                        // TODO: Add enum check if enum info becomes available in context
                        original_type.contains("=>"); // Mappings are not assigned this way, but good to exclude

    if is_value_type {
        original_type.to_string()
    } else {
        format!("{} memory", original_type)
    }
}
