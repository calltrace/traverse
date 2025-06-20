use crate::teststubs::{
    generate_constructor_args,
    sanitize_identifier, to_pascal_case, ContractInfo,
    Expression, FunctionInfo, SolidityTestBuilder, SolidityTestContract, StateVariable, Statement,
    TestFunction, TestType,
};
use crate::invariant_breaker::{break_invariant, InvariantBreakerValue};
use anyhow::Result;
use graph::cg::{CallGraph, EdgeType, NodeType, ParameterInfo};
use std::collections::HashMap;

/// Revert test generation that uses invariant breaker to find counterexamples
pub fn generate_revert_tests_from_cfg(
    graph: &CallGraph,
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
) -> Result<Vec<TestFunction>> {
    eprintln!(
        "[REVERT DEBUG] Starting enhanced revert test generation for {}.{}",
        contract_name, function_name
    );
    eprintln!(
        "[REVERT DEBUG] Function has {} parameters",
        function_params.len()
    );
    for (i, param) in function_params.iter().enumerate() {
        eprintln!(
            "[REVERT DEBUG] Param {}: {} {}",
            i, param.param_type, param.name
        );
    }

    let mut test_functions = Vec::new();

    let func_node = graph.nodes.iter().find(|n| {
        n.contract_name.as_deref() == Some(contract_name)
            && n.name == function_name
            && n.node_type == NodeType::Function
    });

    if let Some(func_node) = func_node {
        eprintln!(
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

        eprintln!(
            "[REVERT DEBUG] Found {} require edges for function",
            require_edges.len()
        );

        for (edge_idx, edge) in require_edges.iter().enumerate() {
            eprintln!(
                "[REVERT DEBUG] Processing require edge {} of {}",
                edge_idx + 1,
                require_edges.len()
            );
            eprintln!(
                "[REVERT DEBUG] Edge details: source={}, target={}, seq={}",
                edge.source_node_id, edge.target_node_id, edge.sequence_number
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

            eprintln!(
                "[REVERT DEBUG] Processing condition: '{}'",
                descriptive_condition_text
            );

            let condition_expression = descriptive_condition_text.clone();
            
            eprintln!(
                "[REVERT DEBUG] Attempting to break invariant for condition: '{}'",
                condition_expression
            );
            
            let rt = tokio::runtime::Runtime::new().unwrap();
            let invariant_result = match rt.block_on(break_invariant(&condition_expression)) {
                Ok(result) => {
                    eprintln!(
                        "[REVERT DEBUG] Invariant breaker for '{}': success={}, entries={}",
                        condition_expression, result.success, result.entries.len()
                    );
                    if result.success && !result.entries.is_empty() {
                        Some(result)
                    } else {
                        eprintln!(
                            "[REVERT DEBUG] Invariant breaker did not find a counterexample for '{}'",
                            condition_expression
                        );
                        None
                    }
                }
                Err(e) => {
                    eprintln!(
                        "[REVERT DEBUG] Error calling invariant breaker for '{}': {}",
                        condition_expression, e
                    );
                    None
                }
            };

            if invariant_result.is_none() {
                eprintln!(
                    "[REVERT DEBUG] Skipping test for condition '{}' - invariant breaker did not provide a usable counterexample or failed.",
                    condition_expression
                );
                continue;
            }

            let mut test_name_condition_identifier = sanitize_identifier(&descriptive_condition_text);
            if test_name_condition_identifier.len() > 40 { 
                test_name_condition_identifier.truncate(40);
                while test_name_condition_identifier.ends_with('_') {
                    test_name_condition_identifier.pop(); 
                }
            }
             if test_name_condition_identifier.is_empty() { 
                test_name_condition_identifier = "condition".to_string(); 
            }

            let test_name = format!(
                "test_{}_reverts_{}_{}",
                function_name,
                test_name_condition_identifier,
                edge_idx
            );
            eprintln!("[REVERT DEBUG] Generated test name: '{}'", test_name);

            let mut body = Vec::new();

            // Step a.3: Extract computed variables and use them for vm.prank() if needed
            let invariant_result = invariant_result.unwrap(); // Safe unwrap based on check above
            let first_entry = &invariant_result.entries[0];
            
            let (needs_prank, prank_address) = extract_prank_info(&first_entry.variables, &condition_expression);

            eprintln!(
                "[REVERT DEBUG] Prank analysis: needs_prank={}, address={:?}",
                needs_prank, prank_address
            );
            
            if needs_prank {
                if let Some(prank_addr) = prank_address {
                    body.push(Statement::FunctionCall {
                        target: Some("vm".to_string()),
                        function: "prank".to_string(),
                        args: vec![Expression::Literal(prank_addr)],
                    });
                } else {
                    eprintln!(
                        "[REVERT DEBUG] Prank needed but no address found in invariant breaker results"
                    );
                    continue; // Skip this test if we need a prank but don't have an address
                }
            }

            body.push(Statement::Comment {
                text: format!("Test that {} reverts when: {}", function_name, descriptive_condition_text),
            });

            // Add invariant breaker information as comments
            body.push(Statement::Comment {
                text: format!("Invariant breaker found {} counterexample(s)", invariant_result.entries.len()),
            });
            
            for (var_name, var_value) in &first_entry.variables {
                body.push(Statement::Comment {
                    text: format!("Counterexample: {} = {}", var_name, format_invariant_value(var_value)),
                });
            }

            body.push(Statement::ExpectRevert {
                error_message: error_message.clone(),
            });

            // Generate function arguments using invariant breaker results
            eprintln!("[REVERT DEBUG] Generating function arguments...");

            let function_args = match generate_args_from_invariant_result(function_params, &first_entry.variables) {
                Ok(args) => args,
                Err(e) => {
                    eprintln!(
                        "[REVERT DEBUG] Failed to generate arguments from invariant result: {}",
                        e
                    );
                    continue; // Skip this test if we can't generate arguments
                }
            };

            eprintln!(
                "[REVERT DEBUG] Generated {} function arguments",
                function_args.len()
            );
            for (i, arg) in function_args.iter().enumerate() {
                eprintln!("[REVERT DEBUG] Function arg {}: {:?}", i, arg);
            }

            body.push(Statement::FunctionCall {
                target: Some("contractInstance".to_string()),
                function: function_name.to_string(),
                args: function_args,
            });

            if needs_prank {
                body.push(Statement::FunctionCall {
                    target: Some("vm".to_string()),
                    function: "stopPrank".to_string(),
                    args: vec![],
                });
            }

            let test_function = TestFunction {
                name: test_name.clone(),
                visibility: "public".to_string(),
                body,
                test_type: TestType::RevertTest {
                    expected_error: error_message,
                    condition: descriptive_condition_text,
                },
            };

            eprintln!("[REVERT DEBUG] Created enhanced test function: '{}'", test_name);
            test_functions.push(test_function);
        }
    } else {
        eprintln!(
            "[REVERT DEBUG] Function node not found in graph for {}.{}",
            contract_name, function_name
        );
        eprintln!("[REVERT DEBUG] Available nodes in graph:");
        for (i, node) in graph.nodes.iter().enumerate() {
            eprintln!(
                "[REVERT DEBUG] Node {}: contract={:?}, name='{}', type={:?}",
                i, node.contract_name, node.name, node.node_type
            );
        }
    }

    eprintln!(
        "[REVERT DEBUG] Generated {} enhanced revert test functions",
        test_functions.len()
    );
    Ok(test_functions)
}

fn extract_prank_info(
    variables: &HashMap<String, InvariantBreakerValue>,
    condition: &str,
) -> (bool, Option<String>) {
    // Check if condition involves msg.sender or similar access control
    let lower_condition = condition.to_lowercase();
    let involves_sender = lower_condition.contains("msg.sender") || lower_condition.contains("caller");
    let involves_access_control = lower_condition.contains("owner") 
        || lower_condition.contains("admin") 
        || lower_condition.contains("role");
    
    if involves_sender && involves_access_control {
        // Look for address variables in the counterexample
        for (var_name, var_value) in variables {
            if let InvariantBreakerValue::Address(addr) = var_value {
                eprintln!(
                    "[REVERT DEBUG] Found address variable '{}' = '{}' for prank",
                    var_name, addr
                );
                return (true, Some(addr.clone()));
            }
        }
        
        // No address found in variables
        eprintln!(
            "[REVERT DEBUG] No address variable found in invariant breaker results for condition: {}",
            condition
        );
        (true, None) // We need a prank but don't have an address
    } else {
        (false, None)
    }
}

fn format_invariant_value(value: &InvariantBreakerValue) -> String {
    match value {
        InvariantBreakerValue::Bool(b) => b.to_string(),
        InvariantBreakerValue::UInt(n) => n.to_string(),
        InvariantBreakerValue::Int(n) => n.to_string(),
        InvariantBreakerValue::String(s) => format!("\"{}\"", s),
        InvariantBreakerValue::Address(addr) => addr.clone(),
        InvariantBreakerValue::Bytes(b) => format!("0x{}", hex::encode(b)),
    }
}

fn generate_args_from_invariant_result(
    function_params: &[ParameterInfo],
    variables: &HashMap<String, InvariantBreakerValue>,
) -> Result<Vec<Expression>> {
    let mut args = Vec::new();
    let mut missing_params = Vec::new();
    
    for param in function_params {
        if let Some(var_value) = variables.get(&param.name) {
            eprintln!(
                "[REVERT DEBUG] Using invariant value for param '{}': {:?}",
                param.name, var_value
            );
            args.push(invariant_value_to_expression(var_value));
        } else {
            eprintln!(
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

fn invariant_value_to_expression(value: &InvariantBreakerValue) -> Expression {
    match value {
        InvariantBreakerValue::Bool(b) => Expression::Literal(b.to_string()),
        InvariantBreakerValue::UInt(n) => Expression::Literal(n.to_string()),
        InvariantBreakerValue::Int(n) => Expression::Literal(n.to_string()),
        InvariantBreakerValue::String(s) => Expression::Literal(format!("\"{}\"", s)),
        InvariantBreakerValue::Address(addr) => Expression::Literal(addr.clone()),
        InvariantBreakerValue::Bytes(b) => Expression::Literal(format!("0x{}", hex::encode(b))),
    }
}

pub fn create_revert_test_contract(
    contract_info: &ContractInfo,
    function_info: &FunctionInfo,
    revert_tests: Vec<TestFunction>,
) -> SolidityTestContract {
    eprintln!(
        "[REVERT DEBUG] Creating enhanced revert test contract for {}.{}",
        contract_info.name, function_info.name
    );
    eprintln!(
        "[REVERT DEBUG] Contract has constructor: {}",
        contract_info.has_constructor
    );
    eprintln!(
        "[REVERT DEBUG] Constructor has {} parameters",
        contract_info.constructor_params.len()
    );
    eprintln!(
        "[REVERT DEBUG] Creating contract with {} revert tests",
        revert_tests.len()
    );

    let contract_name = format!(
        "{}{}EnhancedRevertTest",
        contract_info.name,
        to_pascal_case(&function_info.name)
    );
    eprintln!(
        "[REVERT DEBUG] Generated contract name: '{}'",
        contract_name
    );

    // Generate constructor arguments for setup
    eprintln!("[REVERT DEBUG] Generating constructor arguments for setup...");
    let constructor_args = generate_constructor_args(&contract_info.constructor_params, None);
    eprintln!(
        "[REVERT DEBUG] Generated {} constructor arguments",
        constructor_args.len()
    );
    for (i, arg) in constructor_args.iter().enumerate() {
        eprintln!("[REVERT DEBUG] Constructor arg {}: {:?}", i, arg);
    }

    let mut builder = SolidityTestBuilder::new(contract_name)
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
                        args: constructor_args,
                    },
                },
            ],
        });

    for (i, test_func) in revert_tests.iter().enumerate() {
        eprintln!(
            "[REVERT DEBUG] Adding test function {}: '{}'",
            i + 1,
            test_func.name
        );
        builder = builder.add_test_function(test_func.clone());
    }

    let contract = builder.build();
    eprintln!(
        "[REVERT DEBUG] Built enhanced revert test contract with {} functions",
        contract.functions.len()
    );
    contract
}
