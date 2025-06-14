use crate::teststubs::{
    capitalize_first_letter, generate_constructor_args, generate_realistic_args_for_function,
    generate_valid_args_for_function, sanitize_identifier, to_pascal_case, ContractInfo,
    Expression, FunctionInfo, SolidityTestBuilder, SolidityTestContract, StateVariable, Statement,
    TestFunction, TestType,
};
use anyhow::Result;
use graph::cg::{CallGraph, EdgeType, NodeType, ParameterInfo};

pub(crate) fn generate_revert_tests_from_cfg(
    graph: &CallGraph,
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
) -> Result<Vec<TestFunction>> {
    eprintln!(
        "[REVERT DEBUG] Starting revert test generation for {}.{}",
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

        // Count require edges for this function
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

            let needs_prank = condition_node
                 .condition_expression 
                 .as_ref()
                 .map_or(false, |expr_text| {
                     let lower_expr = expr_text.to_lowercase();
                     // Heuristic: check for common access control patterns
                     (lower_expr.contains("msg.sender") || lower_expr.contains("caller"))
                         && (lower_expr.contains("owner")
                             || lower_expr.contains("admin")
                             || lower_expr.contains("role")
                         )
                 });
            eprintln!(
                "[REVERT DEBUG] Checking if prank is needed for condition expression '{}'",
                condition_node.condition_expression.as_deref().unwrap_or("unknown")
            );
            
            if needs_prank {
                body.push(Statement::FunctionCall {
                    target: Some("vm".to_string()),
                    function: "prank".to_string(),
                    args: vec![Expression::Literal("address(1)".to_string())], // Prank with a non-owner address
                });
            }

            body.push(Statement::Comment {
                text: format!("Test that {} reverts when: {}", function_name, descriptive_condition_text),
            });

            body.push(Statement::ExpectRevert {
                error_message: error_message.clone(),
            });

            // Generate function arguments
            eprintln!("[REVERT DEBUG] Generating function arguments...");

            // For revert tests, we typically want to use placeholder values that might trigger the revert
            // rather than actual call-site arguments (which would be successful calls)
            let function_args = generate_valid_args_for_function(function_params, None)?;
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
                    expected_error: error_message, // This remains the actual error string or empty for panic
                    condition: descriptive_condition_text, // Store the more descriptive condition text
                },
            };

            eprintln!("[REVERT DEBUG] Created test function: '{}'", test_name);
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
        "[REVERT DEBUG] Generated {} revert test functions",
        test_functions.len()
    );
    Ok(test_functions)
}

pub(crate) fn create_revert_test_contract(
    contract_info: &ContractInfo,
    function_info: &FunctionInfo,
    revert_tests: Vec<TestFunction>,
) -> SolidityTestContract {
    eprintln!(
        "[REVERT DEBUG] Creating revert test contract for {}.{}",
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
        "{}{}RevertTest",
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
        "[REVERT DEBUG] Built revert test contract with {} functions",
        contract.functions.len()
    );
    contract
}
