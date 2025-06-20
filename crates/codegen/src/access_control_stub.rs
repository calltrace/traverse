//! Access Control Revert Test Generation
//!
//! This module specializes in generating revert tests for access control patterns in Solidity contracts.
//! It focuses on conditions involving msg.sender, ownership, roles, and permissions, using vm.prank()
//! to simulate different caller addresses and trigger access control violations.

use crate::invariant_breaker::{break_invariant, InvariantBreakerValue};
use crate::teststubs::{
    generate_constructor_args, generate_valid_args_for_function, sanitize_identifier,
    to_pascal_case, ContractInfo, Expression, FunctionInfo, SolidityTestBuilder,
    SolidityTestContract, StateVariable, Statement, TestFunction, TestType,
};
use anyhow::Result;
use graph::cg::{CallGraph, EdgeType, NodeType, ParameterInfo};
use solidity::parse_expression;
use std::collections::HashMap;

pub fn generate_access_control_tests_from_cfg(
    graph: &CallGraph,
    contract_name: &str,
    function_name: &str,
    function_params: &[ParameterInfo],
) -> Result<Vec<TestFunction>> {
    eprintln!(
        "[ACCESS CONTROL DEBUG] Starting access control test generation for {}.{}",
        contract_name, function_name
    );

    let mut test_functions = Vec::new();

    let func_node = graph.nodes.iter().find(|n| {
        n.contract_name.as_deref() == Some(contract_name)
            && n.name == function_name
            && n.node_type == NodeType::Function
    });

    if let Some(func_node) = func_node {
        eprintln!(
            "[ACCESS CONTROL DEBUG] Found function node with ID: {}",
            func_node.id
        );

        // Find require edges for this function
        let require_edges: Vec<_> = graph
            .edges
            .iter()
            .filter(|edge| {
                edge.source_node_id == func_node.id && edge.edge_type == EdgeType::Require
            })
            .collect();

        eprintln!(
            "[ACCESS CONTROL DEBUG] Found {} require edges for function",
            require_edges.len()
        );

        for (edge_idx, edge) in require_edges.iter().enumerate() {
            let condition_node = &graph.nodes[edge.target_node_id];
            let condition_expression = condition_node
                .condition_expression
                .as_ref()
                .filter(|s| !s.is_empty())
                .cloned()
                .unwrap_or_else(|| condition_node.name.clone());

            let error_message = condition_node.revert_message.clone().unwrap_or_default();

            eprintln!(
                "[ACCESS CONTROL DEBUG] Processing condition: '{}'",
                condition_expression
            );

            let access_control_info = analyze_access_control_pattern(&condition_expression);

            if !access_control_info.is_access_control {
                eprintln!(
                    "[ACCESS CONTROL DEBUG] Condition '{}' is not access control related, skipping",
                    condition_expression
                );
                continue;
            }

            eprintln!(
                "[ACCESS CONTROL DEBUG] Detected access control pattern: {:?}",
                access_control_info
            );

            let test_function = generate_access_control_violation_test(
                function_name,
                &condition_expression,
                &error_message,
                function_params,
                &access_control_info,
                edge_idx,
            )?;

            test_functions.push(test_function);
        }
    } else {
        eprintln!(
            "[ACCESS CONTROL DEBUG] Function node not found in graph for {}.{}",
            contract_name, function_name
        );
    }

    eprintln!(
        "[ACCESS CONTROL DEBUG] Generated {} access control test functions",
        test_functions.len()
    );
    Ok(test_functions)
}

#[derive(Debug, Clone)]
struct AccessControlInfo {
    is_access_control: bool,
    pattern_type: AccessControlPattern,
    unauthorized_addresses: Vec<String>,
    authorized_address: Option<String>,
}

#[derive(Debug, Clone)]
enum AccessControlPattern {
    OwnerOnly,      // msg.sender == owner
    RoleBased,      // hasRole(ROLE, msg.sender)
    Whitelist,      // whitelist[msg.sender]
    NotBlacklisted, // !blacklist[msg.sender]
    Custom,         // Other patterns involving msg.sender
}

fn analyze_access_control_pattern(condition: &str) -> AccessControlInfo {
    let lower_condition = condition.to_lowercase();

    if !lower_condition.contains("msg.sender") && !lower_condition.contains("caller") {
        return AccessControlInfo {
            is_access_control: false,
            pattern_type: AccessControlPattern::Custom,
            unauthorized_addresses: vec![],
            authorized_address: None,
        };
    }

    let (pattern_type, unauthorized_addresses, authorized_address) =
        if lower_condition.contains("owner") {
            (
                AccessControlPattern::OwnerOnly,
                vec![
                    "address(0x1)".to_string(),
                    "address(0x2)".to_string(),
                    "address(0xDEADBEEF)".to_string(),
                ],
                Some("owner".to_string()),
            )
        } else if lower_condition.contains("role") || lower_condition.contains("hasrole") {
            (
                AccessControlPattern::RoleBased,
                vec![
                    "address(0x1)".to_string(),
                    "address(0x2)".to_string(),
                    "address(0xBADACCESS)".to_string(),
                ],
                None,
            )
        } else if lower_condition.contains("whitelist") {
            (
                AccessControlPattern::Whitelist,
                vec![
                    "address(0x1)".to_string(),
                    "address(0xNOTWHITELISTED)".to_string(),
                ],
                None,
            )
        } else if lower_condition.contains("blacklist") {
            (
                AccessControlPattern::NotBlacklisted,
                vec![
                    "address(0xBLACKLISTED)".to_string(),
                    "address(0xBADACTOR)".to_string(),
                ],
                None,
            )
        } else {
            (
                AccessControlPattern::Custom,
                vec!["address(0x1)".to_string(), "address(0x2)".to_string()],
                None,
            )
        };

    AccessControlInfo {
        is_access_control: true,
        pattern_type,
        unauthorized_addresses,
        authorized_address,
    }
}

fn generate_access_control_violation_test(
    function_name: &str,
    condition: &str,
    error_message: &str,
    function_params: &[ParameterInfo],
    access_control_info: &AccessControlInfo,
    edge_idx: usize,
) -> Result<TestFunction> {
    let pattern_name = match access_control_info.pattern_type {
        AccessControlPattern::OwnerOnly => "onlyOwner",
        AccessControlPattern::RoleBased => "roleRequired",
        AccessControlPattern::Whitelist => "whitelistOnly",
        AccessControlPattern::NotBlacklisted => "notBlacklisted",
        AccessControlPattern::Custom => "accessControl",
    };

    let test_name = format!(
        "test_{}_reverts_{}_{}",
        function_name, pattern_name, edge_idx
    );

    eprintln!(
        "[ACCESS CONTROL DEBUG] Generated test name: '{}'",
        test_name
    );

    let invariant_result = if is_simple_expression(condition) {
        eprintln!(
            "[ACCESS CONTROL DEBUG] Attempting to break invariant for: '{}'",
            condition
        );

        let rt = tokio::runtime::Runtime::new().unwrap();
        match rt.block_on(break_invariant(condition)) {
            Ok(result) => {
                eprintln!(
                    "[ACCESS CONTROL DEBUG] Invariant breaker result: success={}, entries={}",
                    result.success,
                    result.entries.len()
                );
                Some(result)
            }
            Err(e) => {
                eprintln!("[ACCESS CONTROL DEBUG] Failed to break invariant: {}", e);
                None
            }
        }
    } else {
        None
    };

    let prank_address = if let Some(ref result) = invariant_result {
        if result.success && !result.entries.is_empty() {
            extract_address_from_invariant_result(&result.entries[0].variables)
                .unwrap_or_else(|| access_control_info.unauthorized_addresses[0].clone())
        } else {
            access_control_info.unauthorized_addresses[0].clone()
        }
    } else {
        access_control_info.unauthorized_addresses[0].clone()
    };

    eprintln!(
        "[ACCESS CONTROL DEBUG] Using prank address: {}",
        prank_address
    );

    let mut body = Vec::new();

    body.push(Statement::Comment {
        text: format!(
            "Test that {} reverts when called by unauthorized address",
            function_name
        ),
    });

    body.push(Statement::Comment {
        text: format!("Access control condition: {}", condition),
    });

    if let Some(ref result) = invariant_result {
        if result.success && !result.entries.is_empty() {
            body.push(Statement::Comment {
                text: format!(
                    "Invariant breaker found {} counterexample(s)",
                    result.entries.len()
                ),
            });

            let first_entry = &result.entries[0];
            for (var_name, var_value) in &first_entry.variables {
                body.push(Statement::Comment {
                    text: format!(
                        "Counterexample: {} = {}",
                        var_name,
                        format_invariant_value(var_value)
                    ),
                });
            }
        }
    }

    body.push(Statement::FunctionCall {
        target: Some("vm".to_string()),
        function: "prank".to_string(),
        args: vec![Expression::Literal(prank_address)],
    });

    body.push(Statement::ExpectRevert {
        error_message: error_message.to_string(),
    });

    let function_args = if let Some(ref result) = invariant_result {
        if result.success && !result.entries.is_empty() {
            generate_args_from_invariant_result(function_params, &result.entries[0].variables)?
        } else {
            generate_valid_args_for_function(function_params, None)?
        }
    } else {
        generate_access_control_friendly_args(function_params)?
    };

    body.push(Statement::FunctionCall {
        target: Some("contractInstance".to_string()),
        function: function_name.to_string(),
        args: function_args,
    });

    // Stop prank
    body.push(Statement::FunctionCall {
        target: Some("vm".to_string()),
        function: "stopPrank".to_string(),
        args: vec![],
    });

    let test_function = TestFunction {
        name: test_name.clone(),
        visibility: "public".to_string(),
        body,
        test_type: TestType::RevertTest {
            expected_error: error_message.to_string(),
            condition: condition.to_string(),
        },
    };

    eprintln!(
        "[ACCESS CONTROL DEBUG] Created access control test function: '{}'",
        test_name
    );
    Ok(test_function)
}

fn is_simple_expression(expression: &str) -> bool {
    match parse_expression(expression) {
        Ok(_) => {
            let complexity_score = calculate_expression_complexity(expression);
            eprintln!(
                "[ACCESS CONTROL DEBUG] Expression '{}' complexity score: {}",
                expression, complexity_score
            );
            complexity_score <= 10
        }
        Err(e) => {
            eprintln!(
                "[ACCESS CONTROL DEBUG] Expression '{}' failed to parse: {}",
                expression, e
            );
            false
        }
    }
}

fn calculate_expression_complexity(expression: &str) -> u32 {
    let mut score = 0;

    // Count operators
    for op in &[
        "&&", "||", "==", "!=", ">=", "<=", ">", "<", "+", "-", "*", "/", "%",
    ] {
        score += expression.matches(op).count() as u32;
    }

    // Count function calls
    score += expression.matches('(').count() as u32;

    // Count member accesses
    score += expression.matches('.').count() as u32;

    // Count array accesses
    score += expression.matches('[').count() as u32;

    score
}

fn extract_address_from_invariant_result(
    variables: &HashMap<String, InvariantBreakerValue>,
) -> Option<String> {
    for (var_name, var_value) in variables {
        if let InvariantBreakerValue::Address(addr) = var_value {
            eprintln!(
                "[ACCESS CONTROL DEBUG] Found address variable '{}' = '{}' from invariant",
                var_name, addr
            );
            return Some(addr.clone());
        }
    }
    None
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

    for param in function_params {
        let arg = if let Some(var_value) = variables.get(&param.name) {
            eprintln!(
                "[ACCESS CONTROL DEBUG] Using invariant value for param '{}': {:?}",
                param.name, var_value
            );
            invariant_value_to_expression(var_value)
        } else {
            eprintln!(
                "[ACCESS CONTROL DEBUG] No invariant value found for param '{}', using default",
                param.name
            );
            generate_access_control_friendly_arg(&param.param_type)
        };

        args.push(arg);
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

fn generate_access_control_friendly_args(
    function_params: &[ParameterInfo],
) -> Result<Vec<Expression>> {
    let args = function_params
        .iter()
        .map(|param| generate_access_control_friendly_arg(&param.param_type))
        .collect();
    Ok(args)
}

fn generate_access_control_friendly_arg(param_type: &str) -> Expression {
    match param_type {
        "string" => Expression::Literal("\"test\"".to_string()),
        "address" => Expression::Literal("address(0x123)".to_string()), // Different from prank address
        "bool" => Expression::Literal("true".to_string()),
        t if t.starts_with("uint") => Expression::Literal("100".to_string()), // Non-zero value
        t if t.starts_with("int") => Expression::Literal("100".to_string()),
        _ => Expression::Literal("1".to_string()),
    }
}

pub fn create_access_control_test_contract(
    contract_info: &ContractInfo,
    function_info: &FunctionInfo,
    access_control_tests: Vec<TestFunction>,
) -> SolidityTestContract {
    eprintln!(
        "[ACCESS CONTROL DEBUG] Creating access control test contract for {}.{}",
        contract_info.name, function_info.name
    );

    let contract_name = format!(
        "{}{}AccessControlTest",
        contract_info.name,
        to_pascal_case(&function_info.name)
    );

    eprintln!(
        "[ACCESS CONTROL DEBUG] Generated contract name: '{}'",
        contract_name
    );

    let constructor_args = generate_constructor_args(&contract_info.constructor_params, None);

    let mut builder = SolidityTestBuilder::new(contract_name)
        .add_import(format!("../src/{}.sol", contract_info.name))
        .add_state_variable(StateVariable {
            name: "contractInstance".to_string(),
            var_type: contract_info.name.clone(),
            visibility: "private".to_string(),
            initial_value: None,
        });

    builder = builder.add_state_variable(StateVariable {
        name: "owner".to_string(),
        var_type: "address".to_string(),
        visibility: "private".to_string(),
        initial_value: Some("address(this)".to_string()),
    });

    let mut setup_statements = vec![
        Statement::Comment {
            text: format!("Deploy {} for access control testing", contract_info.name),
        },
        Statement::Assignment {
            target: "contractInstance".to_string(),
            value: Expression::FunctionCall {
                target: None,
                function: format!("new {}", contract_info.name),
                args: constructor_args,
            },
        },
    ];

    if contract_info
        .constructor_params
        .iter()
        .any(|p| p.name == "initialOwner")
    {
        setup_statements.push(Statement::Comment {
            text: "Owner is set during contract deployment".to_string(),
        });
    }

    builder = builder.set_setup_function(crate::teststubs::SetupFunction {
        body: setup_statements,
    });

    for (i, test_func) in access_control_tests.iter().enumerate() {
        eprintln!(
            "[ACCESS CONTROL DEBUG] Adding test function {}: '{}'",
            i + 1,
            test_func.name
        );
        builder = builder.add_test_function(test_func.clone());
    }

    let contract = builder.build();
    eprintln!(
        "[ACCESS CONTROL DEBUG] Built access control test contract with {} functions",
        contract.functions.len()
    );
    contract
}
