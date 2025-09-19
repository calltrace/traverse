use crate::teststubs::ContractInfo;
use anyhow::Result;
use traverse_solidity::ast::*;
use traverse_solidity::builder::*;

pub fn generate_foundry_deployer_test_contract(contract_info: &ContractInfo) -> Result<SourceUnit> {
    let mut builder = SolidityBuilder::new();
    let test_name = format!("{}DeployerTest", contract_info.name);
    let contract_import_path = format!("../src/{}.sol", contract_info.name);

    builder
        .pragma("solidity", "^0.8.0")
        .import("forge-std/Test.sol")
        .import(contract_import_path)
        .contract(test_name, |contract| {
            contract.inherits("Test").function("testDeploy", |func| {
                func.visibility(Visibility::Public).body(|body| {
                    // Generate deployment test logic
                    if contract_info.has_constructor {
                        // Add constructor args generation
                        for (i, param) in contract_info.constructor_params.iter().enumerate() {
                            let var_name = format!("arg{}", i);
                            let type_name = param_type_to_ast(&param.param_type);
                            let default_value = generate_default_value(&param.param_type);

                            // Use variable_declaration_with_location for types that require data location
                            if traverse_solidity::builder::requires_data_location(&type_name) {
                                let data_location =
                                    traverse_solidity::builder::get_default_data_location(&type_name);
                                body.variable_declaration_with_location(
                                    type_name,
                                    var_name,
                                    data_location,
                                    Some(default_value),
                                );
                            } else {
                                body.variable_declaration(type_name, var_name, Some(default_value));
                            }
                        }
                    }

                    // Add deployment statement
                    let deploy_expr = if contract_info.has_constructor {
                        let args: Vec<Expression> = (0..contract_info.constructor_params.len())
                            .map(|i| identifier(format!("arg{}", i)))
                            .collect();

                        Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(identifier(format!("new {}", contract_info.name))),
                            arguments: args,
                        })
                    } else {
                        Expression::FunctionCall(FunctionCallExpression {
                            function: Box::new(identifier(format!("new {}", contract_info.name))),
                            arguments: vec![],
                        })
                    };

                    body.variable_declaration(
                        user_type(&contract_info.name),
                        "instance",
                        Some(deploy_expr),
                    );

                    // Add assertion
                    body.expression(Expression::FunctionCall(FunctionCallExpression {
                        function: Box::new(identifier("assertTrue")),
                        arguments: vec![Expression::Binary(BinaryExpression {
                            left: Box::new(identifier("address(instance)")),
                            operator: BinaryOperator::NotEqual,
                            right: Box::new(identifier("address(0)")),
                        })],
                    }));
                });
            });
        });

    Ok(builder.build())
}

fn param_type_to_ast(param_type: &str) -> TypeName {
    match param_type {
        "address" => address(),
        "bool" => bool(),
        "string" => string(),
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
        t if t.starts_with("bytes") => {
            if t == "bytes" {
                bytes()
            } else if let Some(size_str) = t.strip_prefix("bytes") {
                if let Ok(size) = size_str.parse::<u8>() {
                    bytes_fixed(size)
                } else {
                    bytes()
                }
            } else {
                bytes()
            }
        }
        _ => user_type(param_type), // Custom type
    }
}

fn generate_default_value(param_type: &str) -> Expression {
    match param_type {
        "address" => identifier("address(0x1)"),
        "bool" => boolean(true),
        "string" => string_literal("test"),
        t if t.starts_with("uint") => number("1"),
        t if t.starts_with("int") => number("1"),
        t if t.starts_with("bytes") => string_literal("0x01"),
        _ => number("0"),
    }
}
