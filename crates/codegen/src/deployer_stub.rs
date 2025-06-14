use crate::teststubs::{
    generate_constructor_args, ContractInfo, Expression, SolidityTestBuilder, SolidityTestContract, StateVariable, Statement, TestFunction, TestType
};
use anyhow::Result;

pub fn generate_foundry_deployer_test_contract(
    contract_info: &ContractInfo,
) -> Result<SolidityTestContract> {
    let deployer_contract_name = format!("Deployer_{}", contract_info.name);
    let deployer_contract = SolidityTestBuilder::new(deployer_contract_name)
        .add_import(format!("../src/{}.sol", contract_info.name)) 
        .add_state_variable(StateVariable {
            name: "deployedContract".to_string(),
            var_type: contract_info.name.clone(),
            visibility: "public".to_string(),
            initial_value: None,
        })
        .add_test_function(TestFunction {
            name: format!("test_Deploy_{}", contract_info.name),
            visibility: "public".to_string(), 
            body: vec![
                Statement::Comment {
                    text: format!("Deploy {} with constructor parameters", contract_info.name),
                },
                Statement::Assignment {
                    target: "deployedContract".to_string(),
                    value: Expression::FunctionCall {
                        target: None,
                        function: format!("new {}", contract_info.name),
                        args: generate_constructor_args(&contract_info.constructor_params, None),
                    },
                },
                Statement::Assert { 
                    condition: Expression::BinaryOp {
                        left: Box::new(Expression::FunctionCall {
                            target: None,
                            function: "address".to_string(),
                            args: vec![Expression::Identifier("deployedContract".to_string())],
                        }),
                        operator: "!=".to_string(),
                        right: Box::new(Expression::Literal("address(0)".to_string())),
                    }
                }
            ],
            test_type: TestType::StateChangeTest {
                variable_name: "deployedContract".to_string(),
                expected_change: "should be a valid address (non-zero)".to_string(),
            },
        })
        .build();

    Ok(deployer_contract)
}

