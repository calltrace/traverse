//! Test generation module using proper Solidity AST
//!
//! This module provides a comprehensive framework for generating Solidity test contracts

use tracing::debug;

use anyhow::{Context, Result};
use traverse_graph::cg::{CallGraph, CallGraphGeneratorContext, ParameterInfo};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::PathBuf};
use std::{fs, path::Path};

// Import proper Solidity AST and builders
use traverse_solidity::ast::*;
use traverse_solidity::builder::*;
use traverse_solidity::solidity_writer::write_source_unit;

use crate::deployer_stub;
use crate::revert_stub;
use crate::state_change_stub;
use crate::access_control_stub;
use crate::CodeGenError;

// Re-export for backward compatibility during migration
pub use traverse_solidity::ast::{Expression, Statement, TypeName, Visibility, StateMutability};
pub use traverse_solidity::builder::{SolidityBuilder, ContractBuilder, FunctionBuilder, BlockBuilder};

#[derive(Debug, serde::Serialize)]
pub struct ContractInfo {
    pub name: String,
    pub has_constructor: bool,
    pub constructor_params: Vec<ParameterInfo>,
    pub functions: Vec<FunctionInfo>,
}

#[derive(Debug, serde::Serialize, Clone)]
pub struct FunctionInfo {
    pub name: String,
    pub visibility: String,
    pub return_type: Option<String>,
    pub parameters: Vec<ParameterInfo>,
}

/// Enhanced test contract representation using proper Solidity AST
#[derive(Debug, Clone)]
pub struct SolidityTestContract {
    pub source_unit: SourceUnit,
    pub contract_name: String,
}

impl SolidityTestContract {
    pub fn new(contract_name: String, source_unit: SourceUnit) -> Self {
        Self {
            contract_name,
            source_unit,
        }
    }

    /// Generate Solidity source code from the AST
    pub fn to_solidity_code(&self) -> String {
        write_source_unit(&self.source_unit)
    }
}

/// Enhanced test type representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestType {
    RevertTest {
        expected_error: String,
        condition: String,
    },
    StateChangeTest {
        variable_name: String,
        expected_change: String,
    },
    AccessControlTest {
        role_required: String,
        unauthorized_caller: String,
    },
    DeployerTest {
        contract_name: String,
        constructor_args: Vec<String>,
    },
    FuzzTest {
        property: String,
        input_constraints: Vec<String>,
    },
}

/// Enhanced test builder using proper Solidity AST
pub struct SolidityTestContractBuilder {
    builder: SolidityBuilder,
    contract_name: String,
}

impl SolidityTestContractBuilder {
    pub fn new(contract_name: String) -> Self {
        let mut builder = SolidityBuilder::new();
        
        // Add standard pragma and imports for test contracts
        builder
            .pragma("solidity", "^0.8.0")
            .import("forge-std/Test.sol"); // Test.sol should correctly import and expose Vm

        Self {
            builder,
            contract_name,
        }
    }

    pub fn add_import(mut self, import_path: String) -> Self {
        self.builder.import(import_path);
        self
    }

    pub fn build_with_contract<F>(mut self, build_contract: F) -> SolidityTestContract
    where
        F: FnOnce(&mut ContractBuilder),
    {
        self.builder.contract(&self.contract_name, |contract| {
            // Add Test inheritance by default
            contract.inherits("Test");

            // The Vm instance (vm) is inherited from forge-std/Test.sol.
            // No need to declare it explicitly here.

            build_contract(contract);
        });

        let source_unit = self.builder.build();
        SolidityTestContract::new(self.contract_name.clone(), source_unit)
    }
}

pub struct FoundryIntegration {
    pub project_root: PathBuf,
}

impl FoundryIntegration {
    pub fn new_with_project_setup(project_root: PathBuf) -> Result<Self> {
        if !project_root.exists() {
            fs::create_dir_all(&project_root).context("Failed to create project root directory")?;
        }

        let foundry = Self { project_root };

        if !foundry.project_root.join("foundry.toml").exists() {
            FoundryIntegration::init_foundry_project(&foundry.project_root)?;
        }

        Ok(foundry)
    }

    fn init_foundry_project(project_root: &Path) -> Result<()> {
        use std::process::Command;

        debug!("🔧 Initializing Foundry project at: {}", project_root.display());

        let output = Command::new("forge")
            .arg("init")
            .arg("--force")
            .current_dir(project_root)
            .output()
            .context("Failed to execute 'forge init' command")?;

        if output.status.success() {
            debug!("✅ Foundry project initialized successfully");
            Ok(())
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            if stderr.contains("already exists") || stderr.contains("already initialized") {
                debug!("✅ Foundry project initialized (some files already existed)");
                Ok(())
            } else {
                Err(anyhow::anyhow!("Failed to initialize Foundry project: {}", stderr))
            }
        }
    }

    pub fn copy_contract_to_src(&self, contract_path: &Path, contract_name: &str) -> Result<()> {
        let src_dir = self.project_root.join("src");
        fs::create_dir_all(&src_dir).context("Failed to create src directory")?;

        let dest_path = src_dir.join(format!("{}.sol", contract_name));
        if contract_path.exists() {
            fs::copy(contract_path, &dest_path)
                .context("Failed to copy contract to src directory")?;
            debug!("📄 Copied {} to {}", contract_path.display(), dest_path.display());
        }

        Ok(())
    }

    pub fn write_test_contract(
        &self,
        contract: &SolidityTestContract,
        test_file_path: &Path,
    ) -> Result<()> {
        let source_code = contract.to_solidity_code();

        if let Some(parent_dir) = test_file_path.parent() {
            fs::create_dir_all(parent_dir).context("Failed to create test directory")?;
        }

        fs::write(test_file_path, &source_code).context(format!(
            "Failed to write test contract to {}",
            test_file_path.display()
        ))?;

        Ok(())
    }

    pub fn run_project_build(&self) -> Result<bool> {
        use std::process::Command;

        let output = Command::new("forge")
            .arg("build")
            .current_dir(&self.project_root)
            .output()
            .context("Failed to execute 'forge build' command")?;

        if output.status.success() {
            Ok(true)
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            debug!("Forge build failed: {}", stderr);
            Ok(false)
        }
    }

    pub fn run_tests(&self, test_pattern: Option<&str>) -> Result<bool> {
        use std::process::Command;

        let mut cmd = Command::new("forge");
        cmd.arg("test").current_dir(&self.project_root);

        if let Some(pattern) = test_pattern {
            cmd.arg("--match-test").arg(pattern);
        }

        let output = cmd.output().context("Failed to execute 'forge test' command")?;

        if output.status.success() {
            debug!("✅ All tests passed!");
            Ok(true)
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            debug!("Some tests failed: {}", stderr);
            Ok(false)
        }
    }
}

pub mod expression_helpers {
    use super::*;
    

    /// Create a require statement with condition and message
    pub fn require_statement(condition: Expression, message: &str) -> Statement {
        Statement::Expression(ExpressionStatement {
            expression: Expression::FunctionCall(FunctionCallExpression {
                function: Box::new(Expression::Identifier("require".to_string())),
                arguments: vec![
                    condition,
                    Expression::Literal(Literal::String(StringLiteral {
                        value: message.to_string(),
                    })),
                ],
            }),
        })
    }

    /// Create an expectRevert statement for Foundry tests
    pub fn expect_revert_statement(error_message: &str) -> Statement {
        Statement::Expression(ExpressionStatement {
            expression: Expression::FunctionCall(FunctionCallExpression {
                function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                    object: Box::new(Expression::Identifier("vm".to_string())),
                    member: "expectRevert".to_string(),
                })),
                arguments: vec![Expression::FunctionCall(FunctionCallExpression {
                    function: Box::new(Expression::Identifier("bytes".to_string())),
                    arguments: vec![Expression::Literal(Literal::String(StringLiteral {
                        value: error_message.to_string(),
                    }))],
                })],
            }),
        })
    }

    /// Create an assert statement
    pub fn assert_statement(condition: Expression) -> Statement {
        Statement::Expression(ExpressionStatement {
            expression: Expression::FunctionCall(FunctionCallExpression {
                function: Box::new(Expression::Identifier("assert".to_string())),
                arguments: vec![condition],
            }),
        })
    }

    /// Create a variable declaration with initialization
    pub fn declare_and_assign(type_name: TypeName, name: &str, value: Expression) -> Statement {
        Statement::Variable(VariableDeclarationStatement {
            declaration: VariableDeclaration {
                type_name,
                data_location: None,
                name: name.to_string(),
            },
            initial_value: Some(value),
        })
    }

    /// Create a function call expression
    pub fn function_call(target: Option<Expression>, function_name: &str, args: Vec<Expression>) -> Expression {
        if let Some(target_expr) = target {
            Expression::FunctionCall(FunctionCallExpression {
                function: Box::new(Expression::MemberAccess(MemberAccessExpression {
                    object: Box::new(target_expr),
                    member: function_name.to_string(),
                })),
                arguments: args,
            })
        } else {
            Expression::FunctionCall(FunctionCallExpression {
                function: Box::new(Expression::Identifier(function_name.to_string())),
                arguments: args,
            })
        }
    }
}

pub fn strings_to_expressions(arg_strings: &[String]) -> Vec<Expression> {
    arg_strings
        .iter()
        .map(|s| Expression::Literal(Literal::String(StringLiteral {
            value: s.clone(),
        })))
        .collect()
}

pub fn generate_valid_args_for_function(
    function_params: &[ParameterInfo],
    actual_args_opt: Option<&Vec<String>>,
) -> Result<Vec<Expression>> {
    if let Some(actual_args) = actual_args_opt {
        Ok(strings_to_expressions(actual_args))
    } else {
        let args = function_params
            .iter()
            .map(|param| match param.param_type.as_str() {
                "string" => string_literal("updated test value"),
                "address" => Expression::FunctionCall(FunctionCallExpression {
                    function: Box::new(Expression::Identifier("address".to_string())),
                    arguments: vec![number("1")],
                }),
                "bool" => boolean(true),
                t if t.starts_with("uint") => number("42"),
                t if t.starts_with("int") => number("42"),
                _ => number("1"),
            })
            .collect();

        Ok(args)
    }
}

pub fn sanitize_identifier(input: &str) -> String {
    input
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect::<String>()
        .trim_matches('_')
        .to_string()
}

pub fn capitalize_first_letter(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

pub fn to_pascal_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;

    for c in s.chars() {
        if c == '_' || c == '-' || c == ' ' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(c.to_uppercase().next().unwrap_or(c));
            capitalize_next = false;
        } else {
            result.push(c);
        }
    }
    result
}

pub(crate) fn extract_contracts_from_graph(
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext,
) -> Vec<ContractInfo> {
    // Keep existing implementation
    let mut contracts_map: HashMap<String, ContractInfo> = HashMap::new();

    for node in graph.nodes.iter() {
        if let Some(contract_name_str) = &node.contract_name {
            let is_interface_scope = ctx.all_interfaces.contains_key(contract_name_str);
            
            if node.node_type == traverse_graph::cg::NodeType::Interface && &node.name == contract_name_str {
                continue;
            }

            let contract_info = contracts_map
                .entry(contract_name_str.clone())
                .or_insert_with(|| ContractInfo {
                    name: contract_name_str.clone(),
                    has_constructor: false,
                    constructor_params: Vec::new(),
                    functions: Vec::new(),
                });

            match node.node_type {
                traverse_graph::cg::NodeType::Constructor => {
                    contract_info.has_constructor = true;
                    contract_info.constructor_params = graph.nodes[node.id].parameters.clone();
                }
                traverse_graph::cg::NodeType::Function => {
                    if !is_interface_scope {
                        let params = graph.nodes[node.id].parameters.clone();
                        contract_info.functions.push(FunctionInfo {
                            name: node.name.clone(),
                            visibility: "public".to_string(),
                            return_type: None,
                            parameters: params,
                        });
                    }
                }
                _ => {}
            }
        }
    }

    contracts_map.into_values().collect()
}

pub(crate) fn generate_and_write_test_file(
    foundry: &FoundryIntegration,
    contract: &SolidityTestContract,
    test_file_path: &Path,
    verbose: bool,
) -> Result<()> {
    foundry
        .write_test_contract(contract, test_file_path)
        .map_err(|e| {
            CodeGenError::FoundryError(format!(
                "Failed to write test file {}: {}",
                test_file_path.display(),
                e
            ))
        })?;

    if verbose {
        debug!("📝 Generated test file: {}", test_file_path.display());
    }

    Ok(())
}

pub fn generate_tests_with_foundry(
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext,
    verbose: bool,
    output_dir: &Path,
    foundry_root: Option<PathBuf>,
    deployer_only: bool,
    validate_compilation: bool,
    original_contract_paths: &HashMap<String, PathBuf>,
) -> Result<()> {
    if verbose {
        debug!("🚀 Starting sol2test with enhanced Foundry integration");
    }

    let foundry_root = foundry_root
        .clone()
        .unwrap_or_else(|| output_dir.parent().unwrap_or(Path::new(".")).to_path_buf());

    let foundry = FoundryIntegration::new_with_project_setup(foundry_root)
        .map_err(|e| CodeGenError::FoundryError(format!("Failed to initialize Foundry: {}", e)))?;

    let contracts = extract_contracts_from_graph(graph, ctx);

    for contract_info in &contracts {
        let contract_name = &contract_info.name;
        if let Some(original_path) = original_contract_paths.get(contract_name) {
            if verbose {
                debug!(
                    "📜 Copying original contract '{}' from {} to Foundry src...",
                    contract_name,
                    original_path.display()
                );
            }
            foundry
                .copy_contract_to_src(original_path, contract_name)
                .map_err(|e| {
                    CodeGenError::FoundryError(format!(
                        "Failed to copy contract {}: {}",
                        contract_name, e
                    ))
                })?;
        } else {
            debug!(
                "⚠️ Warning: Original source path for contract '{}' not found. Skipping copy.",
                contract_name
            );
        }
    }

    if verbose {
        debug!("🏗️  Found {} contracts in CFG:", contracts.len());
        for contract in &contracts {
            debug!(
                "  - {} (functions: {})",
                contract.name,
                contract.functions.len()
            );
        }
    }

    let mut generated_count = 0;
    let mut validated_count = 0;

    for contract_info in &contracts {
        if graph
            .nodes
            .iter()
            .any(|n| n.node_type == traverse_graph::cg::NodeType::Interface && n.name == contract_info.name)
        {
            if verbose {
                debug!("  ⏭️  Skipping interface: {}", contract_info.name);
            }
            continue;
        }

        let test_dir = foundry.project_root.join("test");
        fs::create_dir_all(&test_dir).context("Failed to create test directory")?;

        if !deployer_only {
            match deployer_stub::generate_foundry_deployer_test_contract(contract_info) {
                Ok(deployer_source_unit) => {
                    let deployer_test_contract = SolidityTestContract::new(
                        format!("{}DeployerTest", contract_info.name),
                        deployer_source_unit,
                    );
                    let deployer_test_filename = format!("{}.t.sol", deployer_test_contract.contract_name);
                    let deployer_test_path = test_dir.join(deployer_test_filename);

                    generate_and_write_test_file(
                        &foundry,
                        &deployer_test_contract,
                        &deployer_test_path,
                        verbose,
                    )?;
                    generated_count += 1;
                }
                Err(e) => {
                    debug!("Failed to generate deployer test for {}: {}", contract_info.name, e);
                }
            }
        }

        for function_info in &contract_info.functions {
            if verbose {
                debug!(
                    "Processing function: {}.{}",
                    contract_info.name, function_info.name
                );
            }

            // Generate revert tests using new AST approach
            match revert_stub::generate_revert_tests_from_cfg(
                graph,
                &contract_info.name,
                &function_info.name,
                &function_info.parameters,
            ) {
                Ok(revert_test_contracts) => {
                    for (i, test_contract) in revert_test_contracts.iter().enumerate() {
                        let test_filename = format!("{}RevertTest{}.t.sol", 
                            format!("{}{}", contract_info.name, function_info.name), i);
                        let test_path = test_dir.join(test_filename);

                        generate_and_write_test_file(&foundry, test_contract, &test_path, verbose)?;
                        generated_count += 1;
                    }
                }
                Err(e) => {
                    if verbose {
                        debug!(
                            "Error generating revert tests for {}.{}: {}",
                            contract_info.name, function_info.name, e
                        );
                    }
                }
            }

            match state_change_stub::generate_state_change_tests_from_cfg(
                graph,
                ctx,
                &contract_info.name,
                &function_info.name,
                &function_info.parameters,
            ) {
                Ok(state_test_contracts) => {
                    for (i, test_contract) in state_test_contracts.iter().enumerate() {
                        let test_filename = format!("{}StateTest{}.t.sol", 
                            format!("{}{}", contract_info.name, function_info.name), i);
                        let test_path = test_dir.join(test_filename);

                        generate_and_write_test_file(&foundry, test_contract, &test_path, verbose)?;
                        generated_count += 1;
                    }
                }
                Err(e) => {
                    if verbose {
                        debug!(
                            "Error generating state change tests for {}.{}: {}",
                            contract_info.name, function_info.name, e
                        );
                    }
                }
            }

            match access_control_stub::generate_access_control_tests_from_cfg(
                graph,
                &contract_info.name,
                &function_info.name,
                &function_info.parameters,
                &contract_info.constructor_params,
            ) {
                Ok(access_test_contracts) => {
                    for (i, test_contract) in access_test_contracts.iter().enumerate() {
                        let test_filename = format!("{}AccessTest{}.t.sol", 
                            format!("{}{}", contract_info.name, function_info.name), i);
                        let test_path = test_dir.join(test_filename);

                        generate_and_write_test_file(&foundry, test_contract, &test_path, verbose)?;
                        generated_count += 1;
                    }
                }
                Err(e) => {
                    if verbose {
                        debug!(
                            "Error generating access control tests for {}.{}: {}",
                            contract_info.name, function_info.name, e
                        );
                    }
                }
            }
        }
    }

    if validate_compilation && generated_count > 0 {
        if verbose {
            debug!("\n⚙️ Attempting to compile the entire project with 'forge build'...");
        }
        match foundry.run_project_build() {
            Ok(build_successful) => {
                if build_successful {
                    validated_count = generated_count;
                    if verbose {
                        debug!("✅ Project build successful. All {} generated test contracts are valid.", generated_count);
                    }
                    
                    // Run tests after successful build
                    if verbose {
                        debug!("\n🧪 Running generated tests with 'forge test'...");
                    }
                    match foundry.run_tests(None) {
                        Ok(tests_passed) => {
                            if tests_passed {
                                if verbose {
                                    debug!("✅ All tests passed successfully!");
                                }
                            } else if verbose {
                                debug!("❌ Some tests failed. Check 'forge test' output above for details.");
                            }
                        }
                        Err(e) => {
                            if verbose {
                                debug!("⚠️ Error running tests: {}", e);
                            }
                        }
                    }
                } else if verbose {
                    debug!("❌ Project build failed. Some of the {} generated test contracts may have errors. Check 'forge build' output above.", generated_count);
                }
            }
            Err(e) => {
                if verbose {
                    debug!(
                        "⚠️ Error during final project build: {}. Validation status uncertain.",
                        e
                    );
                }
            }
        }
    } else if generated_count == 0 && verbose && validate_compilation {
        debug!("\n🤷 No test contracts were generated, skipping compilation validation.");
    } else if !validate_compilation && verbose {
        debug!("\nℹ️ Compilation validation was skipped via configuration.");
    }

    if verbose {
        debug!("\n📊 Generation Summary:");
        debug!("  - Generated: {} test contracts", generated_count);
        if validate_compilation {
            debug!("  - Validated: {} test contracts", validated_count);
            debug!(
                "  - Validation rate: {:.1}%",
                if generated_count > 0 {
                    (validated_count as f64 / generated_count as f64) * 100.0
                } else {
                    0.0
                }
            );
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enhanced_test_contract_builder() {
        let contract = SolidityTestContractBuilder::new("TestContract".to_string())
            .add_import("../src/MyContract.sol".to_string())
            .build_with_contract(|contract| {
                contract
                    .state_variable(uint256(), "testVar", Some(Visibility::Private), Some(number("42")))
                    .function("testSetValue", |func| {
                        func.parameter(uint256(), "_value")
                            .visibility(Visibility::Public)
                            .body(|body| {
                                body.expression(Expression::Assignment(AssignmentExpression {
                                    left: Box::new(identifier("testVar")),
                                    operator: AssignmentOperator::Assign,
                                    right: Box::new(identifier("_value")),
                                }));
                            });
                    });
            });

        let solidity_code = contract.to_solidity_code();
        assert!(solidity_code.contains("pragma solidity ^0.8.0;"));
        assert!(solidity_code.contains("import \"forge-std/Test.sol\";"));
        assert!(solidity_code.contains("import \"../src/MyContract.sol\";"));
        assert!(solidity_code.contains("contract TestContract is Test"));
        assert!(solidity_code.contains("uint256 private testVar = 42;"));
        assert!(solidity_code.contains("function testSetValue(uint256 _value) public"));
    }

    #[test]
    fn test_expression_helpers() {
        use expression_helpers::*;

        let require_stmt = require_statement(
            binary(identifier("balance"), BinaryOperator::GreaterThanOrEqual, identifier("amount")),
            "Insufficient balance"
        );

        // Verify the statement structure
        if let Statement::Expression(ExpressionStatement { expression }) = require_stmt {
            if let Expression::FunctionCall(call) = expression {
                assert_eq!(call.arguments.len(), 2);
            } else {
                panic!("Expected function call expression");
            }
        } else {
            panic!("Expected expression statement");
        }
    }

    #[test]
    fn test_type_safety_improvements() {
        // Test that we can use proper type enums instead of strings
        let type_name = uint256();
        assert!(matches!(type_name, TypeName::Elementary(ElementaryTypeName::UnsignedInteger(Some(256)))));

        let visibility = Visibility::Public;
        assert_eq!(visibility.to_string(), "public");

        let operator = BinaryOperator::GreaterThanOrEqual;
        assert_eq!(operator.to_string(), ">=");
    }
}
