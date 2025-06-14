// crates/codegen/src/teststubs.rs
//
// This module is responsible for generating Solidity test stubs, primarily for Foundry projects.
// It defines data structures representing Solidity test contracts, functions, statements,
// and expressions, allowing for programmatic construction of test files.
//
// Key functionalities include:
// - Defining structures for test contracts (`SolidityTestContract`), functions (`TestFunction`),
//   state variables (`StateVariable`), and various statement/expression types.
// - A `SolidityTestBuilder` for fluently constructing `SolidityTestContract` instances.
// - `FoundryIntegration` for interacting with a Foundry project, including:
//    - Setting up a test project structure (foundry.toml, src, test directories).
//    - Creating default `BaseTest.sol` and sample contracts.
//    - Compiling generated test contracts using `solc`.
//    - Generating Solidity code from `SolidityTestContract` structures.
//    - Validating generated code by attempting compilation.
//    - Running tests using `forge test`.
// - Functions to generate specific types of tests (revert tests, state change tests)
//   based on a `CallGraph` and contract/function information.
// - Helper functions for string manipulation (e.g., `to_pascal_case`, `sanitize_identifier`).
//
// The main entry point for test generation using this module is typically
// `generate_tests_with_foundry`, which orchestrates the process of extracting
// contract information from a call graph, building test contracts, and interacting
// with Foundry for code generation and validation.

use anyhow::{Context, Result};
use graph::cg::{
    CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, EdgeType, NodeType,
    ParameterInfo, Visibility,
};
use semver::Version;
use serde::{Deserialize, Serialize};
use std::str::FromStr;
use std::{collections::HashMap, path::PathBuf};
use std::{fs, path::Path};

use crate::deployer_stub; 
use crate::revert_stub;
use crate::state_change_stub; 
use crate::CodeGenError;

#[derive(Debug, serde::Serialize)]
pub struct ContractInfo {
    pub name: String,                           
    pub has_constructor: bool,                  
    pub constructor_params: Vec<ParameterInfo>,
    pub functions: Vec<FunctionInfo>,           
}

#[derive(Debug, serde::Serialize, Clone)]
pub struct FunctionInfo {
    // Made public
    pub name: String,
    pub visibility: String,
    pub return_type: Option<String>,
    pub parameters: Vec<ParameterInfo>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SolidityTestContract {
    pub name: String,
    pub imports: Vec<String>,
    pub inheritance: Vec<String>,
    pub state_variables: Vec<StateVariable>,
    pub functions: Vec<TestFunction>,
    pub setup_function: Option<SetupFunction>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateVariable {
    pub name: String,
    pub var_type: String,
    pub visibility: String,
    pub initial_value: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestFunction {
    pub name: String,
    pub visibility: String,
    pub body: Vec<Statement>,
    pub test_type: TestType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetupFunction {
    pub body: Vec<Statement>,
}

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
    EventEmissionTest {
        event_name: String,
        expected_args: Vec<String>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    VariableDeclaration {
        var_type: String,
        name: String,
        value: Option<Expression>,
    },
    Assignment {
        target: String,
        value: Expression,
    },
    FunctionCall {
        target: Option<String>,
        function: String,
        args: Vec<Expression>,
    },
    ExpectRevert {
        error_message: String,
    },
    ExpectEmit {
        event_signature: String,
    },
    Assert {
        condition: Expression,
    },
    Comment {
        text: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    Literal(String),
    Identifier(String),
    FunctionCall {
        target: Option<String>,
        function: String,
        args: Vec<Expression>,
    },
    BinaryOp {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>,
    },
    UnaryOp {
        operator: String,
        operand: Box<Expression>,
    },
}

pub struct FoundryIntegration {
    project_root: PathBuf,
}

impl FoundryIntegration {
    pub fn new(project_root: PathBuf) -> Result<Self> {
        Ok(Self { project_root })
    }

    pub fn new_with_project_setup(project_root: PathBuf) -> Result<Self> {
        // Create project directory if it doesn't exist
        if !project_root.exists() {
            fs::create_dir_all(&project_root).context("Failed to create project directory")?;
        }

        // Check if this is already a Foundry project
        let foundry_toml_path = project_root.join("foundry.toml");
        if !foundry_toml_path.exists() {
            // Initialize Foundry project
            Self::init_foundry_project(&project_root)?;
        }

        Self::new(project_root)
    }

    fn init_foundry_project(project_root: &Path) -> Result<()> {
        use std::process::Command;

        println!(
            "🔧 Initializing Foundry project at: {}",
            project_root.display()
        );

        let output = Command::new("forge")
            .arg("init")
            .arg("--force") 
            .current_dir(project_root)
            .output()
            .context("Failed to execute 'forge init' command")?;

        if output.status.success() {
            println!("✅ Successfully initialized Foundry project");
            Ok(())
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);

            if stderr.contains("already exists") || stdout.contains("Initialized") {
                println!("✅ Foundry project initialized (some files already existed)");
                Ok(())
            } else {
                Err(anyhow::anyhow!(
                    "Failed to initialize Foundry project: {}",
                    stderr
                ))
            }
        }
    }

    pub fn copy_contract_to_src(&self, contract_path: &Path, contract_name: &str) -> Result<()> {
        let src_dir = self.project_root.join("src");
        let dest_path = src_dir.join(format!("{}.sol", contract_name));

        if contract_path.exists() {
            fs::copy(contract_path, &dest_path)
                .context("Failed to copy contract to src directory")?;
            println!(
                "📄 Copied {} to {}",
                contract_path.display(),
                dest_path.display()
            );
        }

        Ok(())
    }

    fn create_base_test_contract(&self, path: &Path) -> Result<()> {
        let base_test_content = r#"// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "forge-std/Test.sol";

/**
 * @title BaseTest
 * @dev Base test contract that provides common testing utilities
 */
abstract contract BaseTest is Test {
    
    function setUp() public virtual {
        // Base setup logic
    }
    
    function isContract(address addr) internal view returns (bool) {
        uint256 size;
        assembly {
            size := extcodesize(addr)
        }
        return size > 0;
    }
    
    function getCurrentTimestamp() internal view returns (uint256) {
        return block.timestamp;
    }
    
    function advanceTime(uint256 timeToAdvance) internal {
        vm.warp(block.timestamp + timeToAdvance);
    }
    
    function advanceBlocks(uint256 blocksToAdvance) internal {
        vm.roll(block.number + blocksToAdvance);
    }
}
"#;
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, base_test_content).context("Failed to create BaseTest.sol")?;
        Ok(())
    }

    pub fn write_solidity_test_file(
        &self,
        contract: &SolidityTestContract,
        test_file_path: &Path,
    ) -> Result<()> {
        let source_code = self.generate_solidity_code(contract)?;

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
            eprintln!(
                "Forge project build failed:\nProject Root: {}\nStdout:\n{}\nStderr:\n{}",
                self.project_root.display(),
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
            Ok(false)
        }
    }

    pub fn generate_solidity_code(&self, contract: &SolidityTestContract) -> Result<String> {
        let mut code = String::new();

        code.push_str("pragma solidity ^0.8.0;\n\n");

        for import in &contract.imports {
            code.push_str(&format!("import \"{}\";\n", import));
        }
        code.push('\n');

        code.push_str(&format!("contract {}", contract.name));
        if !contract.inheritance.is_empty() {
            code.push_str(" is ");
            code.push_str(&contract.inheritance.join(", "));
        }
        code.push_str(" {\n");

        for var in &contract.state_variables {
            code.push_str(&format!(
                "    {} {} {}",
                var.var_type, var.visibility, var.name
            ));
            if let Some(value) = &var.initial_value {
                code.push_str(&format!(" = {}", value));
            }
            code.push_str(";\n");
        }
        if !contract.state_variables.is_empty() {
            code.push('\n');
        }

        if let Some(setup) = &contract.setup_function {
            code.push_str("    function setUp() public {\n");
            for statement in &setup.body {
                code.push_str(&format!(
                    "        {}\n",
                    self.generate_statement(statement)?
                ));
            }
            code.push_str("    }\n\n");
        }

        for function in &contract.functions {
            code.push_str(&format!("    function {}() public {{\n", function.name));
            for statement in &function.body {
                code.push_str(&format!(
                    "        {}\n",
                    self.generate_statement(statement)?
                ));
            }
            code.push_str("    }\n\n");
        }

        code.push_str("}\n");

        Ok(code)
    }

    fn generate_statement(&self, statement: &Statement) -> Result<String> {
        match statement {
            Statement::VariableDeclaration {
                var_type,
                name,
                value,
            } => {
                let mut stmt = format!("{} {}", var_type, name);
                if let Some(val) = value {
                    stmt.push_str(&format!(" = {}", self.generate_expression(val)?));
                }
                stmt.push(';');
                Ok(stmt)
            }
            Statement::Assignment { target, value } => Ok(format!(
                "{} = {};",
                target,
                self.generate_expression(value)?
            )),
            Statement::FunctionCall {
                target,
                function,
                args,
            } => {
                let mut call = String::new();
                if let Some(t) = target {
                    call.push_str(&format!("{}.", t));
                }
                call.push_str(function);
                call.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        call.push_str(", ");
                    }
                    call.push_str(&self.generate_expression(arg)?);
                }
                call.push_str(");");
                Ok(call)
            }
            Statement::ExpectRevert { error_message } => {
                Ok(format!("vm.expectRevert(bytes(\"{}\"));", error_message))
            }
            Statement::ExpectEmit { event_signature } => Ok(format!(
                "vm.expectEmit(true, true, true, true);\n        emit {};",
                event_signature
            )),
            Statement::Assert { condition } => {
                Ok(format!("assert({});", self.generate_expression(condition)?))
            }
            Statement::Comment { text } => Ok(format!("// {}", text)),
        }
    }

    fn generate_expression(&self, expression: &Expression) -> Result<String> {
        match expression {
            Expression::Literal(value) => Ok(value.clone()),
            Expression::Identifier(name) => Ok(name.clone()),
            Expression::FunctionCall {
                target,
                function,
                args,
            } => {
                let mut call = String::new();
                if let Some(t) = target {
                    call.push_str(&format!("{}.", t));
                }
                call.push_str(function);
                call.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        call.push_str(", ");
                    }
                    call.push_str(&self.generate_expression(arg)?);
                }
                call.push(')');
                Ok(call)
            }
            Expression::BinaryOp {
                left,
                operator,
                right,
            } => Ok(format!(
                "{} {} {}",
                self.generate_expression(left)?,
                operator,
                self.generate_expression(right)?
            )),
            Expression::UnaryOp { operator, operand } => Ok(format!(
                "{}{}",
                operator,
                self.generate_expression(operand)?
            )),
        }
    }

    pub fn run_tests(&self, test_pattern: Option<&str>) -> Result<bool> {
        use std::process::Command;

        let mut cmd = Command::new("forge");
        cmd.arg("test").current_dir(&self.project_root);

        if let Some(pattern) = test_pattern {
            cmd.arg("--match-test").arg(pattern);
        }

        cmd.arg("--verbose");

        let output = cmd
            .output()
            .context("Failed to execute forge test command")?;

        let success = output.status.success();

        if !success {
            eprintln!("Forge test output:");
            eprintln!("{}", String::from_utf8_lossy(&output.stdout));
            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        }

        Ok(success)
    }
}

pub struct SolidityTestBuilder {
    contract: SolidityTestContract,
}

impl SolidityTestBuilder {
    pub fn new(name: String) -> Self {
        Self {
            contract: SolidityTestContract {
                name,
                imports: vec!["forge-std/Test.sol".to_string()],
                inheritance: vec!["Test".to_string()],
                state_variables: Vec::new(),
                functions: Vec::new(),
                setup_function: None,
            },
        }
    }

    pub fn add_import(mut self, import: String) -> Self {
        self.contract.imports.push(import);
        self
    }

    pub fn add_inheritance(mut self, base: String) -> Self {
        self.contract.inheritance.push(base);
        self
    }

    pub fn add_state_variable(mut self, var: StateVariable) -> Self {
        self.contract.state_variables.push(var);
        self
    }

    pub fn add_test_function(mut self, function: TestFunction) -> Self {
        self.contract.functions.push(function);
        self
    }

    pub fn set_setup_function(mut self, setup: SetupFunction) -> Self {
        self.contract.setup_function = Some(setup);
        self
    }

    pub fn build(self) -> SolidityTestContract {
        self.contract
    }
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
        println!("🔧 Initializing Foundry integration...");
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
                println!(
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
            eprintln!(
                "⚠️ Warning: Original source path for contract '{}' not found. Skipping copy.",
                contract_name
            );
        }
    }

    if verbose {
        println!("🏗️  Found {} contracts in CFG:", contracts.len());
        for contract in &contracts {
            println!(
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
            .any(|n| n.node_type == graph::cg::NodeType::Interface && n.name == contract_info.name)
        {
            if verbose {
                println!("  ⏭️  Skipping interface: {}", contract_info.name);
            }
            continue;
        }

        let test_dir = foundry.project_root.join("test");
        fs::create_dir_all(&test_dir).context("Failed to create test directory")?;

        if !deployer_only {
            let deployer_test_contract =
                deployer_stub::generate_foundry_deployer_test_contract(contract_info)?;
            let deployer_test_filename = format!("{}.t.sol", deployer_test_contract.name);
            let deployer_test_path = test_dir.join(deployer_test_filename);

            generate_and_write_test_file(
                &foundry,
                &deployer_test_contract,
                &deployer_test_path,
                verbose,
            )?;
            generated_count += 1;
        }

        for function_info in &contract_info.functions {
            eprintln!(
                "[MAIN DEBUG] Processing function: {}.{}",
                contract_info.name, function_info.name
            );
            eprintln!(
                "[MAIN DEBUG] Function visibility: {}",
                function_info.visibility
            );
            eprintln!(
                "[MAIN DEBUG] Function has {} parameters",
                function_info.parameters.len()
            );
            for (i, param) in function_info.parameters.iter().enumerate() {
                eprintln!(
                    "[MAIN DEBUG] Function param {}: {} {}",
                    i, param.param_type, param.name
                );
            }

            eprintln!(
                "[MAIN DEBUG] Attempting to generate revert tests for {}.{}",
                contract_info.name, function_info.name
            );
            match revert_stub::generate_revert_tests_from_cfg(
                graph,
                &contract_info.name,
                &function_info.name,
                &function_info.parameters,
            ) {
                Ok(revert_tests) => {
                    eprintln!(
                        "[MAIN DEBUG] Successfully generated {} revert tests",
                        revert_tests.len()
                    );
                    if !revert_tests.is_empty() {
                        eprintln!("[MAIN DEBUG] Creating revert test contract...");
                        let test_contract = revert_stub::create_revert_test_contract(
                            contract_info,
                            function_info,
                            revert_tests,
                        );
                        let test_filename = format!("{}.t.sol", test_contract.name);
                        let test_path = test_dir.join(test_filename);

                        eprintln!(
                            "[MAIN DEBUG] Writing revert test contract to: {}",
                            test_path.display()
                        );

                        generate_and_write_test_file(
                            &foundry,
                            &test_contract,
                            &test_path,
                            verbose,
                        )?;
                        generated_count += 1;
                        eprintln!("[MAIN DEBUG] Successfully wrote revert test contract");
                    } else {
                        eprintln!(
                            "[MAIN DEBUG] No revert tests generated for {}.{}",
                            contract_info.name, function_info.name
                        );
                    }
                }
                Err(e) => {
                    eprintln!(
                        "[MAIN DEBUG] Error generating revert tests for {}.{}: {}",
                        contract_info.name, function_info.name, e
                    );
                }
            }

            eprintln!(
                "[MAIN DEBUG] Attempting to generate state change tests for {}.{}",
                contract_info.name, function_info.name
            );
            match state_change_stub::generate_state_change_tests_from_cfg(
                graph,
                ctx, 
                &contract_info.name,
                &function_info.name,
                &function_info.parameters,
            ) {
                Ok(state_tests) => {
                    eprintln!(
                        "[MAIN DEBUG] Successfully generated {} state change tests",
                        state_tests.len()
                    );
                    if !state_tests.is_empty() {
                        let test_contract = state_change_stub::create_state_test_contract(
                            contract_info,
                            function_info,
                            state_tests,
                        );
                        let test_filename = format!("{}.t.sol", test_contract.name);
                        let test_path = test_dir.join(test_filename);

                        eprintln!(
                            "[MAIN DEBUG] Writing revert test contract to: {}",
                            test_path.display()
                        );

                        generate_and_write_test_file(
                            &foundry,
                            &test_contract,
                            &test_path,
                            verbose,
                        )?;
                        generated_count += 1;
                        eprintln!("[MAIN DEBUG] Successfully wrote revert test contract");
                    } else {
                        eprintln!(
                            "[MAIN DEBUG] No revert tests generated for {}.{}",
                            contract_info.name, function_info.name
                        );
                    }
                }
                Err(e) => {
                    eprintln!(
                        "[MAIN DEBUG] Error generating revert tests for {}.{}: {}",
                        contract_info.name, function_info.name, e
                    );
                }
            }
        }
    }

    if validate_compilation && generated_count > 0 {
        if verbose {
            println!("\n⚙️ Attempting to compile the entire project with 'forge build'...");
        }
        match foundry.run_project_build() {
            Ok(build_successful) => {
                if build_successful {
                    validated_count = generated_count;
                    if verbose {
                        println!("✅ Project build successful. All {} generated test contracts are valid.", generated_count);
                    }
                } else {
                    if verbose {
                        eprintln!("❌ Project build failed. Some of the {} generated test contracts may have errors. Check 'forge build' output above.", generated_count);
                    }
                }
            }
            Err(e) => {
                if verbose {
                    eprintln!(
                        "⚠️ Error during final project build: {}. Validation status uncertain.",
                        e
                    );
                }
            }
        }
    } else if generated_count == 0 && verbose && validate_compilation {
        println!("\n🤷 No test contracts were generated, skipping compilation validation.");
    } else if !validate_compilation && verbose {
        println!("\nℹ️ Compilation validation was skipped via configuration.");
    }

    if verbose {
        println!("\n📊 Generation Summary:");
        println!("  - Generated: {} test contracts", generated_count);
        if validate_compilation {
            println!("  - Validated: {} test contracts", validated_count);
            println!(
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

fn extract_contracts_from_graph(
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext,
) -> Vec<ContractInfo> {
    eprintln!("[EXTRACT DEBUG] Starting contract extraction from graph");
    eprintln!("[EXTRACT DEBUG] Graph has {} nodes", graph.nodes.len());
    eprintln!("[EXTRACT DEBUG] Graph has {} edges", graph.edges.len());

    let mut contracts_map: HashMap<String, ContractInfo> = HashMap::new();

    for (node_idx, node) in graph.nodes.iter().enumerate() {
        eprintln!(
            "[EXTRACT DEBUG] Node {}: name='{}', type={:?}, contract={:?}",
            node_idx, node.name, node.node_type, node.contract_name
        );
        if let Some(contract_name_str) = &node.contract_name {
            let is_interface_scope = ctx.all_interfaces.contains_key(contract_name_str);
            eprintln!(
                "[EXTRACT DEBUG] Processing node in contract '{}', is_interface_scope={}",
                contract_name_str, is_interface_scope
            );
            if node.node_type == graph::cg::NodeType::Interface && &node.name == contract_name_str {
                eprintln!(
                    "[EXTRACT DEBUG] Skipping interface definition node: {}",
                    contract_name_str
                );
                continue;
            }

            let contract_info = contracts_map
                .entry(contract_name_str.clone())
                .or_insert_with(|| {
                    eprintln!(
                        "[EXTRACT DEBUG] Creating new ContractInfo for '{}'",
                        contract_name_str
                    );
                    ContractInfo {
                        name: contract_name_str.clone(),
                        has_constructor: false,
                        constructor_params: Vec::new(),
                        functions: Vec::new(),
                    }
                });

            match node.node_type {
                graph::cg::NodeType::Constructor => {
                    eprintln!(
                        "[EXTRACT DEBUG] Found constructor for '{}'",
                        contract_name_str
                    );
                    contract_info.has_constructor = true;
                    if let Some((_, node_info_for_ctor, _)) = ctx
                        .definition_nodes_info
                        .iter()
                        .find(|(id, _, _)| *id == node.id)
                    {
                        contract_info.constructor_params = graph.nodes[node.id].parameters.clone();
                    }
                }
                graph::cg::NodeType::Function => {
                    if !is_interface_scope {
                        eprintln!(
                            "[EXTRACT DEBUG] Found function '{}' in contract '{}'",
                            node.name, contract_name_str
                        );
                        let params = graph.nodes[node.id].parameters.clone();
                        contract_info.functions.push(FunctionInfo {
                            name: node.name.clone(),
                            visibility: format!("{:?}", node.visibility).to_lowercase(),
                            return_type: node.declared_return_type.clone(),
                            parameters: params,
                        });
                    }
                }
                _ => {}
            }
        }
    }

    let result: Vec<ContractInfo> = contracts_map.into_values().collect();
    eprintln!("[EXTRACT DEBUG] Extracted {} contracts", result.len());
    for (i, contract) in result.iter().enumerate() {
        eprintln!(
            "[EXTRACT DEBUG] Contract {}: name='{}', has_constructor={}, functions={}",
            i,
            contract.name,
            contract.has_constructor,
            contract.functions.len()
        );
    }

    result
}

fn generate_and_write_test_file(
    foundry: &FoundryIntegration,
    contract: &SolidityTestContract,
    test_file_path: &Path,
    verbose: bool,
) -> Result<()> {
    foundry
        .write_solidity_test_file(contract, test_file_path)
        .map_err(|e| {
            CodeGenError::FoundryError(format!(
                "Failed to write test file {}: {}",
                test_file_path.display(),
                e
            ))
        })?;

    if verbose {
        println!("  📄 Generated: {}", test_file_path.display());
    }

    Ok(())
}


pub(crate) fn generate_constructor_args(
    params: &[ParameterInfo],
    actual_args_opt: Option<&Vec<String>>,
) -> Vec<Expression> {
    eprintln!(
        "[DEBUG] generate_constructor_args called with {} params",
        params.len()
    );
    for (i, param) in params.iter().enumerate() {
        eprintln!("[DEBUG] Param {}: {} {}", i, param.param_type, param.name);
    }
    eprintln!("[DEBUG] Actual args provided: {:?}", actual_args_opt);

    let result = if let Some(actual_args) = actual_args_opt {
        eprintln!("[DEBUG] Using actual args: {:?}", actual_args);
        strings_to_expressions(actual_args)
    } else {
        eprintln!("[DEBUG] Using placeholder args based on parameter types");
        params
            .iter()
            .map(|param| {
                match param.param_type.as_str() {
                    "string" => Expression::Literal("\"test\"".to_string()),
                    "address" => Expression::Literal("address(0x1)".to_string()),
                    "bool" => Expression::Literal("true".to_string()),
                    t if t.starts_with("uint") => Expression::Literal("1".to_string()),
                    t if t.starts_with("int") => Expression::Literal("1".to_string()),
                    _ => Expression::Literal("0".to_string()), // Default fallback
                }
            })
            .collect()
    };

    eprintln!("[DEBUG] Generated {} constructor args", result.len());
    result
}

pub(crate) fn strings_to_expressions(arg_strings: &[String]) -> Vec<Expression> {
    arg_strings
        .iter()
        .map(|s| Expression::Literal(s.clone()))
        .collect()
}

pub(crate) fn generate_valid_args_for_function(
    function_params: &[ParameterInfo],
    actual_args_opt: Option<&Vec<String>>,
) -> Result<Vec<Expression>> {
    if let Some(actual_args) = actual_args_opt {
        Ok(strings_to_expressions(actual_args))
    } else {
        let args = function_params
            .iter()
            .map(|param| match param.param_type.as_str() {
                "string" => Expression::Literal("\"updated test value\"".to_string()),
                "address" => Expression::Literal("address(0x1)".to_string()),
                "bool" => Expression::Literal("true".to_string()),
                t if t.starts_with("uint") => Expression::Literal("42".to_string()),
                t if t.starts_with("int") => Expression::Literal("42".to_string()),
                _ => Expression::Literal("1".to_string()),
            })
            .collect();

        Ok(args)
    }
}

pub(crate) fn sanitize_identifier(input: &str) -> String {
    input
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect::<String>()
        .trim_matches('_')
        .to_string()
}

pub(crate) fn capitalize_first_letter(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

pub(crate) fn generate_realistic_args_for_function(
    _graph: &CallGraph,
    _func_node: &graph::cg::Node,
    condition: &str,
) -> Result<Vec<Expression>> {
    let args = if condition.contains("empty") || condition.contains("length") {
        vec![Expression::Literal("\"\"".to_string())] 
    } else if condition.contains("zero") || condition.contains("0") {
        vec![Expression::Literal("0".to_string())] 
    } else if condition.contains("negative") {
        vec![Expression::Literal("-1".to_string())] 
    } else if condition.contains("address") {
        vec![Expression::Literal("address(0)".to_string())] 
    } else {
        vec![Expression::Literal("0".to_string())]
    };

    Ok(args)
}

pub(crate) fn to_pascal_case(s: &str) -> String {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solidity_test_builder() {
        let contract = SolidityTestBuilder::new("TestContract".to_string())
            .add_import("../src/MyContract.sol".to_string())
            .add_state_variable(StateVariable {
                name: "testVar".to_string(),
                var_type: "uint256".to_string(),
                visibility: "private".to_string(),
                initial_value: Some("42".to_string()),
            })
            .build();

        assert_eq!(contract.name, "TestContract");
        assert!(contract
            .imports
            .contains(&"../src/MyContract.sol".to_string()));
        assert_eq!(contract.state_variables.len(), 1);
        assert_eq!(contract.state_variables[0].name, "testVar");
    }

    #[test]
    fn test_sanitize_identifier() {
        assert_eq!(sanitize_identifier("valid_name"), "valid_name");
        assert_eq!(sanitize_identifier("invalid-name!"), "invalid_name");
        assert_eq!(sanitize_identifier("123_start"), "123_start");
        assert_eq!(sanitize_identifier("_underscore_"), "underscore");
    }

    #[test]
    fn test_capitalize_first_letter() {
        assert_eq!(capitalize_first_letter("hello"), "Hello");
        assert_eq!(capitalize_first_letter("WORLD"), "WORLD");
        assert_eq!(capitalize_first_letter(""), "");
        assert_eq!(capitalize_first_letter("a"), "A");
    }
}
