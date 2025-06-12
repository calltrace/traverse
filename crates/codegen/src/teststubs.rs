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

use graph::cg::{
    CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput,
    EdgeType, NodeType, Visibility
};
use anyhow::{Context, Result};
use foundry_compilers::{
    artifacts::{Settings, Source, Sources},
    solc::Solc,
};
use foundry_compilers_artifacts_solc::SolcInput;
use semver::Version;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, path::PathBuf};
use std::str::FromStr;
use std::{fs, path::Path};

use crate::CodeGenError;

#[derive(Debug, serde::Serialize)]
pub struct ContractInfo {
    name: String,
    has_constructor: bool,
    constructor_params: Vec<ParameterInfo>,
    functions: Vec<FunctionInfo>,
}

#[derive(Debug, serde::Serialize, Clone)]
struct FunctionInfo {
    name: String,
    visibility: String,
    return_type: Option<String>,
    parameters: Vec<ParameterInfo>,
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ParameterInfo {
    pub name: String,
    #[serde(rename = "type")]
    pub param_type: String,
    pub description: Option<String>,
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
    solc: Solc,
}

impl FoundryIntegration {
    pub fn new(project_root: PathBuf) -> Result<Self> {
        let version = Version::from_str("0.8.20").context("Failed to parse Solidity version")?;

        let solc = Solc::find_svm_installed_version(&version)
            .context("Failed to find Solc version")?
            .unwrap_or_else(|| {
                Solc::new("solc").unwrap_or_else(|_| {
                    Solc::find_or_install(&version)
                        .unwrap_or_else(|_| Solc::new_with_version("solc", version.clone()))
                })
            });

        Ok(Self { project_root, solc })
    }

    pub fn new_with_project_setup(project_root: PathBuf) -> Result<Self> {
        // Ensure foundry.toml exists
        let foundry_toml_path = project_root.join("foundry.toml");
        if !foundry_toml_path.exists() {
            Self::create_default_foundry_toml(&foundry_toml_path)?;
        }

        // Ensure src directory exists
        let src_dir = project_root.join("src");
        if !src_dir.exists() {
            fs::create_dir_all(&src_dir).context("Failed to create src directory")?;
        }

        // Ensure test directory exists
        let test_dir = project_root.join("test");
        if !test_dir.exists() {
            fs::create_dir_all(&test_dir).context("Failed to create test directory")?;
        }

        Self::new(project_root)
    }

    fn create_default_foundry_toml(path: &Path) -> Result<()> {
        let foundry_config = r#"[profile.default]
src = "src"
out = "out"
libs = ["lib"]
test = "test"
cache_path = "cache"
solc_version = "0.8.20a

[fmt]
line_length = 120
tab_width = 4
bracket_spacing = true
"#;
        fs::write(path, foundry_config).context("Failed to create foundry.toml")?;
        Ok(())
    }

    pub fn setup_test_project(&self, contracts: &[&str]) -> Result<()> {
        // Create BaseTest.sol if it doesn't exist
        let base_test_path = self.project_root.join("test/BaseTest.sol");
        if !base_test_path.exists() {
            self.create_base_test_contract(&base_test_path)?;
        }

        for contract_name in contracts {
            let contract_path = self.project_root.join(format!("src/{}.sol", contract_name));
            if !contract_path.exists() {
                self.create_sample_contract(&contract_path, contract_name)?;
            }
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

    fn create_sample_contract(&self, path: &Path, contract_name: &str) -> Result<()> {
        let contract_content = format!(
            r#"// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * @title {}
 * @dev Sample contract generated for testing
 */
contract {} {{
    string public greeting;
    uint256 public value;
    
    event GreetingChanged(string newGreeting);
    event ValueChanged(uint256 newValue);
    
    constructor(string memory _greeting) {{
        require(bytes(_greeting).length > 0, "Greeting cannot be empty");
        greeting = _greeting;
        value = 0;
    }}
    
    function setGreeting(string memory _greeting) public {{
        require(bytes(_greeting).length > 0, "Greeting cannot be empty");
        greeting = _greeting;
        emit GreetingChanged(_greeting);
    }}
    
    function setValue(uint256 _value) public {{
        require(_value > 0, "Value must be positive");
        value = _value;
        emit ValueChanged(_value);
    }}
    
    function getGreeting() public view returns (string memory) {{
        return greeting;
    }}
    
    function getValue() public view returns (uint256) {{
        return value;
    }}
}}
"#,
            contract_name, contract_name
        );

        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, contract_content).context("Failed to create sample contract")?;
        Ok(())
    }

    pub fn compile_test_contract(&self, contract: &SolidityTestContract) -> Result<bool> {
        let source_code = self.generate_solidity_code(contract)?;
        let file_name = format!("{}.t.sol", contract.name);

        let mut sources = Sources::new();
        sources.insert(PathBuf::from(&file_name), Source::new(source_code));

        let input = SolcInput {
            language: foundry_compilers_artifacts_solc::SolcLanguage::Solidity,
            sources,
            settings: Settings::default(),
        };

        let output = self
            .solc
            .compile(&input)
            .context("Failed to compile Solidity contract")?;

        if output.has_error() {
            for error in &output.errors {
                eprintln!(
                    "Compilation error: {}",
                    error
                        .formatted_message
                        .as_deref()
                        .unwrap_or("Unknown error")
                );
            }
            return Ok(false);
        }

        Ok(true)
    }

    pub fn generate_solidity_code(&self, contract: &SolidityTestContract) -> Result<String> {
        let mut code = String::new();

        code.push_str("pragma solidity ^0.8.0;\n\n");

        // Imports
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
            code.push_str("    function setUp() public override {\n");
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

    pub fn validate_generated_code(&self, contract: &SolidityTestContract) -> Result<bool> {
        self.compile_test_contract(contract)
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
                imports: vec![
                    "forge-std/Test.sol".to_string(),
                    "./BaseTest.sol".to_string(),
                ],
                inheritance: vec!["Test".to_string(), "BaseTest".to_string()],
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
    input: &CallGraphGeneratorInput,
    ctx: &CallGraphGeneratorContext,
    verbose: bool,
    output_dir: &Path,
    foundry_root: Option<PathBuf>,
    deployer_only: bool,
    validate_compilation: bool,
) -> Result<()> {
    if verbose {
        println!("🔧 Initializing Foundry integration...");
    }

    let foundry_root = foundry_root.clone().unwrap_or_else(|| {
        output_dir
            .parent()
            .unwrap_or(Path::new("."))
            .to_path_buf()
    });

    let foundry = FoundryIntegration::new_with_project_setup(foundry_root)
        .map_err(|e| CodeGenError::FoundryError(format!("Failed to initialize Foundry: {}", e)))?;

    let contracts = extract_contracts_from_graph(graph, input, ctx);

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

        if !deployer_only {
            generate_foundry_deployer_contract(
                contract_info,
                &foundry,
                &output_dir,
                verbose,
            )?;
            generated_count += 1;
        }

        for function_info in &contract_info.functions {
            // Generate revert tests
            if let Ok(revert_tests) = generate_revert_tests_from_cfg(
                graph,
                &contract_info.name,
                &function_info.name,
                &function_info.parameters,
            ) {
                if !revert_tests.is_empty() {
                    let test_contract =
                        create_revert_test_contract(contract_info, function_info, revert_tests);

                    if generate_and_validate_contract(
                        &foundry,
                        &test_contract,
                        &output_dir,
                        validate_compilation,
                        verbose,
                    )? {
                        validated_count += 1;
                    }
                    generated_count += 1;
                }
            }

            if let Ok(state_tests) = generate_state_change_tests_from_cfg(
                graph,
                &contract_info.name,
                &function_info.name,
                &function_info.parameters,
            ) {
                if !state_tests.is_empty() {
                    let test_contract =
                        create_state_test_contract(contract_info, function_info, state_tests);

                    if generate_and_validate_contract(
                        &foundry,
                        &test_contract,
                        &output_dir,
                        validate_compilation,
                        verbose,
                    )? {
                        validated_count += 1;
                    }
                    generated_count += 1;
                }
            }
        }
    }

    if verbose {
        println!("📊 Generation Summary:");
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
    input: &CallGraphGeneratorInput,
    ctx: &CallGraphGeneratorContext,
) -> Vec<ContractInfo> {
    let mut contracts_map: HashMap<String, ContractInfo> = HashMap::new();

    for node in &graph.nodes {
        if let Some(contract_name_str) = &node.contract_name {
            let is_interface_scope = ctx.all_interfaces.contains_key(contract_name_str);
            if node.node_type == graph::cg::NodeType::Interface && &node.name == contract_name_str {
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
                graph::cg::NodeType::Constructor => {
                    contract_info.has_constructor = true;
                    if let Some((_, node_info_for_ctor, _)) = ctx
                        .definition_nodes_info
                        .iter()
                        .find(|(id, _, _)| *id == node.id)
                    {
                        if let Some(ctor_ts_node) =
                            input.tree.root_node().descendant_for_byte_range(
                                node_info_for_ctor.span.0,
                                node_info_for_ctor.span.1,
                            )
                        {
                            contract_info.constructor_params =
                                extract_function_parameters(ctor_ts_node, &input.source);
                        }
                    }
                }
                graph::cg::NodeType::Function => {
                    if !is_interface_scope {
                        let mut params = Vec::new();
                        if let Some((_, node_info_for_func, _)) = ctx
                            .definition_nodes_info
                            .iter()
                            .find(|(id, _, _)| *id == node.id)
                        {
                            if let Some(func_ts_node) =
                                input.tree.root_node().descendant_for_byte_range(
                                    node_info_for_func.span.0,
                                    node_info_for_func.span.1,
                                )
                            {
                                params = extract_function_parameters(func_ts_node, &input.source);
                            }
                        }
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

    contracts_map.into_values().collect()
}

fn extract_function_parameters(
    fn_like_ts_node: tree_sitter::Node,
    source: &str,
) -> Vec<ParameterInfo> {
    let mut parameters = Vec::new();
    if let Some(param_list_node) = fn_like_ts_node.child_by_field_name("parameters") {
        let mut cursor = param_list_node.walk();
        for child in param_list_node.children(&mut cursor) {
            if child.kind() == "parameter" {
                let type_node = child.child_by_field_name("type");
                let name_node = child.child_by_field_name("name");

                if let (Some(tn), Some(nn)) = (type_node, name_node) {
                    parameters.push(ParameterInfo {
                        name: graph::parser::get_node_text(&nn, source).to_string(),
                        param_type: graph::parser::get_node_text(&tn, source).to_string(),
                        description: None,
                    });
                }
            }
        }
    }
    parameters
}



fn create_revert_test_contract(
    contract_info: &ContractInfo,
    function_info: &FunctionInfo,
    revert_tests: Vec<TestFunction>,
) -> SolidityTestContract {
    let mut builder = SolidityTestBuilder::new(format!(
        "{}{}RevertTest",
        contract_info.name,
        to_pascal_case(&function_info.name)
    ))
    .add_import(format!("../src/{}.sol", contract_info.name))
    .add_state_variable(StateVariable {
        name: "contractInstance".to_string(),
        var_type: contract_info.name.clone(),
        visibility: "private".to_string(),
        initial_value: None,
    })
    .set_setup_function(SetupFunction {
        body: vec![
            Statement::Comment {
                text: format!("Deploy {} for testing", contract_info.name),
            },
            Statement::Assignment {
                target: "contractInstance".to_string(),
                value: Expression::FunctionCall {
                    target: None,
                    function: format!("new {}", contract_info.name),
                    args: generate_constructor_args(&contract_info.constructor_params),
                },
            },
        ],
    });

    for test_func in revert_tests {
        builder = builder.add_test_function(test_func);
    }

    builder.build()
}


fn create_state_test_contract(
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
    .set_setup_function(SetupFunction {
        body: vec![
            Statement::Comment {
                text: format!("Deploy {} for testing", contract_info.name),
            },
            Statement::Assignment {
                target: "contractInstance".to_string(),
                value: Expression::FunctionCall {
                    target: None,
                    function: format!("new {}", contract_info.name),
                    args: generate_constructor_args(&contract_info.constructor_params),
                },
            },
        ],
    });

    for test_func in state_tests {
        builder = builder.add_test_function(test_func);
    }

    builder.build()
}

fn generate_and_validate_contract(
    foundry: &FoundryIntegration,
    contract: &SolidityTestContract,
    output_dir: &Path,
    validate_compilation: bool,
    verbose: bool,
) -> Result<bool> {
    // Generate the Solidity code
    let source_code = foundry
        .generate_solidity_code(contract)
        .map_err(|e| CodeGenError::FoundryError(format!("Failed to generate code: {}", e)))?;

    // Validate compilation if requested
    let is_valid = if validate_compilation {
        match foundry.validate_generated_code(contract) {
            Ok(true) => {
                if verbose {
                    println!("  ✅ Validation passed: {}", contract.name);
                }
                true
            }
            Ok(false) => {
                if verbose {
                    eprintln!("  ❌ Validation failed: {}", contract.name);
                }
                false
            }
            Err(e) => {
                if verbose {
                    eprintln!("  ⚠️  Validation error for {}: {}", contract.name, e);
                }
                false
            }
        }
    } else {
        true // Skip validation
    };

    // Write the generated code to file
    let output_filename = format!("{}.t.sol", contract.name);
    let output_path = output_dir.join(&output_filename);

    fs::write(&output_path, source_code)
        .map_err(|e| CodeGenError::OutputWriteError(output_path.clone(), e))?;

    if verbose {
        let status = if is_valid { "✅" } else { "⚠️ " };
        println!("  {} Generated: {}", status, output_path.display());
    }

    Ok(is_valid)
}


fn generate_foundry_deployer_contract(
    contract_info: &ContractInfo,
    foundry: &FoundryIntegration,
    output_dir: &Path,
    verbose: bool,
) -> Result<()> {
    let deployer_contract = SolidityTestBuilder::new(format!("Deployer_{}", contract_info.name))
        .add_import(format!("../src/{}.sol", contract_info.name))
        .add_state_variable(StateVariable {
            name: "deployedContract".to_string(),
            var_type: contract_info.name.clone(),
            visibility: "public".to_string(),
            initial_value: None,
        })
        .add_test_function(TestFunction {
            name: "deploy".to_string(),
            visibility: "external".to_string(),
            body: vec![
                Statement::Comment {
                    text: format!("Deploy {} with constructor parameters", contract_info.name),
                },
                Statement::Assignment {
                    target: "deployedContract".to_string(),
                    value: Expression::FunctionCall {
                        target: None,
                        function: format!("new {}", contract_info.name),
                        args: generate_constructor_args(&contract_info.constructor_params),
                    },
                },
            ],
            test_type: TestType::StateChangeTest {
                variable_name: "deployedContract".to_string(),
                expected_change: "should be deployed".to_string(),
            },
        })
        .build();

    let source_code = foundry
        .generate_solidity_code(&deployer_contract)
        .map_err(|e| {
            CodeGenError::FoundryError(format!("Failed to generate deployer code: {}", e))
        })?;

    let output_filename = format!("Deployer_{}.sol", contract_info.name);
    let output_path = output_dir.join(&output_filename);

    fs::write(&output_path, source_code)
        .map_err(|e| CodeGenError::OutputWriteError(output_path.clone(), e))?;

    if verbose {
        println!("  📦 Generated deployer: {}", output_path.display());
    }

    Ok(())
}

fn generate_constructor_args(params: &[ParameterInfo]) -> Vec<Expression> {
    params
        .iter()
        .map(|param| {
            // Generate appropriate default values based on type
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
}

pub fn generate_revert_tests_from_cfg(
    graph: &CallGraph,
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
            if edge.source_node_id == func_node.id && edge.edge_type == EdgeType::Require {
                let condition = edge
                    .argument_names
                    .as_ref()
                    .and_then(|args| args.get(0))
                    .cloned()
                    .unwrap_or_else(|| "unknown_condition".to_string());

                let error_message = edge
                    .argument_names
                    .as_ref()
                    .and_then(|args| args.get(1))
                    .cloned()
                    .unwrap_or_default()
                    .trim_matches('"')
                    .to_string();

                let test_name = format!(
                    "test_{}_reverts_{}",
                    function_name,
                    sanitize_identifier(&condition)
                );

                let mut body = Vec::new();

                body.push(Statement::Comment {
                    text: format!("Test that {} reverts when: {}", function_name, condition),
                });

                body.push(Statement::ExpectRevert {
                    error_message: error_message.clone(),
                });

                body.push(Statement::FunctionCall {
                    target: Some("contractInstance".to_string()),
                    function: function_name.to_string(),
                    args: generate_valid_args_for_function(function_params)?,
                });

                test_functions.push(TestFunction {
                    name: test_name,
                    visibility: "public".to_string(),
                    body,
                    test_type: TestType::RevertTest {
                        expected_error: error_message,
                        condition,
                    },
                });
            }
        }
    }

    Ok(test_functions)
}

pub fn generate_state_change_tests_from_cfg(
    graph: &CallGraph,
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

                        body.push(Statement::VariableDeclaration {
                            var_type: "uint256".to_string(), // TODO: Extract actual type
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
                            args: generate_valid_args_for_function(function_params)?,
                        });

                        body.push(Statement::Assert {
                            condition: Expression::BinaryOp {
                                left: Box::new(Expression::FunctionCall {
                                    target: Some("contractInstance".to_string()),
                                    function: getter_name,
                                    args: vec![],
                                }),
                                operator: "!=".to_string(),
                                right: Box::new(Expression::Identifier("initialValue".to_string())),
                            },
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

pub fn generate_enhanced_revert_tests_from_cfg(
    graph: &CallGraph,
    contract_name: &str,
    function_name: &str,
) -> Result<Vec<TestFunction>> {
    let mut test_functions = Vec::new();

    let func_node = graph.nodes.iter().find(|n| {
        n.contract_name.as_deref() == Some(contract_name)
            && n.name == function_name
            && n.node_type == NodeType::Function
    });

    if let Some(func_node) = func_node {
        for edge in &graph.edges {
            if edge.source_node_id == func_node.id && edge.edge_type == EdgeType::Require {
                let condition = edge
                    .argument_names
                    .as_ref()
                    .and_then(|args| args.get(0))
                    .cloned()
                    .unwrap_or_else(|| "unknown_condition".to_string());

                let error_message = edge
                    .argument_names
                    .as_ref()
                    .and_then(|args| args.get(1))
                    .cloned()
                    .unwrap_or_default()
                    .trim_matches('"')
                    .to_string();

                // Generate test function with enhanced parameter handling
                let test_name = format!(
                    "test_{}_reverts_{}",
                    function_name,
                    sanitize_identifier(&condition)
                );

                let mut body = Vec::new();

                // Add setup comments
                body.push(Statement::Comment {
                    text: format!("Test that {} reverts when: {}", function_name, condition),
                });

                // Add expectRevert with proper error handling
                if error_message.is_empty() {
                    body.push(Statement::FunctionCall {
                        target: Some("vm".to_string()),
                        function: "expectRevert".to_string(),
                        args: vec![],
                    });
                } else {
                    body.push(Statement::ExpectRevert {
                        error_message: error_message.clone(),
                    });
                }

                // Add function call that should revert with realistic parameters
                body.push(Statement::FunctionCall {
                    target: Some("contractInstance".to_string()),
                    function: function_name.to_string(),
                    args: generate_realistic_args_for_function(graph, func_node, &condition)?,
                });

                test_functions.push(TestFunction {
                    name: test_name,
                    visibility: "public".to_string(),
                    body,
                    test_type: TestType::RevertTest {
                        expected_error: error_message,
                        condition,
                    },
                });
            }
        }
    }

    Ok(test_functions)
}

fn generate_revert_args_for_function(
    function_params: &[ParameterInfo],
    condition: &str,
) -> Result<Vec<Expression>> {
    let mut args = Vec::new();

    for param in function_params {
        let arg = if condition.contains("empty") || condition.contains("length") {
            if param.param_type == "string" || param.param_type.contains("string") {
                Expression::Literal("\"\"".to_string()) // Empty string
            } else {
                Expression::Literal("0".to_string()) // Zero for other types
            }
        } else if condition.contains("zero") || condition.contains("0") || condition.contains("> 0")
        {
            Expression::Literal("0".to_string()) // Zero value to trigger revert
        } else if condition.contains("negative") {
            Expression::Literal("-1".to_string()) // Negative value
        } else if param.param_type == "address" {
            Expression::Literal("address(0)".to_string()) // Zero address
        } else {
            match param.param_type.as_str() {
                "string" => Expression::Literal("\"\"".to_string()),
                "address" => Expression::Literal("address(0)".to_string()),
                "bool" => Expression::Literal("false".to_string()),
                t if t.starts_with("uint") => Expression::Literal("0".to_string()),
                t if t.starts_with("int") => Expression::Literal("0".to_string()),
                _ => Expression::Literal("0".to_string()),
            }
        };
        args.push(arg);
    }

    Ok(args)
}

fn generate_valid_args_for_function(function_params: &[ParameterInfo]) -> Result<Vec<Expression>> {
    let args = function_params
        .iter()
        .map(|param| match param.param_type.as_str() {
            "string" => Expression::Literal("\"test\"".to_string()),
            "address" => Expression::Literal("address(0x1)".to_string()),
            "bool" => Expression::Literal("true".to_string()),
            t if t.starts_with("uint") => Expression::Literal("42".to_string()),
            t if t.starts_with("int") => Expression::Literal("42".to_string()),
            _ => Expression::Literal("1".to_string()),
        })
        .collect();

    Ok(args)
}

fn sanitize_identifier(input: &str) -> String {
    input
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect::<String>()
        .trim_matches('_')
        .to_string()
}

fn capitalize_first_letter(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

fn generate_realistic_args_for_function(
    _graph: &CallGraph,
    _func_node: &graph::cg::Node,
    condition: &str,
) -> Result<Vec<Expression>> {
    // Analyze the condition to generate appropriate failing arguments
    let args = if condition.contains("empty") || condition.contains("length") {
        vec![Expression::Literal("\"\"".to_string())] // Empty string
    } else if condition.contains("zero") || condition.contains("0") {
        vec![Expression::Literal("0".to_string())] // Zero value
    } else if condition.contains("negative") {
        vec![Expression::Literal("-1".to_string())] // Negative value
    } else if condition.contains("address") {
        vec![Expression::Literal("address(0)".to_string())] // Zero address
    } else {
        // Default case - try to trigger the condition
        vec![Expression::Literal("0".to_string())]
    };

    Ok(args)
}

fn to_pascal_case(s: &str) -> String {
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
