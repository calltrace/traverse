/// Module that implements the compiler for the Traverse language.
///
/// This module provides a complete compilation pipeline for Traverse, transforming source code
/// into executable DDlog programs. The compiler follows a multi-stage process:
///
/// 1. Parsing: Converts Traverse source code into an AST representation
/// 2. IR Generation: Transforms the AST into an intermediate representation (IR)
/// 3. DDlog Generation: Converts the IR into DDlog code
/// 4. Execution: Compiles and runs the generated DDlog program
/// 5. Hydration: Processes the DDlog output into structured data
///
/// The compiler can be configured to enable or disable tracing, which logs detailed information
/// about the compilation and execution process using the `tracing` crate.
///
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};

use backend::ddlog_drain::DdlogDrain;
use backend::ddlog_rt::{build_ddlog_crate, generate_rust_project, validate};
use backend::facts::{DDLogCommand, InsertCommandFn, TreeSitterToDDLog};
use backend::hydrate::{BucketConfig, Hydrator, InputSource};
use language::Language;

use frontend::formatter::Formatter;
use frontend::gen_ir::IrGenerator;
use frontend::parser;
use frontend::syntax::SyntaxTheme;

#[derive(Debug)]
pub enum CompilerError {
    Io(std::io::Error),
    Parse(String),
    IrGeneration(String),
    DdlogGeneration(String),
    DdlogValidation(String),
    DdlogExecution(String),
}

impl std::fmt::Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompilerError::Io(err) => writeln!(f, "IO error: {}", err),
            CompilerError::Parse(msg) => writeln!(f, "Parse error:\n{}", msg),
            CompilerError::IrGeneration(msg) => writeln!(f, "IR generation error:\n{}", msg),
            CompilerError::DdlogGeneration(msg) => writeln!(f, "DDlog generation error:\n{}", msg),
            CompilerError::DdlogValidation(msg) => write!(f, "DDlog validation error:\n{}", msg),
            CompilerError::DdlogExecution(msg) => writeln!(f, "DDlog execution error:\n{}", msg),
        }
    }
}

impl std::error::Error for CompilerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            CompilerError::Io(err) => Some(err),
            _ => None,
        }
    }
}

impl From<std::io::Error> for CompilerError {
    fn from(err: std::io::Error) -> Self {
        CompilerError::Io(err)
    }
}

/// A builder for configuring and executing the compilation process
pub struct Compiler {
    project_name: Option<String>,
    input_ts_grammars: Vec<PathBuf>,
    intermediate_ts_grammars: Vec<PathBuf>,
    source_file: Option<Box<Path>>,
    no_execute: bool,
    no_hydrate: bool,
    enable_tracing: bool,
    hydrator_config: Option<BucketConfig>,
    hydrator_config_file: Option<PathBuf>,
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            project_name: None,
            input_ts_grammars: Vec::new(),
            intermediate_ts_grammars: Vec::new(),
            source_file: None,
            no_execute: false,
            no_hydrate: false,
            enable_tracing: false,
            hydrator_config: None,
            hydrator_config_file: None,
        }
    }

    pub fn with_project_name(mut self, name: String) -> Self {
        self.project_name = Some(name);
        self
    }

    pub fn with_input_ts_grammar(mut self, path: PathBuf) -> Self {
        self.input_ts_grammars.push(path);
        self
    }

    pub fn with_input_ts_grammars(mut self, paths: Vec<PathBuf>) -> Self {
        self.input_ts_grammars.extend(paths);
        self
    }

    pub fn with_intermediate_ts_grammar(mut self, path: PathBuf) -> Self {
        self.intermediate_ts_grammars.push(path);
        self
    }

    /// Add multiple intermediate TreeSitter grammar paths
    pub fn with_intermediate_ts_grammars(mut self, paths: Vec<PathBuf>) -> Self {
        self.intermediate_ts_grammars.extend(paths);
        self
    }

    pub fn with_source_file(mut self, path: Box<Path>) -> Self {
        self.source_file = Some(path);
        self
    }

    pub fn without_execution(mut self) -> Self {
        self.no_execute = true;
        self
    }

    pub fn without_hydration(mut self) -> Self {
        self.no_hydrate = true;
        self
    }

    /// Enable or disable tracing for the compilation process
    pub fn with_tracing(mut self, enable: bool) -> Self {
        self.enable_tracing = enable;
        self
    }

    pub fn with_hydrator_config(mut self, config: BucketConfig) -> Self {
        self.hydrator_config = Some(config);
        self
    }

    /// Set the path to the hydration configuration file
    pub fn with_hydrator_config_file(mut self, path: PathBuf) -> Self {
        self.hydrator_config_file = Some(path);
        self
    }

    /// Parse the source file and extract DDLog commands
    pub fn parse_source_file(
        &self,
        language: &impl Language,
    ) -> Result<Vec<DDLogCommand>, CompilerError> {
        let source_file = self.source_file.as_ref().ok_or_else(|| {
            CompilerError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "Source file not specified",
            ))
        })?;

        let source_code = std::fs::read_to_string(source_file).map_err(CompilerError::Io)?;
        let converter = TreeSitterToDDLog::new(&source_code, language)
            .with_excluded_relations(HashSet::from(["String".to_string()]));
        Ok(converter.extract_commands_with_fact_nodes(None))
    }

    pub fn process_input(&self, input: &str) -> Result<CompilationResult, CompilerError> {
        // Parse the DSL input
        let lval = parser::parse(input).map_err(|e| CompilerError::Parse(format!("{:?}", e)))?;

        // Format the DSL with syntax highlighting
        let formatted = Formatter::new(2)
            .with_syntax_highlighting(Some(SyntaxTheme::default()))
            .format_with_highlighting(&lval);

        if self.enable_tracing {
            println!("DSL:\n\n{}\n", formatted);
        }

        // Generate IR from the parsed DSL
        let mut ir_generator = IrGenerator::new()
            .with_input_relations(false)
            .with_input_treesitter_grammars(self.input_ts_grammars.clone());

        let dl_ir = ir_generator
            .lval_to_ir(&lval)
            .map_err(|e| CompilerError::IrGeneration(format!("{:?}", e)))?;

        // Format the IR
        let formatted_ir = ir::format_program(&dl_ir.to_string(), true, 2, false)
            .map_err(|e| CompilerError::IrGeneration(format!("{:?}", e)))?;

        if self.enable_tracing {
            println!("IR:\n\n{}\n", formatted_ir);
        }

        // Generate DDlog from the IR
        let ddlog_generator = backend::gen_ddlog::DDlogGenerator::new()
            .with_input_relations(false)
            .with_input_treesitter_grammars(self.input_ts_grammars.clone())
            .embed_primitives();

        let ddlog = ddlog_generator
            .generate(*dl_ir)
            .map_err(|e| CompilerError::DdlogGeneration(format!("{}", e)))?;

        // Validate the generated DDlog
        let ddlog_str = ddlog.to_string();
        if let Err(e) = validate(&ddlog_str, self.enable_tracing) {
            return Err(CompilerError::DdlogValidation(e.to_string()));
        }

        // Return the compilation result
        Ok(CompilationResult {
            dsl: formatted,
            ir: formatted_ir,
            ddlog: ddlog_str,
        })
    }

    pub fn compile(
        &self,
        input: &str,
        language: &impl Language,
    ) -> Result<ExecutionResult, CompilerError> {
        let project_name = self.project_name.clone().ok_or_else(|| {
            CompilerError::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Project name not specified",
            ))
        })?;

        let compilation_result = self.process_input(input)?;
        if self.no_execute {
            return Ok(ExecutionResult {
                compilation: compilation_result,
                ddlog_output: None,
                hydrated_output: None,
            });
        }

        let cmds = self.parse_source_file(language)?;

        let base_dir = PathBuf::from(format!("./{}", project_name));
        fs::create_dir_all(&base_dir)?;

        // Generate and build the Rust project for DDlog
        generate_rust_project(&base_dir, &project_name, &compilation_result.ddlog);

        if let Err(e) = build_ddlog_crate(&base_dir, &project_name, self.enable_tracing) {
            return Err(CompilerError::DdlogExecution(format!(
                "Failed to build DDlog project: {}",
                e
            )));
        }

        let ddlog_output = backend::ddlog_rt::run_ddlog_crate(
            &base_dir,
            &project_name,
            &cmds,
            self.enable_tracing,
        )
        .map_err(|e| {
            CompilerError::DdlogExecution(format!("Failed to run DDlog project: {}", e))
        })?;

        let hydrated_output = if !self.no_hydrate {
            let hydrator_config = if let Some(config) = self.hydrator_config.clone() {
                config
            } else if let Some(config_path) = &self.hydrator_config_file {
                match BucketConfig::from_yaml_file(config_path) {
                    Ok(config) => config,
                    Err(e) => {
                        return Err(CompilerError::DdlogExecution(format!(
                            "Failed to load hydration config from {}: {}",
                            config_path.display(),
                            e
                        )));
                    }
                }
            } else {
                // Try to load from default path
                let default_path = PathBuf::from("hydration.yaml");
                    match BucketConfig::from_yaml_file(&default_path) {
                        Ok(config) => config,
                        Err(_) => {
                            // Fall back to hardcoded default if file exists but can't be parsed
                            BucketConfig::new()
                                .with_pool_shape("sequenceDiagram")
                                .with_bucket(
                                    "participants",
                                    100,
                                    "path",
                                    "val",
                                    vec![
                                        InputSource::new(
                                            "EmitMermaidLineMockActorLine",
                                            100,
                                        ),
                                        InputSource::new(
                                            "EmitMermaidLineCallerParticipantLine",
                                            99,
                                        ),
                                        InputSource::new(
                                            "EmitMermaidLineCalleeParticipantLine",
                                            90,
                                        ),
                                        InputSource::new(
                                            "EmitMermaidLineContractParticipantLine",
                                            90,
                                        ),
                                    ],
                                )
                                .with_bucket(
                                    "flow",
                                    90,
                                    "ce_id_path",
                                    "val",
                                    vec![
                                        InputSource::new("EmitMermaidLineSignalLine", 100),
                                        InputSource::new("EmitMermaidLineActivateLine", 90),
                                        InputSource::new("EmitMermaidLineReturnSignalLine", 80),
                                        InputSource::new("EmitMermaidLineDeactivateLine", 70),
                                        InputSource::new("EmitMermaidLineIntraSignalLine", 100),
                                        InputSource::new(
                                            "EmitMermaidLineIntraSignalLineNoReturn",
                                            100,
                                        ),
                                        InputSource::new("EmitMermaidLineIntraActivateLine", 90),
                                        InputSource::new(
                                            "EmitMermaidLineIntraActivateLineNoReturn",
                                            90,
                                        ),
                                        InputSource::new(
                                            "EmitMermaidLineIntraReturnSignalLine",
                                            80,
                                        ),
                                        InputSource::new("EmitMermaidLineIntraDeactivateLine", 70),
                                        InputSource::new(
                                            "EmitMermaidLineIntraDeactivateLineNoReturn",
                                            70,
                                        ),
                                    ],
                                )
                        }
                    }
            };

            // save config
            hydrator_config.to_yaml_file("config.yaml").unwrap();

            let mut hydrator = Hydrator::new(hydrator_config);

            let lines = ddlog_output.lines().map(|s| s.to_string());
            let drain = DdlogDrain::new(lines);

            hydrator.process_drain(drain);
            Some(hydrator.dump())
        } else {
            None
        };

        Ok(ExecutionResult {
            compilation: compilation_result,
            ddlog_output: Some(ddlog_output),
            hydrated_output,
        })
    }
}

pub struct CompilationResult {
    pub dsl: String,
    pub ir: String,
    pub ddlog: String,
}

pub struct ExecutionResult {
    pub compilation: CompilationResult,
    pub ddlog_output: Option<String>,
    pub hydrated_output: Option<String>,
}
