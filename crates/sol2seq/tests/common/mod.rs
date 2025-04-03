use std::collections::HashSet;
use std::fs;
use std::io::{self, Read, Write};
use std::ops::{Deref, DerefMut};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use backend::ddlog_drain::DdlogDrain;
use backend::ddlog_rt::{self, run_ddlog_crate};
use backend::facts::{DDLogCommand, TreeSitterToDDLog};
use backend::hydrate::{self, BucketConfig, Hydrator, InputSource, StreamFactOrderingStrategy};
use compiler::compile::{CompilationResult, Compiler, CompilerError};
use language::{Language, Mermaid, Solidity};
use mermaid::sequence_diagram_ast::SequenceDiagram;
use mermaid::sequence_diagram_parser;

#[derive(Debug, Clone)]
pub enum SourceType {
    Solidity,
    Mermaid,
}
impl SourceType {
    fn to_tree_sitter_language(&self) -> impl Language {
        match self {
            SourceType::Solidity => Solidity,
            SourceType::Mermaid => unimplemented!("Mermaid language support not yet implemented"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub input_path: PathBuf,
    pub project_name: String,
    pub input_parser_homes: Vec<PathBuf>,
    pub intermediate_parser_homes: Vec<PathBuf>,
    pub source_path: PathBuf,
    pub source_type: SourceType,
    pub execute: bool,
    pub hydrate: bool,
    pub enable_tracing: bool,
    pub output_path: Option<PathBuf>,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            input_path: PathBuf::new(),
            project_name: String::new(),
            input_parser_homes: Vec::new(),
            intermediate_parser_homes: Vec::new(),
            source_path: PathBuf::new(),
            source_type: SourceType::Solidity,
            execute: true,
            hydrate: true,
            enable_tracing: true,
            output_path: None,
        }
    }
}

#[derive(Debug)]
pub struct CompilationOutput {
    pub dsl: String,
    pub ir: String,
    pub ddlog: String,
    pub ddlog_output: Option<String>,
    pub hydrated_output: Option<String>,
    pub ddlog_file_path: String,
    pub ddlog_output_file_path: Option<String>,
    pub hydrated_output_file_path: Option<String>,
    pub project_name: String,
}

impl CompilerConfig {
    pub fn validate(&self) -> Result<(), CompilerError> {
        // Validate that all parser homes exist
        for path in &self.input_parser_homes {
            if !path.is_dir() {
                return Err(CompilerError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "Input Parser home directory does not exist: {}",
                        path.display()
                    ),
                )));
            }
        }

        for path in &self.intermediate_parser_homes {
            if !path.is_dir() {
                return Err(CompilerError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "Intermediate Parser home directory does not exist: {}",
                        path.display()
                    ),
                )));
            }
        }

        // Validate that input file exists
        if !self.input_path.exists() {
            return Err(CompilerError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Input path does not exist: {}", self.input_path.display()),
            )));
        }

        // Validate that source file exists
        if !self.source_path.exists() {
            return Err(CompilerError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Source file does not exist: {}", self.source_path.display()),
            )));
        }

        Ok(())
    }
}

pub fn run_compiler(config: &CompilerConfig) -> Result<CompilationOutput, CompilerError> {
    config.validate()?;

    let mut input_file_content = String::new();
    let mut file = fs::File::open(&config.input_path).map_err(|err| CompilerError::Io(err))?;
    fn fun_name(err: io::Error) -> CompilerError {
        CompilerError::Io(err)
    }

    file.read_to_string(&mut input_file_content)
        .map_err(fun_name)?;

    let mut compiler = Compiler::new()
        .with_project_name(config.project_name.clone())
        .with_input_ts_grammars(config.input_parser_homes.clone())
        .with_intermediate_ts_grammars(config.intermediate_parser_homes.clone())
        .with_source_file(config.source_path.clone().into_boxed_path())
        .with_tracing(config.enable_tracing);

    if !config.execute {
        compiler = compiler.without_execution();
    }
    if !config.hydrate {
        compiler = compiler.without_hydration();
    }

    let language = config.source_type.to_tree_sitter_language();

    let result = compiler.compile(&input_file_content, &language)?;

    let ddlog_file = format!("{}.dl", config.project_name);
    fs::write(&ddlog_file, &result.compilation.ddlog).map_err(|err| CompilerError::Io(err))?;

    let mut ddlog_output_file_path = None;
    let mut hydrated_output_file_path = None;

    if let Some(ddlog_output) = &result.ddlog_output {
        let ddlog_output_file = format!("{}.output.txt", config.project_name);
        fs::write(&ddlog_output_file, ddlog_output).map_err(|err| CompilerError::Io(err))?;
        ddlog_output_file_path = Some(ddlog_output_file);
    }

    if let Some(hydrated) = &result.hydrated_output {
        let output_path = config
            .output_path
            .clone()
            .unwrap_or_else(|| PathBuf::from(format!("{}_hydrated.txt", config.project_name)));

        fs::write(&output_path, hydrated).map_err(|err| CompilerError::Io(err))?;

        hydrated_output_file_path = Some(output_path.to_string_lossy().to_string());
    }

    Ok(CompilationOutput {
        dsl: result.compilation.dsl,
        ir: result.compilation.ir,
        ddlog: result.compilation.ddlog,
        ddlog_output: result.ddlog_output,
        hydrated_output: result.hydrated_output,
        ddlog_file_path: ddlog_file,
        ddlog_output_file_path,
        hydrated_output_file_path,
        project_name: config.project_name.clone(),
    })
}

/// Error type for DDLog execution operations
#[derive(Debug)]
pub enum DDLogExecutionError {
    Io(io::Error),
    CommandExecution(String),
    InvalidSourceType,
    MermaidError(String),
}

impl From<io::Error> for DDLogExecutionError {
    fn from(err: io::Error) -> Self {
        DDLogExecutionError::Io(err)
    }
}

pub struct DDLogRunConfig {
    pub ddlog_project_dir: PathBuf,
    pub project_name: String,
    pub source_paths: Vec<PathBuf>,
    pub source_type: SourceType,
    pub commands_output_path: Option<PathBuf>,
    pub excluded_relations: Vec<String>,
    pub enable_tracing: bool,
}

impl Default for DDLogRunConfig {
    fn default() -> Self {
        Self {
            ddlog_project_dir: PathBuf::new(),
            project_name: String::new(),
            source_paths: Vec::new(),
            source_type: SourceType::Solidity,
            commands_output_path: None,
            excluded_relations: vec!["String".to_string()],
            enable_tracing: false,
        }
    }
}

impl DDLogRunConfig {
    pub fn validate(&self) -> Result<(), DDLogExecutionError> {
        println!(
            "Validating DDLog run configuration: {:?}",
            self.ddlog_project_dir
        );
        if !self.ddlog_project_dir.is_dir() {
            return Err(DDLogExecutionError::Io(io::Error::new(
                io::ErrorKind::NotFound,
                format!(
                    "DDLog project directory does not exist: {}",
                    self.ddlog_project_dir.display()
                ),
            )));
        }

        for path in &self.source_paths {
            if !path.exists() {
                return Err(DDLogExecutionError::Io(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("Source file does not exist: {}", path.display()),
                )));
            }
        }

        Ok(())
    }
}

pub struct DDLogRunResult {
    pub raw_output: String,
    pub hydrated_output: String,
    pub commands_file_path: PathBuf,
    pub diagram: SequenceDiagram,
}

pub fn run_compiled_ddlog_with_new_sources(
    compilation_output: &CompilationOutput,
    source_paths: Vec<PathBuf>,
    source_type: SourceType,
    excluded_relations: Vec<String>,
) -> Result<DDLogRunResult, DDLogExecutionError> {
    let ddlog_project_dir = Path::new(&compilation_output.project_name).to_path_buf();

    let config = DDLogRunConfig {
        ddlog_project_dir,
        project_name: compilation_output.project_name.clone(),
        source_paths,
        source_type,
        commands_output_path: None,
        excluded_relations,
        enable_tracing: false,
    };

    run_ddlog_with_sources(&config)
}

pub fn run_ddlog_with_sources(
    config: &DDLogRunConfig,
) -> Result<DDLogRunResult, DDLogExecutionError> {
    config.validate()?;

    let language = match config.source_type {
        SourceType::Solidity => Solidity,
        SourceType::Mermaid => return Err(DDLogExecutionError::InvalidSourceType),
    };

    let excluded_relations = config
        .excluded_relations
        .iter()
        .cloned()
        .collect::<HashSet<String>>();

    let mut all_commands = Vec::new();
    all_commands.push(DDLogCommand::Start);

    for source_path in &config.source_paths {
        println!("Processing source file: {}", source_path.display());

        let source_content = fs::read_to_string(source_path)?;

        let converter = TreeSitterToDDLog::new(&source_content, &language)
            .with_excluded_relations(excluded_relations.clone());

        let mut commands = converter.extract_commands_with_fact_nodes(None);

        if !commands.is_empty() && commands[0] == DDLogCommand::Start {
            commands.remove(0);
        }
        if !commands.is_empty() && commands.last() == Some(&DDLogCommand::CommitDumpChanges) {
            commands.pop();
        }

        all_commands.extend(commands);
    }

    all_commands.push(DDLogCommand::CommitDumpChanges);

    let commands_file_path = match &config.commands_output_path {
        Some(path) => path.clone(),
        None => PathBuf::from(format!("{}_commands.txt", config.project_name)),
    };

    //
    // Write commands to file for reference/debugging
    let mut file = fs::File::create(&commands_file_path)?;
    for command in &all_commands {
        writeln!(file, "{}", command)?;
    }

    let ddlog_output = run_ddlog_crate(
        &config.ddlog_project_dir,
        &config.project_name,
        &all_commands,
        false,
    )
    .map_err(|e| {
        DDLogExecutionError::CommandExecution(format!("Failed to run DDlog project: {}", e))
    })?;

    // Note: disable legacy path-based configuration
    //let hydrator_config = create_default_hydrator_config();
    let hydrator_config = create_dependency_driven_hydrator_config();

    println!("Hydrator config: {:?}", hydrator_config);

    let mut hydrator = Hydrator::new(hydrator_config);

    let lines = ddlog_output.lines().map(|s| s.to_string());

    let ddlog_output_file = format!("{}.output.txt", config.project_name);
    if let Err(err) = fs::write(&ddlog_output_file, &ddlog_output) {
        eprintln!("Warning: Failed to save DDlog output to file: {}", err);
    } else {
        println!("DDlog output saved to: {}", ddlog_output_file);
    }

    let drain = DdlogDrain::new(lines);

    hydrator.process_drain(drain);

    let hydrated_output = hydrator.dump_streams();

    let hydration_output_path = PathBuf::from(format!("{}_hydrated.txt", config.project_name));

    // Save hydrated output to file
    if let Err(err) = fs::write(&hydration_output_path, &hydrated_output) {
        eprintln!("Warning: Failed to save hydrated output to file: {}", err);
    } else {
        println!(
            "Hydrated output saved to: {}",
            hydration_output_path.display()
        );
    }

    sequence_diagram_parser::parse(&hydrated_output)
        .map(|diagram| DDLogRunResult {
            raw_output: ddlog_output,
            hydrated_output,
            commands_file_path,
            diagram,
        })
        .map_err(|err| {
            DDLogExecutionError::MermaidError(format!("Failed to parse sequence diagram: {}", err))
        })
}

fn create_default_hydrator_config() -> BucketConfig {
    BucketConfig::new()
        .with_pool_shape("sequenceDiagram")
        .with_bucket(
            "mock-actor-participants",
            100,
            "",
            "val",
            "dependency",
            ("downstream", "upstream"),
            vec![
                InputSource::new("EmitMermaidLineMockActorLine", 100),
                //InputSource::new("EmitMermaidLineMockActorLineReturn", 100),
                //  InputSource::new("EmitMermaidLineMockActorParticipantLine", 99),
                //  InputSource::new("EmitMermaidLineContractParticipantLine", 90),
            ],
        )
        .with_bucket(
            "contract-participants",
            95,
            "",
            "val",
            "dependency",
            ("downstream", "upstream"),
            vec![InputSource::new(
                "EmitMermaidLineContractParticipantLine",
                95,
            )],
        )
        .with_bucket(
            "library-participants",
            90,
            "",
            "val",
            "dependency",
            ("downstream", "upstream"),
            vec![InputSource::new(
                "EmitMermaidLineLibraryParticipantLine",
                90,
            )],
        )
        .with_bucket(
            "mock-actor-request-flows",
            100,
            "no_return_func_id_path",
            "val",
            "dependency",
            ("downstream", "upstream"),
            vec![InputSource::new(
                "EmitMermaidLineMockActorSignalNoReturnLine",
                100,
            )],
        )
        .with_bucket(
            "mock-actor-return-flows",
            95,
            "return_stmt_id_path",
            "val",
            "dependency",
            ("downstream", "upstream"),
            vec![
                //InputSource::new("EmitMermaidLineMockActorSignalLine", 100),
                InputSource::new("EmitMermaidLineMockActorReturnSignalLine", 95),
            ],
        )
        .with_bucket(
            "intra-contract-flows",
            90,
            "intra_ce_no_return_id_path",
            "val",
            "dependency",
            ("downstream", "upstream"),
            vec![
                InputSource::new("EmitMermaidLineIntraSignalLine", 100),
                InputSource::new("EmitMermaidLineIntraSignalLineNoReturn", 100),
                InputSource::new("EmitMermaidLineIntraActivateLine", 90),
                InputSource::new("EmitMermaidLineIntraActivateLineNoReturn", 90),
                InputSource::new("EmitMermaidLineIntraReturnSignalLine", 80),
                InputSource::new("EmitMermaidLineIntraDeactivateLine", 70),
                InputSource::new("EmitMermaidLineIntraDeactivateLineNoReturn", 70),
            ],
        )
        .with_bucket(
            "inter-contract-flows",
            90,
            "ce_id_path",
            "val",
            "dependency",
            ("downstream", "upstream"),
            vec![
                InputSource::new("EmitMermaidLineSignalLine", 100),
                InputSource::new("EmitMermaidLineActivateLine", 90),
                InputSource::new("EmitMermaidLineReturnSignalLine", 80),
                InputSource::new("EmitMermaidLineDeactivateLine", 70),
            ],
        )
        .with_bucket(
            "library-flows",
            85,
            "call_expr_id_path",
            "val",
            "dependency",
            ("downstream", "upstream"),
            vec![InputSource::new("EmitMermaidLineLibrarySignalLine", 85)],
        )
        .with_stream_shape(
            "solidity-to-mermaid",
            vec![
                "mock-actor-participants",
                "contract-participants",
                "library-participants",
                "mock-actor-request-flows",
                "mock-actor-return-flows",
                "intra-contract-flows",
                "inter-contract-flows",
                "library-flows",
            ],
            "sequenceDiagram",
        )
}

fn create_dependency_driven_hydrator_config() -> BucketConfig {
    BucketConfig::new()
        .with_default_value_attribute("val")
        .with_bucket(
            "inter-contract-flows",
            100,
            "path",
            "val",
            "ce_id_dependency",
            ("caller_contract_downstream", "callee_contract_upstream"),
            vec![InputSource::new("EmitMermaidLineSignalLine", 100)],
        )
        .with_stream(
            "solidity-to-mermaid",
            vec!["inter-contract-flows"],
            StreamFactOrderingStrategy::Dependency,
        )
}
