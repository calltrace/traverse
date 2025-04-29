use anyhow::{bail, Context, Result};
use clap::{Parser, ValueEnum};
use graph::cg::{CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorPipeline};
use graph::cg_dot::CgToDot;
use graph::cg_mermaid::{MermaidGenerator, ToSequenceDiagram};
use graph::parser::parse_solidity;
use graph::steps::{CallsHandling, ContractHandling};
use language::{Language, Solidity};
use mermaid::sequence_diagram_writer;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::io::{stdout, Write};
use std::path::PathBuf;
use thiserror::Error;
use walkdir::WalkDir;

#[derive(Parser, Debug)]
#[command(author, version, about = "Converts Solidity code to a Call Graph (DOT or Mermaid)", long_about = None)]
struct Cli {
    /// Input Solidity file(s) (.sol) or directories containing them.
    #[arg(required = true, num_args = 1..)]
    input_paths: Vec<PathBuf>,

    /// Output file path. If not specified, output goes to stdout.
    #[arg(short, long)]
    output_file: Option<PathBuf>,

    /// Output format.
    #[arg(short, long, value_parser = clap::value_parser!(OutputFormat), default_value_t = OutputFormat::Dot)]
    format: OutputFormat,
    
    /// Disable specific pipeline steps (comma-separated list)
    /// Available steps: Contract-Handling, Calls-Handling
    #[arg(long)]
    disable_steps: Option<String>,
    
    /// Enable specific pipeline steps (comma-separated list)
    /// Available steps: Contract-Handling, Calls-Handling
    #[arg(long)]
    enable_steps: Option<String>,
    
    /// Configuration parameters for pipeline steps (format: key=value,key2=value2)
    #[arg(long)]
    config: Option<String>,

    /// [DOT format only] Exclude nodes that have no incoming or outgoing edges.
    #[arg(long, requires = "format_dot")] // Only relevant for DOT format
    exclude_isolated_nodes: bool,
}

// Helper function to get the value "dot" for the requires condition
fn format_dot() -> OutputFormat {
    OutputFormat::Dot
}


#[derive(Debug, Clone, PartialEq, Eq, clap::ValueEnum)]
enum OutputFormat {
    Dot,
    Mermaid,
}

impl std::fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::Dot => write!(f, "dot"),
            OutputFormat::Mermaid => write!(f, "mermaid"),
        }
    }
}

#[derive(Debug)]
enum Sol2CgError {
    NoSolidityFiles,
    LanguageInitializationError,
    IoError(PathBuf, std::io::Error),
    WalkDirError(walkdir::Error),
    OutputWriteError(PathBuf, std::io::Error),
    StdoutWriteError(std::io::Error),
    MermaidWriteError(String),
}

impl std::error::Error for Sol2CgError {}

impl fmt::Display for Sol2CgError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sol2CgError::NoSolidityFiles => {
                write!(f, "No valid .sol files found in the provided paths.")
            }
            Sol2CgError::LanguageInitializationError => {
                write!(f, "Failed to get Tree-sitter language for Solidity.")
            }
            Sol2CgError::IoError(path, err) => {
                write!(f, "I/O error processing path '{}': {}", path.display(), err)
            }
            Sol2CgError::WalkDirError(err) => write!(f, "Directory traversal error: {}", err),
            Sol2CgError::OutputWriteError(path, err) => write!(
                f,
                "Failed to write output to file '{}': {}",
                path.display(),
                err
            ),
            Sol2CgError::StdoutWriteError(err) => {
                write!(f, "Failed to write output to stdout: {}", err)
            }
            Sol2CgError::MermaidWriteError(err) => {
                write!(f, "Mermaid serialization error: {}", err)
            }
        }
    }
}

impl From<walkdir::Error> for Sol2CgError {
    fn from(err: walkdir::Error) -> Self {
        Sol2CgError::WalkDirError(err)
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // 1. Find all .sol files
    let sol_files = find_solidity_files(&cli.input_paths)?;
    if sol_files.is_empty() {
        bail!(Sol2CgError::NoSolidityFiles);
    }

    // 2. Read and Concatenate all Solidity files
    let mut combined_source = String::new();
    let mut read_errors = Vec::new();

    for sol_file in &sol_files {
        match fs::read_to_string(sol_file) {
            Ok(source) => {
                combined_source.push_str(&source);
                combined_source.push('\n'); // Add newline between files
            }
            Err(e) => {
                let error_msg = format!(
                    "Failed to read file {}: {}",
                    sol_file.display(),
                    Sol2CgError::IoError(sol_file.clone(), e)
                );
                // Error message is stored, but not printed to stderr here.
                read_errors.push(error_msg);
            }
        }
    }

    if !read_errors.is_empty() {
        // Consider logging these errors differently or including them in the final Result error.
        // For now, just bail out as before, but without the eprintln.
        bail!("Errors occurred during file reading.");
    }

    if combined_source.is_empty() {
        bail!("No Solidity source code was successfully read.");
    }

    // 3. Parse the combined source code
    let combined_ast =
        parse_solidity(&combined_source).context("Failed to parse combined Solidity source")?;

    let solidity_lang = Solidity.get_tree_sitter_language();
    
    // Create the pipeline input
    let input = CallGraphGeneratorInput {
        source: combined_source.to_string(),
        tree: combined_ast.tree,
        solidity_lang,
    };
    
    // Create the pipeline context
    let mut ctx = CallGraphGeneratorContext {
        state_var_types: HashMap::new(),
        definition_nodes_info: Vec::new(),
        all_contracts: HashMap::new(),
        contracts_with_explicit_constructors: HashSet::new(),
        using_for_directives: HashMap::new(),
        all_interfaces: HashMap::new(), 
        interface_functions: HashMap::new(),
        contract_implements: HashMap::new(),
        interface_inherits: HashMap::new(), 
        all_libraries: HashMap::new(),
    };
    
    // Create the call graph
    let mut graph = CallGraph::new();
    eprintln!("[Address Debug sol2cg] Graph created at: {:p}", &graph); // Added address log
    
    // Parse configuration parameters
    let config = parse_config_params(cli.config.as_deref());
    
    // Create and configure the pipeline
    let mut pipeline = CallGraphGeneratorPipeline::new();
    
    // Add default steps
    pipeline.add_step(Box::new(ContractHandling::default()));
    pipeline.add_step(Box::new(CallsHandling::default()));
    
    // Process step enabling/disabling
    if let Some(disable_steps) = cli.disable_steps.as_deref() {
        for step_name in disable_steps.split(',').map(|s| s.trim()) {
            if !step_name.is_empty() {
                pipeline.disable_step(step_name);
            }
        }
    }
    
    if let Some(enable_steps) = cli.enable_steps.as_deref() {
        for step_name in enable_steps.split(',').map(|s| s.trim()) {
            if !step_name.is_empty() {
                pipeline.enable_step(step_name);
            }
        }
    }
    
    // Run the pipeline
    eprintln!("[Address Debug sol2cg] Before pipeline.run: {:p}", &graph); // Added address log
    pipeline.run(input.clone(), &mut ctx, &mut graph, &config)
        .context("Failed to generate call graph with pipeline")?;
    eprintln!("[Address Debug sol2cg] After pipeline.run: {:p}", &graph); // Added address log
    
    // 5. Add Explicit Return Edges
    eprintln!("[Address Debug sol2cg] Before add_explicit_return_edges: {:p}", &graph); // Added address log
    graph
        .add_explicit_return_edges(&input, &ctx) // Pass context ctx instead of tree
        .context("Failed to add explicit return edges")?;
    eprintln!("[Address Debug sol2cg] After add_explicit_return_edges: {:p}", &graph); // Added address log

    // 6. Serialize Graph

    // --- DEBUG: Log edge counts before serialization ---
    let total_edges = graph.edges.len();
    let call_edges_count = graph.edges.iter().filter(|e| e.edge_type == graph::cg::EdgeType::Call).count();
    let return_edges_count = graph.edges.iter().filter(|e| e.edge_type == graph::cg::EdgeType::Return).count();
    eprintln!("[DEBUG sol2cg] Before serialization: Total Edges = {}, Call Edges = {}, Return Edges = {}", total_edges, call_edges_count, return_edges_count);
    // --- END DEBUG ---

    let output_string = match cli.format {
        OutputFormat::Dot => {
            // Create DotExportConfig based on CLI flag
            let dot_config = graph::cg_dot::DotExportConfig {
                exclude_isolated_nodes: cli.exclude_isolated_nodes,
            };
            graph.to_dot("Solidity Call Graph", &dot_config) // Pass config
        }
        OutputFormat::Mermaid => {
            let generator = MermaidGenerator::new();
            eprintln!("[Address Debug sol2cg] Before to_sequence_diagram: {:p}", &graph); // Added address log
            let sequence_diagram = generator.to_sequence_diagram(&graph);
            // Use the write_diagram function directly
            sequence_diagram_writer::write_diagram(&sequence_diagram)
        }
    };

    // 6. Write Output
    match cli.output_file {
        Some(ref path) => {
            let mut file = fs::File::create(path)
                .map_err(|e| Sol2CgError::OutputWriteError(path.clone(), e))?;
            file.write_all(output_string.as_bytes())
                .map_err(|e| Sol2CgError::OutputWriteError(path.clone(), e))?;
        }
        None => {
            let mut handle = stdout().lock();
            handle
                .write_all(output_string.as_bytes())
                .map_err(Sol2CgError::StdoutWriteError)?;
            handle.flush().map_err(Sol2CgError::StdoutWriteError)?; // Ensure output is flushed
        }
    }

    Ok(())
}

/// Parse configuration parameters from a string in the format "key=value,key2=value2"
fn parse_config_params(config_str: Option<&str>) -> HashMap<String, String> {
    let mut config = HashMap::new();
    
    if let Some(config_str) = config_str {
        for param in config_str.split(',') {
            if let Some((key, value)) = param.split_once('=') {
                let key = key.trim().to_string();
                let value = value.trim().to_string();
                if !key.is_empty() {
                    config.insert(key, value);
                }
            }
        }
    }
    
    config
}

/// Finds all files with the .sol extension in the given paths (files or directories).
fn find_solidity_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>, Sol2CgError> {
    let mut sol_files = Vec::new();
    for path in paths {
        if path.is_dir() {
            for entry in WalkDir::new(path).into_iter().filter_map(|e| e.ok()) {
                if entry.file_type().is_file()
                    && entry.path().extension().map_or(false, |ext| ext == "sol")
                {
                    sol_files.push(entry.path().to_path_buf());
                }
            }
        } else if path.is_file() && path.extension().map_or(false, |ext| ext == "sol") {
            sol_files.push(path.clone());
        } else if path.is_file() {
            // If it's a file but not .sol, issue a warning or ignore silently.
            // Let's ignore for now.
            // eprintln!("Warning: Skipping non-Solidity file: {}", path.display());
        } else {
            // Handle cases where the path doesn't exist or isn't a file/dir
            return Err(Sol2CgError::IoError(
                path.clone(),
                std::io::Error::new(std::io::ErrorKind::NotFound, "Path not found or invalid"),
            ));
        }
    }
    // Sort for deterministic processing (useful for the "first file" limitation)
    sol_files.sort();
    Ok(sol_files)
}
