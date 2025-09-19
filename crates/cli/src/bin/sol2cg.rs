use anyhow::{bail, Context, Result};
use clap::Parser;
use tracing::warn;
use traverse_graph::cg::{
    CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorPipeline,
};
use traverse_graph::cg_dot::CgToDot;
use traverse_graph::cg_mermaid::{MermaidGenerator, ToSequenceDiagram};
use traverse_graph::interface_resolver::BindingRegistry;
use traverse_graph::manifest::{
    find_solidity_files_for_manifest, Manifest,
    ManifestEntry,
}; // Added ManifestEntry
use traverse_graph::natspec::extract::extract_source_comments; // Added
use traverse_graph::parser::parse_solidity;
use traverse_graph::steps::{CallsHandling, ContractHandling};
use traverse_graph::parser::get_solidity_language;
use traverse_mermaid::sequence_diagram_writer;
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::{stdout, Write};
use std::path::{Path, PathBuf};
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
    #[arg(long, requires_if("dot", "format"))] // Only relevant for DOT format
    exclude_isolated_nodes: bool,

    /// Optional path to the binding.yaml file for interface resolution.
    #[arg(long)]
    bindings: Option<PathBuf>,

    /// Optional path to a pre-generated manifest.yaml file.
    #[arg(long)]
    manifest_file: Option<PathBuf>,
}

// Helper function to get the value "dot" for the requires condition
fn _format_dot() -> OutputFormat {
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
#[allow(dead_code)]
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

    // Initialize logging
    traverse_logging::init_subscriber(false);

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

    let solidity_lang = get_solidity_language();

    // Create the pipeline input
    let input = CallGraphGeneratorInput {
        source: combined_source.to_string(),
        tree: combined_ast.tree,
        solidity_lang,
    };

    // Create the pipeline context
    let mut ctx = CallGraphGeneratorContext::default();

    // --- Manifest Generation and Binding Loading ---
    let project_root = fs::canonicalize(
        sol_files
            .first()
            .map(|p| p.parent().unwrap_or_else(|| Path::new(".")))
            .unwrap_or_else(|| Path::new(".")),
    )
    .context("Failed to determine project root for manifest generation")?; // Basic project root heuristic

    warn!(
        "[sol2cg] Using project root for manifest/bindings: {}",
        project_root.display()
    );

    // --- Manifest Handling ---
    let mut manifest_loaded_from_file = false;
    if let Some(manifest_path_arg) = &cli.manifest_file {
        let absolute_manifest_path = if manifest_path_arg.is_absolute() {
            manifest_path_arg.clone()
        } else {
            project_root.join(manifest_path_arg)
        };
        warn!(
            "[sol2cg] Attempting to load manifest from: {}",
            absolute_manifest_path.display()
        );
        // Use traverse_graph::manifest::load_manifest directly
        match traverse_graph::manifest::load_manifest(&absolute_manifest_path) {
            Ok(loaded_manifest) => {
                warn!(
                    "[sol2cg] Manifest loaded successfully from file with {} entries.",
                    loaded_manifest.entries.len()
                );
                ctx.manifest = Some(loaded_manifest);
                manifest_loaded_from_file = true;
            }
            Err(e) => {
                warn!(
                    "Warning: Failed to load manifest from {}: {}. Will attempt to generate from source files.",
                    absolute_manifest_path.display(),
                    e
                );
                // ctx.manifest remains None, allowing fallback
            }
        }
    }

    if !manifest_loaded_from_file {
        warn!("[sol2cg] Generating manifest in-memory from source files.");
        let mut manifest_in_memory = Manifest::default();
        let sol_files_relative_for_manifest =
            find_solidity_files_for_manifest(&cli.input_paths, &project_root).context(
                "Failed to find Solidity files relative to project root for manifest generation",
            )?;

        if sol_files_relative_for_manifest.is_empty() {
            warn!("[sol2cg] No Solidity files found for in-memory manifest generation. Proceeding without manifest.");
            // ctx.manifest remains None
        } else {
            for relative_file_path in &sol_files_relative_for_manifest {
                let full_file_path = project_root.join(relative_file_path);
                match fs::read_to_string(&full_file_path) {
                    Ok(source_content) => match extract_source_comments(&source_content) {
                        Ok(source_comments) => {
                            let entries: Vec<ManifestEntry> = source_comments
                                .into_iter()
                                .map(|sc| ManifestEntry::from((sc, relative_file_path.clone())))
                                .collect();
                            manifest_in_memory.extend_entries(entries);
                        }
                        Err(e) => {
                            warn!(
                                    "Warning: Failed to extract comments from {}: {}. Skipping file for manifest.",
                                    relative_file_path.display(),
                                    e
                                );
                        }
                    },
                    Err(e) => {
                        warn!(
                            "Warning: Failed to read file {} for manifest generation: {}",
                            full_file_path.display(),
                            e
                        );
                    }
                }
            }
            warn!(
                "[sol2cg] In-memory manifest generated with {} entries.",
                manifest_in_memory.entries.len()
            );
            if !manifest_in_memory.entries.is_empty() {
                ctx.manifest = Some(manifest_in_memory);
            } else {
                warn!("[sol2cg] In-memory manifest generated, but it is empty. Proceeding without manifest in context.");
                // ctx.manifest remains None
            }
        }
    }
    // --- End Manifest Handling ---

    // --- Binding Registry Handling ---
    let mut binding_registry_option: Option<BindingRegistry> = None;

    if let Some(bindings_path) = &cli.bindings {
        let absolute_bindings_path = if bindings_path.is_absolute() {
            bindings_path.clone()
        } else {
            project_root.join(bindings_path) // project_root must be defined before this block
        };
        warn!(
            "[sol2cg] Attempting to load bindings from file: {}",
            absolute_bindings_path.display()
        );
        match BindingRegistry::load(&absolute_bindings_path) {
            Ok(registry) => {
                warn!(
                    "[sol2cg] BindingRegistry loaded successfully from file with {} keys.",
                    registry.bindings.len()
                );
                binding_registry_option = Some(registry);
            }
            Err(e) => {
                warn!(
                    "Warning: Failed to load bindings from file {}: {}. Proceeding with an empty or Natspec-derived registry.",
                    absolute_bindings_path.display(),
                    e
                );
                // binding_registry_option remains None, will be defaulted if still None later
            }
        }
    }

    // Ensure we have a BindingRegistry instance to work with, default to empty if not loaded from file.
    if binding_registry_option.is_none() {
        warn!("[sol2cg] No bindings file loaded or specified. Initializing a default BindingRegistry.");
        binding_registry_option = Some(BindingRegistry::default());
    }

    // Populate/enrich the registry from manifest Natspec (specifically from concrete contracts)
    // This happens regardless of whether bindings were loaded from a file or started as default.
    if let Some(registry) = binding_registry_option.as_mut() {
        if let Some(ref manifest_content) = ctx.manifest {
            warn!("[sol2cg] Populating BindingRegistry from manifest Natspec (processing @custom:binds-to on concrete contracts)...");
            let initial_keys_count = registry.bindings.len();
            let initial_concrete_bindings = registry.bindings.values().filter(|bc| bc.contract_name.is_some()).count();
            
            registry.populate_from_manifest(manifest_content);
            
            let final_keys_count = registry.bindings.len();
            let final_concrete_bindings = registry.bindings.values().filter(|bc| bc.contract_name.is_some()).count();
            
            warn!(
                "[sol2cg] BindingRegistry population from manifest complete. Keys: {} -> {}. Concrete contract bindings: {} -> {}.",
                initial_keys_count, final_keys_count, initial_concrete_bindings, final_concrete_bindings
            );
        } else {
            warn!("[sol2cg] Manifest not available, skipping Natspec-based population for BindingRegistry.");
        }
    }
    
    ctx.binding_registry = binding_registry_option;
    // --- End Binding Registry Handling ---

    // Create the call graph
    let mut graph = CallGraph::new();
    warn!("[Address Debug sol2cg] Graph created at: {:p}", &graph); // Added address log

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
    warn!("[Address Debug sol2cg] Before pipeline.run: {:p}", &graph); // Added address log
    pipeline
        .run(input.clone(), &mut ctx, &mut graph, &config)
        .context("Failed to generate call graph with pipeline")?;
    warn!("[Address Debug sol2cg] After pipeline.run: {:p}", &graph); // Added address log

    // 5. Add Explicit Return Edges
    warn!(
        "[Address Debug sol2cg] Before add_explicit_return_edges: {:p}",
        &graph
    ); // Added address log
    graph
        .add_explicit_return_edges(&input, &ctx) // Pass context ctx instead of tree
        .context("Failed to add explicit return edges")?;
    warn!(
        "[Address Debug sol2cg] After add_explicit_return_edges: {:p}",
        &graph
    ); // Added address log

    // 6. Serialize Graph

    // --- DEBUG: Log edge counts before serialization ---
    let total_edges = graph.edges.len();
    let call_edges_count = graph
        .edges
        .iter()
        .filter(|e| e.edge_type == traverse_graph::cg::EdgeType::Call)
        .count();
    let return_edges_count = graph
        .edges
        .iter()
        .filter(|e| e.edge_type == traverse_graph::cg::EdgeType::Return)
        .count();
    warn!(
        "[DEBUG sol2cg] Before serialization: Total Edges = {}, Call Edges = {}, Return Edges = {}",
        total_edges, call_edges_count, return_edges_count
    );
    // --- END DEBUG ---

    let output_string = match cli.format {
        OutputFormat::Dot => {
            // Create DotExportConfig based on CLI flag
            let dot_config = traverse_graph::cg_dot::DotExportConfig {
                exclude_isolated_nodes: cli.exclude_isolated_nodes,
            };
            graph.to_dot("Solidity Call Graph", &dot_config) // Pass config
        }
        OutputFormat::Mermaid => {
            let generator = MermaidGenerator::new();
            warn!(
                "[Address Debug sol2cg] Before to_sequence_diagram: {:p}",
                &graph
            ); // Added address log
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
                    && entry.path().extension().is_some_and(|ext| ext == "sol")
                {
                    sol_files.push(entry.path().to_path_buf());
                }
            }
        } else if path.is_file() && path.extension().is_some_and(|ext| ext == "sol") {
            sol_files.push(path.clone());
        } else if path.is_file() {
            // If it's a file but not .sol, issue a warning or ignore silently.
            // Let's ignore for now.
            // warn!("Warning: Skipping non-Solidity file: {}", path.display());
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
