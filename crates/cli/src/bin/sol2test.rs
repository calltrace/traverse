// crates/cli/src/bin/sol2test.rs
//
// This file contains the main entry point and logic for the `sol2test` command-line tool.
// `sol2test` is designed to generate Foundry tests for Solidity smart contracts.
// It acts as a wrapper around the core code generation module, handling command-line
// argument parsing, file system interactions, and orchestrating the test generation process.
// It parses Solidity files, constructs a call graph, and then uses this graph
// along with configurable templates or native Foundry integration to produce test files.
use anyhow::{bail, Context, Result};
use clap::{Parser, ValueEnum};
use codegen::generate_tests_with_foundry;
use graph::cg::{
    CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorPipeline,
};
use graph::interface_resolver::{BindingConfig, BindingRegistry};
use graph::manifest::{
    find_solidity_files_for_manifest, generate_manifest as generate_manifest_to_file, Manifest,
    ManifestEntry,
};
use graph::natspec::extract::extract_source_comments;
use graph::natspec::{parse_natspec_comment, NatSpecKind};
use graph::parser::parse_solidity;
use graph::steps::{CallsHandling, ContractHandling};
use language::{Language, Solidity};
use serde_json;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::io::{stdout, Write};
use std::path::{Path, PathBuf};
use tera::{Context as TeraContext, Tera};
use thiserror::Error;
use toml;
use walkdir::WalkDir;

#[derive(Parser, Debug)]
#[command(author, version, about = "Solidity test generator with native Foundry integration", long_about = None)]
struct Cli {
    #[arg(required = false, num_args = 1..)]
    input_paths: Vec<PathBuf>,
    #[arg(
        long,
        help = "Process a Foundry project directory instead of individual files"
    )]
    project: Option<PathBuf>,
    #[arg(short, long, default_value = "foundry-tests/test")]
    output_dir: PathBuf,
    #[arg(short, long, default_value = "templates")]
    template_dir: PathBuf,
    #[arg(short, long)]
    verbose: bool,
    #[arg(long, default_value = "true")]
    use_foundry: bool,
    #[arg(long, default_value = "true")]
    validate_compilation: bool,
    #[arg(long)]
    deployer_only: bool,
    #[arg(long)]
    disable_steps: Option<String>,
    #[arg(long)]
    enable_steps: Option<String>,
    #[arg(long)]
    config: Option<String>,
    #[arg(long)]
    bindings: Option<PathBuf>,
    #[arg(long)]
    manifest_file: Option<PathBuf>,
    #[arg(long)]
    foundry_root: Option<PathBuf>,
}

#[derive(Debug)]
enum Sol2TestError {
    NoSolidityFiles,
    IoError(PathBuf, std::io::Error),
    WalkDirError(walkdir::Error),
    CodeGenError(codegen::CodeGenError),
    ForgeError(String),
    ConfigError(String),
}

impl std::error::Error for Sol2TestError {}

impl fmt::Display for Sol2TestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sol2TestError::NoSolidityFiles => {
                write!(f, "No valid .sol files found in the provided paths.")
            }
            Sol2TestError::IoError(path, err) => {
                write!(f, "I/O error processing path '{}': {}", path.display(), err)
            }
            Sol2TestError::WalkDirError(err) => write!(f, "Directory traversal error: {}", err),
            Sol2TestError::CodeGenError(err) => write!(f, "Code generation error: {}", err),
            Sol2TestError::ForgeError(msg) => write!(f, "Forge command error: {}", msg),
            Sol2TestError::ConfigError(msg) => write!(f, "Configuration error: {}", msg),
        }
    }
}

impl From<walkdir::Error> for Sol2TestError {
    fn from(err: walkdir::Error) -> Self {
        Sol2TestError::WalkDirError(err)
    }
}

#[derive(Debug, Clone)]
struct FoundryConfig {
    src: String,
    out: String,
    test: String,
    libs: Vec<String>,
    remappings: Vec<String>,
    solc_version: Option<String>,
    evm_version: Option<String>,
}

impl Default for FoundryConfig {
    fn default() -> Self {
        Self {
            src: "src".to_string(),
            out: "out".to_string(),
            test: "test".to_string(),
            libs: vec!["lib".to_string()],
            remappings: Vec::new(),
            solc_version: None,
            evm_version: None,
        }
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    if cli.verbose {
        println!("🚀 Starting sol2test with Foundry integration");
        println!(
            "Mode: {}",
            if cli.use_foundry {
                "Native Foundry"
            } else {
                "Template-based"
            }
        );
    }

    // Validate command-line arguments
    match (&cli.project, &cli.input_paths.is_empty()) {
        (Some(_), false) => {
            bail!("Cannot specify both --project and input file paths. Use either --project for Foundry project mode or provide individual .sol files.");
        }
        (None, true) => {
            bail!("Must specify either --project <directory> or provide input .sol file paths.");
        }
        _ => {} // Valid combinations
    }

    let (sol_files, combined_source, foundry_config) = if let Some(project_path) = &cli.project {
        // Project mode: validate, discover, and flatten contracts
        validate_foundry_project(project_path)?;
        let config = load_foundry_config(project_path, cli.verbose)?;
        let discovered_files =
            discover_project_contracts_with_config(project_path, &config, cli.verbose)?;
        let flattened_source = flatten_project_contracts_with_config(
            project_path,
            &discovered_files,
            &config,
            cli.verbose,
        )?;
        (discovered_files, flattened_source, Some(config))
    } else {
        // Single file mode: existing behavior
        let files = find_solidity_files(&cli.input_paths)?;
        let source = read_and_combine_files(&files)?;
        (files, source, None)
    };

    if sol_files.is_empty() {
        bail!(Sol2TestError::NoSolidityFiles);
    }

    if combined_source.is_empty() {
        bail!("No Solidity source code was successfully processed.");
    }

    let combined_ast =
        parse_solidity(&combined_source).context("Failed to parse combined Solidity source")?;

    let solidity_lang = Solidity.get_tree_sitter_language();

    let input = CallGraphGeneratorInput {
        source: combined_source.to_string(),
        tree: combined_ast.tree,
        solidity_lang,
    };

    let (graph, ctx) = generate_call_graph(&input, &cli)?;

    if cli.verbose {
        println!(
            "📊 Call graph generated with {} nodes and {} edges",
            graph.nodes.len(),
            graph.edges.len()
        );
    }

    fs::create_dir_all(&cli.output_dir)
        .map_err(|e| Sol2TestError::IoError(cli.output_dir.clone(), e))?;

    if cli.verbose {
        println!("📂 Created output directory: {}", cli.output_dir.display());
    }

    // Construct original_contract_paths
    let project_root_for_paths = fs::canonicalize(
        cli.input_paths
            .first()
            .map(|p| p.parent().unwrap_or_else(|| Path::new(".")))
            .unwrap_or_else(|| Path::new(".")),
    )
    .context("Failed to determine project root for original_contract_paths in main")?;

    let mut original_contract_paths: HashMap<String, PathBuf> = HashMap::new();

    // First, try to populate from manifest
    if let Some(manifest) = &ctx.manifest {
        for entry in &manifest.entries {
            if entry.item_kind == graph::natspec::extract::SourceItemKind::Contract {
                if let Some(contract_name) = &entry.item_name {
                    let absolute_path = project_root_for_paths.join(&entry.file_path);
                    original_contract_paths.insert(contract_name.clone(), absolute_path);
                    if cli.verbose {
                        println!(
                            "🗺️ Mapping contract '{}' to original path: {}",
                            contract_name,
                            original_contract_paths
                                .get(contract_name)
                                .unwrap()
                                .display()
                        );
                    }
                }
            }
        }
    }

    // If manifest didn't provide contract paths, fall back to direct file mapping
    if original_contract_paths.is_empty() {
        for sol_file in &sol_files {
            if let Some(file_stem) = sol_file.file_stem().and_then(|s| s.to_str()) {
                let absolute_path =
                    fs::canonicalize(sol_file).context("Failed to canonicalize input file path")?;
                original_contract_paths.insert(file_stem.to_string(), absolute_path);
                if cli.verbose {
                    println!(
                        "🗺️ Direct mapping contract '{}' to original path: {}",
                        file_stem,
                        original_contract_paths.get(file_stem).unwrap().display()
                    );
                }
            }
        }
    }

    if cli.verbose && original_contract_paths.is_empty() {
        println!("🗺️ No contract paths could be determined for copying original sources.");
    }

    generate_tests_with_foundry_enhanced(
        &graph,
        &ctx, // Pass &ctx instead of &input
        cli.verbose,
        &cli.output_dir,
        cli.foundry_root.clone(), // Clone Option<PathBuf>
        cli.deployer_only,
        cli.validate_compilation,
        &original_contract_paths, // Pass the newly constructed map
        foundry_config.as_ref(),  // Pass Foundry configuration
    )?;
    if cli.verbose {
        println!("✅ Test generation completed successfully!");
    }

    Ok(())
}

fn generate_call_graph(
    input: &CallGraphGeneratorInput,
    cli: &Cli,
) -> Result<(CallGraph, CallGraphGeneratorContext)> {
    let mut ctx = CallGraphGeneratorContext::default();

    setup_manifest_and_bindings(&mut ctx, cli)?;

    let mut pipeline = CallGraphGeneratorPipeline::new();
    pipeline.add_step(Box::new(ContractHandling::default()));
    pipeline.add_step(Box::new(CallsHandling::default()));

    configure_pipeline_steps(&mut pipeline, cli);

    let config = parse_config_params(cli.config.as_deref());

    let mut graph = CallGraph::new();
    pipeline
        .run(input.clone(), &mut ctx, &mut graph, &config)
        .context("Failed to generate call graph with pipeline")?;

    graph
        .add_explicit_return_edges(input, &ctx)
        .context("Failed to add explicit return edges")?;

    Ok((graph, ctx))
}

fn setup_manifest_and_bindings(ctx: &mut CallGraphGeneratorContext, cli: &Cli) -> Result<()> {
    let project_root = fs::canonicalize(
        cli.input_paths
            .first()
            .map(|p| p.parent().unwrap_or_else(|| Path::new(".")))
            .unwrap_or_else(|| Path::new(".")),
    )
    .context("Failed to determine project root for manifest generation")?;

    if cli.verbose {
        println!("📍 Using project root: {}", project_root.display());
    }

    let mut manifest_loaded_from_file = false;
    if let Some(manifest_path_arg) = &cli.manifest_file {
        let absolute_manifest_path = if manifest_path_arg.is_absolute() {
            manifest_path_arg.clone()
        } else {
            project_root.join(manifest_path_arg)
        };

        if cli.verbose {
            println!(
                "📋 Attempting to load manifest from: {}",
                absolute_manifest_path.display()
            );
        }

        match graph::manifest::load_manifest(&absolute_manifest_path) {
            Ok(loaded_manifest) => {
                if cli.verbose {
                    println!(
                        "✅ Manifest loaded successfully with {} entries.",
                        loaded_manifest.entries.len()
                    );
                }
                ctx.manifest = Some(loaded_manifest);
                manifest_loaded_from_file = true;
            }
            Err(e) => {
                eprintln!(
                    "⚠️  Warning: Failed to load manifest from {}: {}. Will attempt to generate from source files.",
                    absolute_manifest_path.display(),
                    e
                );
            }
        }
    }

    if !manifest_loaded_from_file {
        if cli.verbose {
            println!("🔧 Generating manifest in-memory from source files.");
        }
        generate_in_memory_manifest(ctx, &cli.input_paths, &project_root, cli.verbose)?;
    }

    setup_binding_registry(ctx, &cli.bindings, &project_root, cli.verbose)?;

    Ok(())
}

fn generate_in_memory_manifest(
    ctx: &mut CallGraphGeneratorContext,
    input_paths: &[PathBuf],
    project_root: &Path,
    verbose: bool,
) -> Result<()> {
    let mut manifest_in_memory = Manifest::default();
    let sol_files_relative_for_manifest =
        find_solidity_files_for_manifest(input_paths, project_root).context(
            "Failed to find Solidity files relative to project root for manifest generation",
        )?;

    if !sol_files_relative_for_manifest.is_empty() {
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
                        if verbose {
                            eprintln!(
                                "⚠️  Warning: Failed to extract comments from {}: {}. Skipping file for manifest.",
                                relative_file_path.display(),
                                e
                            );
                        }
                    }
                },
                Err(e) => {
                    if verbose {
                        eprintln!(
                            "⚠️  Warning: Failed to read file {} for manifest generation: {}",
                            full_file_path.display(),
                            e
                        );
                    }
                }
            }
        }
        if verbose {
            println!(
                "📋 In-memory manifest generated with {} entries.",
                manifest_in_memory.entries.len()
            );
        }
        if !manifest_in_memory.entries.is_empty() {
            ctx.manifest = Some(manifest_in_memory);
        }
    }
    Ok(())
}

fn setup_binding_registry(
    ctx: &mut CallGraphGeneratorContext,
    bindings_path: &Option<PathBuf>,
    project_root: &Path,
    verbose: bool,
) -> Result<()> {
    let mut binding_registry_option: Option<BindingRegistry> = None;

    if let Some(bindings_path) = bindings_path {
        let absolute_bindings_path = if bindings_path.is_absolute() {
            bindings_path.clone()
        } else {
            project_root.join(bindings_path)
        };
        if verbose {
            println!(
                "🔗 Attempting to load bindings from file: {}",
                absolute_bindings_path.display()
            );
        }
        match BindingRegistry::load(&absolute_bindings_path) {
            Ok(registry) => {
                if verbose {
                    println!(
                        "✅ BindingRegistry loaded successfully with {} keys.",
                        registry.bindings.len()
                    );
                }
                binding_registry_option = Some(registry);
            }
            Err(e) => {
                if verbose {
                    eprintln!(
                        "⚠️  Warning: Failed to load bindings from file {}: {}. Proceeding with default registry.",
                        absolute_bindings_path.display(),
                        e
                    );
                }
            }
        }
    }

    if binding_registry_option.is_none() {
        if verbose {
            println!("🔗 Initializing default BindingRegistry.");
        }
        binding_registry_option = Some(BindingRegistry::default());
    }

    if let Some(registry) = binding_registry_option.as_mut() {
        if let Some(ref manifest_content) = ctx.manifest {
            if verbose {
                println!("🔗 Populating BindingRegistry from manifest Natspec...");
            }
            registry.populate_from_manifest(manifest_content);
        }
    }

    ctx.binding_registry = binding_registry_option;
    Ok(())
}

fn configure_pipeline_steps(pipeline: &mut CallGraphGeneratorPipeline, cli: &Cli) {
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
}

fn find_solidity_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>, Sol2TestError> {
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
        }
    }
    sol_files.sort();
    Ok(sol_files)
}

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

fn validate_foundry_project(project_path: &Path) -> Result<()> {
    if !project_path.is_dir() {
        bail!(
            "Project path '{}' is not a directory",
            project_path.display()
        );
    }

    let foundry_toml = project_path.join("foundry.toml");
    if !foundry_toml.exists() {
        bail!(
            "No foundry.toml found in project directory '{}'",
            project_path.display()
        );
    }

    let src_dir = project_path.join("src");
    if !src_dir.is_dir() {
        bail!(
            "No src/ directory found in project '{}'",
            project_path.display()
        );
    }

    Ok(())
}

fn load_foundry_config(project_path: &Path, verbose: bool) -> Result<FoundryConfig> {
    let foundry_toml_path = project_path.join("foundry.toml");

    if verbose {
        println!(
            "⚙️  Loading Foundry configuration from: {}",
            foundry_toml_path.display()
        );
    }

    let mut config = FoundryConfig::default();

    if foundry_toml_path.exists() {
        let toml_content =
            fs::read_to_string(&foundry_toml_path).context("Failed to read foundry.toml")?;

        let toml_value: toml::Value =
            toml::from_str(&toml_content).context("Failed to parse foundry.toml")?;

        // Extract configuration from [profile.default] section
        if let Some(profile) = toml_value.get("profile") {
            if let Some(default_profile) = profile.get("default") {
                if let Some(src) = default_profile.get("src").and_then(|v| v.as_str()) {
                    config.src = src.to_string();
                }
                if let Some(out) = default_profile.get("out").and_then(|v| v.as_str()) {
                    config.out = out.to_string();
                }
                if let Some(test) = default_profile.get("test").and_then(|v| v.as_str()) {
                    config.test = test.to_string();
                }
                if let Some(libs) = default_profile.get("libs").and_then(|v| v.as_array()) {
                    config.libs = libs
                        .iter()
                        .filter_map(|v| v.as_str())
                        .map(|s| s.to_string())
                        .collect();
                }
                if let Some(remappings) =
                    default_profile.get("remappings").and_then(|v| v.as_array())
                {
                    config.remappings = remappings
                        .iter()
                        .filter_map(|v| v.as_str())
                        .map(|s| s.to_string())
                        .collect();
                }
                if let Some(solc_version) =
                    default_profile.get("solc_version").and_then(|v| v.as_str())
                {
                    config.solc_version = Some(solc_version.to_string());
                }
                if let Some(evm_version) =
                    default_profile.get("evm_version").and_then(|v| v.as_str())
                {
                    config.evm_version = Some(evm_version.to_string());
                }
            }
        }

        if verbose {
            println!("✅ Foundry configuration loaded:");
            println!("  📁 Source directory: {}", config.src);
            println!("  📁 Output directory: {}", config.out);
            println!("  🧪 Test directory: {}", config.test);
            println!("  📚 Libraries: {:?}", config.libs);
            if !config.remappings.is_empty() {
                println!("  🔗 Remappings: {:?}", config.remappings);
            }
            if let Some(ref version) = config.solc_version {
                println!("  🔧 Solc version: {}", version);
            }
            if let Some(ref evm_version) = config.evm_version {
                println!("  ⚡ EVM version: {}", evm_version);
            }
        }
    } else {
        if verbose {
            println!("⚙️  Using default Foundry configuration (no foundry.toml found)");
        }
    }

    Ok(config)
}

fn discover_project_contracts_with_config(
    project_path: &Path,
    config: &FoundryConfig,
    verbose: bool,
) -> Result<Vec<PathBuf>, Sol2TestError> {
    let src_dir = project_path.join(&config.src);

    if verbose {
        println!(
            "🔍 Discovering contracts in: {} (from foundry.toml)",
            src_dir.display()
        );
    }

    let mut sol_files = Vec::new();
    for entry in WalkDir::new(&src_dir).into_iter().filter_map(|e| e.ok()) {
        if entry.file_type().is_file() && entry.path().extension().map_or(false, |ext| ext == "sol")
        {
            sol_files.push(entry.path().to_path_buf());
            if verbose {
                println!("  📄 Found: {}", entry.path().display());
            }
        }
    }

    sol_files.sort();

    if verbose {
        println!("📊 Discovered {} contract files", sol_files.len());
    }

    Ok(sol_files)
}

fn discover_project_contracts(
    project_path: &Path,
    verbose: bool,
) -> Result<Vec<PathBuf>, Sol2TestError> {
    let config = FoundryConfig::default();
    discover_project_contracts_with_config(project_path, &config, verbose)
}

fn flatten_project_contracts_with_config(
    project_path: &Path,
    sol_files: &[PathBuf],
    config: &FoundryConfig,
    verbose: bool,
) -> Result<String> {
    if verbose {
        println!("🔧 Flattening contracts using forge flatten with Foundry config...");
    }

    let mut combined_flattened = String::new();
    let mut flatten_errors = Vec::new();

    for sol_file in sol_files {
        // Make path relative to project root for forge flatten
        let relative_path = sol_file
            .strip_prefix(project_path)
            .context("Failed to make contract path relative to project root")?;

        if verbose {
            println!("  🔨 Flattening: {}", relative_path.display());
        }

        // Build forge flatten command with configuration
        let mut cmd = std::process::Command::new("forge");
        cmd.arg("flatten")
            .arg(relative_path)
            .current_dir(project_path);

        // Add remappings if specified
        for remapping in &config.remappings {
            cmd.arg("--remappings").arg(remapping);
        }

        // Execute the command
        let output = cmd
            .output()
            .context("Failed to execute forge flatten command")?;

        if !output.status.success() {
            let error_msg = format!(
                "forge flatten failed for {}: {}",
                relative_path.display(),
                String::from_utf8_lossy(&output.stderr)
            );
            flatten_errors.push(error_msg);
            continue;
        }

        let flattened_content =
            String::from_utf8(output.stdout).context("forge flatten output is not valid UTF-8")?;

        if verbose {
            println!(
                "    ✅ Flattened {} lines",
                flattened_content.lines().count()
            );
        }

        combined_flattened.push_str(&flattened_content);
        combined_flattened.push('\n');
    }

    if !flatten_errors.is_empty() {
        bail!(
            "Errors occurred during contract flattening: {}",
            flatten_errors.join("; ")
        );
    }

    if verbose {
        println!(
            "🎯 Combined flattened source: {} lines total",
            combined_flattened.lines().count()
        );
    }

    Ok(combined_flattened)
}

fn flatten_project_contracts(
    project_path: &Path,
    sol_files: &[PathBuf],
    verbose: bool,
) -> Result<String> {
    let config = FoundryConfig::default();
    flatten_project_contracts_with_config(project_path, sol_files, &config, verbose)
}

fn read_and_combine_files(sol_files: &[PathBuf]) -> Result<String> {
    let mut combined_source = String::new();
    let mut read_errors = Vec::new();

    for sol_file in sol_files {
        match fs::read_to_string(sol_file) {
            Ok(source) => {
                combined_source.push_str(&source);
                combined_source.push('\n');
            }
            Err(e) => {
                let error_msg = format!(
                    "Failed to read file {}: {}",
                    sol_file.display(),
                    Sol2TestError::IoError(sol_file.clone(), e)
                );
                read_errors.push(error_msg);
            }
        }
    }

    if !read_errors.is_empty() {
        bail!(
            "Errors occurred during file reading: {}",
            read_errors.join("; ")
        );
    }

    Ok(combined_source)
}

fn generate_tests_with_foundry_enhanced(
    graph: &CallGraph,
    ctx: &CallGraphGeneratorContext,
    verbose: bool,
    output_dir: &Path,
    foundry_root: Option<PathBuf>,
    deployer_only: bool,
    validate_compilation: bool,
    original_contract_paths: &HashMap<String, PathBuf>,
    foundry_config: Option<&FoundryConfig>,
) -> Result<()> {
    if verbose {
        println!("🚀 Starting enhanced Foundry test generation...");
        if let Some(config) = foundry_config {
            println!("📋 Using Foundry configuration:");
            println!(
                "  📁 Test output will respect '{}' directory structure",
                config.test
            );
            println!("  🔧 Compiler settings will be inherited from foundry.toml");
        }
    }

    // Use the enhanced configuration-aware output directory
    let enhanced_output_dir = if let Some(config) = foundry_config {
        // Respect foundry.toml test directory configuration
        let foundry_tests_root = output_dir.join("foundry-tests");
        let test_dir = foundry_tests_root.join(&config.test);

        // Ensure the test directory exists
        fs::create_dir_all(&test_dir).context("Failed to create enhanced test directory")?;

        if verbose {
            println!("📂 Enhanced test directory: {}", test_dir.display());
        }

        test_dir
    } else {
        output_dir.to_path_buf()
    };

    // Call the original function with enhanced parameters
    generate_tests_with_foundry(
        graph,
        ctx,
        verbose,
        &enhanced_output_dir,
        foundry_root,
        deployer_only,
        validate_compilation,
        original_contract_paths,
    )
}
