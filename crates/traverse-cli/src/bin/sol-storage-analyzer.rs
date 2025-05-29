use anyhow::{bail, Context, Result};
use clap::Parser;
use graph::cg::{
    CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput, CallGraphGeneratorPipeline,
    Node
};
use graph::reachability::NodeId;
use graph::interface_resolver::BindingRegistry;
use graph::manifest::{find_solidity_files_for_manifest, Manifest, ManifestEntry};
use graph::natspec::extract::extract_source_comments;
use graph::parser::parse_solidity;
use graph::steps::{CallsHandling, ContractHandling};
use graph::storage_access::StorageAccessSummary; 
use language::{Language, Solidity};
use std::collections::HashMap;
use std::fs;
use std::io::{stdout, Write};
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[command(author, version, about = "Analyzes Solidity code and generates a markdown table of storage reads/writes for public/external functions.", long_about = None)]
struct Cli {
    #[arg(required = true, num_args = 1..)]
    input_paths: Vec<PathBuf>,

    #[arg(short, long)]
    output_file: Option<PathBuf>,

    #[arg(long)]
    bindings: Option<PathBuf>,

    #[arg(long)]
    manifest_file: Option<PathBuf>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let sol_files = find_solidity_files(&cli.input_paths)?;
    if sol_files.is_empty() {
        bail!("No valid .sol files found in the provided paths.");
    }

    let mut combined_source = String::new();
    let mut read_errors = Vec::new();

    for sol_file in &sol_files {
        match fs::read_to_string(sol_file) {
            Ok(source) => {
                combined_source.push_str(&source);
                combined_source.push('\n'); 
            }
            Err(e) => {
                read_errors.push(format!("Failed to read file {}: {}", sol_file.display(), e));
            }
        }
    }

    if !read_errors.is_empty() {
        bail!(
            "Errors occurred during file reading:\n{}",
            read_errors.join("\n")
        );
    }

    if combined_source.is_empty() {
        bail!("No Solidity source code was successfully read.");
    }

    let combined_ast =
        parse_solidity(&combined_source).context("Failed to parse combined Solidity source")?;

    let solidity_lang = Solidity.get_tree_sitter_language();

    let input = CallGraphGeneratorInput {
        source: combined_source.to_string(),
        tree: combined_ast.tree,
        solidity_lang,
    };

    let mut ctx = CallGraphGeneratorContext::default();

    let project_root = fs::canonicalize(
        sol_files
            .first()
            .map(|p| p.parent().unwrap_or_else(|| Path::new(".")))
            .unwrap_or_else(|| Path::new(".")),
    )
    .context("Failed to determine project root for manifest/bindings")?;

    eprintln!(
        "[sol-storage-analyzer] Using project root for manifest/bindings: {}",
        project_root.display()
    );

    let mut manifest_loaded_from_file = false;
    if let Some(manifest_path_arg) = &cli.manifest_file {
        let absolute_manifest_path = if manifest_path_arg.is_absolute() {
            manifest_path_arg.clone()
        } else {
            project_root.join(manifest_path_arg)
        };
        eprintln!(
            "[sol-storage-analyzer] Attempting to load manifest from: {}",
            absolute_manifest_path.display()
        );
        match graph::manifest::load_manifest(&absolute_manifest_path) {
            Ok(loaded_manifest) => {
                eprintln!(
                    "[sol-storage-analyzer] Manifest loaded successfully from file with {} entries.",
                    loaded_manifest.entries.len()
                );
                ctx.manifest = Some(loaded_manifest);
                manifest_loaded_from_file = true;
            }
            Err(e) => {
                eprintln!(
                    "Warning: Failed to load manifest from {}: {}. Will attempt to generate from source files.",
                    absolute_manifest_path.display(),
                    e
                );
            }
        }
    }

    if !manifest_loaded_from_file {
        eprintln!("[sol-storage-analyzer] Generating manifest in-memory from source files.");
        let mut manifest_in_memory = Manifest::default();
        let sol_files_relative_for_manifest =
            find_solidity_files_for_manifest(&cli.input_paths, &project_root).context(
                "Failed to find Solidity files relative to project root for manifest generation",
            )?;

        if sol_files_relative_for_manifest.is_empty() {
            eprintln!(
                "[sol-storage-analyzer] No Solidity files found for in-memory manifest generation."
            );
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
                            eprintln!(
                                "Warning: Failed to extract comments from {}: {}. Skipping file for manifest.",
                                relative_file_path.display(),
                                e
                            );
                        }
                    },
                    Err(e) => {
                        eprintln!(
                            "Warning: Failed to read file {} for manifest generation: {}",
                            full_file_path.display(),
                            e
                        );
                    }
                }
            }
            eprintln!(
                "[sol-storage-analyzer] In-memory manifest generated with {} entries.",
                manifest_in_memory.entries.len()
            );
            if !manifest_in_memory.entries.is_empty() {
                ctx.manifest = Some(manifest_in_memory);
            }
        }
    }

    let mut binding_registry_option: Option<BindingRegistry> = None;
    if let Some(bindings_path) = &cli.bindings {
        let absolute_bindings_path = if bindings_path.is_absolute() {
            bindings_path.clone()
        } else {
            project_root.join(bindings_path)
        };
        eprintln!(
            "[sol-storage-analyzer] Attempting to load bindings from file: {}",
            absolute_bindings_path.display()
        );
        match BindingRegistry::load(&absolute_bindings_path) {
            Ok(registry) => {
                eprintln!(
                    "[sol-storage-analyzer] BindingRegistry loaded successfully from file with {} keys.",
                    registry.bindings.len()
                );
                binding_registry_option = Some(registry);
            }
            Err(e) => {
                eprintln!(
                    "Warning: Failed to load bindings from file {}: {}. Proceeding with an empty or Natspec-derived registry.",
                    absolute_bindings_path.display(),
                    e
                );
            }
        }
    }
    if binding_registry_option.is_none() {
        eprintln!("[sol-storage-analyzer] No bindings file loaded or specified. Initializing a default BindingRegistry.");
        binding_registry_option = Some(BindingRegistry::default());
    }
    if let Some(registry) = binding_registry_option.as_mut() {
        if let Some(ref manifest_content) = ctx.manifest {
            eprintln!("[sol-storage-analyzer] Populating BindingRegistry from manifest Natspec...");
            registry.populate_from_manifest(manifest_content);
        }
    }
    ctx.binding_registry = binding_registry_option;

    let mut graph = CallGraph::new();

    let mut pipeline = CallGraphGeneratorPipeline::new();
    pipeline.add_step(Box::new(ContractHandling::default()));
    pipeline.add_step(Box::new(CallsHandling::default()));

    pipeline
        .run(input.clone(), &mut ctx, &mut graph, &HashMap::new()) 
        .context("Failed to generate call graph with pipeline")?;

    graph
        .add_explicit_return_edges(&input, &ctx)
        .context("Failed to add explicit return edges")?;

    let storage_summary_map = graph::storage_access::analyze_storage_access(&graph);

    let markdown_output = format_storage_summary_to_markdown(&graph, &storage_summary_map);

    match cli.output_file {
        Some(ref path) => {
            let mut file = fs::File::create(path)
                .with_context(|| format!("Failed to create output file: {}", path.display()))?;
            file.write_all(markdown_output.as_bytes())
                .with_context(|| format!("Failed to write to output file: {}", path.display()))?;
            eprintln!("Storage access summary written to: {}", path.display());
        }
        None => {
            let mut handle = stdout().lock();
            handle
                .write_all(markdown_output.as_bytes())
                .context("Failed to write output to stdout")?;
            handle.flush().context("Failed to flush stdout")?;
        }
    }

    Ok(())
}

fn find_solidity_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>> {
    let mut sol_files = Vec::new();
    for path in paths {
        if path.is_dir() {
            for entry in walkdir::WalkDir::new(path)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                if entry.file_type().is_file()
                    && entry.path().extension().map_or(false, |ext| ext == "sol")
                {
                    sol_files.push(entry.path().to_path_buf());
                }
            }
        } else if path.is_file() && path.extension().map_or(false, |ext| ext == "sol") {
            sol_files.push(path.clone());
        } else if path.is_file() {
            // Silently ignore non-Solidity files
        } else {
            bail!("Path not found or invalid: {}", path.display());
        }
    }
    sol_files.sort(); // For deterministic processing
    Ok(sol_files)
}

fn format_storage_summary_to_markdown(
    graph: &CallGraph,
    summary_map: &HashMap<NodeId, StorageAccessSummary>,
) -> String {
    let mut md = String::new();
    md.push_str("| Endpoint | Reads | Writes |\n");
    md.push_str("|----------|-------|--------|\n");

    let mut sorted_entries: Vec<(&NodeId, &StorageAccessSummary)> = summary_map.iter().collect();
    sorted_entries.sort_by_key(|(node_id, _)| {
        graph.nodes.get(**node_id).map_or_else(String::new, |n| {
            format!(
                "{}.{}",
                n.contract_name.as_deref().unwrap_or("Global"),
                n.name
            )
        })
    });

    for (func_node_id, summary) in sorted_entries {
        let func_node = graph
            .nodes
            .get(*func_node_id)
            .expect("Function node ID from summary_map not found in graph.nodes"); // Should not happen

        let endpoint_name = format!(
            "{}.{}",
            func_node.contract_name.as_deref().unwrap_or("Global"),
            func_node.name
        );

        let mut reads_vec: Vec<String> = summary
            .reads
            .iter()
            .map(|id| {
                graph.nodes.get(*id).map_or_else(
                    || format!("UnknownVar({})", id), // Fallback for unknown ID
                    |n| format!("{}.{}", n.contract_name.as_deref().unwrap_or("?"), n.name),
                )
            })
            .collect();
        reads_vec.sort(); // Sort for consistent output

        let mut writes_vec: Vec<String> = summary
            .writes
            .iter()
            .map(|id| {
                graph.nodes.get(*id).map_or_else(
                    || format!("UnknownVar({})", id), // Fallback for unknown ID
                    |n| format!("{}.{}", n.contract_name.as_deref().unwrap_or("?"), n.name),
                )
            })
            .collect();
        writes_vec.sort(); // Sort for consistent output

        md.push_str(&format!(
            "| {} | {} | {} |\n",
            endpoint_name,
            reads_vec.join(", "),
            writes_vec.join(", ")
        ));
    }
    md
}
