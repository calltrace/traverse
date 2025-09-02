use anyhow::{Context, Result};
use clap::Parser;
use tracing::warn;
use graph::{
    interface_resolver::{BindingConfig, BindingFile}, // Renamed to avoid conflict
    manifest::{find_solidity_files_for_manifest, Manifest, ManifestEntry},
    natspec::{extract::extract_source_comments, parse_natspec_comment, NatSpecKind},
};
use std::{
    collections::HashSet,
    fs,
    path::PathBuf,
};

#[derive(Parser, Debug)]
#[command(author, version, about = "Generates a skeleton binding.yaml file from Solidity Natspec annotations.", long_about = None)]
struct Cli {
    #[arg(required = true)]
    project_path: PathBuf,

    #[arg(short, long)]
    output_file: Option<PathBuf>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    logging::init_subscriber(false);

    let project_root = fs::canonicalize(&cli.project_path).with_context(|| {
        format!(
            "Failed to canonicalize project path: {}",
            cli.project_path.display()
        )
    })?;

    let output_file_path = cli
        .output_file
        .unwrap_or_else(|| project_root.join("binding.yaml"));

    warn!("Scanning project at: {}", project_root.display());

    let sol_files_relative = find_solidity_files_for_manifest(&[PathBuf::from(".")], &project_root)
        .context("Failed to find Solidity files for manifest generation")?;

    if sol_files_relative.is_empty() {
        warn!("No Solidity files found in the project. No binding file will be generated.");
        return Ok(());
    }

    warn!("Found {} Solidity files.", sol_files_relative.len());

    let mut manifest = Manifest::default();
    for relative_file_path in &sol_files_relative {
        let full_file_path = project_root.join(relative_file_path);
        let source = fs::read_to_string(&full_file_path).with_context(|| {
            format!(
                "Failed to read Solidity source file: {}",
                full_file_path.display()
            )
        })?;

        match extract_source_comments(&source) {
            Ok(source_comments) => {
                let entries: Vec<ManifestEntry> = source_comments
                    .into_iter()
                    .map(|sc| ManifestEntry::from((sc, relative_file_path.clone())))
                    .collect();
                manifest.extend_entries(entries);
            }
            Err(e) => {
                warn!(
                    "Warning: Failed to extract comments from {}: {}. Skipping file.",
                    relative_file_path.display(),
                    e
                );
            }
        }
    }
    warn!(
        "Manifest generated with {} entries.",
        manifest.entries.len()
    );

    let mut binding_configs: Vec<BindingConfig> = Vec::new();
    let mut processed_binding_keys: HashSet<String> = HashSet::new();

    for entry in &manifest.entries {
        if entry.is_natspec {
            if let Ok(natspec) = parse_natspec_comment(&entry.text) {
                for item in natspec.items {
                    if let NatSpecKind::Custom { tag } = item.kind {
                        if tag == "binds-to" {
                            let key = item.comment.trim().to_string();
                            if !key.is_empty() && !processed_binding_keys.contains(&key) {
                                processed_binding_keys.insert(key.clone());
                                let note = format!(
                                    "From @custom:binds-to tag in '{}' for item '{:?}' ({:?})",
                                    entry.file_path.display(),
                                    entry.item_name.as_deref().unwrap_or("<unnamed>"),
                                    entry.item_kind
                                );
                                binding_configs.push(BindingConfig {
                                    key,
                                    contract_name: None,
                                    address: None,
                                    chain_id: None,
                                    notes: Some(note),
                                });
                            }
                        }
                    }
                }
            }
        }
    }

    if binding_configs.is_empty() {
        warn!("No '@custom:binds-to' tags found. An empty binding file will be created.");
    } else {
        warn!(
            "Found {} unique '@custom:binds-to' keys for the skeleton binding file.",
            binding_configs.len()
        );
    }

    let binding_file_content = BindingFile::new(binding_configs); // Use constructor
    let yaml_string = serde_yaml::to_string(&binding_file_content)
        .context("Failed to serialize binding configurations to YAML")?;

    if let Some(parent_dir) = output_file_path.parent() {
        fs::create_dir_all(parent_dir).with_context(|| {
            format!(
                "Failed to create parent directories for output file: {}",
                parent_dir.display()
            )
        })?;
    }

    fs::write(&output_file_path, yaml_string).with_context(|| {
        format!(
            "Failed to write binding.yaml to: {}",
            output_file_path.display()
        )
    })?;

    warn!(
        "Skeleton binding file saved to: {}",
        output_file_path.display()
    );

    Ok(())
}
