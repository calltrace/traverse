use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::{
    fs,
    path::{Path, PathBuf},
};
use tracing::debug;
use walkdir::WalkDir;

use crate::natspec::extract::{extract_source_comments, SourceComment, SourceItemKind};
use crate::natspec::TextRange; // Ensure TextRange and TextIndex are available

// Re-defining TextRange and TextIndex here if we don't want to make them public from natspec::mod
// For now, assuming they are accessible or we might need to duplicate/re-export them.
// If they are already Serialize/Deserialize, we can use them directly.

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestEntry {
    pub file_path: PathBuf, // Relative path to the source file from the project root
    pub text: String,
    pub raw_comment_span: TextRange,
    pub item_kind: SourceItemKind,
    pub item_name: Option<String>,
    pub item_span: TextRange,
    pub is_natspec: bool,
}

impl From<(SourceComment, PathBuf)> for ManifestEntry {
    fn from((sc, file_path): (SourceComment, PathBuf)) -> Self {
        ManifestEntry {
            file_path,
            text: sc.text,
            raw_comment_span: sc.raw_comment_span,
            item_kind: sc.item_kind,
            item_name: sc.item_name,
            item_span: sc.item_span,
            is_natspec: sc.is_natspec,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct Manifest {
    pub entries: Vec<ManifestEntry>,
    // Could include other metadata like generation timestamp, project name, etc.
}

impl Manifest {
    pub fn query_entries(
        &self,
        kind: SourceItemKind,
        name_pattern: Option<&str>,
    ) -> Vec<&ManifestEntry> {
        self.entries
            .iter()
            .filter(|entry| {
                entry.item_kind == kind
                    && name_pattern.is_none_or(|pattern| {
                        entry.item_name.as_ref().is_some_and(|name| {
                            name.to_lowercase().contains(&pattern.to_lowercase())
                        })
                    })
            })
            .collect()
    }

    /// Adds a single entry to the manifest.
    pub fn add_entry(&mut self, entry: ManifestEntry) {
        self.entries.push(entry);
    }

    /// Extends the manifest with multiple entries.
    pub fn extend_entries(&mut self, entries: Vec<ManifestEntry>) {
        self.entries.extend(entries);
    }
}

pub fn find_solidity_files_for_manifest(
    paths: &[PathBuf],
    project_root: &Path,
) -> Result<Vec<PathBuf>> {
    let mut sol_files = Vec::new();
    for path_arg in paths {
        let absolute_path_arg = if path_arg.is_absolute() {
            path_arg.clone()
        } else {
            project_root.join(path_arg)
        };

        if absolute_path_arg.is_dir() {
            for entry in WalkDir::new(&absolute_path_arg)
                .into_iter()
                .filter_map(|e| e.ok())
            {
                if entry.file_type().is_file()
                    && entry.path().extension().is_some_and(|ext| ext == "sol")
                {
                    // Store path relative to project_root for consistency
                    let relative_path = entry
                        .path()
                        .strip_prefix(project_root)
                        .unwrap_or_else(|_| entry.path()) // Fallback to absolute if not under root
                        .to_path_buf();
                    sol_files.push(relative_path);
                }
            }
        } else if absolute_path_arg.is_file()
            && absolute_path_arg
                .extension()
                .is_some_and(|ext| ext == "sol")
        {
            let relative_path = absolute_path_arg
                .strip_prefix(project_root)
                .unwrap_or_else(|_| &absolute_path_arg) // Fallback to absolute if not under root
                .to_path_buf();
            sol_files.push(relative_path);
        } else if absolute_path_arg.is_file() {
            // Log or handle non-Solidity files if necessary
        } else {
            // Log or handle non-existent paths
            debug!("Warning: Path not found or invalid: {}", path_arg.display());
        }
    }
    sol_files.sort();
    sol_files.dedup(); // Remove duplicates that might arise from overlapping paths
    Ok(sol_files)
}

pub fn generate_manifest(
    input_paths: &[PathBuf],
    project_root: &Path,
    manifest_file_path: &Path,
) -> Result<Manifest> {
    let mut manifest = Manifest::default();
    let sol_files_relative = find_solidity_files_for_manifest(input_paths, project_root)
        .context("Failed to find Solidity files")?;

    if sol_files_relative.is_empty() {
        // It's not an error to find no .sol files, just return an empty manifest
        // and save it.
        save_manifest(&manifest, manifest_file_path)?;
        return Ok(manifest);
    }

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
                debug!(
                    "Warning: Failed to extract comments from {}: {}. Skipping file.",
                    relative_file_path.display(),
                    e
                );
            }
        }
    }

    save_manifest(&manifest, manifest_file_path)?;
    Ok(manifest)
}

pub fn load_manifest(manifest_file_path: &Path) -> Result<Manifest> {
    let file_content = fs::read_to_string(manifest_file_path).with_context(|| {
        format!(
            "Failed to read manifest file: {}",
            manifest_file_path.display()
        )
    })?;
    let manifest: Manifest = serde_yaml::from_str(&file_content).with_context(|| {
        format!(
            "Failed to deserialize manifest from YAML: {}",
            manifest_file_path.display()
        )
    })?;
    Ok(manifest)
}

pub fn save_manifest(manifest: &Manifest, manifest_file_path: &Path) -> Result<()> {
    let yaml_string =
        serde_yaml::to_string(manifest).context("Failed to serialize manifest to YAML")?;

    if let Some(parent_dir) = manifest_file_path.parent() {
        fs::create_dir_all(parent_dir).with_context(|| {
            format!(
                "Failed to create parent directories for manifest file: {}",
                parent_dir.display()
            )
        })?;
    }

    fs::write(manifest_file_path, yaml_string).with_context(|| {
        format!(
            "Failed to write manifest to file: {}",
            manifest_file_path.display()
        )
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    fn create_test_sol_file(dir: &Path, filename: &str, content: &str) -> PathBuf {
        let file_path = dir.join(filename);
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "{}", content).unwrap();
        file_path
    }

    #[test]
    fn test_generate_and_load_manifest_empty() -> Result<()> {
        let tmp_dir = tempdir()?;
        let project_root = tmp_dir.path();
        let manifest_path = project_root.join("manifest.yaml");
        let input_paths: [PathBuf; 0] = [];

        let generated_manifest = generate_manifest(&input_paths, project_root, &manifest_path)?;
        assert!(generated_manifest.entries.is_empty());
        assert!(manifest_path.exists());

        let loaded_manifest = load_manifest(&manifest_path)?;
        assert_eq!(generated_manifest, loaded_manifest);
        Ok(())
    }

    #[test]
    fn test_generate_manifest_single_file() -> Result<()> {
        let tmp_dir = tempdir()?;
        let project_root = tmp_dir.path();
        let contracts_dir = project_root.join("contracts");
        fs::create_dir_all(&contracts_dir)?;

        let sol_content = r#"
        /// This is a contract
        contract MyContract {}

        /// This is a function
        function myFunction() public {}
        "#;
        let sol_file_rel_path = PathBuf::from("contracts/MyContract.sol");
        create_test_sol_file(
            project_root,
            sol_file_rel_path.to_str().unwrap(),
            sol_content,
        );

        let manifest_path = project_root.join("manifest.yaml");
        let input_paths = [PathBuf::from("contracts")]; // Input is the directory

        let generated_manifest = generate_manifest(&input_paths, project_root, &manifest_path)?;

        assert_eq!(generated_manifest.entries.len(), 2);
        assert!(manifest_path.exists());

        let contract_entry = generated_manifest
            .entries
            .iter()
            .find(|e| e.item_name == Some("MyContract".to_string()))
            .unwrap();
        assert_eq!(contract_entry.item_kind, SourceItemKind::Contract);
        assert_eq!(contract_entry.text, "/// This is a contract");
        assert_eq!(contract_entry.file_path, sol_file_rel_path);

        let function_entry = generated_manifest
            .entries
            .iter()
            .find(|e| e.item_name == Some("myFunction".to_string()))
            .unwrap();
        assert_eq!(function_entry.item_kind, SourceItemKind::Function);
        assert_eq!(function_entry.text, "/// This is a function");
        assert_eq!(function_entry.file_path, sol_file_rel_path);

        let loaded_manifest = load_manifest(&manifest_path)?;
        assert_eq!(generated_manifest, loaded_manifest);

        Ok(())
    }

    #[test]
    fn test_find_solidity_files_for_manifest_logic() -> Result<()> {
        let tmp_dir = tempdir()?;
        let project_root = tmp_dir.path();

        let src_dir = project_root.join("src");
        fs::create_dir(&src_dir)?;
        let interfaces_dir = src_dir.join("interfaces");
        fs::create_dir(&interfaces_dir)?;
        let lib_dir = project_root.join("lib");
        fs::create_dir(&lib_dir)?;

        create_test_sol_file(project_root, "A.sol", "// A");
        create_test_sol_file(&src_dir, "B.sol", "// B");
        create_test_sol_file(&interfaces_dir, "C.sol", "// C");
        create_test_sol_file(&lib_dir, "D.sol", "// D");
        // Non-solidity file
        create_test_sol_file(&src_dir, "E.txt", "// E");

        // Test case 1: Specific files
        let paths1 = [
            PathBuf::from("A.sol"),
            src_dir
                .join("B.sol")
                .strip_prefix(project_root)?
                .to_path_buf(),
        ];
        let files1 = find_solidity_files_for_manifest(&paths1, project_root)?;
        assert_eq!(files1.len(), 2);
        assert!(files1.contains(&PathBuf::from("A.sol")));
        assert!(files1.contains(&PathBuf::from("src/B.sol")));

        // Test case 2: Directory
        let paths2 = [PathBuf::from("src")];
        let files2 = find_solidity_files_for_manifest(&paths2, project_root)?;
        assert_eq!(files2.len(), 2); // B.sol, interfaces/C.sol
        assert!(files2.contains(&PathBuf::from("src/B.sol")));
        assert!(files2.contains(&PathBuf::from("src/interfaces/C.sol")));

        // Test case 3: Multiple directories and files, some outside project root context for paths
        let paths3 = [
            PathBuf::from("A.sol"),
            PathBuf::from("src"),
            lib_dir.clone(),
        ];
        let files3 = find_solidity_files_for_manifest(&paths3, project_root)?;
        assert_eq!(files3.len(), 4);
        assert!(files3.contains(&PathBuf::from("A.sol")));
        assert!(files3.contains(&PathBuf::from("src/B.sol")));
        assert!(files3.contains(&PathBuf::from("src/interfaces/C.sol")));
        assert!(files3.contains(&PathBuf::from("lib/D.sol")));

        // Test case 4: Project root itself
        let paths4 = [PathBuf::from(".")]; // Representing project root
        let files4 = find_solidity_files_for_manifest(&paths4, project_root)?;
        assert_eq!(files4.len(), 4); // A.sol, src/B.sol, src/interfaces/C.sol, lib/D.sol

        // Test case 5: Empty input
        let paths5: [PathBuf; 0] = [];
        let files5 = find_solidity_files_for_manifest(&paths5, project_root)?;
        assert!(files5.is_empty());

        // Test case 6: Path to a non-sol file
        let paths6 = [PathBuf::from("src/E.txt")];
        let files6 = find_solidity_files_for_manifest(&paths6, project_root)?;
        assert!(files6.is_empty());

        Ok(())
    }

    #[test]
    fn test_query_entries() -> Result<()> {
        let tmp_dir = tempdir()?;
        let project_root = tmp_dir.path();
        let manifest_path = project_root.join("manifest.yaml");

        let sol_content1 = r#"
        /// @title Contract Alpha
        contract Alpha {}
        /// @notice A public function
        function doAlpha() public {}
        "#;
        create_test_sol_file(project_root, "Alpha.sol", sol_content1);

        let sol_content2 = r#"
        /// @title Interface Beta
        interface Beta {
            /// @dev A beta function
            function doBeta() external;
        }
        /// @notice Another contract
        contract Gamma {}
        "#;
        create_test_sol_file(project_root, "BetaGamma.sol", sol_content2);

        let input_paths = [PathBuf::from(".")]; // Scan whole project root

        let manifest = generate_manifest(&input_paths, project_root, &manifest_path)?;

        // Query for all contracts
        let contracts = manifest.query_entries(SourceItemKind::Contract, None);
        assert_eq!(contracts.len(), 2);
        assert!(contracts
            .iter()
            .any(|e| e.item_name == Some("Alpha".to_string())));
        assert!(contracts
            .iter()
            .any(|e| e.item_name == Some("Gamma".to_string())));

        // Query for specific contract by name
        let alpha_contract = manifest.query_entries(SourceItemKind::Contract, Some("Alpha"));
        assert_eq!(alpha_contract.len(), 1);
        assert_eq!(alpha_contract[0].item_name, Some("Alpha".to_string()));

        // Query for specific contract by partial name (case-insensitive)
        let ga_contract = manifest.query_entries(SourceItemKind::Contract, Some("gam"));
        assert_eq!(ga_contract.len(), 1);
        assert_eq!(ga_contract[0].item_name, Some("Gamma".to_string()));

        // Query for all functions
        let functions = manifest.query_entries(SourceItemKind::Function, None);
        assert_eq!(functions.len(), 2); // doAlpha, doBeta
        assert!(functions
            .iter()
            .any(|e| e.item_name == Some("doAlpha".to_string())));
        assert!(functions
            .iter()
            .any(|e| e.item_name == Some("doBeta".to_string())));

        // Query for specific function by name
        let do_beta_function = manifest.query_entries(SourceItemKind::Function, Some("doBeta"));
        assert_eq!(do_beta_function.len(), 1);
        assert_eq!(do_beta_function[0].item_name, Some("doBeta".to_string()));
        assert_eq!(
            do_beta_function[0].file_path,
            PathBuf::from("BetaGamma.sol")
        );

        // Query for interfaces
        let interfaces = manifest.query_entries(SourceItemKind::Interface, None);
        assert_eq!(interfaces.len(), 1);
        assert_eq!(interfaces[0].item_name, Some("Beta".to_string()));

        // Query for non-existent kind
        let events = manifest.query_entries(SourceItemKind::Event, None);
        assert!(events.is_empty());

        // Query for existing kind but non-existent name
        let non_existent_func =
            manifest.query_entries(SourceItemKind::Function, Some("noSuchFunction"));
        assert!(non_existent_func.is_empty());

        Ok(())
    }
}
