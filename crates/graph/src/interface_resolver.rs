/*
    This module is designed to enhance static analysis of smart contracts by enabling
    the resolution of abstract code elements (such as interfaces or uninitialized
    library-type state variables) to concrete implementations specified by the user.

    The core mechanism involves:
    1. Defining a `BindingConfig` structure, which represents a concrete implementation
       (e.g., a specific contract name, and optionally its address or chain ID).
       These configurations are typically loaded from an external `binding.yaml` file.
    2. A `BindingRegistry` loads and stores these `BindingConfig`s, indexed by a unique key.
    3. An `InterfaceResolver` utilizes Natspec documentation comments extracted from
       Solidity source code (via a `Manifest`). It specifically looks for
       `@custom:binds-to <key>` tags within these comments.
    4. When such a tag is found, the `InterfaceResolver` uses the `<key>` to query the
       `BindingRegistry` for the corresponding `BindingConfig`. This effectively substitutes
       the abstract element in the source code with the user-defined concrete
       implementation for the purpose of the analysis.

    By allowing users to specify these bindings, the static analysis can achieve
    greater coverage and provide more accurate insights into the behavior of
    complex smart contract systems where parts might be abstract or deployed separately.
*/

use crate::manifest::{Manifest, ManifestEntry};
use crate::natspec::{
    extract::SourceItemKind, parse_natspec_comment, NatSpec, NatSpecKind, TextRange,
};
use anyhow::{Context, Result};
use nom::Finish;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BindingConfig {
    pub key: String,
    pub contract_name: Option<String>,
    pub address: Option<String>,
    pub chain_id: Option<u64>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize, Default)]
pub struct BindingFile {
    bindings: Vec<BindingConfig>,
}

impl BindingFile {
    pub fn new(bindings: Vec<BindingConfig>) -> Self {
        Self { bindings }
    }
}

#[derive(Debug, Clone, Default)]
pub struct BindingRegistry {
    bindings: HashMap<String, BindingConfig>,
}

impl BindingRegistry {
    pub fn load(path: &Path) -> Result<Self> {
        let file_content = fs::read_to_string(path)
            .with_context(|| format!("Failed to read binding file: {}", path.display()))?;
        let binding_file: BindingFile = serde_yaml::from_str(&file_content).with_context(|| {
            format!(
                "Failed to deserialize binding file from YAML: {}",
                path.display()
            )
        })?;

        let mut bindings_map = HashMap::new();
        for config in binding_file.bindings {
            if bindings_map.contains_key(&config.key) {
                eprintln!(
                    "Warning: Duplicate binding key '{}' found in {}. The last definition will be used.",
                    config.key,
                    path.display()
                );
            }
            bindings_map.insert(config.key.clone(), config);
        }

        Ok(BindingRegistry {
            bindings: bindings_map,
        })
    }

    pub fn get_binding(&self, key: &str) -> Option<&BindingConfig> {
        self.bindings.get(key)
    }
}

pub struct InterfaceResolver<'m, 'r> {
    manifest: &'m Manifest,
    registry: &'r BindingRegistry,
}

impl<'m, 'r> InterfaceResolver<'m, 'r> {
    pub fn new(manifest: &'m Manifest, registry: &'r BindingRegistry) -> Self {
        Self { manifest, registry }
    }

    pub fn resolve_for_entry(&self, entry: &ManifestEntry) -> Result<Option<&'r BindingConfig>> {
        if !entry.is_natspec {
            return Ok(None);
        }

        match parse_natspec_comment(&entry.text) {
            Ok(natspec) => {
                for item in natspec.items {
                    if let NatSpecKind::Custom { tag } = item.kind {
                        if tag == "binds-to" {
                            let binding_key = item.comment.trim();
                            if !binding_key.is_empty() {
                                return Ok(self.registry.get_binding(binding_key));
                            } else {
                                eprintln!(
                                    "Warning: Found '@custom:binds-to' with empty key in Natspec for item {:?} in file {}",
                                    entry.item_name, entry.file_path.display()
                                );
                            }
                        }
                    }
                }
                Ok(None)
            }
            Err(e) => Err(anyhow::anyhow!(
                "Failed to parse Natspec for item {:?} (file: {}): {}",
                entry.item_name.as_deref().unwrap_or("<unknown>"),
                entry.file_path.display(),
                e.to_string()
            )),
        }
    }

    pub fn resolve_for_item_span(
        &self,
        target_item_span: &TextRange,
    ) -> Result<Option<&'r BindingConfig>> {
        for entry in &self.manifest.entries {
            if entry.item_span == *target_item_span {
                return self.resolve_for_entry(entry);
            }
        }
        Ok(None)
    }

    pub fn resolve_for_item_name_kind(
        &self,
        item_name_to_find: &str,
        item_kind_to_find: SourceItemKind,
        file_path_hint: Option<&Path>,
    ) -> Result<Option<&'r BindingConfig>> {
        for entry in &self.manifest.entries {
            if entry.item_kind == item_kind_to_find
                && entry.item_name.as_deref() == Some(item_name_to_find)
            {
                if let Some(hint_path) = file_path_hint {
                    if entry.file_path != hint_path {
                        continue;
                    }
                }
                return self.resolve_for_entry(entry);
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::natspec::extract::SourceItemKind;
    use crate::natspec::{TextIndex, TextRange};
    use std::fs::File;
    use std::io::Write;
    use tempfile::tempdir;

    fn create_binding_file(dir: &Path, content: &str) -> PathBuf {
        let file_path = dir.join("binding.yaml");
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "{}", content).unwrap();
        file_path
    }

    fn default_text_range() -> TextRange {
        TextRange {
            start: TextIndex {
                utf8: 0,
                line: 0,
                column: 0,
            },
            end: TextIndex {
                utf8: 0,
                line: 0,
                column: 0,
            },
        }
    }

    #[test]
    fn test_load_binding_registry() -> Result<()> {
        let tmp_dir = tempdir()?;
        let yaml_content = r#"
bindings:
  - key: "USDC_Mainnet"
    contract_name: "FiatTokenProxy"
    address: "0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48"
    chain_id: 1
    notes: "USDC on Ethereum Mainnet"
  - key: "MyService_Test"
    contract_name: "MyServiceImpl"
    address: "0x1234567890abcdef1234567890abcdef12345678"
    chain_id: 4
    notes: "MyService on Rinkeby (test)"
"#;
        let binding_file_path = create_binding_file(tmp_dir.path(), yaml_content);
        let registry = BindingRegistry::load(&binding_file_path)?;

        assert!(registry.get_binding("USDC_Mainnet").is_some());
        assert_eq!(
            registry.get_binding("USDC_Mainnet").unwrap().address,
            Some("0xA0b86991c6218b36c1d19D4a2e9Eb0cE3606eB48".to_string())
        );
        assert_eq!(
            registry.get_binding("USDC_Mainnet").unwrap().chain_id,
            Some(1)
        );

        assert!(registry.get_binding("MyService_Test").is_some());
        assert_eq!(
            registry
                .get_binding("MyService_Test")
                .unwrap()
                .contract_name,
            Some("MyServiceImpl".to_string())
        );

        assert!(registry.get_binding("NonExistentKey").is_none());
        Ok(())
    }

    #[test]
    fn test_load_binding_registry_duplicate_keys() -> Result<()> {
        let tmp_dir = tempdir()?;
        let yaml_content = r#"
bindings:
  - key: "DuplicateKey"
    address: "0x111"
  - key: "UniqueKey"
    address: "0x222"
  - key: "DuplicateKey" # This should overwrite the first one
    address: "0x333"
    notes: "This is the one"
"#;
        let binding_file_path = create_binding_file(tmp_dir.path(), yaml_content);
        let registry = BindingRegistry::load(&binding_file_path)?;

        assert_eq!(registry.bindings.len(), 2); // DuplicateKey and UniqueKey
        let duplicate_entry = registry.get_binding("DuplicateKey").unwrap();
        assert_eq!(duplicate_entry.address, Some("0x333".to_string()));
        assert_eq!(duplicate_entry.notes, Some("This is the one".to_string()));
        Ok(())
    }

    #[test]
    fn test_resolve_for_entry_success() -> Result<()> {
        let registry = BindingRegistry {
            bindings: HashMap::from([(
                "BoundContract_Key".to_string(),
                BindingConfig {
                    key: "BoundContract_Key".to_string(),
                    contract_name: Some("BoundContractImpl".to_string()),
                    address: Some("0xabc".to_string()),
                    chain_id: Some(1),
                    notes: None,
                },
            )]),
        };
        let manifest = Manifest::default(); // Not used directly by this specific path of resolve_for_entry

        let entry = ManifestEntry {
            file_path: PathBuf::from("test.sol"),
            text: "/// @custom:binds-to BoundContract_Key".to_string(),
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Contract,
            item_name: Some("MyContract".to_string()),
            item_span: default_text_range(),
            is_natspec: true,
        };

        let resolver = InterfaceResolver::new(&manifest, &registry);
        let binding_config = resolver.resolve_for_entry(&entry)?.unwrap();

        assert_eq!(binding_config.key, "BoundContract_Key");
        assert_eq!(
            binding_config.contract_name,
            Some("BoundContractImpl".to_string())
        );
        Ok(())
    }

    #[test]
    fn test_resolve_for_entry_multiline_natspec() -> Result<()> {
        let registry = BindingRegistry {
            bindings: HashMap::from([(
                "MultiKey".to_string(),
                BindingConfig {
                    key: "MultiKey".to_string(),
                    contract_name: Some("MultiImpl".to_string()),
                    address: None,
                    chain_id: None,
                    notes: None,
                },
            )]),
        };
        let manifest = Manifest::default();
        let entry = ManifestEntry {
            file_path: PathBuf::from("test.sol"),
            text: "/**\n * @notice Some notice\n * @custom:binds-to MultiKey\n * @dev Some dev comment\n */".to_string(),
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Function,
            item_name: Some("doSomething".to_string()),
            item_span: default_text_range(),
            is_natspec: true,
        };
        let resolver = InterfaceResolver::new(&manifest, &registry);
        let binding_config = resolver.resolve_for_entry(&entry)?.unwrap();
        assert_eq!(binding_config.key, "MultiKey");
        Ok(())
    }

    #[test]
    fn test_resolve_for_entry_no_binding_tag() -> Result<()> {
        let registry = BindingRegistry::default();
        let manifest = Manifest::default();
        let entry = ManifestEntry {
            file_path: PathBuf::from("test.sol"),
            text: "/// @notice Just a regular comment".to_string(),
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Contract,
            item_name: Some("MyContract".to_string()),
            item_span: default_text_range(),
            is_natspec: true,
        };

        let resolver = InterfaceResolver::new(&manifest, &registry);
        assert!(resolver.resolve_for_entry(&entry)?.is_none());
        Ok(())
    }

    #[test]
    fn test_resolve_for_entry_not_natspec() -> Result<()> {
        let registry = BindingRegistry::default();
        let manifest = Manifest::default();
        let entry = ManifestEntry {
            file_path: PathBuf::from("test.sol"),
            text: "// Regular comment, @custom:binds-to SomeKey".to_string(),
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Contract,
            item_name: Some("MyContract".to_string()),
            item_span: default_text_range(),
            is_natspec: false, // Crucial part
        };

        let resolver = InterfaceResolver::new(&manifest, &registry);
        assert!(resolver.resolve_for_entry(&entry)?.is_none());
        Ok(())
    }

    #[test]
    fn test_resolve_for_entry_key_not_in_registry() -> Result<()> {
        let registry = BindingRegistry::default(); // Empty registry
        let manifest = Manifest::default();
        let entry = ManifestEntry {
            file_path: PathBuf::from("test.sol"),
            text: "/// @custom:binds-to NonExistentKey".to_string(),
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Contract,
            item_name: Some("MyContract".to_string()),
            item_span: default_text_range(),
            is_natspec: true,
        };

        let resolver = InterfaceResolver::new(&manifest, &registry);
        assert!(resolver.resolve_for_entry(&entry)?.is_none());
        Ok(())
    }

    #[test]
    fn test_resolve_for_entry_natspec_parse_error() {
        let registry = BindingRegistry::default();
        let manifest = Manifest::default();
        let entry = ManifestEntry {
            file_path: PathBuf::from("test.sol"),
            text: "/*** Invalid Natspec @custom:binds-to SomeKey".to_string(), // Invalid start
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Contract,
            item_name: Some("MyContract".to_string()),
            item_span: default_text_range(),
            is_natspec: true,
        };
        let resolver = InterfaceResolver::new(&manifest, &registry);
        let result = resolver.resolve_for_entry(&entry);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Failed to parse Natspec"));
    }

    #[test]
    fn test_resolve_for_item_span_found() -> Result<()> {
        let registry = BindingRegistry {
            bindings: HashMap::from([(
                "SpanKey".to_string(),
                BindingConfig {
                    key: "SpanKey".to_string(),
                    address: Some("0xspan".to_string()),
                    contract_name: None,
                    chain_id: None,
                    notes: None,
                },
            )]),
        };
        let item_span_to_find = TextRange {
            start: TextIndex {
                utf8: 10,
                line: 1,
                column: 0,
            },
            end: TextIndex {
                utf8: 20,
                line: 1,
                column: 10,
            },
        };
        let mut manifest = Manifest::default();
        manifest.add_entry(ManifestEntry {
            file_path: PathBuf::from("a.sol"),
            text: "/// @custom:binds-to SpanKey".to_string(),
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Function,
            item_name: Some("funcA".to_string()),
            item_span: item_span_to_find.clone(),
            is_natspec: true,
        });
        manifest.add_entry(ManifestEntry {
            // Another entry with different span
            file_path: PathBuf::from("b.sol"),
            text: "/// @custom:binds-to OtherKey".to_string(),
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Contract,
            item_name: Some("ContractB".to_string()),
            item_span: default_text_range(),
            is_natspec: true,
        });

        let resolver = InterfaceResolver::new(&manifest, &registry);
        let binding = resolver.resolve_for_item_span(&item_span_to_find)?.unwrap();
        assert_eq!(binding.key, "SpanKey");
        assert_eq!(binding.address, Some("0xspan".to_string()));
        Ok(())
    }

    #[test]
    fn test_resolve_for_item_name_kind_found() -> Result<()> {
        let registry = BindingRegistry {
            bindings: HashMap::from([(
                "NameKindKey".to_string(),
                BindingConfig {
                    key: "NameKindKey".to_string(),
                    contract_name: Some("ImplForName".to_string()),
                    address: None,
                    chain_id: None,
                    notes: None,
                },
            )]),
        };
        let mut manifest = Manifest::default();
        manifest.add_entry(ManifestEntry {
            file_path: PathBuf::from("c.sol"),
            text: "/// @custom:binds-to NameKindKey".to_string(),
            raw_comment_span: default_text_range(),
            item_kind: SourceItemKind::Interface,
            item_name: Some("IMyInterface".to_string()),
            item_span: default_text_range(),
            is_natspec: true,
        });

        let resolver = InterfaceResolver::new(&manifest, &registry);
        let binding = resolver
            .resolve_for_item_name_kind("IMyInterface", SourceItemKind::Interface, None)?
            .unwrap();
        assert_eq!(binding.key, "NameKindKey");
        assert_eq!(binding.contract_name, Some("ImplForName".to_string()));

        // Test with file path hint
        let binding_with_hint = resolver
            .resolve_for_item_name_kind(
                "IMyInterface",
                SourceItemKind::Interface,
                Some(&PathBuf::from("c.sol")),
            )?
            .unwrap();
        assert_eq!(binding_with_hint.key, "NameKindKey");

        // Test with wrong file path hint
        let no_binding_wrong_hint = resolver.resolve_for_item_name_kind(
            "IMyInterface",
            SourceItemKind::Interface,
            Some(&PathBuf::from("wrong.sol")),
        )?;
        assert!(no_binding_wrong_hint.is_none());
        Ok(())
    }
}
