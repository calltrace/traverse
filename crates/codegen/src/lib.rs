pub mod deployer_stub;
//pub mod falsifier;
pub mod revert_stub;
pub mod state_change_stub;
pub mod teststubs;

use std::{fmt, path::PathBuf};

pub use teststubs::{generate_tests_with_foundry};

#[derive(Debug)]
pub enum CodeGenError {
    NoSolidityFiles,
    LanguageInitializationError,
    IoError(PathBuf, std::io::Error),
    WalkDirError(walkdir::Error),
    OutputWriteError(PathBuf, std::io::Error),
    TemplateError(String),
    SerializationError(String),
    FoundryError(String),
}

impl std::error::Error for CodeGenError {}

impl fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodeGenError::NoSolidityFiles => {
                write!(f, "No valid .sol files found in the provided paths.")
            }
            CodeGenError::LanguageInitializationError => {
                write!(f, "Failed to get Tree-sitter language for Solidity.")
            }
            CodeGenError::IoError(path, err) => {
                write!(f, "I/O error processing path '{}': {}", path.display(), err)
            }
            CodeGenError::WalkDirError(err) => write!(f, "Directory traversal error: {}", err),
            CodeGenError::OutputWriteError(path, err) => write!(
                f,
                "Failed to write output to file '{}': {}",
                path.display(),
                err
            ),
            CodeGenError::TemplateError(err) => {
                write!(f, "Template processing error: {}", err)
            }
            CodeGenError::SerializationError(err) => {
                write!(f, "CFG serialization error: {}", err)
            }
            CodeGenError::FoundryError(err) => {
                write!(f, "Foundry integration error: {}", err)
            }
        }
    }
}
