pub mod access_control_stub;
pub mod deployer_stub;
pub mod invariant_breaker;
pub mod revert_stub;
pub mod state_change_stub;
pub mod teststubs;

use std::{fmt, path::PathBuf};

pub use access_control_stub::{
    create_comprehensive_access_control_test_contract, generate_access_control_tests_from_cfg,
};
pub use revert_stub::{create_comprehensive_revert_test_contract, generate_revert_tests_from_cfg};
pub use state_change_stub::{
    create_comprehensive_state_change_test_contract, generate_state_change_tests_from_cfg,
};
pub use teststubs::generate_tests_with_foundry;

#[derive(Debug)]
pub enum CodeGenError {
    FoundryError(String),
    TemplateError(String),
    IoError(PathBuf, std::io::Error),
    ParseError(String),
}

impl std::error::Error for CodeGenError {}

impl fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CodeGenError::FoundryError(msg) => write!(f, "Foundry error: {}", msg),
            CodeGenError::TemplateError(msg) => write!(f, "Template error: {}", msg),
            CodeGenError::IoError(path, err) => {
                write!(f, "I/O error for path '{}': {}", path.display(), err)
            }
            CodeGenError::ParseError(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl From<std::io::Error> for CodeGenError {
    fn from(err: std::io::Error) -> Self {
        CodeGenError::IoError(PathBuf::new(), err)
    }
}
