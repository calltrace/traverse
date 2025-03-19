/// Hydration module for Traverse
///
/// This module provides functionality to hydrate DDlog output by replacing node IDs
/// with their corresponding source code snippets. It serves as a bridge between
/// the raw DDlog output and human-readable representations.
///
use std::error::Error;
use std::fmt;
use std::io;

use backend::ddlog_drain::DdlogDrain;
use backend::hydrate::{BucketConfig, Hydrator, InputSource};
use language::Language;

#[derive(Debug)]
pub enum HydrationError {
    Io(io::Error),
    Parse(String),
    Config(String),
    Execution(String),
}

impl fmt::Display for HydrationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HydrationError::Io(err) => write!(f, "IO error: {}", err),
            HydrationError::Parse(msg) => write!(f, "Parse error: {}", msg),
            HydrationError::Config(msg) => write!(f, "Configuration error: {}", msg),
            HydrationError::Execution(msg) => write!(f, "Execution error: {}", msg),
        }
    }
}

impl Error for HydrationError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            HydrationError::Io(err) => Some(err),
            _ => None,
        }
    }
}

impl From<io::Error> for HydrationError {
    fn from(err: io::Error) -> Self {
        HydrationError::Io(err)
    }
}

/// Hydrate DDlog output using the provided source content and language
///
/// # Arguments
///
/// * `ddlog_output` - The raw DDlog output to hydrate
/// * `source_content` - The source code content used for hydration
/// * `language` - The language implementation for parsing the source
///
/// # Returns
///
/// * `Result<String, HydrationError>` - The hydrated output or an error
pub fn hydrate_output(ddlog_output: &str) -> Result<String, HydrationError> {
    // Create a default hydration configuration
    let hydrator_config = create_default_hydrator_config();

    // Create a hydrator with the configuration
    let mut hydrator = Hydrator::new(hydrator_config);

    // Process the DDlog output
    let lines = ddlog_output.lines().map(|s| s.to_string());
    let drain = DdlogDrain::new(lines);

    hydrator.process_drain(drain);

    // Return the hydrated output
    Ok(hydrator.dump_streams())
}

/// Hydrate DDlog output using a custom configuration
///
/// # Arguments
///
/// * `ddlog_output` - The raw DDlog output to hydrate
/// * `config` - The hydration configuration
///
/// # Returns
///
/// * `Result<String, HydrationError>` - The hydrated output or an error
pub fn hydrate_output_with_config(
    ddlog_output: &str,
    config: BucketConfig,
) -> Result<String, HydrationError> {
    // Create a hydrator with the provided configuration
    let mut hydrator = Hydrator::new(config);

    // Process the DDlog output
    let lines = ddlog_output.lines().map(|s| s.to_string());
    let drain = DdlogDrain::new(lines);

    hydrator.process_drain(drain);

    // Return the hydrated output
    Ok(hydrator.dump_streams())
}

/// Create a default hydration configuration
///
/// This function creates a default configuration for hydrating DDlog output,
/// which can be used when no custom configuration is provided.
fn create_default_hydrator_config() -> BucketConfig {
    BucketConfig::new()
        .with_pool_shape("sequenceDiagram")
        .with_bucket(
            "participants",
            100,
            "path",
            "val",
            vec![
                InputSource::new("EmitMermaidLineMockActorLine", 100),
                InputSource::new("EmitMermaidLineMockActorParticipantLine", 99),
                InputSource::new("EmitMermaidLineCallerParticipantLine", 95),
                InputSource::new("EmitMermaidLineCalleeParticipantLine", 90),
                InputSource::new("EmitMermaidLineContractParticipantLine", 90),
            ],
        )
        .with_bucket(
            "mock-actor-flows",
            95,
            "mock_actor_func_id_path",
            "val",
            vec![InputSource::new("EmitMermaidLineMockActorSignalLine", 100)],
        )
        .with_bucket(
            "intra-contract-flows",
            90,
            "intra_ce_no_return_id_path",
            "val",
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
            vec![
                InputSource::new("EmitMermaidLineSignalLine", 100),
                InputSource::new("EmitMermaidLineActivateLine", 90),
                InputSource::new("EmitMermaidLineReturnSignalLine", 80),
                InputSource::new("EmitMermaidLineDeactivateLine", 70),
            ],
        )
        .with_stream_shape(
            "solidity-to-mermaid",
            vec![
                "mock-actor-flows",
                "intra-contract-flows",
                "inter-contract-flows",
            ],
            "sequenceDiagram",
        )
}
