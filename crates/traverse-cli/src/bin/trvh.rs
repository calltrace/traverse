/// Traverse Hydrator CLI (trvh)
///
/// This module implements a command-line interface for hydrating DDlog output.
/// It provides a user-friendly way to take the raw output from a DDlog application
/// and convert it into a more readable format by replacing node IDs with their
/// corresponding source code snippets.
///
/// The CLI follows a standard workflow:
/// 1. Parse and validate command-line arguments
/// 2. Read input files (DDlog output and source file)
/// 3. Configure and run the hydration process
/// 4. Save the hydrated output
///
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process;

use clap::Parser;
use traverse_cli::hydrate::{hydrate_output, hydrate_output_with_config, HydrationError};
use language::{Language, Mermaid, Solidity};
use tracing;
use tracing_subscriber;

#[derive(Debug, clap::ValueEnum, Clone)]
pub enum SourceType {
    Solidity,
    Mermaid,
}

impl SourceType {
    fn to_tree_sitter_language(&self) -> impl Language {
        match self {
            SourceType::Solidity => Solidity,
            SourceType::Mermaid => unimplemented!("Mermaid language support not yet implemented"),
        }
    }
}

#[derive(Parser, Debug)]
#[command(
    name = "trvh",
    author = "Traverse Team",
    version,
    about = "Traverse Hydrator - Convert DDlog output to human-readable format",
    long_about = None
)]
struct Args {
    /// Path to the DDlog output file (if not provided, reads from stdin)
    #[arg(short = 'd', long = "ddlog-output", value_name = "FILE")]
    ddlog_output_path: Option<PathBuf>,

    /// Output file for the hydrated result
    #[arg(short = 'o', long = "output", value_name = "FILE")]
    output_path: Option<PathBuf>,

    /// Path to a custom hydration configuration file
    #[arg(short = 'c', long = "config", value_name = "FILE")]
    config_path: Option<PathBuf>,

    /// Enable tracing for debugging
    #[arg(long = "trace", default_value = "false")]
    enable_tracing: bool,
}

impl Args {
    fn validate(&self) -> Result<(), HydrationError> {
        // Validate that DDlog output file exists if provided
        if let Some(path) = &self.ddlog_output_path {
            if !path.exists() {
                return Err(HydrationError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("DDlog output file does not exist: {}", path.display()),
                )));
            }
        }

        // Validate that config file exists if provided
        if let Some(path) = &self.config_path {
            if !path.exists() {
                return Err(HydrationError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("Configuration file does not exist: {}", path.display()),
                )));
            }
        }

        Ok(())
    }
}

fn main() {
    // Parse command line arguments
    let args = Args::parse();

    // Setup tracing if enabled
    if args.enable_tracing {
        tracing_subscriber::fmt::init();
    }

    // Validate arguments
    if let Err(err) = args.validate() {
        eprintln!("Error: {}", err);
        process::exit(1);
    }

    // Read the DDlog output from file or stdin
    let mut ddlog_output = String::new();
    if let Some(path) = &args.ddlog_output_path {
        // Read from file
        match fs::File::open(path) {
            Ok(mut file) => {
                if let Err(err) = file.read_to_string(&mut ddlog_output) {
                    eprintln!(
                        "Error reading DDlog output file {}: {}",
                        path.display(),
                        err
                    );
                    process::exit(1);
                }
            }
            Err(err) => {
                eprintln!(
                    "Error opening DDlog output file {}: {}",
                    path.display(),
                    err
                );
                process::exit(1);
            }
        }
    } else {
        // Read from stdin
        println!("Reading DDlog output from stdin...");
        if let Err(err) = io::stdin().read_to_string(&mut ddlog_output) {
            eprintln!("Error reading from stdin: {}", err);
            process::exit(1);
        }
    }

    // Perform hydration
    println!("Hydrating DDlog output...");
    
    // Determine if we should use a config file
    let result = if let Some(config_path) = &args.config_path {
        // Load config from file
        match backend::hydrate::BucketConfig::from_yaml_file(config_path) {
            Ok(config) => hydrate_output_with_config(&ddlog_output, config),
            Err(err) => Err(HydrationError::Config(format!("Failed to load config: {}", err)))
        }
    } else {
        // Use default hydration with source content and language
        hydrate_output(&ddlog_output)
    };

    match result {
        Ok(hydrated) => {
            // Determine output path for hydrated result
            let output_path = args.output_path.unwrap_or_else(|| {
                if let Some(path) = &args.ddlog_output_path {
                    let input_stem = path.file_stem().unwrap_or_default();
                    let mut path = PathBuf::from(input_stem);
                    path.set_extension("hydrated.txt");
                    path
                } else {
                    PathBuf::from("stdin_hydrated.txt")
                }
            });

            // Save hydrated output to file
            if let Err(err) = fs::write(&output_path, &hydrated) {
                eprintln!("Warning: Failed to save hydrated output to file: {}", err);
                process::exit(1);
            } else {
                println!("Hydrated output saved to: {}", output_path.display());
            }

            println!("Hydration completed successfully!");
        }
        Err(err) => {
            eprintln!("Hydration failed: {}", err);
            process::exit(1);
        }
    }
}
