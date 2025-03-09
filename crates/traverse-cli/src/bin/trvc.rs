/// Traverse Compiler CLI (trvc)
///
/// This module implements the command-line interface for the Traverse compiler.
/// It provides a user-friendly way to compile Traverse DSL programs into DDlog,
/// execute them against source files, and process the results.
///
/// The CLI follows a standard workflow:
/// 1. Parse and validate command-line arguments
/// 2. Read input files
/// 3. Configure and run the compiler
/// 4. Process and save outputs
///
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process;

use clap::Parser;
use compiler::compile::{Compiler, CompilerError};
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
    name = "trvc",
    author = "Traverse Team",
    version,
    about = "Traverse Compiler - Build and analyze Traverse programs",
    long_about = None
)]
struct Args {
    /// Path to the DSL input file
    #[arg(short = 'f', long = "input", value_name = "FILE")]
    input_path: PathBuf,

    /// Project name for the generated DDlog project
    #[arg(short = 'p', long = "project-name", required = true)]
    project_name: String,

    /// Paths to input parser home directories
    #[arg(short = 'i', long = "input-parser-home", value_name = "DIR")]
    input_parser_homes: Vec<PathBuf>,

    /// Paths to intermediate parser home directories
    #[arg(short = 'n', long = "intermediate-parser-home", value_name = "DIR")]
    intermediate_parser_homes: Vec<PathBuf>,

    /// Path to the source file to analyze
    #[arg(short = 's', long = "source", value_name = "FILE")]
    source_path: PathBuf,

    /// Type of the source file
    #[arg(short = 't', long = "type", value_enum)]
    source_type: SourceType,

    /// Skip executing the generated DDlog script
    #[arg(long = "no-execute", default_value = "false")]
    no_execute: bool,

    /// Skip hydrating the DDlog output
    #[arg(long = "no-hydrate", default_value = "false")]
    no_hydrate: bool,

    /// Enable tracing for debugging
    #[arg(long = "trace", default_value = "false")]
    enable_tracing: bool,

    /// Output file for the hydrated result (if hydration is enabled)
    #[arg(short = 'o', long = "output", value_name = "FILE")]
    output_path: Option<PathBuf>,
}

impl Args {
    fn validate(&self) -> Result<(), CompilerError> {
        // Validate that all parser homes exist
        for path in &self.input_parser_homes {
            if !path.is_dir() {
                return Err(CompilerError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "Input Parser home directory does not exist: {}",
                        path.display()
                    ),
                )));
            }
        }

        for path in &self.intermediate_parser_homes {
            if !path.is_dir() {
                return Err(CompilerError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "Intermediate Parser home directory does not exist: {}",
                        path.display()
                    ),
                )));
            }
        }

        // Validate that input file exists
        if !self.input_path.exists() {
            return Err(CompilerError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Input path does not exist: {}", self.input_path.display()),
            )));
        }

        // Validate that source file exists
        if !self.source_path.exists() {
            return Err(CompilerError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Source file does not exist: {}", self.source_path.display()),
            )));
        }

        Ok(())
    }
}

fn main() {
    // Parse command line arguments
    let args = Args::parse();

    // Validate arguments
    if let Err(err) = args.validate() {
        eprintln!("Error: {}", err);
        process::exit(1);
    }

    // Read the input file
    let mut input_file_content = String::new();
    match fs::File::open(&args.input_path) {
        Ok(mut file) => {
            if let Err(err) = file.read_to_string(&mut input_file_content) {
                eprintln!(
                    "Error reading input file {}: {}",
                    &args.input_path.display(),
                    err
                );
                process::exit(1);
            }
        }
        Err(err) => {
            eprintln!(
                "Error opening input file {}: {}",
                args.input_path.display(),
                err
            );
            process::exit(1);
        }
    }

    // Configure the compiler
    let mut compiler = Compiler::new()
        .with_project_name(args.project_name.clone())
        .with_input_ts_grammars(args.input_parser_homes)
        .with_intermediate_ts_grammars(args.intermediate_parser_homes)
        .with_source_file(args.source_path.into_boxed_path())
        .with_tracing(args.enable_tracing);

    // Apply optional configurations
    if args.no_execute {
        compiler = compiler.without_execution();
    }
    if args.no_hydrate {
        compiler = compiler.without_hydration();
    }

    // Get the appropriate language for the source type
    let language = args.source_type.to_tree_sitter_language();

    // Run the compilation process
    println!("Compiling Traverse program...");
    match compiler.compile(&input_file_content, &language) {
        Ok(result) => {
            // Print compilation results
            println!(
                "
DSL:
{}",
                result.compilation.dsl
            );
            println!(
                "
IR:
{}",
                result.compilation.ir
            );

            // Print a summary of the DDlog code (first few lines)
            let ddlog_preview: String = result
                .compilation
                .ddlog
                .lines()
                .take(10)
                .collect::<Vec<_>>()
                .join(
                    "
",
                );
            println!(
                "
DDlog (preview):
{}
...",
                ddlog_preview
            );

            // Save the DDlog to a file
            let ddlog_file = format!("{}.dl", args.project_name);
            if let Err(err) = fs::write(&ddlog_file, &result.compilation.ddlog) {
                eprintln!("Warning: Failed to save DDlog to file: {}", err);
            } else {
                println!("Full DDlog saved to: {}", ddlog_file);
            }

            // Handle DDlog execution results if available
            if let Some(ddlog_output) = result.ddlog_output {
                println!(
                    "
DDlog execution completed successfully"
                );

                // Save DDlog output to a file
                let ddlog_output_file = format!("{}.output.txt", args.project_name);
                if let Err(err) = fs::write(&ddlog_output_file, &ddlog_output) {
                    eprintln!("Warning: Failed to save DDlog output to file: {}", err);
                } else {
                    println!("DDlog output saved to: {}", ddlog_output_file);
                }
            }

            // Handle hydrated output if available
            if let Some(hydrated) = result.hydrated_output {
                println!(
                    "
Hydration completed successfully"
                );

                // Determine output path for hydrated result
                let output_path = args.output_path.unwrap_or_else(|| {
                    PathBuf::from(format!("{}_hydrated.txt", args.project_name))
                });

                // Save hydrated output to file
                if let Err(err) = fs::write(&output_path, &hydrated) {
                    eprintln!("Warning: Failed to save hydrated output to file: {}", err);
                } else {
                    println!("Hydrated output saved to: {}", output_path.display());
                }
            }

            println!(
                "
Compilation completed successfully!"
            );
        }
        Err(err) => {
            eprintln!("Compilation failed: {}", err);
            process::exit(1);
        }
    }
}
