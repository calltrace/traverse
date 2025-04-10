use anyhow::{bail, Context, Result};
use clap::Parser;
use graph::cg::{CallGraph, CallGraphGenerator};
use graph::cg_dot::CgToDot;
use graph::cg_mermaid::{MermaidGenerator, ToSequenceDiagram};
use graph::parser::parse_solidity;
use language::{Language, Solidity};
use mermaid::sequence_diagram_writer;
use std::fmt;
use std::fs;
use std::io::{stdout, Write};
use std::path::PathBuf;
use thiserror::Error;
use walkdir::WalkDir;

#[derive(Parser, Debug)]
#[command(author, version, about = "Converts Solidity code to a Call Graph (DOT or Mermaid)", long_about = None)]
struct Cli {
    /// Input Solidity file(s) (.sol) or directories containing them.
    #[arg(required = true, num_args = 1..)]
    input_paths: Vec<PathBuf>,

    /// Output file path. If not specified, output goes to stdout.
    #[arg(short, long)]
    output_file: Option<PathBuf>,

    /// Output format.
    #[arg(short, long, value_parser = clap::value_parser!(OutputFormat), default_value_t = OutputFormat::Dot)]
    format: OutputFormat,
}

#[derive(Debug, Clone, PartialEq, Eq, clap::ValueEnum)]
enum OutputFormat {
    Dot,
    Mermaid,
}

impl std::fmt::Display for OutputFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OutputFormat::Dot => write!(f, "dot"),
            OutputFormat::Mermaid => write!(f, "mermaid"),
        }
    }
}

#[derive(Debug)]
enum Sol2CgError {
    NoSolidityFiles,
    LanguageInitializationError,
    IoError(PathBuf, std::io::Error),
    WalkDirError(walkdir::Error),
    OutputWriteError(PathBuf, std::io::Error),
    StdoutWriteError(std::io::Error),
    MermaidWriteError(String),
}

impl std::error::Error for Sol2CgError {}

impl fmt::Display for Sol2CgError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sol2CgError::NoSolidityFiles => {
                write!(f, "No valid .sol files found in the provided paths.")
            }
            Sol2CgError::LanguageInitializationError => {
                write!(f, "Failed to get Tree-sitter language for Solidity.")
            }
            Sol2CgError::IoError(path, err) => {
                write!(f, "I/O error processing path '{}': {}", path.display(), err)
            }
            Sol2CgError::WalkDirError(err) => write!(f, "Directory traversal error: {}", err),
            Sol2CgError::OutputWriteError(path, err) => write!(
                f,
                "Failed to write output to file '{}': {}",
                path.display(),
                err
            ),
            Sol2CgError::StdoutWriteError(err) => {
                write!(f, "Failed to write output to stdout: {}", err)
            }
            Sol2CgError::MermaidWriteError(err) => {
                write!(f, "Mermaid serialization error: {}", err)
            }
        }
    }
}

impl From<walkdir::Error> for Sol2CgError {
    fn from(err: walkdir::Error) -> Self {
        Sol2CgError::WalkDirError(err)
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // 1. Find all .sol files
    let sol_files = find_solidity_files(&cli.input_paths)?;
    if sol_files.is_empty() {
        bail!(Sol2CgError::NoSolidityFiles);
    }

    // 2. Read and Concatenate all Solidity files
    let mut combined_source = String::new();
    let mut read_errors = Vec::new();

    for sol_file in &sol_files {
        match fs::read_to_string(sol_file) {
            Ok(source) => {
                combined_source.push_str(&source);
                combined_source.push('\n'); // Add newline between files
            }
            Err(e) => {
                let error_msg = format!(
                    "Failed to read file {}: {}",
                    sol_file.display(),
                    Sol2CgError::IoError(sol_file.clone(), e)
                );
                // Error message is stored, but not printed to stderr here.
                read_errors.push(error_msg);
            }
        }
    }

    if !read_errors.is_empty() {
        // Consider logging these errors differently or including them in the final Result error.
        // For now, just bail out as before, but without the eprintln.
        bail!("Errors occurred during file reading.");
    }

    if combined_source.is_empty() {
        bail!("No Solidity source code was successfully read.");
    }

    // 3. Parse the combined source code
    let combined_ast =
        parse_solidity(&combined_source).context("Failed to parse combined Solidity source")?;

    // 4. Generate Call Graph
    let generator =
        CallGraphGenerator::new(&combined_ast).context("Failed to create CallGraphGenerator")?;
    let mut graph = generator
        .generate()
        .context("Failed to generate initial call graph")?;

    // 5. Add Explicit Return Edges

    let solidity_lang = Solidity.get_tree_sitter_language();
    graph
        .add_explicit_return_edges(&combined_source, &combined_ast.tree, &solidity_lang)
        .context("Failed to add explicit return edges")?;

    // 6. Serialize Graph

    let output_string = match cli.format {
        OutputFormat::Dot => graph.to_dot("Solidity Call Graph"),
        OutputFormat::Mermaid => {
            let generator = MermaidGenerator::new();
            let sequence_diagram = generator.to_sequence_diagram(&graph);
            // Use the write_diagram function directly
            sequence_diagram_writer::write_diagram(&sequence_diagram)
        }
    };

    // 6. Write Output
    match cli.output_file {
        Some(ref path) => {
            let mut file = fs::File::create(path)
                .map_err(|e| Sol2CgError::OutputWriteError(path.clone(), e))?;
            file.write_all(output_string.as_bytes())
                .map_err(|e| Sol2CgError::OutputWriteError(path.clone(), e))?;
        }
        None => {
            let mut handle = stdout().lock();
            handle
                .write_all(output_string.as_bytes())
                .map_err(Sol2CgError::StdoutWriteError)?;
            handle.flush().map_err(Sol2CgError::StdoutWriteError)?; // Ensure output is flushed
        }
    }

    Ok(())
}

/// Finds all files with the .sol extension in the given paths (files or directories).
fn find_solidity_files(paths: &[PathBuf]) -> Result<Vec<PathBuf>, Sol2CgError> {
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
        } else if path.is_file() {
            // If it's a file but not .sol, issue a warning or ignore silently.
            // Let's ignore for now.
            // eprintln!("Warning: Skipping non-Solidity file: {}", path.display());
        } else {
            // Handle cases where the path doesn't exist or isn't a file/dir
            return Err(Sol2CgError::IoError(
                path.clone(),
                std::io::Error::new(std::io::ErrorKind::NotFound, "Path not found or invalid"),
            ));
        }
    }
    // Sort for deterministic processing (useful for the "first file" limitation)
    sol_files.sort();
    Ok(sol_files)
}
