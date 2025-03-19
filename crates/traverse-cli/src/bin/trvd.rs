/// Traverse DDlog Command Generator CLI (trvd)
///
/// This module implements a command-line interface for generating DDlog commands from source files.
/// It parses source files using tree-sitter, converts them to facts, and outputs DDlog commands
/// that can be used as input to a DDlog application.
///
/// The CLI follows a standard workflow:
/// 1. Parse and validate command-line arguments
/// 2. Read input source files
/// 3. Parse the source files with tree-sitter
/// 4. Convert the parsed trees to DDlog commands
/// 5. Output the commands to a file or stdout
///
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use backend::facts::{DDLogCommand, MultiSourceTreeSitterToDDLog, SourceItem};
use clap::Parser;
use language::{Language, Solidity};
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

    fn file_extension(&self) -> &'static str {
        match self {
            SourceType::Solidity => ".sol",
            SourceType::Mermaid => ".mmd",
        }
    }
}

#[derive(Parser, Debug)]
#[command(
    name = "trvd",
    version,
    about = "Traverse DDlog Command Generator - Convert source files to DDlog commands",
    long_about = None
)]
struct Args {
    #[arg(
        short = 's',
        long = "source",
        value_name = "FILE_OR_DIR",
        required = true
    )]
    source: Vec<PathBuf>,

    #[arg(long = "recursive", default_value = "false")]
    recursive: bool,

    #[arg(short = 't', long = "type", value_enum)]
    source_type: SourceType,

    #[arg(short = 'o', long = "output", value_name = "FILE")]
    output_path: Option<PathBuf>,

    #[arg(short = 'e', long = "exclude", value_name = "RELATION")]
    excluded_relations: Vec<String>,

    #[arg(long = "trace", default_value = "false")]
    enable_tracing: bool,
}

#[derive(Debug)]
enum CommandError {
    Io(std::io::Error),
    Parse(String),
}

impl std::fmt::Display for CommandError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandError::Io(err) => write!(f, "IO error: {}", err),
            CommandError::Parse(msg) => write!(f, "Parse error: {}", msg),
        }
    }
}

impl From<std::io::Error> for CommandError {
    fn from(err: std::io::Error) -> Self {
        CommandError::Io(err)
    }
}

impl Args {
    fn validate(&self) -> Result<(), CommandError> {
        // Validate that all source paths exist
        for path in &self.source {
            if !path.exists() {
                return Err(CommandError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("Source path does not exist: {}", path.display()),
                )));
            }
        }

        Ok(())
    }
}

fn collect_files_from_directory(
    dir: &Path,
    extension: &str,
    recursive: bool,
) -> Result<Vec<PathBuf>, CommandError> {
    let mut files = Vec::new();

    let entries = fs::read_dir(dir)?;
    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().map_or(false, |ext| ext == &extension[1..]) {
            files.push(path);
        } else if recursive && path.is_dir() {
            let mut subdir_files = collect_files_from_directory(&path, extension, recursive)?;
            files.append(&mut subdir_files);
        }
    }

    Ok(files)
}

fn collect_source_files(
    sources: &[PathBuf],
    file_extension: &str,
    recursive: bool,
) -> Result<Vec<PathBuf>, CommandError> {
    let mut all_files = Vec::new();

    for source in sources {
        if source.is_file() {
            all_files.push(source.clone());
        } else if source.is_dir() {
            match collect_files_from_directory(source, file_extension, recursive) {
                Ok(mut files) => {
                    if files.is_empty() {
                        eprintln!(
                            "Warning: No {}*{} files found in {}{}",
                            if recursive { "recursive " } else { "" },
                            file_extension,
                            source.display(),
                            if recursive {
                                " (including subdirectories)"
                            } else {
                                ""
                            }
                        );
                    } else {
                        all_files.append(&mut files);
                    }
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }
    }

    Ok(all_files)
}

fn process_source_files(
    source_files: &[PathBuf],
    language: &impl Language,
    excluded_relations: &HashSet<String>,
) -> Result<Vec<DDLogCommand>, CommandError> {
    // Read all source files into memory first
    let mut source_codes = Vec::new();
    let mut file_names = Vec::new();

    for path in source_files {
        let source_code = fs::read_to_string(path).map_err(|e| {
            CommandError::Io(std::io::Error::new(
                e.kind(),
                format!("{}: {}", path.display(), e),
            ))
        })?;

        source_codes.push(source_code);
        file_names.push(path.to_string_lossy().to_string());
    }

    let source_items: Vec<_> = source_codes
        .iter()
        .zip(file_names.iter())
        .map(|(code, name)| SourceItem::with_name(code, name.clone()))
        .collect();

    let converter = MultiSourceTreeSitterToDDLog::new(source_items, language)
        .with_excluded_relations(excluded_relations.clone());

    Ok(converter.extract_commands_with_fact_nodes(None))
}

fn main() {
    // Parse command line arguments
    let args = Args::parse();

    // Setup tracing if enabled
    if args.enable_tracing {
        tracing_subscriber::fmt::init();
    }

    if let Err(err) = args.validate() {
        eprintln!("Error: {}", err);
        process::exit(1);
    }

    let language = args.source_type.to_tree_sitter_language();
    let file_extension = args.source_type.file_extension();

    let excluded_relations: HashSet<String> = args
        .excluded_relations
        .iter()
        .cloned()
        .chain(std::iter::once("String".to_string())) // Always exclude String relation
        .collect();

    let source_files = match collect_source_files(&args.source, file_extension, args.recursive) {
        Ok(files) => files,
        Err(err) => {
            eprintln!("Error collecting source files: {}", err);
            process::exit(1);
        }
    };

    if source_files.is_empty() {
        eprintln!("Error: No source files to process");
        process::exit(1);
    }

    let commands = match process_source_files(&source_files, &language, &excluded_relations) {
        Ok(cmds) => cmds,
        Err(err) => {
            eprintln!("Error processing source files: {}", err);
            process::exit(1);
        }
    };

    let ddlog_commands = commands
        .iter()
        .map(|cmd| cmd.to_string())
        .collect::<Vec<_>>()
        .join(
            "
",
        );

    if let Some(output_path) = &args.output_path {
        if let Err(err) = fs::write(output_path, &ddlog_commands) {
            eprintln!(
                "Error writing to output file {}: {}",
                output_path.display(),
                err
            );
            process::exit(1);
        }
        println!("DDlog commands written to: {}", output_path.display());
    } else {
        println!("{}", ddlog_commands);
    }
}
