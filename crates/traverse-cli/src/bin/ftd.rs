/// Facts Tree Dump CLI (ftd)
///
/// This module implements a command-line interface for parsing source files
/// using the facts module and dumping the corresponding tree representation.
/// It provides a simple way to visualize the tree-sitter parse tree of a source file.
///
/// The CLI follows a simple workflow:
/// 1. Parse and validate command-line arguments
/// 2. Read the source file
/// 3. Parse the source file using the appropriate language
/// 4. Dump the tree representation to the specified output
///
use std::fs;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process;

use clap::Parser;
use language::{Language, Mermaid, Solidity};
use backend::facts::{FactNodeTreeDumper, TreeSitterToDDLog};

#[derive(Debug, clap::ValueEnum, Clone)]
pub enum SourceType {
    Solidity,
    Mermaid,
}

impl SourceType {
    fn to_tree_sitter_language(&self) -> impl Language {
        match self {
            SourceType::Solidity => Solidity,
            SourceType::Mermaid => Mermaid,
        }
    }
}

#[derive(Parser, Debug)]
#[command(
    name = "ftd",
    author = "Traverse Team",
    version,
    about = "Facts Tree Dump - Parse source files and dump the tree representation",
    long_about = None
)]
struct Args {
    /// Path to the source file to parse
    #[arg(short = 's', long = "source", value_name = "FILE")]
    source_path: PathBuf,

    /// Type of the source file
    #[arg(short = 't', long = "type", value_enum)]
    source_type: SourceType,

    /// Output file for the tree dump (defaults to stdout if not provided)
    #[arg(short = 'o', long = "output", value_name = "FILE")]
    output_path: Option<PathBuf>,

    /// Enable verbose output
    #[arg(short = 'v', long = "verbose", default_value = "false")]
    verbose: bool,
    
    /// Exclude specific node types from the tree (comma-separated list)
    #[arg(short = 'e', long = "exclude", value_name = "TYPES")]
    exclude: Option<String>,
}

impl Args {
    fn validate(&self) -> Result<(), io::Error> {
        // Validate that source file exists
        if !self.source_path.exists() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("Source file does not exist: {}", self.source_path.display()),
            ));
        }

        // If output path is provided, check if its directory exists
        if let Some(output_path) = &self.output_path {
            if let Some(parent) = output_path.parent() {
                if !parent.is_dir() {
                    return Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("Output directory does not exist: {}", parent.display()),
                    ));
                }
            }
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

    // Read the source file
    let mut source_content = String::new();
    match fs::File::open(&args.source_path) {
        Ok(mut file) => {
            if let Err(err) = file.read_to_string(&mut source_content) {
                eprintln!(
                    "Error reading source file {}: {}",
                    &args.source_path.display(),
                    err
                );
                process::exit(1);
            }
        }
        Err(err) => {
            eprintln!(
                "Error opening source file {}: {}",
                args.source_path.display(),
                err
            );
            process::exit(1);
        }
    }

    // Get the appropriate language for the source type
    let language = args.source_type.to_tree_sitter_language();

    // Create the TreeSitterToDDLog parser
    let mut parser = TreeSitterToDDLog::new(&source_content, &language);
    
    // Apply exclusions if specified
    if let Some(exclude_str) = &args.exclude {
        use std::collections::HashSet;
        let excluded_relations: HashSet<String> = exclude_str
            .split(',')
            .map(|s| s.trim().to_string())
            .collect();
        
        if args.verbose {
            println!("Excluding node types: {:?}", excluded_relations);
        }
        
        parser = parser.with_excluded_relations(excluded_relations);
    }

    // Create the tree dumper
    let tree_dumper = parser.create_tree_dumper();
    
    // Get the tree representation
    let tree_dump = tree_dumper.dump_tree();
    
    if args.verbose {
        println!("Successfully parsed source file: {}", args.source_path.display());
        println!("Tree representation generated");
    }

    // Output the tree representation
    match &args.output_path {
        Some(output_path) => {
            // Write to file
            if let Err(err) = fs::write(output_path, &tree_dump) {
                eprintln!("Error writing to output file: {}", err);
                process::exit(1);
            }
            
            if args.verbose {
                println!("Tree dump written to: {}", output_path.display());
            }
        }
        None => {
            // Write to stdout
            io::stdout().write_all(tree_dump.as_bytes()).unwrap();
        }
    }
    
    if args.verbose {
        println!("Facts tree dump completed successfully!");
    }
}
