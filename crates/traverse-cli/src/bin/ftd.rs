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

use backend::facts::{FactNodeTreeDumper, TreeSitterToDDLog};
use clap::Parser as ClapParser;
use language::{Language, Mermaid, Solidity};
use tree_sitter::{Node, Parser, Tree};

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

#[derive(ClapParser, Debug)]
#[command(
    name = "ftd",
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

    /// Dump the native Tree-sitter tree (including anonymous nodes)
    #[arg(long = "raw-tree", default_value = "false")]
    raw_tree: bool,
}

impl Args {
    fn validate(&self) -> Result<(), io::Error> {
        if !self.source_path.exists() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("Source file does not exist: {}", self.source_path.display()),
            ));
        }

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

/// Dumps a Tree-sitter tree to a string, including both named and anonymous nodes
fn dump_raw_tree_sitter_tree(tree: &Tree, source: &str) -> String {
    let mut result = String::new();
    let root_node = tree.root_node();

    dump_node(&root_node, source, &mut result, "", true);

    result
}

/// Recursively dumps a Tree-sitter node and its children
fn dump_node(node: &Node, source: &str, result: &mut String, prefix: &str, is_last: bool) {
    // Add the appropriate prefix for this node
    if is_last {
        result.push_str(&format!("{}└── ", prefix));
    } else {
        result.push_str(&format!("{}├── ", prefix));
    }

    // Add node information
    let node_type = node.kind();
    let is_named = node.is_named();
    let start_position = node.start_position();
    let end_position = node.end_position();

    // Get node text from source
    let node_text = if node.byte_range().len() <= 40 {
        source[node.byte_range()].replace('\n', "\\n")
    } else {
        format!(
            "{}...",
            &source[node.byte_range()][..37].replace('\n', "\\n")
        )
    };

    // Format node information
    result.push_str(&format!(
        "{}{} [{},{}]-[{},{}] '{}'",
        if is_named { "" } else { "(anon) " },
        node_type,
        start_position.row,
        start_position.column,
        end_position.row,
        end_position.column,
        node_text
    ));
    result.push('\n');

    // Create the new prefix for child nodes
    let new_prefix = if is_last {
        format!("{}    ", prefix)
    } else {
        format!("{}│   ", prefix)
    };

    // Get all children
    let child_count = node.child_count();
    for i in 0..child_count {
        if let Some(child) = node.child(i) {
            let is_last_child = i == child_count - 1;
            dump_node(&child, source, result, &new_prefix, is_last_child);
        }
    }
}

fn main() {
    let args = Args::parse();

    if let Err(err) = args.validate() {
        eprintln!("Error: {}", err);
        process::exit(1);
    }

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

    let language = args.source_type.to_tree_sitter_language();

    let tree_dump = if args.raw_tree {
        // Parse with native Tree-sitter
        let tree = language
            .parse(&source_content)
            .expect("Failed to parse source code");

        if args.verbose {
            println!("Dumping raw Tree-sitter tree (including anonymous nodes)");
        }

        dump_raw_tree_sitter_tree(&tree, &source_content)
    } else {
        // Use the existing FactNodeTreeDumper
        let mut parser = TreeSitterToDDLog::new(&source_content, &language);

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

        let tree_dumper = parser.create_tree_dumper();
        tree_dumper.dump_tree()
    };

    if args.verbose {
        println!(
            "Successfully parsed source file: {}",
            args.source_path.display()
        );
        println!("Tree representation generated");
    }

    match &args.output_path {
        Some(output_path) => {
            if let Err(err) = fs::write(output_path, &tree_dump) {
                eprintln!("Error writing to output file: {}", err);
                process::exit(1);
            }

            if args.verbose {
                println!("Tree dump written to: {}", output_path.display());
            }
        }
        None => {
            io::stdout().write_all(tree_dump.as_bytes()).unwrap();
        }
    }

    if args.verbose {
        println!("Tree dump completed successfully!");
    }
}
