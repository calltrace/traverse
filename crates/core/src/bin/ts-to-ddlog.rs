use language::Language;
use clap::{Parser};
use std::collections::HashSet;
use std::fs;

#[derive(Parser, Debug)]
#[command(name = "ts-to-ddlog")]
#[command(about = "Convert Tree-sitter AST to DDLog commands", long_about = None)]
struct Args {
    /// Path to the input source code file
    #[arg(short, long)]
    input: String,

    /// Path to the output file
    #[arg(short, long)]
    output: String,
}

fn main() {
    let args = Args::parse();

    let source_code =
        fs::read_to_string(&args.input).expect("Failed to read the input source code file.");

    let valid_types: HashSet<String> = vec![
        "source_file".to_string(),
        "function_item".to_string(),
        "identifier".to_string(),
        "block".to_string(),
        "let_declaration".to_string(),
        "number_literal".to_string(),
    ]
    .into_iter()
    .collect();

    // Initialize the converter
    let language = language::Solidity.get_tree_sitter_language();
    let converter = core::facts::TreeSitterToDDLog::new(&source_code, language, valid_types);

    // Extract DDLog commands
    let commands = converter.extract_commands();

    // Save the commands to the output file
    converter.save_to_file(&commands, &args.output);
}
