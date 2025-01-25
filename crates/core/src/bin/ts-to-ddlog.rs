use clap::Parser;
use core::facts::{to_pascal_case, DDLogCommand};
use std::fs;
use tree_sitter::Node;

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
    // Initialize the converter
    let language = language::Solidity;
    let converter = core::facts::TreeSitterToDDLog::new(&source_code, &language);

    //
    // Extract DDLog commands

    let create_insert_command = |source: &str, node: &Node, id: usize, parent_id: Option<usize>| {
        let fact_type = to_pascal_case(node.kind());
        if node.child_count() > 0 {
            DDLogCommand::Insert {
                fact_type,
                id,
                parent_id: parent_id.unwrap_or(0),
                field_name: None,
            }
            .into()
        } else {
            let field_name = source[node.start_byte()..node.end_byte()].to_string();
            DDLogCommand::Insert {
                fact_type,
                id,
                parent_id: parent_id.unwrap_or(0),
                field_name: Some(field_name),
            }
            .into()
        }
    };
    let commands = converter.extract_commands(create_insert_command);

    // Save the commands to the output file
    converter.save_to_file(&commands, &args.output);
}
