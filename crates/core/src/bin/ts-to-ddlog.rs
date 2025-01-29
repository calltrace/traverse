use clap::Parser;
use core::facts::{DDLogCommand, TreeSitterToDDLog};
use std::io;
use std::path::PathBuf;
use tree_sitter::Node;

#[derive(Parser, Debug)]
#[command(name = "ts-to-ddlog")]
#[command(about = "Convert Tree-sitter AST to DDLog commands", long_about = None)]
struct Args {
    /// Path to the input source code file
    #[arg(short, long)]
    input: PathBuf,

    /// Path to the output file (stdout if not specified)
    #[arg(short, long)]
    output: Option<PathBuf>,
}


fn run() -> io::Result<()> {
    let args = Args::parse();
    let source_code = std::fs::read_to_string(&args.input)?;
    let language = language::Solidity;
    let converter = TreeSitterToDDLog::new(&source_code, &language);
    let commands = converter.extract_commands::<core::facts::InsertCommandFn>(None);

    match args.output {
        Some(path) => converter.save_to_file(&commands, path.to_str().unwrap()),
        None => converter.save_to_stdout(&commands),
    }

    Ok(())
}

fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
