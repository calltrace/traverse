use clap::Parser;
use ir::IRProgram;
use std::fs;
use std::path::PathBuf;

use backend::gen_ddlog::DDlogGenerator;

#[derive(Parser)]
#[command(name = "gen_datalog")]
#[command(about = "Generate .dl files from input and intermediate language tree-sitter parsers")]
struct Cli {
    /// Path to the input language tree-sitter parser home
    #[arg(long = "input", short = 'i', required = true)]
    input_parser: PathBuf,

    /// Path to the intermediate language tree-sitter parser home
    #[arg(long = "intermediate", short = 'n', required = true)]
    intermediate_parser: PathBuf,

    /// Aggregator file to create
    #[arg(long, short = 'A', default_value = "aggregated.dl")]
    aggregate: PathBuf,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();
    let mut aggregator_imports = String::new();

    let input_parser_dirname = cli
        .input_parser
        .file_name()
        .map(|os| os.to_string_lossy().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    println!("Processing input parser home: {}", input_parser_dirname);

    // e.g. "tree-sitter-solidity" => "solidity"
    let stripped_name = strip_treesitter_prefix(&input_parser_dirname);
    // Our .dl file => "solidity.dl"
    let out_dl = format!("{}.dl", stripped_name);

    println!(
        "Processing input parser home: {}",
        cli.input_parser.display()
    );
    println!(" -> output dl: {}", out_dl);

    let intermediate_parser_dirname = cli
        .intermediate_parser
        .file_name()
        .map(|os| os.to_string_lossy().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    println!(
        "Processing intermediate parser home: {}",
        intermediate_parser_dirname
    );

    let stripped_name = strip_treesitter_prefix(&intermediate_parser_dirname);

    let out_dl = format!("{}.dl", stripped_name);

    // Create program, add nodes, and generate DDlog
    let prog = IRProgram::new();
    let ddlog_dump = DDlogGenerator::new()
        .with_input_treesitter_grammar(input_parser_dirname.into())
        .with_intermediate_treesitter_grammar(intermediate_parser_dirname.into())
        .generate(prog)
        .map(|gen| gen.to_string())
        .map_err(|e| {
            std::io::Error::new(
                std::io::ErrorKind::Other,
                format!("Error generating DDlog {:?}", e),
            )
        })?;

    // write to <stripped_name>.dl
    fs::write(&out_dl, ddlog_dump).expect("Failed to write .dl file");

    // aggregator line => e.g. `import solidity`
    aggregator_imports.push_str(&format!("import {}\n", stripped_name));

    // final aggregator file
    let aggregator_contents = format!(
        "// Aggregator file: imports all generated .dl files by base name.\n\
           // No file extension is used in these imports.\n\
           \n\
           {}",
        aggregator_imports
    );

    fs::write(&cli.aggregate, aggregator_contents)?;
    println!("Aggregator written to {}", cli.aggregate.display());

    Ok(())
}

/// e.g. "tree-sitter-solidity" => "solidity"
fn strip_treesitter_prefix(name: &str) -> String {
    if let Some(stripped) = name.strip_prefix("tree-sitter-") {
        stripped.to_string()
    } else {
        name.to_string()
    }
}
