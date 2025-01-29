use clap::Parser;
use ir::IRProgram;
use std::fs;
use std::path::PathBuf;

use backend::gen_ddlog::DDlogGenerator;

#[derive(Parser)]
#[command(name = "gen_datalog")]
#[command(about = "Generate .dl files from multiple tree-sitter parser homes, plus an aggregator.")]
struct Cli {
    /// One or more tree-sitter parser home paths
    #[arg(required = true)]
    parser_homes: Vec<PathBuf>,

    /// Aggregator file to create
    #[arg(long, short = 'A', default_value = "aggregated.dl")]
    aggregate: PathBuf,
}

fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    // We'll accumulate lines like: `import solidity`
    // (no file extension, as requested).
    let mut aggregator_imports = String::new();

    for parser_home in &cli.parser_homes {
        let parser_dirname = parser_home
            .file_name()
            .map(|os| os.to_string_lossy().to_string())
            .unwrap_or_else(|| "unknown".to_string());

        println!("Processing parser home: {}", parser_dirname);

        // e.g. "tree-sitter-solidity" => "solidity"
        let stripped_name = strip_treesitter_prefix(&parser_dirname);
        // Our .dl file => "solidity.dl"
        let out_dl = format!("{}.dl", stripped_name);

        println!("Processing parser home: {}", parser_home.display());
        println!(" -> output dl: {}", out_dl);

        // Create program, add nodes, and generate DDlog
        let prog = IRProgram::new();
        let ddlog_dump = DDlogGenerator::new()
            .with_treesitter_grammar(parser_home.clone())
            .generate(prog)
            .map(|gen| {
                gen.to_string()
            }).map_err(|e| {
                std::io::Error::new(std::io::ErrorKind::Other, format!("Error generating DDlog {:?}", e))
            })?;

        // write to <stripped_name>.dl
        fs::write(&out_dl, ddlog_dump)
            .expect("Failed to write .dl file");

        // aggregator line => e.g. `import solidity`
        aggregator_imports.push_str(&format!("import {}\n", stripped_name));
    }

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
