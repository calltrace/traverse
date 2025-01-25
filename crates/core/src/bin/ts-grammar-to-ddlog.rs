use clap::Parser;
use std::fs;
use std::path::PathBuf;

use backend::gen_ddlog::DDlogGenerator;
use language::node_types::parse_node_types;

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
        // e.g. /some/dir/tree-sitter-solidity => node_types_path = /some/dir/tree-sitter-solidity/src/node-types.json
        let node_types_path = parser_home.join("src").join("node-types.json");

        let parser_dirname = parser_home
            .file_name()
            .map(|os| os.to_string_lossy().to_string())
            .unwrap_or_else(|| "unknown".to_string());

        // e.g. "tree-sitter-solidity" => "solidity"
        let stripped_name = strip_treesitter_prefix(&parser_dirname);
        // Our .dl file => "solidity.dl"
        let out_dl = format!("{}.dl", stripped_name);

        println!("Processing parser home: {}", parser_home.display());
        println!(" -> node-types: {}", node_types_path.display());
        println!(" -> output dl: {}", out_dl);

        // read and parse JSON
        let json_data = fs::read_to_string(&node_types_path)
            .expect("Could not read node-types.json from parser home");
        let nodes = parse_node_types(&json_data)
            .expect("Failed to parse node-types.json");

        // generate .dl content
        let ddlog = DDlogGenerator::new().generate_ddlog_file(&nodes);

        // write to <stripped_name>.dl
        fs::write(&out_dl, ddlog)
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
