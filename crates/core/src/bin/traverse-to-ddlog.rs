use clap::Parser;
use std::fs;
use std::path::PathBuf;

use core::scaffold;

#[derive(Parser, Debug)]
#[command(name = "traverse-to-ddlog")]
struct Args {
    #[arg(short = 't', long = "treesitter-dirs", required = true, num_args = 1..)]
    treesitter_dirs: Vec<PathBuf>,
    #[arg(short = 'd', long = "dsl-file", required = true)]
    dsl_file: PathBuf,
    #[arg(short = 'p', long = "project-name", required = true)]
    project_name: String,
}

fn main() {
    let cli = Args::parse();
    let ddlog_input_relations = scaffold::read_treesitter_grammars(&cli.treesitter_dirs);
    let dsl_source = fs::read_to_string(&cli.dsl_file).expect("Failed to read DSL file");
    let parsed_dsl_ast = scaffold::parse_custom_dsl(&dsl_source);
    let ddlog_rules = scaffold::dsl_to_ddlog(&parsed_dsl_ast);

    let combined_dl_content = format!(
        "// --- BEGIN Tree-Sitter Input Relations ---\n\n{}\n\n// --- BEGIN DSL-derived Rules ---\n\n{}",
        ddlog_input_relations, ddlog_rules
    );

    let base_dir = PathBuf::from(format!("./{}", cli.project_name));
    fs::create_dir_all(&base_dir).expect("Failed to create base directory");

    scaffold::generate_rust_project(&base_dir, &cli.project_name, &combined_dl_content);

    match scaffold::build_ddlog_crate(&base_dir, &cli.project_name) {
        Ok(_) => println!("Build succeeded"),
        Err(e) => eprintln!("Build failed: {}", e),
    }

    println!("Scaffolded DDlog project '{}' at: {:?}", cli.project_name, base_dir);
}
