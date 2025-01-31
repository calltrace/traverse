
use clap::Parser;
use frontend::formatter::Formatter;
use frontend::parser;
use frontend::syntax::SyntaxTheme;
use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;

#[derive(Parser)]
#[command(author, version, about = "Format DSL code", long_about = None)]
struct Cli {
    /// Input file (reads from stdin if not provided)
    #[arg(value_name = "FILE")]
    input: Option<PathBuf>,

    /// Output file (writes to stdout if not provided) 
    #[arg(short, long, value_name = "FILE")]
    output: Option<PathBuf>,

    /// Number of spaces for indentation
    #[arg(short, long, default_value_t = 2)]
    indent: usize,

    /// Check if file is formatted without modifying it
    #[arg(short, long)]
    check: bool,
}

#[derive(Clone, Debug)]
enum OutputFormat {
    Ansi,
}

impl std::str::FromStr for OutputFormat {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "ansi" => Ok(OutputFormat::Ansi),
            _ => Err("Invalid output format. Use 'ansi'".to_string()),
        }
    }
}


fn main() -> io::Result<()> {
    let cli = Cli::parse();
    
    // Read input
    let input = match cli.input {
        Some(path) => fs::read_to_string(path)?,
        None => {
            let mut buffer = String::new();
            io::stdin().read_to_string(&mut buffer)?;
            buffer
        }
    };

    // Parse and format
    let parsed = match parser::parse(&input) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            std::process::exit(1);
        }
    };

    let mut formatter = Formatter::new(cli.indent).with_syntax_highlighting(Some(SyntaxTheme::default()));
    let formatted = formatter.format_with_highlighting(&parsed);

    if cli.check {
        if formatted == input {
            println!("File is properly formatted.");
            std::process::exit(0);
        } else {
            println!("File is not properly formatted.");
            std::process::exit(1);
        }
    }

    // Write output
    match cli.output {
        Some(path) => fs::write(path, formatted)?,
        None => println!("{}", formatted),
    }

    Ok(())
}
