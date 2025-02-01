
use clap::Parser;
use std::fs;
use std::io::{self, Read, Write};
use std::path::PathBuf;

/// Format SSA IR programs with optional syntax highlighting
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input file (reads from stdin if not specified)
    #[arg(short = 'i', long)]
    input: Option<PathBuf>,

    /// Output file (writes to stdout if not specified)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Enable syntax highlighting
    #[arg(short = 'c', long)]
    highlight: bool,

    /// Number of spaces for indentation
    #[arg(short = 'n', long, default_value_t = 2)]
    indent: usize,

    /// Use tabs instead of spaces for indentation
    #[arg(short, long)]
    tabs: bool,
}

fn main() -> io::Result<()> {
    let args = Args::parse();

    // Read input
    let input = match args.input {
        Some(path) => fs::read_to_string(path)?,
        None => {
            let mut buffer = String::new();
            io::stdin().read_to_string(&mut buffer)?;
            buffer
        }
    };

    // Parse and format
    let result = match ir::format_program(&input, args.highlight, args.indent, args.tabs) {
        Ok(formatted) => formatted,
        Err(e) => {
            eprintln!("Error formatting program: {}", e);
            return Ok(());
        }
    };

    // Write output
    match args.output {
        Some(path) => fs::write(path, result)?,
        None => {
            io::stdout().write_all(result.as_bytes())?;
            io::stdout().write_all(b"\n")?;
        }
    }

    Ok(())
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_simple_program() {
        let input = r#"
relations {
test_relation(x: Number, y: String): Input;
}

rules {
}
"#;

        let result = ir::format_program(input, false, 4, false).unwrap();
        assert!(result.contains("test_relation"));
        assert!(result.contains("Number"));
        assert!(result.contains("String"));
    }

    #[test]
    fn test_format_with_highlighting() {
        let input = r#"
relations {
test_relation(x: Number): Input;
}

rules {
}
"#;

        let result = format_program(input, true, 4, false).unwrap();
        assert!(result.contains("\x1b[")); // Contains ANSI escape codes
    }

    #[test]
    fn test_format_invalid_input() {
        let input = "invalid { program";
        assert!(format_program(input, false, 4, false).is_err());
    }
}
