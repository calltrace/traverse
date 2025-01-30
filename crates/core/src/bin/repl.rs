use core::facts::{DDLogCommand, TreeSitterToDDLog};
use std::{
    fs,
    path::{Path, PathBuf},
};

use backend::ddlog_rt::{build_ddlog_crate, generate_rust_project, validate};
use clap::Parser;
use frontend::gen_ir::IrGenerator;
use language::{Language, Solidity};
use reedline::{DefaultPrompt, DefaultValidator, FileBackedHistory, Reedline, Signal};

#[derive(Debug)]
enum ReplError {
    Io(std::io::Error),
    Parse(String),
    IrGeneration(String),
    DdlogGeneration(String),
    DdlogValidation(String),
}

impl std::fmt::Display for ReplError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReplError::Io(err) => write!(f, "IO error: {}", err),
            ReplError::Parse(msg) => write!(f, "Parse error: {}", msg),
            ReplError::IrGeneration(msg) => write!(f, "IR generation error: {}", msg),
            ReplError::DdlogGeneration(msg) => write!(f, "DDlog generation error: {}", msg),
            ReplError::DdlogValidation(msg) => write!(f, "DDlog validation error: {}", msg),
        }
    }
}

impl std::error::Error for ReplError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ReplError::Io(err) => Some(err),
            _ => None,
        }
    }
}

impl From<std::io::Error> for ReplError {
    fn from(err: std::io::Error) -> Self {
        ReplError::Io(err)
    }
}

// Error implementations remain the same...

struct Repl {
    line_editor: Reedline,
    prompt: DefaultPrompt,
    project_name: String,
    ts_grammars: Vec<String>,
    source_file: Box<Path>,
    source_type: SourceType,
}

enum Command {
    Exit,
    Clear,
    History,
    ClearHistory,
    Process(String),
}

impl Repl {
    fn new(
        project_name: String,
        parser_homes: Vec<String>,
        source_file: Box<Path>,
        source_type: SourceType,
    ) -> Result<Self, ReplError> {
        let history = Box::new(
            FileBackedHistory::with_file(5, "history.txt".into())
                .map_err(|e| ReplError::Io(std::io::Error::new(std::io::ErrorKind::Other, e)))?,
        );

        let line_editor = Reedline::create()
            .with_history(history)
            .with_validator(Box::new(DefaultValidator))
            .use_kitty_keyboard_enhancement(true)
            .with_ansi_colors(true);

        Ok(Self {
            line_editor,
            prompt: DefaultPrompt::default(),
            project_name,
            ts_grammars: parser_homes,
            source_file,
            source_type,
        })
    }

    fn parse_source_file(&self) -> Result<Vec<DDLogCommand>, ReplError> {
        let source_code = std::fs::read_to_string(&self.source_file).map_err(ReplError::Io)?;
        let language = self.source_type.to_tree_sitter_language();
        let converter = TreeSitterToDDLog::new(&source_code, &language);
        Ok(converter.extract_commands::<core::facts::InsertCommandFn>(None))
    }

    fn parse_command(input: &str) -> Command {
        match input.trim() {
            "exit" | "logout" => Command::Exit,
            "clear" => Command::Clear,
            "history" => Command::History,
            "clear-history" => Command::ClearHistory,
            other => Command::Process(other.to_string()),
        }
    }

    fn handle_command(&mut self, cmd: Command) -> Result<bool, ReplError> {
        match cmd {
            Command::Exit => Ok(false),
            Command::Clear => {
                self.line_editor.clear_scrollback()?;
                Ok(true)
            }
            Command::History => {
                self.line_editor.print_history()?;
                Ok(true)
            }
            Command::ClearHistory => {
                let hstry = Box::new(self.line_editor.history_mut());
                hstry.clear().map_err(|e| {
                    ReplError::Io(std::io::Error::new(std::io::ErrorKind::Other, e))
                })?;
                Ok(true)
            }
            Command::Process(input) => {
                self.process_input(&input)?;
                Ok(true)
            }
        }
    }

    fn process_input(&self, input: &str) -> Result<(), ReplError> {
        let lval =
            frontend::parser::parse(input).map_err(|e| ReplError::Parse(format!("{:?}", e)))?;

        println!("AST:\n{:#?}\n", lval);

        let dl_ir = IrGenerator::new()
            .with_input_relations(true)
            .lval_to_ir(&lval)
            .map_err(|e| ReplError::IrGeneration(format!("{:?}", e)))?;

        println!("Datalog IR:\n\n{}", dl_ir);

        let ddlog = backend::gen_ddlog::DDlogGenerator::new()
            .with_input_relations(true)
            .with_treesitter_grammars(self.ts_grammars.clone())
            .generate(*dl_ir)
            .map_err(|e| ReplError::DdlogGeneration(format!("{:?}", e)))?;

        println!("\nGenerated DDLog:\n\n{}", ddlog.to_string().trim());

        validate(&ddlog.to_string()).map_err(|e| ReplError::DdlogValidation(e.to_string()))?;

        println!("\nDDLog Validation succeeded");
        let base_dir = PathBuf::from(format!("./{}", self.project_name));
        fs::create_dir_all(&base_dir).expect("Failed to create base directory");

        generate_rust_project(&base_dir, &self.project_name, &ddlog.to_string());

        match build_ddlog_crate(&base_dir, &self.project_name) {
            Ok(_) => println!("Build succeeded"),
            Err(e) => eprintln!("Build failed: {}", e),
        }

        println!(
            "Scaffolded DDlog project '{}' at: {:?}",
            self.project_name, base_dir
        );

        // TODO: run the generated ddlog CLI application with the supplied facts
        // TODO: dump and collect emit output relation

        Ok(())
    }

    fn run(&mut self) -> Result<(), ReplError> {
        let cmds = self.parse_source_file()?;

        for cmd in cmds {
            println!("{}", cmd);
        }

        loop {
            match self.line_editor.read_line(&self.prompt)? {
                Signal::Success(buffer) => {
                    let cmd = Self::parse_command(&buffer);
                    if !self.handle_command(cmd)? {
                        break;
                    }
                }
                Signal::CtrlD | Signal::CtrlC => {
                    println!("\nAborted!");
                    break;
                }
            }
        }
        Ok(())
    }
}

#[derive(Debug, clap::ValueEnum, Clone)]
pub enum SourceType {
    Solidity,
    Mermaid,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short = 'p', long = "project-name", required = true)]
    project_name: String,

    /// Paths to parser home directories
    #[arg(short = 'l', long = "parser-home", value_name = "DIR")]
    parser_homes: Vec<PathBuf>,

    /// Path to the source file to analyze
    #[arg(short = 's', long = "source", value_name = "FILE")]
    source_path: PathBuf,

    /// Type of the source file
    #[arg(short = 't', long = "type", value_enum)]
    source_type: SourceType,
}

impl Args {
    fn validate(&self) -> Result<Vec<String>, ReplError> {
        // Validate that all parser homes exist
        for path in &self.parser_homes {
            if !path.is_dir() {
                return Err(ReplError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("Parser home directory does not exist: {}", path.display()),
                )));
            }
        }

        // Validate that source file exists
        if !self.source_path.exists() {
            return Err(ReplError::Io(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("Source file does not exist: {}", self.source_path.display()),
            )));
        }

        // Convert PathBuf to String for compatibility
        Ok(self
            .parser_homes
            .iter()
            .map(|p| p.to_string_lossy().into_owned())
            .collect())
    }
}

impl SourceType {
    fn to_tree_sitter_language(&self) -> impl Language {
        match self {
            SourceType::Solidity => {
                Solidity
            }
            // TODO: Add Mermaid language support once available
            SourceType::Mermaid => unimplemented!("Mermaid language support not yet implemented"),
        }
    }
}

fn main() -> Result<(), ReplError> {
    // Parse command line arguments
    let args = Args::parse();

    // Validate parser homes and get as strings
    let parser_homes = args.validate()?;

    let mut repl = Repl::new(args.project_name, parser_homes, args.source_path.into(), args.source_type)?;
    repl.run()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_command_parsing() {
        assert!(matches!(Repl::parse_command("exit"), Command::Exit));
        assert!(matches!(Repl::parse_command("clear"), Command::Clear));
        assert!(matches!(
            Repl::parse_command("select * from table"),
            Command::Process(s) if s == "select * from table"
        ));
    }

    #[test]
    fn test_process_valid_input() {
        let repl = Repl::new(vec![]).unwrap();
        let result = repl.process_input("x := 1");
        assert!(result.is_ok());
    }

    #[test]
    fn test_process_invalid_input() {
        let repl = Repl::new(vec![]).unwrap();
        let result = repl.process_input("invalid := ::");
        assert!(matches!(result, Err(ReplError::Parse(_))));
    }
}
