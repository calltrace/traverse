use std::path::PathBuf;

use clap::Parser;
use frontend::gen_ir::IrGenerator;
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
    ts_grammars: Vec<String>,
}

enum Command {
    Exit,
    Clear,
    History,
    ClearHistory,
    Process(String),
}

impl Repl {
    fn new(parser_homes: Vec<String>) -> Result<Self, ReplError> {
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
            ts_grammars: parser_homes,
        })
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
            .with_treesitter_grammars(&self.ts_grammars)
            .generate(*dl_ir)
            .map_err(|e| ReplError::DdlogGeneration(format!("{:?}", e)))?;

        println!("\nGenerated DDLog:\n\n{}", ddlog.to_string().trim());

        backend::ddlog_rt::validate(&ddlog.to_string())
            .map_err(|e| ReplError::DdlogValidation(e.to_string()))?;

        println!("\nDDLog Validation succeeded");
        Ok(())
    }

    fn run(&mut self) -> Result<(), ReplError> {
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

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Paths to parser home directories
    #[arg(short = 'p', long = "parser-home", value_name = "DIR")]
    parser_homes: Vec<PathBuf>,
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

        // Convert PathBuf to String for compatibility
        Ok(self
            .parser_homes
            .iter()
            .map(|p| p.to_string_lossy().into_owned())
            .collect())
    }
}

fn main() -> Result<(), ReplError> {
    // Parse command line arguments
    let args = Args::parse();

    // Validate parser homes and get as strings
    let parser_homes = args.validate()?;

    let mut repl = Repl::new(parser_homes)?;
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
