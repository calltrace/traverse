use std::{
    fs,
    path::{Path, PathBuf},
};

use clap::Parser;
use compiler::compile::{Compiler, CompilerError, ExecutionResult};
use language::{Language, Solidity};
use reedline::{DefaultPrompt, DefaultValidator, FileBackedHistory, Reedline, Signal};
use std::io::Write;

#[derive(Debug)]
enum ReplError {
    Io(std::io::Error),
    Compiler(CompilerError),
    Other(String),
}

impl std::fmt::Display for ReplError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReplError::Io(err) => writeln!(f, "IO error: {}", err),
            ReplError::Compiler(err) => writeln!(f, "Compiler error: {}", err),
            ReplError::Other(msg) => writeln!(f, "Error: {}", msg),
        }
    }
}

impl std::error::Error for ReplError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            ReplError::Io(err) => Some(err),
            ReplError::Compiler(err) => Some(err),
            ReplError::Other(_) => None,
        }
    }
}

impl From<std::io::Error> for ReplError {
    fn from(err: std::io::Error) -> Self {
        ReplError::Io(err)
    }
}

impl From<CompilerError> for ReplError {
    fn from(err: CompilerError) -> Self {
        ReplError::Compiler(err)
    }
}

struct Repl {
    line_editor: Reedline,
    prompt: DefaultPrompt,
    project_name: String,
    input_ts_grammars: Vec<PathBuf>,
    intermediate_ts_grammars: Vec<PathBuf>,
    source_file: Box<Path>,
    source_type: SourceType,
    no_execute: bool,
    no_hydrate: bool,
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
        input_parser_homes: Vec<PathBuf>,
        intermediate_parser_homes: Vec<PathBuf>,
        source_file: Box<Path>,
        source_type: SourceType,
        no_execute: bool,
        no_hydrate: bool,
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
            input_ts_grammars: input_parser_homes,
            intermediate_ts_grammars: intermediate_parser_homes,
            source_file,
            source_type,
            no_execute,
            no_hydrate,
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
        // Create a compiler instance with our configuration
        let compiler = Compiler::new()
            .with_project_name(self.project_name.clone())
            .with_input_ts_grammars(self.input_ts_grammars.clone())
            .with_intermediate_ts_grammars(self.intermediate_ts_grammars.clone())
            .with_source_file(self.source_file.clone());

        // Apply execution and hydration settings
        let compiler = if self.no_execute {
            compiler.without_execution()
        } else {
            compiler
        };

        let compiler = if self.no_hydrate {
            compiler.without_hydration()
        } else {
            compiler
        };

        // Get the appropriate language for the source type
        let language = self.source_type.to_tree_sitter_language();

        // Compile the input
        let result = compiler.compile(input, &language)?;

        // Display the results
        self.display_compilation_result(&result)?;

        Ok(())
    }

    fn display_compilation_result(&self, result: &ExecutionResult) -> Result<(), ReplError> {
        // Display DSL
        println!(
            "DSL:

{}
",
            result.compilation.dsl
        );

        // Display IR
        println!(
            "IR:

{}",
            result.compilation.ir
        );

        // Display DDlog
        let re = regex::Regex::new(r"^(?:input|output|relation)").unwrap();
        let filtered_ddlog = result.compilation.ddlog.lines().collect::<Vec<_>>().join(
            "
",
        );

        println!(
            "
Generated DDLog (no relations for brevity):{}",
            filtered_ddlog
        );

        println!(
            "
DDLog Validation succeeded"
        );

        // If execution was performed, display the results
        if let Some(ddlog_output) = &result.ddlog_output {
            println!("Ingesting source file");

            // Save commands to dump.txt for debugging
            let mut file = fs::File::create("dump.txt").expect("Failed to create dump file");
            writeln!(file, "{}", ddlog_output).expect("Failed to write to dump file");

            println!("Dumped output to dump.txt");
            println!("DDlog project executed successfully");

            // Display DDlog output
            println!("{}", ddlog_output);

            // If hydration was performed, display the results
            if let Some(hydrated_output) = &result.hydrated_output {
                println!("Hydrating DDlog output...");

                // Save the hydrated output to a file
                let hydrated_file_path = format!("{}_hydrated.txt", self.project_name);
                fs::write(&hydrated_file_path, hydrated_output).map_err(|e| ReplError::Io(e))?;

                println!("Hydrated output saved to: {}", hydrated_file_path);

                // Print a preview of the hydrated output
                let preview_lines: Vec<&str> = hydrated_output.lines().take(20).collect();
                if !preview_lines.is_empty() {
                    println!(
                        "
Hydrated output preview (first 20 lines):"
                    );
                    for line in preview_lines {
                        println!("{}", line);
                    }

                    if hydrated_output.lines().count() > 20 {
                        println!("... (more lines in the output file)");
                    }
                } else {
                    println!("No hydrated output generated.");
                }
            } else {
                println!("Skipping hydration (--no-hydrate was specified)");
            }
        } else {
            println!("Skipping DDlog execution (--no-execute was specified)");
        }

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
                    println!(
                        "
Aborted!"
                    );
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
    #[arg(short = 'i', long = "input-parser-home", value_name = "DIR")]
    input_parser_homes: Vec<PathBuf>,

    #[arg(short = 'n', long = "intermediate-parser-home", value_name = "DIR")]
    intermediate_parser_homes: Vec<PathBuf>,

    /// Path to the source file to analyze
    #[arg(short = 's', long = "source", value_name = "FILE")]
    source_path: PathBuf,

    /// Type of the source file
    #[arg(short = 't', long = "type", value_enum)]
    source_type: SourceType,

    /// Skip executing the generated DDlog script
    #[arg(long = "no-execute", default_value = "false")]
    no_execute: bool,

    /// Skip hydrating the DDlog output
    #[arg(long = "no-hydrate", default_value = "false")]
    no_hydrate: bool,
}

impl Args {
    fn validate(&self) -> Result<(), ReplError> {
        // Validate that all parser homes exist
        for path in &self.input_parser_homes {
            if !path.is_dir() {
                return Err(ReplError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "Input Parser home directory does not exist: {}",
                        path.display()
                    ),
                )));
            }
        }

        for path in &self.intermediate_parser_homes {
            if !path.is_dir() {
                return Err(ReplError::Io(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!(
                        "Intermediate Parser home directory does not exist: {}",
                        path.display()
                    ),
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

        Ok(())
    }
}

impl SourceType {
    fn to_tree_sitter_language(&self) -> impl Language {
        match self {
            SourceType::Solidity => Solidity,
            // TODO: Add Mermaid language support once available
            SourceType::Mermaid => unimplemented!("Mermaid language support not yet implemented"),
        }
    }
}

fn main() -> Result<(), ReplError> {
    // Parse command line arguments
    let args = Args::parse();

    // Validate parser homes and source file
    args.validate()?;

    let mut repl = Repl::new(
        args.project_name,
        args.input_parser_homes,
        args.intermediate_parser_homes,
        args.source_path.into(),
        args.source_type,
        args.no_execute,
        args.no_hydrate,
    )?;
    repl.run()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Write;
    use tempfile::TempDir;

    // Test helper functions
    fn setup_test_environment() -> (TempDir, PathBuf) {
        let temp_dir = TempDir::new().unwrap();
        let source_file = temp_dir.path().join("test.sol");
        File::create(&source_file)
            .unwrap()
            .write_all(b"contract Test {}")
            .unwrap();
        (temp_dir, source_file)
    }

    fn create_test_repl(source_file: PathBuf) -> Repl {
        Repl::new(
            "test_project".to_string(),
            vec![],
            vec![],
            source_file.into_boxed_path(),
            SourceType::Solidity,
            true,
            true, // no_hydrate = true for tests
        )
        .unwrap()
    }

    mod command_tests {
        use super::*;

        #[test]
        fn test_command_parsing() {
            assert!(matches!(Repl::parse_command("exit"), Command::Exit));
            assert!(matches!(Repl::parse_command("logout"), Command::Exit));
            assert!(matches!(Repl::parse_command("clear"), Command::Clear));
            assert!(matches!(Repl::parse_command("history"), Command::History));
            assert!(matches!(
                Repl::parse_command("clear-history"),
                Command::ClearHistory
            ));
            assert!(matches!(
                Repl::parse_command("select * from table"),
                Command::Process(s) if s == "select * from table"
            ));
        }

        #[test]
        fn test_command_with_whitespace() {
            assert!(matches!(Repl::parse_command("  exit  "), Command::Exit));
            assert!(matches!(
                Repl::parse_command("  select * from table  "),
                Command::Process(s) if s.trim() == "select * from table"
            ));
        }
    }

    mod repl_initialization_tests {
        use super::*;

        #[test]
        fn test_repl_creation_with_valid_params() {
            let (_temp_dir, source_file) = setup_test_environment();
            let result = Repl::new(
                "test".to_string(),
                vec![],
                vec![],
                source_file.into_boxed_path(),
                SourceType::Solidity,
                true,
                true, // no_hydrate = true for tests
            );
            assert!(result.is_ok());
        }
    }

    mod args_validation_tests {
        use super::*;

        #[test]
        fn test_args_validation_with_valid_paths() {
            let temp_dir = TempDir::new().unwrap();
            let args = Args {
                project_name: "test".to_string(),
                input_parser_homes: vec![temp_dir.path().to_path_buf()],
                intermediate_parser_homes: vec![temp_dir.path().to_path_buf()],
                source_path: temp_dir.path().join("test.sol"),
                source_type: SourceType::Solidity,
                no_execute: true,
                no_hydrate: true,
            };
            File::create(temp_dir.path().join("test.sol")).unwrap();
            let result = args.validate();
            assert!(result.is_ok());
        }

        #[test]
        fn test_args_validation_with_invalid_source() {
            let temp_dir = TempDir::new().unwrap();
            let args = Args {
                project_name: "test".to_string(),
                input_parser_homes: vec![temp_dir.path().to_path_buf()],
                intermediate_parser_homes: vec![temp_dir.path().to_path_buf()],
                source_path: temp_dir.path().join("nonexistent.sol"),
                source_type: SourceType::Solidity,
                no_execute: true,
                no_hydrate: true,
            };
            let result = args.validate();
            assert!(result.is_err());
        }
    }
}
