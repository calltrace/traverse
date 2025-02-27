use backend::facts::{DDLogCommand, InsertCommandFn, TreeSitterToDDLog};
use std::{
    collections::HashSet,
    fs,
    path::{Path, PathBuf},
};

use backend::ddlog_drain::DdlogDrain;
use backend::ddlog_rt::{build_ddlog_crate, generate_rust_project, validate};
use backend::hydrate::{BucketConfig, Hydrator, InputSource};
use clap::Parser;
use frontend::{gen_ir::IrGenerator, syntax::SyntaxTheme};
use language::{Language, Solidity};
use reedline::{DefaultPrompt, DefaultValidator, FileBackedHistory, Reedline, Signal};
use std::io::Write;

#[derive(Debug)]
enum ReplError {
    Io(std::io::Error),
    Parse(String),
    IrGeneration(String),
    DdlogGeneration(String),
    DdlogValidation(String),
    DdlogExecution(String),
}

impl std::fmt::Display for ReplError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReplError::Io(err) => writeln!(f, "IO error: {}", err),
            ReplError::Parse(msg) => writeln!(f, "Parse error:\n{}", msg),
            ReplError::IrGeneration(msg) => writeln!(f, "IR generation error:\n{}", msg),
            ReplError::DdlogGeneration(msg) => writeln!(f, "DDlog generation error:\n{}", msg),
            ReplError::DdlogValidation(msg) => write!(f, "DDlog validation error:\n{}", msg),
            ReplError::DdlogExecution(msg) => writeln!(f, "DDlog execution error:\n{}", msg),
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

    fn parse_source_file(&self) -> Result<Vec<DDLogCommand>, ReplError> {
        let source_code = std::fs::read_to_string(&self.source_file).map_err(ReplError::Io)?;
        let language = self.source_type.to_tree_sitter_language();
        let converter = TreeSitterToDDLog::new(&source_code, &language)
            .with_excluded_relations(HashSet::from(["SourceFile".to_string()]));
        Ok(converter.extract_commands(None))
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

        println!("Parsed DSL:\n\n{:?}", lval);
        let formatted = frontend::formatter::Formatter::new(2)
            .with_syntax_highlighting(Some(SyntaxTheme::default()))
            .format_with_highlighting(&lval);

        println!("DSL:\n\n{}\n", formatted);

        let dl_ir = IrGenerator::new()
            //.with_input_relations(true)
            .with_input_relations(false)
            .with_input_treesitter_grammars(self.input_ts_grammars.clone())
            //.with_intermediate_treesitter_grammars(self.intermediate_ts_grammars.clone())
            .lval_to_ir(&lval)
            .map_err(|e| ReplError::IrGeneration(format!("{:?}", e)))?;

        println!("IR:\n\n{}", dl_ir);

        let formatted_ir = ir::format_program(&dl_ir.to_string(), true, 2, false)
            .map_err(|e| ReplError::IrGeneration(format!("{:?}", e)))?;

        println!("Datalog IR:\n\n{}", formatted_ir);

        let ddlog = backend::gen_ddlog::DDlogGenerator::new()
            //.with_input_relations(true)
            .with_input_relations(false)
            .with_input_treesitter_grammars(self.input_ts_grammars.clone())
            .embed_primitives()
            //.with_intermediate_treesitter_grammars(self.intermediate_ts_grammars.clone())
            .generate(*dl_ir)
            .map_err(|e| ReplError::DdlogGeneration(format!("{}", e)))?;

        let re = regex::Regex::new(r"^(?:input|output|relation)\b").unwrap();
        let filtered_ddlog = ddlog
            .to_string()
            .lines()
            //   .filter(|line| !re.is_match(line.trim()))
            .collect::<Vec<_>>()
            .join("\n");

        println!(
            "\nGenerated DDLog (no relations for brevity):{}",
            filtered_ddlog
        );
        let ddlog_str = ddlog.to_string();
        if let Err(e) = validate(&ddlog_str) {
            let numbered_ddlog = ddlog_str
                .lines()
                .enumerate()
                .map(|(i, line)| format!("{:4} | {}", i + 1, line))
                .collect::<Vec<_>>()
                .join("\n");

            println!(
                "\nDDLog Validation failed. Full DDLog code:\n{}",
                numbered_ddlog
            );
            return Err(ReplError::DdlogValidation(e.to_string()));
        }

        println!("\nDDLog Validation succeeded");

        if !self.no_execute {
            println!("Ingesting source file");
            let mut cmds = self.parse_source_file()?;

            //cmds.push(DDLogCommand::Dump(Some("FunctionContract".to_string())));
            //cmds.push(DDLogCommand::Dump(Some("Node".to_string())));

            let mut file = fs::File::create("dump.txt").expect("Failed to create dump file");
            for cmd in &cmds {
                writeln!(file, "{}", cmd).expect("Failed to write to dump file");
            }

            println!("Dumped commands to dump.txt");

            let base_dir = PathBuf::from(format!("./{}", self.project_name));
            fs::create_dir_all(&base_dir).expect("Failed to create base directory");

            println!("Generating Rust project");
            generate_rust_project(&base_dir, &self.project_name, &ddlog.to_string());

            match build_ddlog_crate(&base_dir, &self.project_name) {
                Ok(_) => println!("Build succeeded"),
                Err(e) => eprintln!("Build failed: {}", e),
            }

            println!(
                "Scaffolded DDlog project '{}' at: {:?}",
                self.project_name, base_dir
            );

            // dump cmds
            println!("Commands to execute:");
            for cmd in &cmds {
                println!("{}", cmd);
            }
            println!("Finished");

            let ddlog_out =
                backend::ddlog_rt::run_ddlog_crate(&base_dir, &self.project_name, &cmds).map_err(
                    |e| ReplError::DdlogExecution(format!("Failed to run DDlog project: {}", e)),
                )?;

            println!("{}", ddlog_out);
            println!("DDlog project executed successfully");

            // Process the DDlog output through the hydration module if not disabled
            if !self.no_hydrate {
                println!("Hydrating DDlog output...");

                // Configure the hydrator for Mermaid-style output
                let hydrator_config = BucketConfig::new()
                    .with_pool_shape("sequenceDiagram")
                    .with_bucket(
                        "participants",
                        100,
                        "path",
                        "val",
                        vec![
                            InputSource::new("EmitMermaidLineCallerParticipantLine", 100),
                            InputSource::new("EmitMermaidLineCalleeParticipantLine", 90),
                        ],
                    )
                    .with_bucket(
                        "flow",
                        90,
                        "ce_id_path",
                        "val",
                        vec![
                            InputSource::new("EmitMermaidLineSignalLine", 100),
                            InputSource::new("EmitMermaidLineActivate", 90),
                        ],
                    );

                let mut hydrator = Hydrator::new(hydrator_config);

                // Create a drain from the DDlog output lines
                let lines = ddlog_out.lines().map(|s| s.to_string());
                let drain = DdlogDrain::new(lines);

                // Process the drained facts
                hydrator.process_drain(drain);

                // Get the hydrated output
                let hydrated_output = hydrator.dump();

                // Save the hydrated output to a file
                let hydrated_file_path = format!("{}_hydrated.txt", self.project_name);
                fs::write(&hydrated_file_path, &hydrated_output).map_err(|e| ReplError::Io(e))?;

                println!("Hydrated output saved to: {}", hydrated_file_path);

                // Print a preview of the hydrated output
                let preview_lines: Vec<&str> = hydrated_output.lines().take(20).collect();
                if !preview_lines.is_empty() {
                    println!("\nHydrated output preview (first 20 lines):");
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
    fn validate(&self) -> Result<(Vec<String>, Vec<String>), ReplError> {
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

        // Convert PathBuf to String for compatibility
        Ok((
            self.input_parser_homes
                .iter()
                .map(|p| p.to_string_lossy().into_owned())
                .collect(),
            self.intermediate_parser_homes
                .iter()
                .map(|p| p.to_string_lossy().into_owned())
                .collect(),
        ))
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

    // Validate parser homes and get as strings
    args.validate()?;

    let mut repl = Repl::new(
        args.project_name,
        args.input_parser_homes,        //input_parser_homes,
        args.intermediate_parser_homes, //intermediate_parser_homes,
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

    mod input_processing_tests {
        use super::*;

        #[test]
        fn test_process_valid_input() {
            let (_temp_dir, source_file) = setup_test_environment();
            let repl = create_test_repl(source_file);
            let result = repl.process_input("x := 1");
            assert!(result.is_ok());
        }

        #[test]
        fn test_process_invalid_input() {
            let (_temp_dir, source_file) = setup_test_environment();
            let repl = create_test_repl(source_file);
            let result = repl.process_input("invalid := ::");
            assert!(matches!(result, Err(ReplError::Parse(_))));
        }

        #[test]
        fn test_empty_input() {
            let (_temp_dir, source_file) = setup_test_environment();
            let repl = create_test_repl(source_file);
            let result = repl.process_input("");
            assert!(matches!(result, Err(ReplError::Parse(_))));
        }
    }

    mod source_file_tests {
        use super::*;

        #[test]
        fn test_parse_valid_source_file() {
            let (temp_dir, source_file) = setup_test_environment();
            let repl = create_test_repl(source_file);
            let result = repl.parse_source_file();
            assert!(result.is_ok());
            temp_dir.close().unwrap();
        }

        #[test]
        fn test_parse_nonexistent_source_file() {
            let (_temp_dir, mut source_file) = setup_test_environment();
            source_file.pop();
            source_file.push("nonexistent.sol");
            let repl = create_test_repl(source_file);
            let result = repl.parse_source_file();
            assert!(matches!(result, Err(ReplError::Io(_))));
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
            assert!(matches!(result, Err(ReplError::Io(_))));
        }
    }
}
