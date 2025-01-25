use frontend::{
    compiler::Compiler,
    parser::{to_ast, Grammar},
};

use ir::IRProgram;
use backend::ddlog_lang::DatalogProgram;

use pest::Parser;
use reedline::{DefaultPrompt, DefaultValidator, FileBackedHistory, Reedline, Signal};
use std::io;

fn main() -> io::Result<()> {
    let history = Box::new(
        FileBackedHistory::with_file(5, "history.txt".into())
            .expect("Error configuring history with file"),
    );

    let mut line_editor = Reedline::create()
        .with_history(history)
        .with_validator(Box::new(DefaultValidator))
        .use_kitty_keyboard_enhancement(true)
        .with_ansi_colors(true);

    let prompt = DefaultPrompt::default();

    loop {
        let sig = line_editor.read_line(&prompt)?;
        match sig {
            Signal::Success(buffer) => {
                if (buffer.trim() == "exit") || (buffer.trim() == "logout") {
                    break;
                }
                if buffer.trim() == "clear" {
                    line_editor.clear_scrollback()?;
                    continue;
                }
                // Get the full history
                if buffer.trim() == "history" {
                    line_editor.print_history()?;
                    continue;
                }
                if buffer.trim() == "clear-history" {
                    let hstry = Box::new(line_editor.history_mut());
                    hstry
                        .clear()
                        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))?;
                    continue;
                } else {
                    let parse_result = frontend::parser::Grammar::parse(frontend::parser::Rule::program, &buffer);
                    if parse_result.is_err() {
                        println!("Failed to parse DSL: {:?}", parse_result);
                        continue;
                    }

                    match to_ast(&buffer) {
                        Ok(lval) => {
                            println!("AST:\n{:#?}\n", lval);

                            let dl_ir: IRProgram =
                                Compiler::new().with_input_relations(true).lval_to_ir(&lval);

                            println!("Datalog IR:\n\n{}", dl_ir);

                            let ddlog: DatalogProgram = backend::compiler::Compiler::new()
                                .with_input_relations(true)
                                .ir_to_ddlog(dl_ir);
                                
                                //.ir_to_ddlog(dl_ir);

                            println!("\nGenerated DDLog:\n\n{}", ddlog.to_string().trim());

                            match core::ddlog::validate(&ddlog.to_string()) {
                                Ok(_) => {
                                    println!("\nDDLog Validation succeded");
                                }
                                Err(e) => {
                                    println!("\nDDLog Validation Error:\n{}", e);
                                }
                                                           
                            }

                        }
                        Err(e) => {
                            println!("Failed to parse DSL: {:?}", e);
                        }
                    }

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
