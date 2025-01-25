use frontend::gen_ir::IrGenerator;

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
                    let parse_result = frontend::parser::parse(&buffer);

                    match parse_result {
                        Ok(lval) => {
                            println!("AST:\n{:#?}\n", lval);

                            if let Ok(dl_ir) = IrGenerator::new()
                                .with_input_relations(true)
                                .lval_to_ir(&lval)
                            {
                                println!("Datalog IR:\n\n{}", dl_ir);

                                let ddlog =
                                    backend::gen_ddlog::DDlogGenerator::new()
                                        .with_input_relations(true)
                                        .generate(*dl_ir);

                                if let Ok(ddlog) = ddlog {
                                    println!("\nGenerated DDLog:\n\n{}", ddlog.to_string().trim());
                                    match backend::ddlog_rt::validate(&ddlog.to_string()) {
                                        Ok(_) => {
                                            println!("\nDDLog Validation succeded");
                                        }
                                        Err(e) => {
                                            println!("\nDDLog Validation Error:\n{}", e);
                                        }
                                    }
                                } else {
                                    println!("Failed to generate DDLog");
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
