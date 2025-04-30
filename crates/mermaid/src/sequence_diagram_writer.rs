//! Writes a SequenceDiagram AST to MermaidJS plain text format.

use crate::sequence_diagram_ast::*;
use std::fmt::Write; // Use std::fmt::Write for efficient string building

const INDENT_STEP: &str = "  "; // Define indentation step

/// Writes the entire SequenceDiagram to a Mermaid string.
pub fn write_diagram(diagram: &SequenceDiagram) -> String {
    let mut output = String::new();
    writeln!(output, "sequenceDiagram").unwrap(); // Start with the diagram type

    let mut indent = String::new(); // Start with no indentation

    for statement in &diagram.statements {
        write_statement(&mut output, statement, &mut indent);
    }

    output
}

/// Writes a single statement to the output string, handling indentation.
fn write_statement(output: &mut String, statement: &Statement, indent: &mut String) {
    match statement {
        Statement::Title(s) => writeln!(output, "{indent}title {}", s.title).unwrap(),
        Statement::AccTitle(s) => writeln!(output, "{indent}accTitle: {}", s.title).unwrap(),
        Statement::AccDescr(s) => writeln!(output, "{indent}accDescr: {}", s.description).unwrap(),
        Statement::AccDescrMultiline(s) => {
            writeln!(output, "{indent}accDescr {{").unwrap();
            // Basic multiline handling, might need refinement based on actual MermaidJS parsing
            for line in s.description.lines() {
                writeln!(output, "{indent}  {}", line).unwrap();
            }
            writeln!(output, "{indent}}}").unwrap();
        }
        Statement::Participant(s) => {
            write!(output, "{indent}{}", s.participant_type.to_mermaid()).unwrap();
            write!(output, " {}", s.id).unwrap();
            if let Some(alias) = &s.alias {
                write!(output, " as {}", alias).unwrap();
            }
            writeln!(output).unwrap();
        }
        Statement::Signal(s) => {
            write!(output, "{indent}{}", s.from).unwrap();
            write!(output, "{}", s.arrow.to_mermaid()).unwrap();
            write!(output, "{}", s.to).unwrap();
            if let Some(msg) = &s.message {
                write!(output, ": {}", msg.content).unwrap(); // TODO: Handle wrap indicator?
            }
            writeln!(output).unwrap();
        }
        Statement::Activate(s) => writeln!(output, "{indent}activate {}", s.id).unwrap(),
        Statement::Deactivate(s) => writeln!(output, "{indent}deactivate {}", s.id).unwrap(),
        Statement::Note(s) => {
            write!(output, "{indent}Note {}", s.placement.to_mermaid()).unwrap();
            write!(output, " {}: {}", s.actors.join(", "), s.text).unwrap(); // Join actors if multiple
            writeln!(output).unwrap();
        }
        Statement::Loop(s) => {
            write!(output, "{indent}loop").unwrap();
            if let Some(label) = &s.label {
                write!(output, " {}", label).unwrap();
            }
            writeln!(output).unwrap();

            indent.push_str(INDENT_STEP);
            for inner_stmt in &s.statements {
                write_statement(output, inner_stmt, indent);
            }
            indent.truncate(indent.len() - INDENT_STEP.len());

            writeln!(output, "{indent}end").unwrap();
        }
        Statement::Opt(s) => {
            write!(output, "{indent}opt").unwrap();
            if let Some(label) = &s.label {
                write!(output, " {}", label).unwrap();
            }
            writeln!(output).unwrap();

            indent.push_str(INDENT_STEP);
            for inner_stmt in &s.statements {
                write_statement(output, inner_stmt, indent);
            }
            indent.truncate(indent.len() - INDENT_STEP.len());

            writeln!(output, "{indent}end").unwrap();
        }
         Statement::Rect(s) => {
            write!(output, "{indent}rect").unwrap();
            if let Some(label) = &s.label { // Often used for RGB color
                write!(output, " {}", label).unwrap();
            }
            writeln!(output).unwrap();

            indent.push_str(INDENT_STEP);
            for inner_stmt in &s.statements {
                write_statement(output, inner_stmt, indent);
            }
            indent.truncate(indent.len() - INDENT_STEP.len());

            writeln!(output, "{indent}end").unwrap();
        }
        Statement::Autonumber(s) => {
            if s.off {
                writeln!(output, "{indent}autonumber off").unwrap();
            } else if let (Some(start), Some(inc)) = (s.start, s.increment) {
                writeln!(output, "{indent}autonumber {} {}", start, inc).unwrap();
            } else {
                writeln!(output, "{indent}autonumber").unwrap();
            }
        }
        Statement::Create(s) => {
             write!(output, "{indent}create").unwrap();
             if let Some(ptype) = &s.participant_type {
                 write!(output, " {}", ptype.to_mermaid()).unwrap();
             }
             write!(output, " {}", s.id).unwrap();
             if let Some(alias) = &s.alias {
                 write!(output, " as {}", alias).unwrap();
             }
             writeln!(output).unwrap();
        }
        Statement::Destroy(s) => writeln!(output, "{indent}destroy {}", s.id).unwrap(),

        // --- Alt Block Statements ---
        Statement::AltStart(s) => writeln!(output, "{indent}alt {}", s.label).unwrap(),
        Statement::AltElse(s) => writeln!(output, "{indent}else {}", s.label).unwrap(),
        Statement::AltEnd(_) => writeln!(output, "{indent}end").unwrap(),


        // --- TODO: Implement remaining statement types ---
        Statement::Box(_) => writeln!(output, "{indent}%% TODO: Box statement not implemented").unwrap(),
        Statement::Links(_) => writeln!(output, "{indent}%% TODO: Links statement not implemented").unwrap(),
        Statement::Link(_) => writeln!(output, "{indent}%% TODO: Link statement not implemented").unwrap(),
        Statement::Properties(_) => writeln!(output, "{indent}%% TODO: Properties statement not implemented").unwrap(),
        Statement::Details(_) => writeln!(output, "{indent}%% TODO: Details statement not implemented").unwrap(),
        Statement::Alt(_) => writeln!(output, "{indent}%% TODO: Alt statement not implemented").unwrap(),
        Statement::Par(_) => writeln!(output, "{indent}%% TODO: Par statement not implemented").unwrap(),
        Statement::ParOver(_) => writeln!(output, "{indent}%% TODO: ParOver statement not implemented").unwrap(),
        Statement::Break(_) => writeln!(output, "{indent}%% TODO: Break statement not implemented").unwrap(),
        Statement::Critical(_) => writeln!(output, "{indent}%% TODO: Critical statement not implemented").unwrap(),
    }
}

// --- Helper Implementations for AST Nodes ---

impl ParticipantType {
    fn to_mermaid(&self) -> &'static str {
        match self {
            ParticipantType::Participant => "participant",
            ParticipantType::Actor => "actor",
        }
    }
}

impl Arrow {
    fn to_mermaid(&self) -> String {
        let marker = match self.activation_marker {
            Some(ActivationMarker::Activate) => "+",
            Some(ActivationMarker::Deactivate) => "-",
            None => "",
        };
        format!("{}{}", self.sequence, marker)
    }
}

impl NotePlacement {
     fn to_mermaid(&self) -> &'static str {
        match self {
            NotePlacement::LeftOf => "left of",
            NotePlacement::RightOf => "right of",
            NotePlacement::Over => "over",
        }
    }
}
