//! Builder for Mermaid Sequence Diagrams

use crate::sequence_diagram_ast::*;
use pest::iterators::Pair;
use crate::sequence_diagram_parser::Rule;
use std::error::Error;
use std::fmt;

/// Error type for builder operations
#[derive(Debug)]
pub enum BuilderError {
    /// Error when processing a parser pair
    InvalidPair(String),
    /// Other builder errors
    Other(String),
}

impl fmt::Display for BuilderError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuilderError::InvalidPair(msg) => write!(f, "Invalid parser pair: {}", msg),
            BuilderError::Other(msg) => write!(f, "Builder error: {}", msg),
        }
    }
}

impl Error for BuilderError {}

/// Builder for creating Mermaid Sequence Diagrams programmatically.
///
/// Provides a fluent interface to construct a `SequenceDiagram` AST.
///
/// # Example
/// ```rust,no_run
/// # use traverse_mermaid::sequence_diagram_ast::*;
/// # use traverse_mermaid::sequence_diagram_builder::SequenceDiagramBuilder;
/// #
/// let mut builder = SequenceDiagramBuilder::new();
/// builder.title("Example Diagram")
///     .participant_as("A", "Alice")
///     .actor_as("B", "Bob")
///     .signal("A", "B", "->>+", Some("Authentication Request")) // Activate B
///     .loop_block(Some("Processing"), |b| {
///         b.signal("B", "B", "->", Some("Process data"));
///     })
///     .signal("B", "A", "-->>-", Some("Authentication Response")) // Deactivate B
///     .note_right_of("A", "Alice checks response");
///
/// let diagram = builder.build();
/// // Now `diagram` holds the structured representation of the sequence diagram.
/// ```
#[derive(Debug, Default, Clone)]
pub struct SequenceDiagramBuilder {
    statements: Vec<Statement>,
}

impl SequenceDiagramBuilder {
    /// Creates a new, empty sequence diagram builder.
    pub fn new() -> Self {
        Default::default()
    }

    /// Adds a statement to the diagram being built.
    /// Internal helper method.
    fn add_statement(&mut self, statement: Statement) -> &mut Self {
        self.statements.push(statement);
        self
    }

    /// Consumes the builder and returns the constructed `SequenceDiagram`.
    pub fn build(self) -> SequenceDiagram {
        SequenceDiagram {
            statements: self.statements,
        }
    }

    /// Returns the current number of statements in the builder.
    pub fn statement_count(&self) -> usize {
        self.statements.len()
    }

    /// Process a parser pair and add the corresponding statement to the builder.
    /// This is used for parsing Mermaid text into a diagram.
    pub fn process_pair(&mut self, pair: Pair<'_, Rule>) -> Result<&mut Self, BuilderError> {
        match pair.as_rule() {
            Rule::sequence_diagram => {
                // Process the diagram content
                for inner_pair in pair.into_inner() {
                    if inner_pair.as_rule() == Rule::diagram_content {
                        for statement_pair in inner_pair.into_inner() {
                            if let Some(statement) = Statement::from_pair(statement_pair.clone()) {
                                self.statements.push(statement);
                            }
                        }
                    }
                }
                Ok(self)
            },
            _ => {
                // Try to convert directly to a statement
                let rule = pair.as_rule();
                if let Some(statement) = Statement::from_pair(pair) {
                    self.statements.push(statement);
                    Ok(self)
                } else {
                    Err(BuilderError::InvalidPair(format!("Could not convert pair to statement: {:?}", rule)))
                }
            }
        }
    }

    // --- Metadata Statements ---

    /// Adds a `title` statement to the diagram.
    pub fn title(&mut self, title: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::Title(TitleStatement { title: title.into() }))
    }

    /// Adds an `accTitle` (accessibility title) statement.
    pub fn acc_title(&mut self, title: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::AccTitle(AccTitleStatement { title: title.into() }))
    }

    /// Adds an `accDescr` (accessibility description) statement.
    pub fn acc_descr(&mut self, description: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::AccDescr(AccDescrStatement { description: description.into() }))
    }

    /// Adds a multiline `accDescr` (accessibility description) statement.
    pub fn acc_descr_multiline(&mut self, description: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::AccDescrMultiline(AccDescrMultilineStatement { description: description.into() }))
    }

    // --- Participant Statements ---

    /// Adds a `participant` declaration.
    pub fn participant(&mut self, id: impl Into<String>) -> &mut Self {
        self.add_participant_internal(ParticipantType::Participant, id.into(), None)
    }

    /// Adds a `participant` declaration with an alias.
    pub fn participant_as(&mut self, id: impl Into<String>, alias: impl Into<String>) -> &mut Self {
        self.add_participant_internal(ParticipantType::Participant, id.into(), Some(alias.into()))
    }

    /// Adds an `actor` declaration.
    pub fn actor(&mut self, id: impl Into<String>) -> &mut Self {
        self.add_participant_internal(ParticipantType::Actor, id.into(), None)
    }

    /// Adds an `actor` declaration with an alias.
    pub fn actor_as(&mut self, id: impl Into<String>, alias: impl Into<String>) -> &mut Self {
        self.add_participant_internal(ParticipantType::Actor, id.into(), Some(alias.into()))
    }

    /// Internal helper for adding participants/actors.
    fn add_participant_internal(&mut self, participant_type: ParticipantType, id: String, alias: Option<String>) -> &mut Self {
        self.add_statement(Statement::Participant(ParticipantStatement {
            participant_type,
            id,
            alias,
        }))
    }

    // --- Signal Statements ---

    /// Adds a signal (message arrow) between participants.
    ///
    /// The `arrow_sequence` string determines the arrow style (e.g., "->", "-->", "->>", "-x").
    /// Appending '+' or '-' to `arrow_sequence` adds activation/deactivation markers
    /// (e.g., "->+", "-->>-").
    ///
    /// # Arguments
    /// * `from` - The sender participant ID.
    /// * `to` - The receiver participant ID.
    /// * `arrow_sequence` - The arrow type string (potentially including activation marker).
    /// * `message` - Optional message content for the signal.
    pub fn signal(
        &mut self,
        from: impl Into<String>,
        to: impl Into<String>,
        arrow_sequence: impl Into<String>,
        message: Option<impl Into<String>>,
    ) -> &mut Self {
        let arrow_str = arrow_sequence.into();
        let mut sequence = arrow_str.as_str();
        let mut activation_marker = None;

        if sequence.ends_with('+') {
            sequence = &sequence[..sequence.len()-1];
            activation_marker = Some(ActivationMarker::Activate);
        } else if sequence.ends_with('-') {
            sequence = &sequence[..sequence.len()-1];
            activation_marker = Some(ActivationMarker::Deactivate);
        }

        let msg = message.map(|m| Message {
            wrap_indicator: None,
            content: m.into(),
        });

        self.add_statement(Statement::Signal(SignalStatement {
            from: from.into(),
            to: to.into(),
            arrow: Arrow { sequence: sequence.to_string(), activation_marker },
            message: msg,
        }))
    }

     // --- Activation Statements ---

    /// Adds an `activate` statement for a participant.
    pub fn activate(&mut self, id: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::Activate(ActivateStatement { id: id.into() }))
    }

    /// Adds a `deactivate` statement for a participant.
    pub fn deactivate(&mut self, id: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::Deactivate(DeactivateStatement { id: id.into() }))
    }

    // --- Note Statements ---

    /// Adds a `Note left of` statement.
    pub fn note_left_of(&mut self, actor: impl Into<String>, text: impl Into<String>) -> &mut Self {
        self.add_note(NotePlacement::LeftOf, vec![actor.into()], text.into())
    }

    /// Adds a `Note right of` statement.
    pub fn note_right_of(&mut self, actor: impl Into<String>, text: impl Into<String>) -> &mut Self {
        self.add_note(NotePlacement::RightOf, vec![actor.into()], text.into())
    }

    /// Adds a `Note over` statement, potentially spanning multiple actors.
    pub fn note_over(&mut self, actors: Vec<impl Into<String>>, text: impl Into<String>) -> &mut Self {
        self.add_note(NotePlacement::Over, actors.into_iter().map(Into::into).collect(), text.into())
    }

    /// Internal helper for adding notes.
    fn add_note(&mut self, placement: NotePlacement, actors: Vec<String>, text: String) -> &mut Self {
        self.add_statement(Statement::Note(NoteStatement {
            placement,
            actors,
            text,
        }))
    }

    // --- Alt Block Statements ---

    /// Starts an `alt` block with the given condition label.
    /// Must be followed by statements and eventually `alt_else` or `alt_end`.
    pub fn alt_start(&mut self, label: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::AltStart(AltStartStatement { label: label.into() }))
    }

    /// Adds an `else` section to an existing `alt` block with the given condition label.
    pub fn alt_else(&mut self, label: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::AltElse(AltElseStatement { label: label.into() }))
    }

    /// Ends the current `alt` block.
    pub fn alt_end(&mut self) -> &mut Self {
        self.add_statement(Statement::AltEnd(AltEndStatement {}))
    }

    // --- Block Statements (using closures) ---

    /// Adds a `loop` block.
    ///
    /// The provided closure `build_inner` receives a mutable reference to a new
    /// `SequenceDiagramBuilder` to define the statements inside the loop.
    ///
    /// # Arguments
    /// * `label` - Optional label for the loop block (e.g., "Every minute").
    /// * `build_inner` - A closure that builds the inner statements of the loop.
    pub fn loop_block<F>(&mut self, label: Option<impl Into<String>>, build_inner: F) -> &mut Self
    where
        F: FnOnce(&mut SequenceDiagramBuilder),
    {
        let mut inner_builder = SequenceDiagramBuilder::new();
        build_inner(&mut inner_builder);
        self.add_statement(Statement::Loop(LoopStatement {
            label: label.map(Into::into),
            statements: inner_builder.statements,
        }))
    }

    /// Adds an `opt` (optional) block.
    ///
    /// The provided closure `build_inner` receives a mutable reference to a new
    /// `SequenceDiagramBuilder` to define the statements inside the optional block.
    ///
    /// # Arguments
    /// * `label` - Optional label for the opt block (e.g., "If condition met").
    /// * `build_inner` - A closure that builds the inner statements of the block.
    pub fn opt_block<F>(&mut self, label: Option<impl Into<String>>, build_inner: F) -> &mut Self
    where
        F: FnOnce(&mut SequenceDiagramBuilder),
    {
        let mut inner_builder = SequenceDiagramBuilder::new();
        build_inner(&mut inner_builder);
        self.add_statement(Statement::Opt(OptStatement {
            label: label.map(Into::into),
            statements: inner_builder.statements,
        }))
    }

    /// Adds a `rect` block with a background color.
    ///
    /// # Arguments
    /// * `rgb_color` - Optional label, often used for RGB color like "rgb(200, 200, 255)".
    /// * `build_inner` - A closure that builds the inner statements of the block.
     pub fn rect_block<F>(&mut self, rgb_color: Option<impl Into<String>>, build_inner: F) -> &mut Self
     where
         F: FnOnce(&mut SequenceDiagramBuilder),
     {
         let mut inner_builder = SequenceDiagramBuilder::new();
         build_inner(&mut inner_builder);
         self.add_statement(Statement::Rect(RectStatement {
             label: rgb_color.map(Into::into),
             statements: inner_builder.statements,
         }))
     }

    // --- Other Statements (Basic Implementations) ---

    /// Adds an `autonumber` statement to enable automatic numbering.
    pub fn autonumber(&mut self) -> &mut Self {
        self.add_statement(Statement::Autonumber(AutonumberStatement {
            start: None,
            increment: None,
            off: false,
        }))
    }

    /// Adds an `autonumber` statement with start and increment values.
    pub fn autonumber_with(&mut self, start: u32, increment: u32) -> &mut Self {
        self.add_statement(Statement::Autonumber(AutonumberStatement {
            start: Some(start),
            increment: Some(increment),
            off: false,
        }))
    }

    /// Adds an `autonumber off` statement to disable automatic numbering.
    pub fn autonumber_off(&mut self) -> &mut Self {
        self.add_statement(Statement::Autonumber(AutonumberStatement {
            start: None,
            increment: None,
            off: true,
        }))
    }

    /// Adds a `create participant` statement.
    pub fn create_participant(&mut self, id: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::Create(CreateStatement {
            participant_type: Some(ParticipantType::Participant),
            id: id.into(),
            alias: None,
        }))
    }

    /// Adds a `create actor` statement.
    pub fn create_actor(&mut self, id: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::Create(CreateStatement {
            participant_type: Some(ParticipantType::Actor),
            id: id.into(),
            alias: None,
        }))
    }

    /// Adds a `destroy` statement for a participant/actor.
    pub fn destroy(&mut self, id: impl Into<String>) -> &mut Self {
        self.add_statement(Statement::Destroy(DestroyStatement { id: id.into() }))
    }


}

// --- Unit Tests ---
#[cfg(test)]
mod tests {
    use super::*;
    // Use crate::... path if types are not re-exported at the crate root
    use crate::sequence_diagram_ast::{ActivationMarker, NotePlacement, ParticipantType, Statement};

    #[test]
    fn build_basic_diagram() {
        let mut builder = SequenceDiagramBuilder::new();
        builder.title("Test Diagram")
            .participant_as("u", "User")
            .actor_as("s", "System")
            .activate("u")
            .signal("u", "s", "->>+", Some("Request Action")) // Activate s
            .opt_block(Some("Optional Step"), |b| {
                b.signal("s", "s", "->", Some("Internal Check"));
            })
            .signal("s", "u", "-->>-", Some("Action Complete")) // Deactivate s
            .deactivate("u")
            .note_over(vec!["u", "s"], "Interaction finished");
        
        let diagram = builder.build();

        // There are 9 statements added above.
        assert_eq!(diagram.statements.len(), 9);

        // Basic checks on statement types and content
        assert!(matches!(diagram.statements[0], Statement::Title(ref t) if t.title == "Test Diagram"));
        assert!(matches!(diagram.statements[1], Statement::Participant(ref p) if p.id == "u" && p.alias == Some("User".to_string()) && p.participant_type == ParticipantType::Participant));
        assert!(matches!(diagram.statements[2], Statement::Participant(ref p) if p.id == "s" && p.alias == Some("System".to_string()) && p.participant_type == ParticipantType::Actor));
        assert!(matches!(diagram.statements[3], Statement::Activate(ref a) if a.id == "u"));
        assert!(matches!(diagram.statements[4], Statement::Signal(ref s) if s.from == "u" && s.to == "s" && s.arrow.sequence == "->>" && s.arrow.activation_marker == Some(ActivationMarker::Activate)));
        assert!(matches!(diagram.statements[5], Statement::Opt(ref o) if o.label == Some("Optional Step".to_string()) && o.statements.len() == 1));
        assert!(matches!(diagram.statements[6], Statement::Signal(ref s) if s.from == "s" && s.to == "u" && s.arrow.sequence == "-->>" && s.arrow.activation_marker == Some(ActivationMarker::Deactivate)));
        assert!(matches!(diagram.statements[7], Statement::Deactivate(ref d) if d.id == "u"));
        // Note check is missing, let's add it
         assert!(matches!(diagram.statements[8], Statement::Note(ref n) if n.placement == NotePlacement::Over && n.actors == vec!["u", "s"] && n.text == "Interaction finished"));


        // Check nested statement in opt block
        if let Statement::Opt(opt_stmt) = &diagram.statements[5] {
            assert_eq!(opt_stmt.statements.len(), 1);
            assert!(matches!(opt_stmt.statements[0], Statement::Signal(ref s) if s.from == "s" && s.to == "s" && s.arrow.sequence == "->"));
        } else {
            panic!("Expected Opt statement at index 5");
        }
    }

     #[test]
    fn build_with_loop_and_notes() {
         let mut builder = SequenceDiagramBuilder::new();
         builder.participant("A")
            .participant("B")
            .note_left_of("A", "A prepares")
            .loop_block(Some("Retry Loop"), |b| {
                b.signal("A", "B", "->", Some("Attempt"));
                b.signal("B", "A", "-->", Some("Maybe Fail"));
            })
            .note_right_of("B", "B responds");
            
         let diagram = builder.build();

        assert_eq!(diagram.statements.len(), 5);
        assert!(matches!(diagram.statements[0], Statement::Participant(ref p) if p.id == "A"));
        assert!(matches!(diagram.statements[1], Statement::Participant(ref p) if p.id == "B"));
        assert!(matches!(diagram.statements[2], Statement::Note(ref n) if n.placement == NotePlacement::LeftOf && n.actors == vec!["A"]));
        assert!(matches!(diagram.statements[3], Statement::Loop(ref l) if l.label == Some("Retry Loop".to_string()) && l.statements.len() == 2));
        assert!(matches!(diagram.statements[4], Statement::Note(ref n) if n.placement == NotePlacement::RightOf && n.actors == vec!["B"]));

        // Check nested statements in loop block
        if let Statement::Loop(loop_stmt) = &diagram.statements[3] {
             assert_eq!(loop_stmt.statements.len(), 2);
             assert!(matches!(loop_stmt.statements[0], Statement::Signal(ref s) if s.from == "A" && s.to == "B"));
             assert!(matches!(loop_stmt.statements[1], Statement::Signal(ref s) if s.from == "B" && s.to == "A"));
        } else {
             panic!("Expected Loop statement at index 3");
        }
    }

    #[test]
    fn build_with_autonumber_create_destroy() {
        let mut builder = SequenceDiagramBuilder::new();
        builder.autonumber()
            .participant("Controller")
            .create_actor("Worker")
            .signal("Controller", "Worker", "->", Some("Do work"))
            .destroy("Worker")
            .autonumber_off();
            
        let diagram = builder.build();

        assert_eq!(diagram.statements.len(), 6);
        assert!(matches!(diagram.statements[0], Statement::Autonumber(ref a) if a.start.is_none() && a.increment.is_none() && !a.off));
        assert!(matches!(diagram.statements[1], Statement::Participant(ref p) if p.id == "Controller"));
        assert!(matches!(diagram.statements[2], Statement::Create(ref c) if c.id == "Worker" && c.participant_type == Some(ParticipantType::Actor)));
        assert!(matches!(diagram.statements[3], Statement::Signal(ref s) if s.from == "Controller" && s.to == "Worker"));
        assert!(matches!(diagram.statements[4], Statement::Destroy(ref d) if d.id == "Worker"));
        assert!(matches!(diagram.statements[5], Statement::Autonumber(ref a) if a.off));
    }
}
