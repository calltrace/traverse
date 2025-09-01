use pest::iterators::Pair;
use crate::sequence_diagram_ast::*;
use pest_derive::Parser;
use pest::Parser;


#[derive(Parser)]
#[grammar = "sequence_diagram.pest"]
pub struct SequenceDiagramParser;

impl From<Pair<'_, Rule>> for SequenceDiagram {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::sequence_diagram => {
                let statements = pair
                    .into_inner()
                    .find(|p| p.as_rule() == Rule::diagram_content)
                    .map(|p| p.into_inner().filter_map(|p| Statement::from_pair(p)).collect())
                    .unwrap_or_default();
                
                SequenceDiagram { statements }
            }
            _ => SequenceDiagram { statements: vec![] },
        }
    }
}

impl Statement {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Option<Self> {
        match pair.as_rule() {
            Rule::signal_statement => Some(Statement::Signal(SignalStatement::from(pair))),
            Rule::participant_statement => Some(Statement::Participant(ParticipantStatement::from(pair))),
            Rule::create_statement => Some(Statement::Create(CreateStatement::from(pair))),
            Rule::destroy_statement => Some(Statement::Destroy(DestroyStatement::from(pair))),
            Rule::box_statement => Some(Statement::Box(BoxStatement::from(pair))),
            Rule::autonumber_statement => Some(Statement::Autonumber(AutonumberStatement::from(pair))),
            Rule::activate_statement => Some(Statement::Activate(ActivateStatement::from(pair))),
            Rule::deactivate_statement => Some(Statement::Deactivate(DeactivateStatement::from(pair))),
            Rule::note_statement => Some(Statement::Note(NoteStatement::from(pair))),
            Rule::links_statement => Some(Statement::Links(LinksStatement::from(pair))),
            Rule::link_statement => Some(Statement::Link(LinkStatement::from(pair))),
            Rule::properties_statement => Some(Statement::Properties(PropertiesStatement::from(pair))),
            Rule::details_statement => Some(Statement::Details(DetailsStatement::from(pair))),
            Rule::title_statement => Some(Statement::Title(TitleStatement::from(pair))),
            Rule::acc_title_statement => Some(Statement::AccTitle(AccTitleStatement::from(pair))),
            Rule::acc_descr_statement => Some(Statement::AccDescr(AccDescrStatement::from(pair))),
            Rule::acc_descr_multiline_statement => Some(Statement::AccDescrMultiline(AccDescrMultilineStatement::from(pair))),
            Rule::loop_statement => Some(Statement::Loop(LoopStatement::from(pair))),
            Rule::rect_statement => Some(Statement::Rect(RectStatement::from(pair))),
            Rule::opt_statement => Some(Statement::Opt(OptStatement::from(pair))),
            Rule::alt_statement => Some(Statement::Alt(AltStatement::from(pair))),
            Rule::par_statement => Some(Statement::Par(ParStatement::from(pair))),
            Rule::par_over_statement => Some(Statement::ParOver(ParOverStatement::from(pair))),
            Rule::break_statement => Some(Statement::Break(BreakStatement::from(pair))),
            Rule::critical_statement => Some(Statement::Critical(CriticalStatement::from(pair))),
            _ => None,
        }
    }
}

impl From<Pair<'_, Rule>> for SignalStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut from = String::new();
        let mut to = String::new();
        let mut arrow = Arrow { sequence: String::new(), activation_marker: None };
        let mut message = None;

        if let Some(signal_content) = pair.into_inner().find(|p| p.as_rule() == Rule::signal_content) {
            let mut inner_pairs = signal_content.into_inner();
            
            if let Some(from_pair) = inner_pairs.next() {
                from = from_pair.as_str().to_string();
            }
            
            if let Some(arrow_pair) = inner_pairs.next() {
                arrow = Arrow::from(arrow_pair);
            }
            
            if let Some(to_pair) = inner_pairs.next() {
                to = to_pair.as_str().to_string();
            }
            
            if let Some(message_pair) = inner_pairs.next() {
                message = Some(Message::from(message_pair));
            }
        }

        SignalStatement { from, to, arrow, message }
    }
}

impl From<Pair<'_, Rule>> for Arrow {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut sequence = String::new();
        let mut activation_marker = None;

        let mut inner_pairs = pair.into_inner();
        
        if let Some(seq_pair) = inner_pairs.next() {
            sequence = seq_pair.as_str().to_string();
        }
        
        if let Some(marker_pair) = inner_pairs.next() {
            activation_marker = match marker_pair.as_str() {
                "+" => Some(ActivationMarker::Activate),
                "-" => Some(ActivationMarker::Deactivate),
                _ => None,
            };
        }

        Arrow { sequence, activation_marker }
    }
}

impl From<Pair<'_, Rule>> for Message {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut wrap_indicator = None;
        let mut content = String::new();
        
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::wrap_indicator => {
                    wrap_indicator = match inner_pair.as_str() {
                        "wrap:" => Some(WrapIndicator::Wrap),
                        "nowrap:" => Some(WrapIndicator::NoWrap),
                        _ => None,
                    };
                },
                Rule::message_content => {
                    content = inner_pair.as_str().to_string();
                },
                _ => {}
            }
        }

        Message { wrap_indicator, content }
    }
}

impl From<Pair<'_, Rule>> for ParticipantStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut participant_type = ParticipantType::Participant;
        let mut id = String::new();
        let mut alias = None;

        let inner_text = pair.as_str();
        if inner_text.trim_start().starts_with("actor") {
            participant_type = ParticipantType::Actor;
        }

        let mut inner_pairs = pair.into_inner();
        
        if let Some(id_pair) = inner_pairs.find(|p| p.as_rule() == Rule::actor_id) {
            id = id_pair.as_str().to_string();
        }
        
        if let Some(alias_pair) = inner_pairs.find(|p| p.as_rule() == Rule::rest_of_line) {
            alias = Some(alias_pair.as_str().to_string());
        }

        ParticipantStatement { participant_type, id, alias }
    }
}

impl From<Pair<'_, Rule>> for CreateStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut participant_type = None;
        let mut id = String::new();
        let mut alias = None;

        let inner_text = pair.as_str();
        if inner_text.contains("participant") {
            participant_type = Some(ParticipantType::Participant);
        } else if inner_text.contains("actor") {
            participant_type = Some(ParticipantType::Actor);
        }

        let mut inner_pairs = pair.into_inner();
        
        if let Some(id_pair) = inner_pairs.find(|p| p.as_rule() == Rule::actor_id) {
            id = id_pair.as_str().to_string();
        }
        
        if let Some(alias_pair) = inner_pairs.find(|p| p.as_rule() == Rule::rest_of_line) {
            alias = Some(alias_pair.as_str().to_string());
        }

        CreateStatement { participant_type, id, alias }
    }
}

impl From<Pair<'_, Rule>> for DestroyStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let id = pair
            .into_inner()
            .find(|p| p.as_rule() == Rule::actor_id)
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        DestroyStatement { id }
    }
}

impl From<Pair<'_, Rule>> for BoxStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = String::new();
        let mut participants = Vec::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(label_pair) = inner_pairs.find(|p| p.as_rule() == Rule::rest_of_line) {
            label = label_pair.as_str().to_string();
        }
        
        if let Some(content_pair) = inner_pairs.find(|p| p.as_rule() == Rule::box_content) {
            participants = content_pair
                .into_inner()
                .map(ParticipantStatement::from)
                .collect();
        }

        BoxStatement { label, participants }
    }
}

impl From<Pair<'_, Rule>> for AutonumberStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut start = None;
        let mut increment = None;
        let off = pair.as_str().contains("off");

        let numbers: Vec<u32> = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::number)
            .filter_map(|p| p.as_str().parse::<u32>().ok())
            .collect();

        if !numbers.is_empty() {
            start = Some(numbers[0]);
        }
        
        if numbers.len() >= 2 {
            increment = Some(numbers[1]);
        }

        AutonumberStatement { start, increment, off }
    }
}

impl From<Pair<'_, Rule>> for ActivateStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let id = pair
            .into_inner()
            .find(|p| p.as_rule() == Rule::actor_id)
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        ActivateStatement { id }
    }
}

impl From<Pair<'_, Rule>> for DeactivateStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let id = pair
            .into_inner()
            .find(|p| p.as_rule() == Rule::actor_id)
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        DeactivateStatement { id }
    }
}

impl From<Pair<'_, Rule>> for NoteStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut placement = NotePlacement::Over;
        let mut actors = Vec::new();
        let mut text = String::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(placement_pair) = inner_pairs.find(|p| p.as_rule() == Rule::note_placement) {
            placement = match placement_pair.as_str() {
                "left of" => NotePlacement::LeftOf,
                "right of" => NotePlacement::RightOf,
                _ => NotePlacement::Over,
            };
        }
        
        if let Some(actors_pair) = inner_pairs.find(|p| p.as_rule() == Rule::actor_pair) {
            actors = actors_pair
                .into_inner()
                .filter(|p| p.as_rule() == Rule::actor_id)
                .map(|p| p.as_str().to_string())
                .collect();
        }
        
        if let Some(text_pair) = inner_pairs.find(|p| p.as_rule() == Rule::rest_of_line) {
            text = text_pair.as_str().to_string();
        }

        NoteStatement { placement, actors, text }
    }
}

impl From<Pair<'_, Rule>> for LinksStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut actor = String::new();
        let mut url = String::new();
        let mut is_json = false;

        let mut inner_pairs = pair.into_inner();
        
        if let Some(actor_pair) = inner_pairs.find(|p| p.as_rule() == Rule::actor_id) {
            actor = actor_pair.as_str().to_string();
        }
        
        if let Some(message_pair) = inner_pairs.find(|p| p.as_rule() == Rule::message) {
            url = message_pair
                .into_inner()
                .find(|p| p.as_rule() == Rule::message_content)
                .map(|p| p.as_str().to_string())
                .unwrap_or_default();
        } else if let Some(json_pair) = inner_pairs.find(|p| p.as_rule() == Rule::json_message) {
            url = json_pair.as_str().to_string();
            is_json = true;
        }

        LinksStatement { actor, url, is_json }
    }
}

impl From<Pair<'_, Rule>> for LinkStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut actor = String::new();
        let mut url = String::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(actor_pair) = inner_pairs.find(|p| p.as_rule() == Rule::actor_id) {
            actor = actor_pair.as_str().to_string();
        }
        
        if let Some(message_pair) = inner_pairs.find(|p| p.as_rule() == Rule::message) {
            url = message_pair
                .into_inner()
                .find(|p| p.as_rule() == Rule::message_content)
                .map(|p| p.as_str().to_string())
                .unwrap_or_default();
        }

        LinkStatement { actor, url }
    }
}

impl From<Pair<'_, Rule>> for PropertiesStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut actor = String::new();
        let mut properties = String::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(actor_pair) = inner_pairs.find(|p| p.as_rule() == Rule::actor_id) {
            actor = actor_pair.as_str().to_string();
        }
        
        if let Some(message_pair) = inner_pairs.find(|p| p.as_rule() == Rule::message) {
            properties = message_pair
                .into_inner()
                .find(|p| p.as_rule() == Rule::message_content)
                .map(|p| p.as_str().to_string())
                .unwrap_or_default();
        }

        PropertiesStatement { actor, properties }
    }
}

impl From<Pair<'_, Rule>> for DetailsStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut actor = String::new();
        let mut details = String::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(actor_pair) = inner_pairs.find(|p| p.as_rule() == Rule::actor_id) {
            actor = actor_pair.as_str().to_string();
        }
        
        if let Some(message_pair) = inner_pairs.find(|p| p.as_rule() == Rule::message) {
            details = message_pair
                .into_inner()
                .find(|p| p.as_rule() == Rule::message_content)
                .map(|p| p.as_str().to_string())
                .unwrap_or_default();
        }

        DetailsStatement { actor, details }
    }
}

impl From<Pair<'_, Rule>> for TitleStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let title = pair
            .into_inner()
            .find(|p| p.as_rule() == Rule::rest_of_line)
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        TitleStatement { title }
    }
}

impl From<Pair<'_, Rule>> for AccTitleStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let title = pair
            .into_inner()
            .find(|p| p.as_rule() == Rule::rest_of_line)
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        AccTitleStatement { title }
    }
}

impl From<Pair<'_, Rule>> for AccDescrStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let description = pair
            .into_inner()
            .find(|p| p.as_rule() == Rule::rest_of_line)
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        AccDescrStatement { description }
    }
}

impl From<Pair<'_, Rule>> for AccDescrMultilineStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let description = pair
            .into_inner()
            .find(|p| p.as_rule() == Rule::acc_descr_multiline_content)
            .map(|p| p.as_str().to_string())
            .unwrap_or_default();

        AccDescrMultilineStatement { description }
    }
}

impl From<Pair<'_, Rule>> for LoopStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = None;
        let mut statements = Vec::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(label_pair) = inner_pairs.find(|p| p.as_rule() == Rule::label) {
            label = Some(label_pair.as_str().to_string());
        }
        
        if let Some(content_pair) = inner_pairs.find(|p| p.as_rule() == Rule::diagram_content) {
            statements = content_pair
                .into_inner()
                .filter_map(Statement::from_pair)
                .collect();
        }

        LoopStatement { label, statements }
    }
}

impl From<Pair<'_, Rule>> for RectStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = None;
        let mut statements = Vec::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(label_pair) = inner_pairs.find(|p| p.as_rule() == Rule::label) {
            label = Some(label_pair.as_str().to_string());
        }
        
        if let Some(content_pair) = inner_pairs.find(|p| p.as_rule() == Rule::diagram_content) {
            statements = content_pair
                .into_inner()
                .filter_map(Statement::from_pair)
                .collect();
        }

        RectStatement { label, statements }
    }
}

impl From<Pair<'_, Rule>> for OptStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = None;
        let mut statements = Vec::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(label_pair) = inner_pairs.find(|p| p.as_rule() == Rule::label) {
            label = Some(label_pair.as_str().to_string());
        }
        
        if let Some(content_pair) = inner_pairs.find(|p| p.as_rule() == Rule::diagram_content) {
            statements = content_pair
                .into_inner()
                .filter_map(Statement::from_pair)
                .collect();
        }

        OptStatement { label, statements }
    }
}

impl From<Pair<'_, Rule>> for AltStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = None;
        let mut main_statements = Vec::new();
        let mut else_sections = Vec::new();

        let mut inner_pairs = pair.into_inner().peekable();
        
        if let Some(label_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::label) {
            label = Some(label_pair.as_str().to_string());
            inner_pairs.next();
        }
        
        if let Some(content_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::diagram_content) {
            main_statements = content_pair
                .clone()
                .into_inner()
                .filter_map(Statement::from_pair)
                .collect();
            inner_pairs.next();
        }
        
        if let Some(else_sections_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::else_sections) {
            let mut else_section_pairs = else_sections_pair.clone().into_inner();
            
            while let (Some(section), Some(content)) = (else_section_pairs.next(), else_section_pairs.next()) {
                let label = section
                    .into_inner()
                    .find(|p| p.as_rule() == Rule::rest_of_line)
                    .map(|p| p.as_str().to_string())
                    .unwrap_or_default();
                
                let statements = content
                    .into_inner()
                    .filter_map(Statement::from_pair)
                    .collect();
                
                else_sections.push(ElseSection { label, statements });
            }
        }

        AltStatement { label, main_statements, else_sections }
    }
}

impl From<Pair<'_, Rule>> for ParStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = None;
        let mut main_statements = Vec::new();
        let mut and_sections = Vec::new();

        let mut inner_pairs = pair.into_inner().peekable();
        
        if let Some(label_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::label) {
            label = Some(label_pair.as_str().to_string());
            inner_pairs.next();
        }
        
        if let Some(content_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::diagram_content) {
            main_statements = content_pair
                .clone()
                .into_inner()
                .filter_map(Statement::from_pair)
                .collect();
            inner_pairs.next();
        }
        
        if let Some(and_sections_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::and_sections) {
            let mut and_section_pairs = and_sections_pair.clone().into_inner();
            
            while let (Some(section), Some(content)) = (and_section_pairs.next(), and_section_pairs.next()) {
                let label = section
                    .into_inner()
                    .find(|p| p.as_rule() == Rule::rest_of_line)
                    .map(|p| p.as_str().to_string())
                    .unwrap_or_default();
                
                let statements = content
                    .into_inner()
                    .filter_map(Statement::from_pair)
                    .collect();
                
                and_sections.push(AndSection { label, statements });
            }
        }

        ParStatement { label, main_statements, and_sections }
    }
}

impl From<Pair<'_, Rule>> for ParOverStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = None;
        let mut main_statements = Vec::new();
        let mut and_sections = Vec::new();

        let mut inner_pairs = pair.into_inner().peekable();
        
        if let Some(label_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::label) {
            label = Some(label_pair.as_str().to_string());
            inner_pairs.next();
        }
        
        if let Some(content_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::diagram_content) {
            main_statements = content_pair
                .clone()
                .into_inner()
                .filter_map(Statement::from_pair)
                .collect();
            inner_pairs.next();
        }
        
        if let Some(and_sections_pair) = inner_pairs.peek().filter(|p| p.as_rule() == Rule::and_sections) {
            let mut and_section_pairs = and_sections_pair.clone().into_inner();
            
            while let (Some(section), Some(content)) = (and_section_pairs.next(), and_section_pairs.next()) {
                let label = section
                    .into_inner()
                    .find(|p| p.as_rule() == Rule::rest_of_line)
                    .map(|p| p.as_str().to_string())
                    .unwrap_or_default();
                
                let statements = content
                    .into_inner()
                    .filter_map(Statement::from_pair)
                    .collect();
                
                and_sections.push(AndSection { label, statements });
            }
        }

        ParOverStatement { label, main_statements, and_sections }
    }
}

impl From<Pair<'_, Rule>> for BreakStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = String::new();
        let mut statements = Vec::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(label_pair) = inner_pairs.find(|p| p.as_rule() == Rule::rest_of_line) {
            label = label_pair.as_str().to_string();
        }
        
        if let Some(content_pair) = inner_pairs.find(|p| p.as_rule() == Rule::diagram_content) {
            statements = content_pair
                .into_inner()
                .filter_map(Statement::from_pair)
                .collect();
        }

        BreakStatement { label, statements }
    }
}

impl From<Pair<'_, Rule>> for CriticalStatement {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut label = None;
        let mut statements = Vec::new();

        let mut inner_pairs = pair.into_inner();
        
        if let Some(label_pair) = inner_pairs.find(|p| p.as_rule() == Rule::label) {
            label = Some(label_pair.as_str().to_string());
        }
        
        if let Some(content_pair) = inner_pairs.find(|p| p.as_rule() == Rule::diagram_content) {
            statements = content_pair
                .into_inner()
                .filter_map(Statement::from_pair)
                .collect();
        }

        CriticalStatement { label, statements }
    }
}

pub fn parse(input: &str) -> Result<SequenceDiagram, pest::error::Error<Rule>> {
    let pairs = SequenceDiagramParser::parse(Rule::sequence_diagram, input)?;
    let diagram = pairs.peek().map(SequenceDiagram::from).unwrap_or_default();
    Ok(diagram)
}

use std::fmt;

/// Error type for high-level Mermaid parsing
#[derive(Debug)]
pub enum MermaidParseError {
    /// Error from the underlying pest parser
    PestError(String),
    /// Error when the input doesn't start with "sequenceDiagram"
    NotASequenceDiagram,
    /// Error when the input is empty
    EmptyInput,
}

impl fmt::Display for MermaidParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MermaidParseError::PestError(err) => write!(f, "Parser error: {}", err),
            MermaidParseError::NotASequenceDiagram => write!(f, "Input is not a sequence diagram (must start with 'sequenceDiagram')"),
            MermaidParseError::EmptyInput => write!(f, "Input is empty"),
        }
    }
}

impl std::error::Error for MermaidParseError {}

impl From<pest::error::Error<Rule>> for MermaidParseError {
    fn from(error: pest::error::Error<Rule>) -> Self {
        MermaidParseError::PestError(error.to_string())
    }
}

/// Parse a Mermaid sequence diagram source code string into an AST representation.
///
/// This is a high-level function that handles common error cases and provides
/// a more user-friendly interface than the lower-level `parse` function.
///
/// # Arguments
///
/// * `source` - The Mermaid sequence diagram source code as a string
///
/// # Returns
///
/// * `Result<SequenceDiagram, MermaidParseError>` - The parsed diagram or an error
///
/// # Examples
///
/// ```rust
/// use mermaid::sequence_diagram_parser::parse_mermaid;
///
/// let source = r#"sequenceDiagram
///     Alice->>Bob: Hello Bob, how are you?
///     Bob-->>Alice: I am good thanks!"#;
///
/// let diagram = parse_mermaid(source).unwrap();
/// assert_eq!(diagram.statements.len(), 2);
/// ```
///
/// # More Examples
///
/// Parsing a complex diagram with multiple features:
///
/// ```rust
/// use mermaid::sequence_diagram_parser::parse_mermaid;
/// use mermaid::sequence_diagram_ast::{Statement, ParticipantStatement, SignalStatement};
///
/// let source = r#"sequenceDiagram
///     participant Alice
///     participant Bob
///     Alice->>Bob: Hello Bob, how are you?
///     Note right of Bob: Bob thinks
///     Bob-->>Alice: I am good thanks!
///     loop Every minute
///         Alice->>Bob: Ping
///         Bob-->>Alice: Pong
///     end"#;
///
/// let diagram = parse_mermaid(source).unwrap();
/// 
/// // Access the statements
/// assert_eq!(diagram.statements.len(), 6);
///
/// // Check participant statements
/// if let Statement::Participant(participant) = &diagram.statements[0] {
///     assert_eq!(participant.id, "Alice");
/// }
///
/// // Check signal statements
/// if let Statement::Signal(signal) = &diagram.statements[2] {
///     assert_eq!(signal.from, "Alice");
///     assert_eq!(signal.to, "Bob");
///     assert_eq!(signal.message.as_ref().unwrap().content, "Hello Bob, how are you?");
/// }
///
/// // Check loop statements
/// if let Statement::Loop(loop_stmt) = &diagram.statements[5] {
///     assert_eq!(loop_stmt.label, Some("Every minute".to_string()));
///     assert_eq!(loop_stmt.statements.len(), 2);
/// }
/// ```
pub fn parse_mermaid(source: &str) -> Result<SequenceDiagram, MermaidParseError> {
    // Check for empty input
    if source.trim().is_empty() {
        return Err(MermaidParseError::EmptyInput);
    }

    // Check if it's a sequence diagram
    if !source.trim().starts_with("sequenceDiagram") {
        return Err(MermaidParseError::NotASequenceDiagram);
    }

    // Use the lower-level parse function and convert any errors
    let diagram = parse(source)?;
    Ok(diagram)
}

#[cfg(test)]
mod high_level_tests {
    use super::*;

    #[test]
    fn test_parse_mermaid_valid() {
        let source = r#"sequenceDiagram
            Alice->>Bob: Hello Bob, how are you?
            Bob-->>Alice: I am good thanks!"#;

        let result = parse_mermaid(source);
        assert!(result.is_ok());
        
        let diagram = result.unwrap();
        assert_eq!(diagram.statements.len(), 2);
    }

    #[test]
    fn test_parse_mermaid_empty() {
        let source = "";
        let result = parse_mermaid(source);
        assert!(result.is_err());
        
        match result {
            Err(MermaidParseError::EmptyInput) => {},
            _ => panic!("Expected EmptyInput error"),
        }
    }

    #[test]
    fn test_parse_mermaid_not_sequence_diagram() {
        let source = "flowchart TD\nA-->B";
        let result = parse_mermaid(source);
        assert!(result.is_err());
        
        match result {
            Err(MermaidParseError::NotASequenceDiagram) => {},
            _ => panic!("Expected NotASequenceDiagram error"),
        }
    }

    #[test]
    fn test_parse_mermaid_syntax_error() {
        let source = r#"sequenceDiagram
            Alice->->Bob: Invalid arrow syntax"#;

        let result = parse_mermaid(source);
        assert!(result.is_err());
        
        match result {
            Err(MermaidParseError::PestError(_)) => {},
            _ => panic!("Expected PestError"),
        }
    }
    
    #[test]
    fn test_parse_complex_diagram() {
        let source = r#"sequenceDiagram
            participant Alice
            participant Bob
            Alice->>Bob: Hello Bob, how are you?
            Note right of Bob: Bob thinks
            Bob-->>Alice: I am good thanks!
            loop Every minute
                Alice->>Bob: Ping
                Bob-->>Alice: Pong
            end"#;

        let result = parse_mermaid(source);
        assert!(result.is_ok());
        
        let diagram = result.unwrap();
        assert_eq!(diagram.statements.len(), 6);
        
        // Check loop statement
        match &diagram.statements[5] {
            Statement::Loop(loop_stmt) => {
                assert_eq!(loop_stmt.label, Some("Every minute".to_string()));
                assert_eq!(loop_stmt.statements.len(), 2);
            },
            _ => panic!("Expected Loop statement"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::sequence_diagram_ast::*;
    use crate::sequence_diagram_parser::parse;

    #[test]
    fn test_simple_diagram() {
        let input = r#"sequenceDiagram
            participant Alice
            participant Bob
            Alice->>Bob: Hello Bob, how are you?
            Bob-->>Alice: I am good thanks!"#;

        let result = parse(input).unwrap();
        
        assert_eq!(result.statements.len(), 4);
        
        match &result.statements[0] {
            Statement::Participant(stmt) => {
                assert_eq!(stmt.id, "Alice");
                assert_eq!(stmt.participant_type, ParticipantType::Participant);
            },
            _ => panic!("Expected Participant statement"),
        }
        
        match &result.statements[2] {
            Statement::Signal(stmt) => {
                assert_eq!(stmt.from, "Alice");
                assert_eq!(stmt.to, "Bob");
                assert_eq!(stmt.arrow.sequence, "->>");
                assert_eq!(stmt.message.as_ref().unwrap().content, "Hello Bob, how are you?");
            },
            _ => panic!("Expected Signal statement"),
        }
    }

    #[test]
    fn test_note_statement() {
        let input = r#"sequenceDiagram
            participant Alice
            participant Bob
            Note left of Alice: Alice thinks
            Note right of Bob: Bob thinks
            Note over Alice,Bob: Both think"#;

        let result = parse(input).unwrap();
        
        assert_eq!(result.statements.len(), 5);
        
        match &result.statements[2] {
            Statement::Note(stmt) => {
                assert_eq!(stmt.placement, NotePlacement::LeftOf);
                assert_eq!(stmt.actors[0], "Alice");
                assert_eq!(stmt.text, "Alice thinks");
            },
            _ => panic!("Expected Note statement"),
        }
        
        match &result.statements[4] {
            Statement::Note(stmt) => {
                assert_eq!(stmt.placement, NotePlacement::Over);
                assert_eq!(stmt.actors.len(), 2);
                assert_eq!(stmt.actors[0], "Alice");
                assert_eq!(stmt.actors[1], "Bob");
                assert_eq!(stmt.text, "Both think");
            },
            _ => panic!("Expected Note statement"),
        }
    }

    #[test]
    fn test_loop_statement() {
        let input = r#"sequenceDiagram
            participant Alice
            participant Bob
            loop Every minute
                Alice->>Bob: Ping
                Bob-->>Alice: Pong
            end"#;

        let result = parse(input).unwrap();
        
        assert_eq!(result.statements.len(), 3);
        
        match &result.statements[2] {
            Statement::Loop(stmt) => {
                assert_eq!(stmt.label, Some("Every minute".to_string()));
                assert_eq!(stmt.statements.len(), 2);
                
                match &stmt.statements[0] {
                    Statement::Signal(signal) => {
                        assert_eq!(signal.from, "Alice");
                        assert_eq!(signal.to, "Bob");
                        assert_eq!(signal.message.as_ref().unwrap().content, "Ping");
                    },
                    _ => panic!("Expected Signal statement"),
                }
            },
            _ => panic!("Expected Loop statement"),
        }
    }

    #[test]
    fn test_alt_statement() {
        let input = r#"sequenceDiagram
            participant Alice
            participant Bob
            alt Success
                Alice->>Bob: Success
            else Error
                Alice->>Bob: Error
            end"#;

        let result = parse(input).unwrap();
        
        assert_eq!(result.statements.len(), 3);
        
        match &result.statements[2] {
            Statement::Alt(stmt) => {
                assert_eq!(stmt.label, Some("Success".to_string()));
                assert_eq!(stmt.main_statements.len(), 1);
                assert_eq!(stmt.else_sections.len(), 1);
                
                assert_eq!(stmt.else_sections[0].label, "Error");
                assert_eq!(stmt.else_sections[0].statements.len(), 1);
                
                match &stmt.else_sections[0].statements[0] {
                    Statement::Signal(signal) => {
                        assert_eq!(signal.message.as_ref().unwrap().content, "Error");
                    },
                    _ => panic!("Expected Signal statement"),
                }
            },
            _ => panic!("Expected Alt statement"),
        }
    }

    #[test]
    fn debug_parsing() {
        let input = r#"sequenceDiagram
            participant Alice
            participant Bob
            Alice->>Bob: Hello Bob, how are you?
            Bob-->>Alice: I am good thanks!"#;

        let result = parse(input).unwrap();
        
        // Print the structure for debugging
        println!("Number of statements: {}", result.statements.len());
        
        for (i, stmt) in result.statements.iter().enumerate() {
            println!("Statement {}: {:?}", i, stmt);
        }
        
        // Check specifically for the signal statement
        if let Statement::Signal(signal) = &result.statements[2] {
            println!("Signal from: {}", signal.from);
            println!("Signal to: {}", signal.to);
            println!("Arrow: {:?}", signal.arrow);
            println!("Message: {:?}", signal.message);
        }
    }
}
