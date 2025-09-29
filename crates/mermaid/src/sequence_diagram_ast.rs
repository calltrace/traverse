//! AST types for Mermaid Sequence Diagrams

/// The root node of a sequence diagram
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SequenceDiagram {
    pub statements: Vec<Statement>,
}

impl SequenceDiagram {
    /// Serializes the diagram into a MermaidJS plain text string.
    pub fn to_mermaid_string(&self) -> String {
        // Assuming mermaid_writer::write_diagram is accessible
        // This might require adjusting visibility or imports based on lib.rs structure
        crate::sequence_diagram_writer::write_diagram(self)
    }
}

/// All possible statement types in a sequence diagram
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Signal(SignalStatement),
    Participant(ParticipantStatement),
    Create(CreateStatement),
    Destroy(DestroyStatement),
    Box(BoxStatement),
    Autonumber(AutonumberStatement),
    Activate(ActivateStatement),
    Deactivate(DeactivateStatement),
    Note(NoteStatement),
    Links(LinksStatement),
    Link(LinkStatement),
    Properties(PropertiesStatement),
    Details(DetailsStatement),
    Title(TitleStatement),
    AccTitle(AccTitleStatement),
    AccDescr(AccDescrStatement),
    AccDescrMultiline(AccDescrMultilineStatement),
    Loop(LoopStatement),
    Rect(RectStatement),
    Opt(OptStatement),
    Alt(AltStatement),
    Par(ParStatement),
    ParOver(ParOverStatement),
    Break(BreakStatement),
    Critical(CriticalStatement),
    // --- Added for Alt blocks ---
    AltStart(AltStartStatement),
    AltElse(AltElseStatement),
    AltEnd(AltEndStatement),
}

/// Signal statement (e.g., "Alice->>John: Hello")
#[derive(Debug, Clone, PartialEq)]
pub struct SignalStatement {
    pub from: String,
    pub to: String,
    pub arrow: Arrow,
    pub message: Option<Message>,
}

/// Arrow used in signal statements
#[derive(Debug, Clone, PartialEq)]
pub struct Arrow {
    pub sequence: String,
    pub activation_marker: Option<ActivationMarker>,
}

/// Activation marker (+/-) used in arrows
#[derive(Debug, Clone, PartialEq)]
pub enum ActivationMarker {
    Activate,
    Deactivate,
}

/// Message content (e.g., ": Hello world")
#[derive(Debug, Clone, PartialEq)]
pub struct Message {
    pub wrap_indicator: Option<WrapIndicator>,
    pub content: String,
}

/// Wrap indicator for messages
#[derive(Debug, Clone, PartialEq)]
pub enum WrapIndicator {
    Wrap,
    NoWrap,
}

/// Participant statement (e.g., "participant Alice")
#[derive(Debug, Clone, PartialEq)]
pub struct ParticipantStatement {
    pub participant_type: ParticipantType,
    pub id: String,
    pub alias: Option<String>,
}

/// Type of participant
#[derive(Debug, Clone, PartialEq)]
pub enum ParticipantType {
    Participant,
    Actor,
}

/// Create statement (e.g., "create participant Alice")
#[derive(Debug, Clone, PartialEq)]
pub struct CreateStatement {
    pub participant_type: Option<ParticipantType>,
    pub id: String,
    pub alias: Option<String>,
}

/// Destroy statement (e.g., "destroy Alice")
#[derive(Debug, Clone, PartialEq)]
pub struct DestroyStatement {
    pub id: String,
}

/// Box statement (e.g., "box Blue\nAlice\nend")
#[derive(Debug, Clone, PartialEq)]
pub struct BoxStatement {
    pub label: String,
    pub participants: Vec<ParticipantStatement>,
}

/// Autonumber statement (e.g., "autonumber 10 10")
#[derive(Debug, Clone, PartialEq)]
pub struct AutonumberStatement {
    pub start: Option<u32>,
    pub increment: Option<u32>,
    pub off: bool,
}

/// Activate statement (e.g., "activate Alice")
#[derive(Debug, Clone, PartialEq)]
pub struct ActivateStatement {
    pub id: String,
}

/// Deactivate statement (e.g., "deactivate Alice")
#[derive(Debug, Clone, PartialEq)]
pub struct DeactivateStatement {
    pub id: String,
}

/// Note statement (e.g., "Note left of Alice: Hello")
#[derive(Debug, Clone, PartialEq)]
pub struct NoteStatement {
    pub placement: NotePlacement,
    pub actors: Vec<String>,
    pub text: String,
}

/// Note placement
#[derive(Debug, Clone, PartialEq)]
pub enum NotePlacement {
    LeftOf,
    RightOf,
    Over,
}

/// Links statement (e.g., "links Alice: https://example.com")
#[derive(Debug, Clone, PartialEq)]
pub struct LinksStatement {
    pub actor: String,
    pub url: String,
    pub is_json: bool,
}

/// Link statement (e.g., "link Alice: https://example.com")
#[derive(Debug, Clone, PartialEq)]
pub struct LinkStatement {
    pub actor: String,
    pub url: String,
}

/// Properties statement (e.g., "properties Alice: {color: blue}")
#[derive(Debug, Clone, PartialEq)]
pub struct PropertiesStatement {
    pub actor: String,
    pub properties: String,
}

/// Details statement (e.g., "details Alice: more info")
#[derive(Debug, Clone, PartialEq)]
pub struct DetailsStatement {
    pub actor: String,
    pub details: String,
}

/// Title statement (e.g., "title My Diagram")
#[derive(Debug, Clone, PartialEq)]
pub struct TitleStatement {
    pub title: String,
}

/// Accessibility title statement (e.g., "accTitle: Accessible Title")
#[derive(Debug, Clone, PartialEq)]
pub struct AccTitleStatement {
    pub title: String,
}

/// Accessibility description statement (e.g., "accDescr: Accessible Description")
#[derive(Debug, Clone, PartialEq)]
pub struct AccDescrStatement {
    pub description: String,
}

/// Multiline accessibility description (e.g., "accDescr { ... }")
#[derive(Debug, Clone, PartialEq)]
pub struct AccDescrMultilineStatement {
    pub description: String,
}

/// Loop statement (e.g., "loop Every minute\n...\nend")
#[derive(Debug, Clone, PartialEq)]
pub struct LoopStatement {
    pub label: Option<String>,
    pub statements: Vec<Statement>,
}

/// Rect statement (e.g., "rect rgb(0, 255, 0)\n...\nend")
#[derive(Debug, Clone, PartialEq)]
pub struct RectStatement {
    pub label: Option<String>,
    pub statements: Vec<Statement>,
}

/// Opt statement (e.g., "opt Successful case\n...\nend")
#[derive(Debug, Clone, PartialEq)]
pub struct OptStatement {
    pub label: Option<String>,
    pub statements: Vec<Statement>,
}

/// Alt statement with else sections (e.g., "alt Success\n...\nelse Failure\n...\nend")
#[derive(Debug, Clone, PartialEq)]
pub struct AltStatement {
    pub label: Option<String>,
    pub main_statements: Vec<Statement>,
    pub else_sections: Vec<ElseSection>,
}

/// Else section in an alt statement
#[derive(Debug, Clone, PartialEq)]
pub struct ElseSection {
    pub label: String,
    pub statements: Vec<Statement>,
}

/// Par statement with and sections (e.g., "par Action 1\n...\nand Action 2\n...\nend")
#[derive(Debug, Clone, PartialEq)]
pub struct ParStatement {
    pub label: Option<String>,
    pub main_statements: Vec<Statement>,
    pub and_sections: Vec<AndSection>,
}

/// Par Over statement with and sections
#[derive(Debug, Clone, PartialEq)]
pub struct ParOverStatement {
    pub label: Option<String>,
    pub main_statements: Vec<Statement>,
    pub and_sections: Vec<AndSection>,
}

/// And section in a par statement
#[derive(Debug, Clone, PartialEq)]
pub struct AndSection {
    pub label: String,
    pub statements: Vec<Statement>,
}

/// Break statement (e.g., "break Error\n...\nend")
#[derive(Debug, Clone, PartialEq)]
pub struct BreakStatement {
    pub label: String,
    pub statements: Vec<Statement>,
}

/// Critical statement (e.g., "critical Database transaction\n...\nend")
#[derive(Debug, Clone, PartialEq)]
pub struct CriticalStatement {
    pub label: Option<String>,
    pub statements: Vec<Statement>,
}

// --- Added for Alt blocks ---

/// Alt Start statement (e.g., "alt Condition 1")
#[derive(Debug, Clone, PartialEq)]
pub struct AltStartStatement {
    pub label: String, // The condition text
}

/// Else statement within an Alt block (e.g., "else Condition 2")
#[derive(Debug, Clone, PartialEq)]
pub struct AltElseStatement {
    pub label: String, // The condition text
}

/// End statement for Alt/Loop/Opt etc. (e.g., "end")
#[derive(Debug, Clone, PartialEq)]
pub struct AltEndStatement {}
// --- End Added for Alt blocks ---
