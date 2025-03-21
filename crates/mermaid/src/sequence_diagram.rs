use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "sequence_diagram.pest"]
pub struct SequenceDiagramParser;
