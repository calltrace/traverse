pub mod sequence_diagram_ast;
pub mod sequence_diagram_parser;
pub mod sequence_diagram_builder;
pub mod sequence_diagram_writer;
pub mod mermaid_chunker;

#[cfg(test)]
pub mod sequence_diagram_tests;

// Re-export the high-level parsing function for easier access
pub use sequence_diagram_parser::parse_mermaid;
