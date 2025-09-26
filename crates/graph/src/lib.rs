pub mod cg;
pub mod cg_dot;
pub mod cg_json;
pub mod cg_mermaid;
pub mod parser;
pub mod natspec;
pub mod interface_resolver;
pub mod chains;
pub mod steps;
pub mod builtin;
pub mod manifest;
pub mod reachability;
pub mod storage_access;

// Re-export commonly used types for easier access
pub use cg::{CallGraph, Node, Edge, NodeType, EdgeType, ParameterInfo, Visibility};

#[cfg(test)]
mod tests;
#[cfg(test)]
mod cg_json_tests;
