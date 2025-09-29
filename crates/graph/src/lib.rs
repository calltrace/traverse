pub mod builtin;
pub mod cg;
pub mod cg_dot;
pub mod cg_json;
pub mod cg_mermaid;
pub mod chains;
pub mod interface_resolver;
pub mod manifest;
pub mod natspec;
pub mod parser;
pub mod reachability;
pub mod steps;
pub mod storage_access;

// Re-export commonly used types for easier access
pub use cg::{CallGraph, Edge, EdgeType, Node, NodeType, ParameterInfo, Visibility};

#[cfg(test)]
mod cg_json_tests;
#[cfg(test)]
mod tests;
