
# Solidity Call Graph Generator (sol2cg) - High-Level Design

## Overview

This tool analyzes Solidity source code to generate a detailed call graph. This graph represents the interactions between functions, contracts, and state variables, serving as a foundation for static analysis, visualization, and understanding code execution flow.

## Core Workflow and Components

The process involves several distinct stages, orchestrated by a flexible pipeline architecture.

```ascii
+-----------------+      +--------+      +--------------------------+      +-----------+      +---------------+
| Solidity Source | ---> | Parser | ---> | AST (Abstract Syntax Tree)| ---> | Pipeline  | ---> | CallGraph     |
+-----------------+      +--------+      +--------------------------+      | (Steps)   |      | (Data Struct) |
                                                                           +-----------+      +-----------+---+
                                                                                |  ^               |        |
                                                                                |  |               |        |
                                           +------------------------------------+  |               |        |
                                           |  +------------------------------------+               |        |
                                           |  |                                                    |        |
                                           v  v                                                    v        v
+--------------------------------+     +-----------+                                          +--------+   +----------+
| Expression Analyzer (chains.rs)| <-- | Calls Step|                                          | DOT Gen|   | Mermaid Gen|
+--------------------------------+     +-----------+                                          +--------+   +----------+
```

1.  **Parsing:** The input Solidity code is first processed by a `tree-sitter`-based parser, utilizing a specific Solidity grammar. This generates an Abstract Syntax Tree (AST), a structured representation of the code.

2.  **Pipeline Execution:** The core analysis happens within a `CallGraphGeneratorPipeline`. This pipeline executes a sequence of distinct steps (`CallGraphGeneratorStep` implementations). Each step is responsible for a specific aspect of graph generation, operating on the AST and contributing to a shared `CallGraphGeneratorContext` and the final `CallGraph`. Key steps include:
    *   **Entity Handling (`steps::entity`):** Identifies fundamental code structures like contracts, libraries, interfaces, functions, constructors, modifiers, state variables, and inheritance relationships. It populates the initial nodes in the graph and gathers essential context information (e.g., types, inheritance maps).
    *   **Channel Handling (`steps::channel`):** Analyzes function bodies to identify interactions like function calls, event emissions, require checks, and storage access (reads/writes). This step is crucial for building the edges of the call graph.

3.  **Expression & Chain Analysis (`chains.rs`):** To handle complex interactions within the `CallsHandling` step, such as chained calls (`a.b().c()`) or calls on return values, a dedicated `Expression Analyzer` module (`chains.rs`) is employed. It recursively breaks down expressions, resolves intermediate types, and determines the specific target of each call segment. This analysis produces detailed information that the `CallsHandling` step uses to create accurate graph edges with correct sequencing.

4.  **Call Graph Data Structure (`cg.rs`):** The central data structure, `cg::CallGraph`, accumulates the results from the pipeline steps. It stores nodes (representing code elements like functions, contracts, state variables) and edges (representing interactions like calls, returns, storage access). This structure provides the complete representation of the analyzed code's interactions.

5.  **Output Generation (`cg_dot.rs`, `cg_mermaid.rs`):** Finally, the populated `CallGraph` can be transformed into standard visualization formats. Dedicated modules generate DOT language output (for Graphviz) and Mermaid sequence diagram syntax, allowing users to visualize the call graph and execution flow.

## Design Principles

The design emphasizes several key principles:

*   **Decoupling & Cohesiveness:** The pipeline architecture allows different analysis phases (entity identification, call analysis, etc.) to be implemented as independent, cohesive steps (`CallGraphGeneratorStep`). Steps operate on the shared context and graph but don't need direct knowledge of each other's internal workings. This separation of concerns makes the system easier to extend, modify, understand, and test.
*   **Minimal Dependencies:** The core graph generation logic relies primarily on the `tree-sitter` parsing library and its corresponding Solidity grammar. This minimizes external dependencies, simplifying integration and maintenance. Output generation modules (DOT, Mermaid) introduce dependencies specific to those formats.
*   **Parsing Resilience:** By leveraging `tree-sitter`, the parser exhibits tolerance to syntax errors and Solidity language version mismatches. `tree-sitter` attempts to parse as much of the code as possible even in the presence of errors, allowing the analysis pipeline to potentially extract partial call graph information from incomplete or slightly incompatible source files. This is crucial when dealing with diverse codebases.
*   **Performance:** While not the primary focus over correctness and extensibility, performance is considered. The choice of **Rust** as the implementation language provides a strong foundation for performance due to its memory safety guarantees without garbage collection and its efficient compilation to native code. `tree-sitter` provides efficient incremental parsing. The pipeline architecture, while currently sequential, offers potential for future parallelization of independent analysis steps. Direct graph manipulation avoids complex intermediate representations where possible.
