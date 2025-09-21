# traverse-solidity

Core Solidity parsing and analysis functionality for the Traverse suite.

## Overview

This crate provides the foundational Solidity parsing capabilities using tree-sitter, enabling syntactic analysis of smart contracts. It serves as the backbone for all Traverse tools that need to understand Solidity code structure.

## Features

- Fast, incremental parsing using tree-sitter
- Complete Solidity syntax support
- AST traversal utilities
- Source location tracking
- Comment preservation

## Usage

```rust
use traverse_solidity::parse_solidity;

fn main() {
    let source_code = r#"
        contract Example {
            function hello() public pure returns (string memory) {
                return "Hello, World!";
            }
        }
    "#;
    
    let tree = parse_solidity(source_code);
    // Process the syntax tree
}
```

## Tree-sitter Grammar

This crate uses a customized tree-sitter grammar for Solidity that provides comprehensive coverage of Solidity syntax including:
- All contract types (contracts, interfaces, libraries, abstract contracts)
- Solidity 0.8+ features
- Inline assembly (Yul)
- NatSpec comments

## Part of Traverse

This crate is part of the [Traverse](https://github.com/calltrace/traverse) suite of tools for Solidity code analysis, visualization, and test generation.

## License

MIT OR Apache-2.0