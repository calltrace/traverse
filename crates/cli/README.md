# traverse-cli

Command-line tools for Solidity code analysis, visualization, and test generation.

## Overview

This crate provides the main command-line interface for all Traverse tools. It includes five powerful utilities for analyzing and working with Solidity smart contracts.

## Installation

### Via Homebrew (macOS/Linux)
```bash
brew tap calltrace/tap
brew install traverse
```

### Via Cargo
```bash
cargo install traverse-cli
```

### Download Binaries
Pre-built binaries are available from the [releases page](https://github.com/calltrace/traverse/releases).

## Tools Included

### `sol2cg` - Call Graph Generator
Generates call graphs from Solidity contracts to visualize function relationships.

```bash
# Generate DOT format call graph
sol2cg contracts/*.sol -o callgraph.dot

# Convert to PNG using Graphviz
dot -Tpng callgraph.dot -o callgraph.png

# Generate Mermaid format
sol2cg contracts/*.sol --format mermaid -o callgraph.mmd
```

### `sol2test` - Test Generator
Automatically generates comprehensive test suites for smart contracts.

```bash
# Generate Foundry tests
sol2test contracts/Token.sol -o test/Token.t.sol

# Generate for all contracts
sol2test contracts/*.sol --output test/

# Specify framework
sol2test contracts/*.sol --framework foundry
```

### `sol2bnd` - Binding Generator
Creates TypeScript or Rust bindings for contract interaction.

```bash
# Generate TypeScript bindings
sol2bnd contracts/*.sol --lang typescript -o bindings/

# Generate Rust bindings
sol2bnd contracts/*.sol --lang rust -o src/bindings/
```

### `sol-storage-analyzer` - Storage Layout Analyzer
Analyzes and visualizes contract storage layouts, identifying optimization opportunities.

```bash
# Analyze storage layout
sol-storage-analyzer contracts/Token.sol

# Generate detailed report
sol-storage-analyzer contracts/*.sol --report storage-report.json

# Check for storage collisions
sol-storage-analyzer contracts/*.sol --check-collisions
```

### `storage-trace` - Storage Operation Tracer
Traces and compares storage operations across contract versions.

```bash
# Trace storage operations
storage-trace contracts/TokenV1.sol contracts/TokenV2.sol

# Generate migration report
storage-trace --old contracts/v1/*.sol --new contracts/v2/*.sol
```

## Common Options

All tools support these common options:

- `--help` - Show help information
- `--version` - Show version information
- `-v, --verbose` - Enable verbose output
- `-q, --quiet` - Suppress non-error output

## Configuration

Tools can be configured via:
1. Command-line arguments (highest priority)
2. `.traverse.yml` configuration file
3. Environment variables
4. Default settings

Example `.traverse.yml`:
```yaml
sol2cg:
  format: mermaid
  exclude_internals: true

sol2test:
  framework: foundry
  coverage_threshold: 80
```

## Output Formats

### Call Graphs
- DOT (Graphviz)
- Mermaid
- JSON
- PlantUML

### Test Generation
- Foundry (Solidity)
- Hardhat (JavaScript/TypeScript)
- Truffle (JavaScript)
- Ape (Python)

### Bindings
- TypeScript
- Rust
- Python
- Go

## Examples

### Complete Project Analysis
```bash
# Generate call graph
sol2cg contracts/*.sol -o docs/architecture.dot

# Generate tests
sol2test contracts/*.sol -o test/

# Check storage layout
sol-storage-analyzer contracts/*.sol --report docs/storage.json

# Generate bindings
sol2bnd contracts/*.sol --lang typescript -o sdk/
```

### CI/CD Integration
```bash
# In your CI pipeline
sol2cg contracts/*.sol --check-circular-deps
sol-storage-analyzer contracts/*.sol --check-collisions
sol2test contracts/*.sol --coverage-threshold 80
```

## Performance

All tools are optimized for large codebases:
- Incremental parsing with tree-sitter
- Parallel file processing
- Efficient memory usage
- Fast execution (typically <1s for small projects, <10s for large ones)

## Part of Traverse

This crate is part of the [Traverse](https://github.com/calltrace/traverse) suite of tools for Solidity code analysis, visualization, and test generation.

## Documentation

For detailed documentation, visit [github.com/calltrace/traverse](https://github.com/calltrace/traverse)

## License

MIT OR Apache-2.0