# Traverse: Solidity Analysis Tools

Analyze, visualize, and test Solidity smart contracts. Built with Rust and tree-sitter parsing.

> 📖 **For architectural details and design principles, see [DESIGN.md](DESIGN.md)**

## Table of Contents

- [Quick Start](#quick-start)
- [Installation](#installation)
- [Tools Included](#tools-included)
- [Tool: sol2cg - Call Graph & Sequence Diagram Generator](#tool-sol2cg---call-graph--sequence-diagram-generator)
- [Tool: sol2test - Foundry Test Generator](#tool-sol2test---foundry-test-generator)
- [Tool: sol-storage-analyzer - Storage Access Analyzer](#tool-sol-storage-analyzer---storage-access-analyzer)
- [Tool: storage-trace - Storage Trace Comparator](#tool-storage-trace---storage-trace-comparator)
- [Tool: sol2bnd - Binding File Generator](#tool-sol2bnd---binding-file-generator)
- [Development Workflow Integration](#development-workflow-integration)
- [Tool Integration & Workflows](#tool-integration--workflows)
- [Building from Source](#building-from-source)
- [Docker Usage](#docker-usage)
- [Configuration](#configuration)
- [Troubleshooting](#troubleshooting)
- [Performance & Limitations](#performance--limitations)
- [Compatibility](#compatibility)
- [Contributing](#contributing)
- [License](#license)

## Quick Start

```bash
# Download sol2cg (choose your platform)
# macOS Intel:
curl -sSfL -o /usr/local/bin/sol2cg https://github.com/calltrace/traverse/releases/latest/download/sol2cg-macos-amd64
chmod +x /usr/local/bin/sol2cg

# macOS Apple Silicon:
curl -sSfL -o /usr/local/bin/sol2cg https://github.com/calltrace/traverse/releases/latest/download/sol2cg-macos-arm64
chmod +x /usr/local/bin/sol2cg

# Linux x86_64:
curl -sSfL -o /usr/local/bin/sol2cg https://github.com/calltrace/traverse/releases/latest/download/sol2cg-linux-amd64
chmod +x /usr/local/bin/sol2cg

# Generate a call graph visualization
sol2cg contracts/*.sol -o callgraph.dot

# Convert to PNG using Graphviz
dot -Tpng callgraph.dot -o callgraph.png
```

## Installation

### Install via Homebrew (macOS/Linux)

```bash
brew tap calltrace/tap
brew install traverse
```

This installs all traverse tools: `sol2cg`, `sol2test`, `sol-storage-analyzer`, `storage-trace`, and `sol2bnd`.

### Download Binaries

Get pre-built binaries from the [releases page](https://github.com/calltrace/traverse/releases/latest) or use curl:

```bash
# Example: Download sol2cg on macOS Apple Silicon
curl -sSfL -o /usr/local/bin/sol2cg \
  https://github.com/calltrace/traverse/releases/latest/download/sol2cg-macos-arm64
chmod +x /usr/local/bin/sol2cg
```

Replace `sol2cg` with any tool name and choose your platform:
- `macos-amd64` (Intel Mac)
- `macos-arm64` (Apple Silicon)
- `linux-amd64` (Linux x86_64)
- `windows-amd64.exe` (Windows)

### Docker

Run tools without installation using Docker:

```bash
# Pull pre-built images from GitHub Container Registry
docker pull ghcr.io/calltrace/traverse:sol2cg
docker pull ghcr.io/calltrace/traverse:all  # All tools in one image

# Run a specific tool
docker run --rm -v $(pwd):/workspace ghcr.io/calltrace/traverse:sol2cg /workspace/contracts/*.sol

# Use docker-compose for complex workflows
curl -O https://raw.githubusercontent.com/calltrace/traverse/main/docker-compose.yml
docker-compose up
```

### Install via Cargo

For Rust developers who have cargo installed:

```bash
# Install all CLI tools at once (available after rate limit expires)
cargo install traverse-cli

# This will install all binary tools:
# - sol2cg: Call graph generator
# - sol2test: Test generator
# - sol2bnd: Solidity boundary analyzer
# - sol-storage-analyzer: Storage access analyzer
# - storage-trace: Storage operation tracer
```

The `traverse-cli` package will be available after Sept 19, 2025 (crates.io rate limit). Build from source meanwhile.

### Build from Source

For contributors and advanced users:

#### Prerequisites

- **Rust toolchain**: Version 1.83 or later ([Install Rust](https://rustup.rs/))
- **Git**: For cloning the repository with submodules

```bash
# Clone the repository with submodules
git clone --recursive https://github.com/calltrace/traverse.git
cd traverse

# Build all tools in release mode
cargo build --release

# Binaries will be available in target/release/
ls -la target/release/sol*

# Optional: Install to system path
cargo install --path crates/cli
```

## Tools Included

| Tool | Purpose | Primary Use Case |
|------|---------|------------------|
| `sol2cg` | Generate call graphs and sequence diagrams | Visualizing contract interactions and control flow |
| `sol2test` | Generate Foundry test stubs | Creating comprehensive test suites automatically |
| `sol-storage-analyzer` | Analyze storage read/write patterns | Understanding state variable access in functions |
| `storage-trace` | Compare storage traces between functions | Finding storage access differences |
| `sol2bnd` | Generate binding configuration files | Creating interface bindings from Natspec |

## sol2cg - Call Graph Generator

Visualize function calls and contract interactions as graphs (DOT) or sequence diagrams (Mermaid). Analyzes complete projects in ~500ms.

### Usage

```bash
sol2cg [OPTIONS] <INPUT_PATHS>...
```

### Options
* `-o, --output-file <OUTPUT_FILE>`: Output file path. If not specified, output goes to stdout
* `-f, --format <FORMAT>`: Output format [default: dot] [possible values: dot, mermaid]
* `--disable-steps <DISABLE_STEPS>`: Disable specific pipeline steps (comma-separated list). Available steps: Contract-Handling, Calls-Handling
* `--enable-steps <ENABLE_STEPS>`: Enable specific pipeline steps (comma-separated list)
* `--config <CONFIG>`: Configuration parameters for pipeline steps (format: key=value,key2=value2)
* `--exclude-isolated-nodes`: [DOT format only] Exclude nodes that have no incoming or outgoing edges
* `--no-chunk`: [Mermaid format only] Disable automatic chunking of large diagrams
* `--chunk-dir <CHUNK_DIR>`: [Mermaid format only] Directory for chunked output (default: ./mermaid-chunks/)
* `--bindings <BINDINGS>`: Optional path to the binding.yaml file for interface resolution
* `--manifest-file <MANIFEST_FILE>`: Optional path to a pre-generated manifest.yaml file
* `-h, --help`: Print help information
* `-V, --version`: Print version information

### Examples

See the [Smart Invoice Example](examples/smart-invoice/) for a complete real-world analysis of a production DeFi application.

```bash
# Generate DOT graph from a single file to stdout
sol2cg path/to/your/Contract.sol

# Generate Mermaid sequence diagram from multiple files
sol2cg -f mermaid -o output/diagram.mmd src/ContractA.sol src/ContractB.sol

# Generate DOT graph from all .sol files in a directory
sol2cg -f dot -o output/full_graph.dot ./contracts/

# Generate a call graph with specific pipeline steps disabled
sol2cg --disable-steps "Calls-Handling" -o output/definitions_only.dot ./contracts/

# Generate a call graph with custom configuration
sol2cg --config "max_depth=3,include_internal=false" -o output/custom_graph.dot ./contracts/

# Exclude isolated nodes from the graph
sol2cg --exclude-isolated-nodes -o clean_graph.dot ./contracts/
```

### Output Formats

* **DOT**: For Graphviz visualization (`dot -Tsvg graph.dot -o graph.svg`)
* **Mermaid**: Works in GitHub/GitLab markdown, automatically chunks large diagrams

## sol2test - Foundry Test Generator

Generates Foundry test suites from your contracts. Creates deployment scripts, setup functions, and test stubs for all public/external functions.

**Requires**: Foundry installed

### Usage

```bash
sol2test [OPTIONS] [INPUT_PATHS]...
```

### Options
* `--project <PROJECT>`: Process a Foundry project directory instead of individual files
* `-o, --output-dir <OUTPUT_DIR>`: Output directory for generated tests [default: foundry-tests/test]
* `-t, --template-dir <TEMPLATE_DIR>`: Template directory [default: templates]
* `-v, --verbose`: Enable verbose output
* `--use-foundry`: Use Foundry for compilation and validation
* `--validate-compilation`: Validate that generated tests compile
* `--deployer-only`: Generate only deployment tests
* `--disable-steps <DISABLE_STEPS>`: Disable specific pipeline steps
* `--enable-steps <ENABLE_STEPS>`: Enable specific pipeline steps
* `--config <CONFIG>`: Configuration parameters for pipeline steps
* `--bindings <BINDINGS>`: Path to binding.yaml file
* `--manifest-file <MANIFEST_FILE>`: Path to pre-generated manifest.yaml
* `--foundry-root <FOUNDRY_ROOT>`: Root directory of Foundry project
* `-h, --help`: Print help information
* `-V, --version`: Print version information

### Examples

```bash
# Generate tests for a single contract
sol2test contracts/Token.sol -o test/

# Process an entire Foundry project
sol2test --project ./my-foundry-project --use-foundry

# Generate tests with compilation validation
sol2test contracts/*.sol --validate-compilation --use-foundry

# Generate only deployment tests
sol2test --deployer-only contracts/ComplexProtocol.sol

# Use custom templates
sol2test -t custom-templates/ contracts/*.sol

# Verbose output for debugging
sol2test -v contracts/*.sol
```

### Output

The tool generates:
* Test contract files with proper imports
* Deployment and setup functions
* Test function stubs for all public/external functions
* Helper functions for common testing patterns

Example generated test structure:
```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../src/Token.sol";

contract TokenTest is Test {
    Token token;
    
    function setUp() public {
        token = new Token();
    }
    
    function test_transfer() public {
        // TODO: Implement test
    }
}
```

## sol-storage-analyzer

Maps storage reads and writes for all public/external functions. Identifies state modification patterns and optimization opportunities.

### Usage

```bash
sol-storage-analyzer [OPTIONS] <INPUT_PATHS>...
```

### Options
* `-o, --output-file <OUTPUT_FILE>`: Output file for the analysis report
* `--bindings <BINDINGS>`: Path to binding.yaml file
* `--manifest-file <MANIFEST_FILE>`: Path to pre-generated manifest.yaml
* `-h, --help`: Print help information
* `-V, --version`: Print version information

### Examples

```bash
# Analyze a single contract
sol-storage-analyzer contracts/Vault.sol

# Analyze multiple contracts and save to file
sol-storage-analyzer contracts/*.sol -o storage-report.md

# Analyze with interface bindings
sol-storage-analyzer --bindings bindings.yaml contracts/

# Use pre-generated manifest
sol-storage-analyzer --manifest-file manifest.yaml contracts/Protocol.sol
```

### Output

Generates a markdown table showing:
* Function endpoints (Contract.function format)
* Storage variables read
* Storage variables written

Example output:
```markdown
| Endpoint | Reads | Writes |
|----------|-------|--------|
| Token.transfer | balances, allowances | balances |
| Token.approve | | allowances |
| Token.balanceOf | balances | |
```

## storage-trace

Compares storage access between two functions. Useful for upgrade compatibility and refactoring validation.

### Usage

```bash
storage-trace [OPTIONS] --func1 <FUNC1> --func2 <FUNC2> <INPUT_PATHS>...
```

### Options
* `--func1 <FUNC1>`: First function to compare (format: functionName or Contract.functionName)
* `--func2 <FUNC2>`: Second function to compare

* `-o, --output-file <OUTPUT_FILE>`: Output file for comparison report
* `--bindings <BINDINGS>`: Path to binding.yaml file
* `--manifest-file <MANIFEST_FILE>`: Path to pre-generated manifest.yaml
* `-h, --help`: Print help information
* `-V, --version`: Print version information

### Examples

```bash
# Compare two functions in the same contract
storage-trace --func1 deposit --func2 depositFor contracts/Vault.sol

# Compare functions across contracts
storage-trace --func1 TokenV1.transfer --func2 TokenV2.transfer contracts/

# Save comparison to file
storage-trace --func1 stake --func2 stakeFor contracts/*.sol -o comparison.md

# Use with bindings
storage-trace --func1 swap --func2 swapExact --bindings bindings.yaml contracts/
```

### Output

Generates a detailed comparison showing:
* Storage variables accessed by each function
* Differences in read patterns
* Differences in write patterns
* Common access patterns

Example output:
```
Storage Trace Comparison: deposit vs depositFor

Function 1 (deposit):
- Reads: [balance, totalSupply]
- Writes: [balance, totalSupply, lastUpdate]

Function 2 (depositFor):
- Reads: [balance, totalSupply, allowance]
- Writes: [balance, totalSupply, lastUpdate, allowance]

Differences:
- Only in depositFor reads: [allowance]
- Only in depositFor writes: [allowance]
```

## sol2bnd

Generates binding configurations from Natspec annotations. Maps interfaces to implementations.

### Usage

```bash
sol2bnd [OPTIONS] <PROJECT_PATH>
```

### Arguments & Options

**Arguments:**
* `<PROJECT_PATH>`: Path to the Solidity project directory

**Options:**
* `-o, --output-file <OUTPUT_FILE>`: Output path for the binding file [default: binding.yaml]
* `-h, --help`: Print help information
* `-V, --version`: Print version information

### Examples

```bash
# Generate bindings for a project
sol2bnd ./contracts -o bindings.yaml

# Generate bindings for current directory
sol2bnd .

# Generate with default output name
sol2bnd my-project/
```

Creates YAML file with interface mappings and contract metadata.

## Development Workflow Integration

### Editor Integration

Traverse provides first-class editor support through dedicated extensions and language server protocol (LSP) implementations:

#### VSCode Extension

Install the **Traverse Solidity Analyzer** extension for integrated Solidity analysis directly in VSCode:

- **Repository**: [traverse-vscode](https://github.com/calltrace/traverse-vscode)
- **Marketplace**: [Traverse Solidity Analyzer](https://marketplace.visualstudio.com/items?itemName=GianlucaBrigandi.traverse-vscode)
- **Features**:
  - Call graph generation (DOT format)
  - Sequence diagram creation (Mermaid format)
  - Storage variable analysis and access patterns
  - Automatic LSP server binary download on first use

Install from VSCode Marketplace:
```bash
# Search "Traverse Solidity Analyzer" in Extensions panel or use command:
code --install-extension GianlucaBrigandi.traverse-vscode
```

#### Language Server Protocol (LSP)

For Neovim, Emacs, and other LSP-compatible editors, use the **Traverse Language Server**:

- **Repository**: [traverse-lsp](https://github.com/calltrace/traverse-lsp)
- **Supported Editors**: Neovim, Emacs, Sublime Text, Helix, any LSP-compatible editor
- **Features**:
  - Go-to-definition for cross-contract calls
  - Hover information with function signatures and storage access
  - Code completion for contract interfaces
  - Diagnostics for potential issues
  - Document symbols and outline

##### Neovim Setup

```lua
-- In your init.lua or config
require('lspconfig').traverse.setup{
  cmd = {'traverse-lsp'},
  filetypes = {'solidity'},
  root_dir = require('lspconfig').util.root_pattern('foundry.toml', '.git'),
}
```

##### Other Editors

Refer to the [traverse-lsp documentation](https://github.com/calltrace/traverse-lsp#installation) for setup instructions specific to your editor.

## Tool Integration & Workflows

The Traverse tools are designed to work together in analysis pipelines. Here are common workflows:

### Workflow 1: Complete Contract Analysis

```bash
# 1. Generate bindings from Natspec
sol2bnd contracts/ -o bindings.yaml

# 2. Create call graph visualization
sol2cg --bindings bindings.yaml contracts/*.sol -o callgraph.dot
dot -Tpng callgraph.dot -o callgraph.png

# 3. Analyze storage patterns
sol-storage-analyzer --bindings bindings.yaml contracts/*.sol -o storage-report.md

# 4. Generate comprehensive tests
sol2test --bindings bindings.yaml contracts/*.sol -o test/
```

### Workflow 2: Upgrade Safety Check

```bash
# 1. Compare storage layouts between versions
storage-trace --func1 TokenV1.transfer --func2 TokenV2.transfer contracts/

# 2. Analyze storage access changes
sol-storage-analyzer contracts/TokenV1.sol -o v1-storage.md
sol-storage-analyzer contracts/TokenV2.sol -o v2-storage.md
diff v1-storage.md v2-storage.md

# 3. Visualize call flow changes
sol2cg contracts/TokenV1.sol -o v1-calls.dot
sol2cg contracts/TokenV2.sol -o v2-calls.dot
```

### Workflow 3: Security Audit Preparation

```bash
# 1. Generate comprehensive documentation
sol2cg -f mermaid contracts/*.sol -o architecture.mmd
sol-storage-analyzer contracts/*.sol -o storage-analysis.md

# 2. Identify complex interactions
sol2cg --config "max_depth=10" contracts/*.sol -o deep-calls.dot

# 3. Generate test coverage baseline
sol2test --validate-compilation contracts/*.sol
```

## Building from Source

### System Requirements

* **Operating System**: Linux, macOS, or Windows
* **Memory**: Minimum 4GB RAM (8GB recommended for large projects)
* **Disk Space**: 2GB for build artifacts
* **Rust**: Version 1.83 or later

### Build Instructions

```bash
# Clone with submodules (required for tree-sitter grammars)
git clone --recursive https://github.com/calltrace/traverse.git
cd traverse

# Build all tools in release mode (optimized)
cargo build --release

# Build specific tool only
cargo build --release --bin sol2cg

# Build with debug symbols (for development)
cargo build

# Run tests
cargo test --workspace
```

### Build Output

After successful build, binaries are located in:
* **Release builds**: `target/release/`
  - `sol2cg`
  - `sol2test`
  - `sol-storage-analyzer`
  - `storage-trace`
  - `sol2bnd`
* **Debug builds**: `target/debug/` (larger, with debug symbols)

### Build Optimization

For maximum performance:
```toml
# Add to Cargo.toml or use with RUSTFLAGS
[profile.release]
lto = true          # Link-time optimization
codegen-units = 1   # Single codegen unit
strip = true        # Strip symbols
opt-level = 3       # Maximum optimization
```

## Docker Usage

The project provides Docker images for all tools, both individually and as an all-in-one image.

### Building Docker Images

```bash
# Build all images
docker build -t traverse .

# Build specific tool image
docker build --target sol2cg -t traverse:sol2cg .
docker build --target sol2test -t traverse:sol2test .
docker build --target sol-storage-analyzer -t traverse:storage-analyzer .
docker build --target storage-trace -t traverse:storage-trace .
docker build --target sol2bnd -t traverse:sol2bnd .

# Build all-in-one image
docker build --target all -t traverse:all .
```

### Running Tools via Docker

#### Individual Tool Images

```bash
# sol2cg
docker run --rm \
  -v $(pwd)/contracts:/workspace \
  traverse:sol2cg /workspace/*.sol

# sol2test with output directory
docker run --rm \
  -v $(pwd)/contracts:/workspace \
  -v $(pwd)/tests:/output \
  traverse:sol2test /workspace/*.sol -o /output

# sol-storage-analyzer
docker run --rm \
  -v $(pwd):/workspace \
  traverse:storage-analyzer /workspace/contracts/*.sol

# storage-trace
docker run --rm \
  -v $(pwd):/workspace \
  traverse:storage-trace \
  --func1 deposit --func2 withdraw \
  /workspace/contracts/*.sol

# sol2bnd
docker run --rm \
  -v $(pwd):/workspace \
  traverse:sol2bnd /workspace/contracts -o /workspace/bindings.yaml
```

#### All-in-One Image

```bash
# Run any tool by overriding entrypoint
docker run --rm \
  -v $(pwd):/workspace \
  --entrypoint sol2test \
  traverse:all /workspace/contracts/*.sol

# Or use default (sol2cg)
docker run --rm \
  -v $(pwd):/workspace \
  traverse:all /workspace/contracts/*.sol
```

### Docker Compose

For complex workflows, use docker-compose:

```yaml
version: '3.8'
services:
  sol2cg:
    image: traverse:sol2cg
    volumes:
      - ./contracts:/workspace
      - ./output:/output
    command: ["/workspace", "-o", "/output/callgraph.dot"]
  
  sol2test:
    image: traverse:sol2test
    volumes:
      - ./contracts:/workspace
      - ./tests:/output
    command: ["/workspace", "-o", "/output"]
```

## Configuration

### Environment Variables

```bash
# Set log level (trace, debug, info, warn, error)
export RUST_LOG=info

# Set specific module logging
export RUST_LOG=sol2cg=debug,traverse=info

# Disable color output
export NO_COLOR=1
```

### Configuration Files

While most configuration is done via command-line arguments, some tools support configuration files:

#### binding.yaml

Used by multiple tools for interface resolution:
```yaml
interfaces:
  - name: IERC20
    file: interfaces/IERC20.sol
    
implementations:
  - interface: IERC20
    contract: Token
    file: contracts/Token.sol
```

#### Pipeline Configuration

Tools using the pipeline architecture accept configuration via `--config`:
```bash
sol2cg --config "max_depth=5,include_modifiers=true,skip_internals=false"
```

## Troubleshooting

**Tree-sitter grammar not found**: Run `git submodule update --init --recursive`

**Foundry not found**: Install with `curl -L https://foundry.paradigm.xyz | bash && foundryup`

**Large outputs**: Use `--exclude-isolated-nodes` or `--config "max_depth=3"`

**Debug mode**: `RUST_LOG=debug sol2cg contracts/*.sol`

For help, run `sol2cg --help` or check [GitHub issues](https://github.com/calltrace/traverse/issues).

## Performance & Limitations

### Performance Characteristics

Based on real-world benchmarks analyzing the [Smart Invoice](examples/smart-invoice/) project (17 contracts, 580 functions, 790 edges) on Apple M3:

* **Processing Speed**: Complete project analysis in ~500ms
* **Call Graph Generation**: DOT format in ~520ms, Mermaid with chunking in ~430ms
* **Output Efficiency**: 206KB DOT file represents 580 nodes and 790 edges
* **Scalability**: Automatic chunking for large diagrams (splits at ~400-600 lines per chunk)
* **Concurrent Processing**: Tools process files sequentially (parallelization planned)

### Limitations

- Best support for Solidity 0.8.x
- Cannot trace dynamic calls (`address.call()`)
- No inline assembly support
- Requires source code for all contracts

For large projects, use `--exclude-isolated-nodes` or process in batches.

## Compatibility

### Solidity Version Support

| Solidity Version | Support Level | Notes |
|-----------------|---------------|-------|
| 0.8.x | Full | Recommended version |
| 0.7.x | Good | Minor limitations |
| 0.6.x | Partial | Some features unsupported |
| 0.5.x and below | Limited | Basic parsing only |

### Framework Compatibility

| Framework | Integration | Notes |
|-----------|------------|-------|
| Foundry | Full | Native support for project structure and testing |
| Hardhat | Not supported | Planned for future release |
| Truffle | Not supported | Planned for future release |
| Brownie | Not supported | Planned for future release |

Currently supports Foundry. Can analyze individual Solidity files from any framework.

### Operating System Support

| OS | Support | Notes |
|----|---------|-------|
| Linux | Full | Primary development platform |
| macOS | Full | Intel and Apple Silicon |
| Windows | Full | Native and WSL2 |

## Using Traverse as a Library

Traverse's modular architecture allows you to use its components as libraries in your own Rust projects. This is useful for building custom analysis tools, IDE plugins, or specialized CLIs.

### Available Libraries

The following crates are available on crates.io:

```toml
[dependencies]
traverse-graph = "0.1.2"      # Call graph generation and analysis
traverse-codegen = "0.1.2"    # Test generation and code synthesis
traverse-solidity = "0.1.2"   # Solidity parser and AST
traverse-mermaid = "0.1.2"    # Mermaid diagram generation
traverse-logging = "0.1.2"    # Logging utilities
```

### Example: Building a Custom Analysis Tool

```rust
use traverse_graph::cg::{CallGraph, CallGraphGeneratorContext, CallGraphGeneratorInput};
use traverse_graph::parser::parse_solidity;
use std::fs;

fn main() -> anyhow::Result<()> {
    // Parse Solidity source
    let source = fs::read_to_string("contract.sol")?;
    let tree = parse_solidity(&source)?;
    
    // Create call graph context
    let mut ctx = CallGraphGeneratorContext::default();
    let mut graph = CallGraph::new();
    
    // Generate call graph
    let input = CallGraphGeneratorInput {
        source: &source,
        tree,
        file_path: "contract.sol".into(),
    };
    
    // Analyze the graph
    for node in graph.nodes.iter() {
        println!("Function: {} in {}", 
            node.name, 
            node.contract_name.as_ref().unwrap_or(&"<global>".to_string())
        );
    }
    
    Ok(())
}
```

### Example: Custom Test Generator

```rust
use traverse_codegen::teststubs::{ContractInfo, generate_tests_with_foundry};
use traverse_graph::cg::CallGraph;

fn generate_custom_tests(graph: &CallGraph) -> anyhow::Result<()> {
    // Extract contract information
    let contracts = extract_contract_info_from_graph(graph);
    
    // Generate Foundry tests
    let foundry_config = FoundryConfig {
        project_root: "./".into(),
        verbose: true,
    };
    
    generate_tests_with_foundry(
        graph,
        &ctx,
        &foundry_config,
        false, // don't validate
    )?;
    
    Ok(())
}
```

### Example: Mermaid Diagram Generation

```rust
use traverse_graph::cg::CallGraph;
use traverse_graph::cg_mermaid::{MermaidGenerator, ToSequenceDiagram};
use traverse_mermaid::sequence_diagram_writer;

fn export_to_mermaid(graph: &CallGraph) -> String {
    let generator = MermaidGenerator::new();
    let sequence_diagram = generator.to_sequence_diagram(graph);
    sequence_diagram_writer::write_diagram(&sequence_diagram)
}
```

### Building Your Own CLI

```rust
use clap::Parser;
use traverse_graph::parser::parse_solidity;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct MyCli {
    /// Solidity files to analyze
    input_files: Vec<String>,
    
    /// Custom analysis flag
    #[arg(long)]
    deep_analysis: bool,
}

fn main() -> anyhow::Result<()> {
    let cli = MyCli::parse();
    
    for file in cli.input_files {
        let source = std::fs::read_to_string(&file)?;
        let tree = parse_solidity(&source)?;
        
        // Your custom analysis logic here
        if cli.deep_analysis {
            // Perform deep analysis
        }
    }
    
    Ok(())
}
```

### API Documentation

Full API documentation for each crate is available on docs.rs:
- [traverse-graph](https://docs.rs/traverse-graph)
- [traverse-codegen](https://docs.rs/traverse-codegen)
- [traverse-solidity](https://docs.rs/traverse-solidity)
- [traverse-mermaid](https://docs.rs/traverse-mermaid)
- [traverse-logging](https://docs.rs/traverse-logging)

## Contributing

We welcome contributions! Before contributing, please review the [architecture documentation](DESIGN.md) to understand the codebase structure and design principles.

### Development Setup

```bash
# Fork and clone the repository
git clone --recursive https://github.com/YOUR-USERNAME/traverse.git
cd traverse

# Install development dependencies
rustup component add rustfmt clippy

# Run tests
cargo test --workspace

# Run lints
cargo clippy --all-targets --all-features
cargo fmt --check
```

### Code Style

* Follow Rust standard naming conventions
* Use `rustfmt` for formatting
* Add tests for new functionality
* Document public APIs
* Keep commits focused and atomic

### Testing

```bash
# Run all tests
cargo test --workspace

# Run specific tool tests
cargo test -p cli --bin sol2cg
```

### Pull Request Process

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/your-feature`
3. Make your changes and add tests
4. Ensure all tests pass: `cargo test --workspace`
5. Run formatters and linters: `cargo fmt && cargo clippy`
6. Commit with descriptive message
7. Push to your fork and open a Pull Request
8. Respond to review feedback

### Reporting Issues

When reporting issues, please include:
* Tool name and version (`sol2cg --version`)
* Operating system and version
* Minimal reproduction steps
* Expected vs actual behavior
* Error messages or logs
* Sample Solidity code (if applicable)

## License

This project is licensed under the MIT License. See [LICENSE](LICENSE) file for details.

## Acknowledgments

* Tree-sitter community for the parsing infrastructure
* Foundry team for the testing framework integration
* Solidity developers and community
* All contributors to the Traverse project

---

For more information, visit the [Traverse GitHub repository](https://github.com/calltrace/traverse).