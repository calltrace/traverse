# traverse-cli

CLI tools for Solidity analysis.

## Installation

```bash
brew tap calltrace/tap && brew install traverse
# or
cargo install traverse-cli
```

## Tools

### `sol2cg` - Call Graph Generator
```bash
sol2cg contracts/*.sol -o graph.dot
dot -Tsvg graph.dot -o graph.svg
```

### `sol2test` - Test Generator
```bash
sol2test contracts/Token.sol -o test/
```

### `sol2bnd` - Binding Generator
```bash
sol2bnd contracts/ -o bindings.yaml
```

### `sol-storage-analyzer` - Storage Analyzer
```bash
sol-storage-analyzer contracts/*.sol -o storage.md
```

### `storage-trace` - Storage Comparator
```bash
storage-trace --func1 deposit --func2 withdraw contracts/Vault.sol
```

## Usage Examples

```bash
# Complete analysis
sol2cg contracts/*.sol -o graph.dot
sol2test contracts/*.sol -o test/
sol-storage-analyzer contracts/*.sol -o storage.md

# CI pipeline
sol2cg contracts/*.sol --exclude-isolated-nodes
sol-storage-analyzer contracts/*.sol
```

## Performance

- Tree-sitter parsing: <1s for most projects
- Handles 500+ contracts efficiently
- Automatic chunking for large outputs

## License

MIT