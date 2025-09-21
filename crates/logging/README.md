# traverse-logging

Logging utilities for the Traverse suite of Solidity analysis tools.

## Overview

This crate provides standardized logging functionality used across all Traverse tools, ensuring consistent output formatting and debugging capabilities.

## Features

- Structured logging with `tracing`
- Configurable log levels
- Consistent formatting across all Traverse tools
- Performance-optimized for analysis workflows

## Usage

```rust
use traverse_logging::init_logger;

fn main() {
    // Initialize logging with default settings
    init_logger();
    
    // Your application code here
}
```

## Part of Traverse

This crate is part of the [Traverse](https://github.com/calltrace/traverse) suite of tools for Solidity code analysis, visualization, and test generation.

## License

MIT OR Apache-2.0