# traverse-codegen

Code generation utilities for Solidity test suites and bindings.

## Overview

This crate provides powerful code generation capabilities for Solidity projects, including automated test generation using Foundry framework and TypeScript/Rust bindings generation. It analyzes contract behavior to create comprehensive, ready-to-run test suites.

## Features

- **Foundry Test Generation**: Creates complete test files with proper setup and assertions
- **Fuzzing Support**: Generates fuzz tests for property-based testing
- **Edge Case Detection**: Identifies boundary conditions and generates appropriate tests
- **Mock Generation**: Creates mock contracts for testing in isolation
- **Binding Generation**: Produces TypeScript and Rust bindings for contract interaction
- **Coverage-Aware**: Generates tests targeting maximum code coverage

## Usage

```rust
use traverse_codegen::{TestGenerator, TestFramework};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let generator = TestGenerator::new(TestFramework::Foundry);
    
    // Generate tests for a contract
    let tests = generator.generate_from_file("contracts/Token.sol")?;
    
    // Write tests to file
    tests.write_to_file("test/Token.t.sol")?;
    
    Ok(())
}
```

## Generated Test Example

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "forge-std/Test.sol";
import "../contracts/Token.sol";

contract TokenTest is Test {
    Token token;
    address alice = address(0x1);
    address bob = address(0x2);
    
    function setUp() public {
        token = new Token("Test Token", "TST");
        vm.deal(alice, 100 ether);
        vm.deal(bob, 100 ether);
    }
    
    function testTransfer() public {
        vm.prank(alice);
        token.mint(alice, 1000);
        
        vm.prank(alice);
        token.transfer(bob, 100);
        
        assertEq(token.balanceOf(bob), 100);
        assertEq(token.balanceOf(alice), 900);
    }
    
    function testFuzzTransfer(uint256 amount) public {
        vm.assume(amount > 0 && amount <= 1000);
        // Fuzz test implementation
    }
}
```

## Command Line Tool

This crate powers the `sol2test` command-line tool:

```bash
# Generate Foundry tests
sol2test contracts/Token.sol -o test/Token.t.sol

# Generate with specific framework
sol2test contracts/*.sol --framework foundry --output test/
```

## Supported Frameworks

- **Foundry**: Modern, fast testing framework
- **Hardhat**: JavaScript-based testing
- **Truffle**: Traditional testing framework
- **Ape**: Python-based testing

## Test Generation Strategies

- State transition testing
- Invariant testing
- Permission/access control testing
- Reentrancy testing
- Gas optimization testing
- Integration testing

## Part of Traverse

This crate is part of the [Traverse](https://github.com/calltrace/traverse) suite of tools for Solidity code analysis, visualization, and test generation.

## License

MIT OR Apache-2.0