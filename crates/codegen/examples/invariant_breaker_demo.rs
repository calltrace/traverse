//! Demo showing how to use the falsifier module
//!
//! This example demonstrates how to use the falsifier to find counterexamples
//! for Solidity expressions.

use anyhow::Result;
use codegen::invariant_breaker::{break_invariant, InvariantBreakerResult};

fn main() -> Result<()> {
    println!("Falsifier Demo");
    println!("==============\n");

    let rt = tokio::runtime::Runtime::new()?;

    println!("Example 1: Simple arithmetic condition");
    println!("Expression: x > 10");
    let result1 = rt
        .block_on(break_invariant("x > 10"))
        .map_err(|e| anyhow::anyhow!("{}", e))?;
    print_result(&result1);

    println!("\nExample 2: Boolean logic");
    println!("Expression: isActive && hasPermission");
    let result2 = rt
        .block_on(break_invariant("isActive && hasPermission"))
        .map_err(|e| anyhow::anyhow!("{}", e))?;
    print_result(&result2);

    Ok(())
}

fn print_result(result: &InvariantBreakerResult) {
    println!("Success: {}", result.success);
    if let Some(error) = &result.error {
        println!("Error: {}", error);
    }

    if result.success && !result.entries.is_empty() {
        println!("Found {} counterexample(s):", result.entries.len());
        for (i, entry) in result.entries.iter().take(3).enumerate() {
            println!("  {}. Variables: {:?}", i + 1, entry.variables);
            println!("     Concrete: {}", entry.concrete_expression);
        }
        if result.entries.len() > 3 {
            println!("     ... and {} more", result.entries.len() - 3);
        }
    } else {
        println!("No counterexamples found.");
    }
}
