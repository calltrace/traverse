//! # Invariant Breaker Module
//!
//! Finds counterexamples to Solidity invariant expressions by generating random variable assignments
//! that make the expression evaluate to `false`.
//!
//! ## Usage
//!
//! The invariant breaker provides async functions to find counterexamples to Solidity expressions.
//! Use `break_invariant()` for simple cases or `break_invariant_with_config()` for custom configuration.
//!
//! ## Supported Types
//!
//! - `Bool`: true/false values
//! - `UInt`: 0 to u64::MAX  
//! - `Int`: i64::MIN to i64::MAX
//! - `String`: random ASCII strings
//! - `Address`: 20-byte Ethereum addresses (hex)
//! - `Bytes`: variable-length byte arrays (hex)
//!
//! ## Examples
//!
//! | Expression | Sample Counterexample |
//! |------------|----------------------|
//! | `x > 10` | `x = 5` → `(5 > 10)` |
//! | `a && b` | `a = false, b = true` → `(false && true)` |
//! | `balance >= amount` | `balance = 50, amount = 100` → `(50 >= 100)` |
//!
//! The module uses the Solidity parser, interpreter, and writer from the `solidity` crate
//! to parse expressions, evaluate them with random values, and generate concrete output.

use rand::Rng;
use serde::{Deserialize, Serialize};
use solidity::ast::*;
use solidity::{
    format_value_for_expression, parse_expression, write_expression_to_string, SolidityInterpreter,
    Value,
};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvariantBreakerResult {
    pub success: bool,
    pub error: Option<String>,
    pub entries: Vec<InvariantBreakerEntry>,
    pub original_expression: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvariantBreakerEntry {
    pub variables: HashMap<String, InvariantBreakerValue>,
    pub concrete_expression: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum InvariantBreakerValue {
    Bool(bool),
    UInt(u64),
    Int(i64),
    String(String),
    Address(String),
    Bytes(Vec<u8>),
}

impl From<Value> for InvariantBreakerValue {
    fn from(value: Value) -> Self {
        match value {
            Value::Bool(b) => InvariantBreakerValue::Bool(b),
            Value::UInt(n) => InvariantBreakerValue::UInt(n),
            Value::Int(n) => InvariantBreakerValue::Int(n),
            Value::String(s) => InvariantBreakerValue::String(s),
            Value::Address(addr) => InvariantBreakerValue::Address(addr),
            Value::Bytes(b) => InvariantBreakerValue::Bytes(b),
            Value::Null => InvariantBreakerValue::Bool(false), // Convert null to false
        }
    }
}

impl From<InvariantBreakerValue> for Value {
    fn from(value: InvariantBreakerValue) -> Self {
        match value {
            InvariantBreakerValue::Bool(b) => Value::Bool(b),
            InvariantBreakerValue::UInt(n) => Value::UInt(n),
            InvariantBreakerValue::Int(n) => Value::Int(n),
            InvariantBreakerValue::String(s) => Value::String(s),
            InvariantBreakerValue::Address(addr) => Value::Address(addr),
            InvariantBreakerValue::Bytes(b) => Value::Bytes(b),
        }
    }
}

impl std::fmt::Display for InvariantBreakerValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvariantBreakerValue::Bool(b) => write!(f, "{}", b),
            InvariantBreakerValue::UInt(n) => write!(f, "{}", n),
            InvariantBreakerValue::Int(n) => write!(f, "{}", n),
            InvariantBreakerValue::String(s) => write!(f, "\"{}\"", s),
            InvariantBreakerValue::Address(addr) => write!(f, "{}", addr),
            InvariantBreakerValue::Bytes(b) => write!(f, "0x{}", hex::encode(b)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct InvariantBreakerConfig {
    pub max_attempts: usize,
    pub max_results: usize,
    pub seed: Option<u64>,
}

impl Default for InvariantBreakerConfig {
    fn default() -> Self {
        Self {
            max_attempts: 1000,
            max_results: 10,
            seed: None,
        }
    }
}

pub async fn break_invariant(
    expression: &str,
) -> Result<InvariantBreakerResult, Box<dyn std::error::Error + Send + Sync>> {
    break_invariant_with_config(expression, InvariantBreakerConfig::default()).await
}

pub async fn break_invariant_with_config(
    expression: &str,
    config: InvariantBreakerConfig,
) -> Result<InvariantBreakerResult, Box<dyn std::error::Error + Send + Sync>> {
    let parsed_expr = match parse_expression(expression) {
        Ok(expr) => expr,
        Err(e) => {
            return Ok(InvariantBreakerResult {
                success: false,
                error: Some(format!("Failed to parse expression: {}", e)),
                entries: vec![],
                original_expression: expression.to_string(),
            });
        }
    };

    let variables = extract_variables(&parsed_expr);

    if variables.is_empty() {
        let interpreter = SolidityInterpreter::new();
        match interpreter.evaluate_predicate(&parsed_expr) {
            Ok(true) => {
                return Ok(InvariantBreakerResult {
                    success: false,
                    error: Some("Expression is always true (no variables to modify)".to_string()),
                    entries: vec![],
                    original_expression: expression.to_string(),
                });
            }
            Ok(false) => {
                return Ok(InvariantBreakerResult {
                    success: true,
                    error: None,
                    entries: vec![InvariantBreakerEntry {
                        variables: HashMap::new(),
                        concrete_expression: expression.to_string(),
                    }],
                    original_expression: expression.to_string(),
                });
            }
            Err(e) => {
                return Ok(InvariantBreakerResult {
                    success: false,
                    error: Some(format!("Failed to evaluate expression: {}", e)),
                    entries: vec![],
                    original_expression: expression.to_string(),
                });
            }
        }
    }

    let mut rng = if let Some(seed) = config.seed {
        use rand::SeedableRng;
        rand::rngs::StdRng::seed_from_u64(seed)
    } else {
        use rand::SeedableRng;
        rand::rngs::StdRng::from_entropy()
    };

    let mut entries = Vec::new();
    let mut attempts = 0;

    while attempts < config.max_attempts && entries.len() < config.max_results {
        attempts += 1;

        let mut variable_assignments = HashMap::new();
        for var_name in &variables {
            let value = generate_random_value(&mut rng);
            variable_assignments.insert(var_name.clone(), value);
        }

        let mut interpreter = SolidityInterpreter::new();
        for (name, value) in &variable_assignments {
            interpreter
                .context_mut()
                .set_variable(name.clone(), value.clone().into());
        }

        match interpreter.evaluate_predicate(&parsed_expr) {
            Ok(false) => {
                let concrete_expr =
                    substitute_variables_in_expression(&parsed_expr, &variable_assignments)?;
                entries.push(InvariantBreakerEntry {
                    variables: variable_assignments
                        .into_iter()
                        .map(|(k, v)| (k, v.into()))
                        .collect(),
                    concrete_expression: concrete_expr,
                });
            }
            Ok(true) => {
                // Expression evaluated to true, continue searching
            }
            Err(_) => {
                // Error evaluating expression, skip this assignment
                continue;
            }
        }
    }

    Ok(InvariantBreakerResult {
        success: !entries.is_empty(),
        error: if entries.is_empty() {
            Some(format!(
                "No counterexamples found after {} attempts",
                attempts
            ))
        } else {
            None
        },
        entries,
        original_expression: expression.to_string(),
    })
}

fn extract_variables(expr: &Expression) -> Vec<String> {
    let mut variables = Vec::new();
    extract_variables_recursive(expr, &mut variables);
    variables.sort();
    variables.dedup();
    variables
}

fn extract_variables_recursive(expr: &Expression, variables: &mut Vec<String>) {
    match expr {
        Expression::Identifier(name) => {
            if !is_builtin_identifier(name) {
                variables.push(name.clone());
            }
        }
        Expression::Binary(bin_expr) => {
            extract_variables_recursive(&bin_expr.left, variables);
            extract_variables_recursive(&bin_expr.right, variables);
        }
        Expression::Unary(unary_expr) => {
            extract_variables_recursive(&unary_expr.operand, variables);
        }
        Expression::FunctionCall(call_expr) => {
            extract_variables_recursive(&call_expr.function, variables);
            for arg in &call_expr.arguments {
                extract_variables_recursive(arg, variables);
            }
        }
        Expression::MemberAccess(member_expr) => {
            extract_variables_recursive(&member_expr.object, variables);
        }
        Expression::IndexAccess(index_expr) => {
            extract_variables_recursive(&index_expr.object, variables);
            if let Some(index) = &index_expr.index {
                extract_variables_recursive(index, variables);
            }
        }
        Expression::Conditional(cond_expr) => {
            extract_variables_recursive(&cond_expr.condition, variables);
            extract_variables_recursive(&cond_expr.true_expr, variables);
            extract_variables_recursive(&cond_expr.false_expr, variables);
        }
        Expression::Assignment(assign_expr) => {
            extract_variables_recursive(&assign_expr.left, variables);
            extract_variables_recursive(&assign_expr.right, variables);
        }
        Expression::Tuple(tuple_expr) => {
            for element in &tuple_expr.elements {
                if let Some(expr) = element {
                    extract_variables_recursive(expr, variables);
                }
            }
        }
        Expression::Array(array_expr) => {
            for element in &array_expr.elements {
                extract_variables_recursive(element, variables);
            }
        }
        Expression::TypeConversion(conv_expr) => {
            extract_variables_recursive(&conv_expr.expression, variables);
        }
        Expression::New(new_expr) => {
            // New expressions don't contain variables in the type name
        }
        Expression::Literal(_) => {
            // Literals don't contain variables
        }
    }
}

fn is_builtin_identifier(name: &str) -> bool {
    matches!(
        name,
        "true"
            | "false"
            | "msg"
            | "block"
            | "tx"
            | "now"
            | "keccak256"
            | "sha256"
            | "ripemd160"
            | "ecrecover"
            | "this"
            | "super"
            | "selfdestruct"
            | "revert"
            | "require"
            | "assert"
    )
}

fn generate_random_value(rng: &mut impl Rng) -> Value {
    match rng.gen_range(0..6) {
        0 => Value::Bool(rng.gen()),
        1 => Value::UInt(rng.gen_range(0..1000)),
        2 => Value::Int(rng.gen_range(-500..500)),
        3 => {
            let strings = ["", "hello", "world", "test", "value"];
            Value::String(strings[rng.gen_range(0..strings.len())].to_string())
        }
        4 => {
            let addresses = [
                "0x0000000000000000000000000000000000000000",
                "0x1234567890123456789012345678901234567890",
                "0xabcdefabcdefabcdefabcdefabcdefabcdefabcdef",
            ];
            Value::Address(addresses[rng.gen_range(0..addresses.len())].to_string())
        }
        5 => {
            let byte_arrays = [vec![], vec![0x12, 0x34], vec![0xff, 0x00, 0xaa, 0xbb]];
            Value::Bytes(byte_arrays[rng.gen_range(0..byte_arrays.len())].clone())
        }
        _ => Value::Bool(false),
    }
}

fn substitute_variables_in_expression(
    expr: &Expression,
    assignments: &HashMap<String, Value>,
) -> Result<String, Box<dyn std::error::Error + Send + Sync>> {
    Ok(substitute_expression_recursive(expr, assignments))
}

fn substitute_expression_recursive(
    expr: &Expression,
    assignments: &HashMap<String, Value>,
) -> String {
    match expr {
        Expression::Identifier(name) => {
            if let Some(value) = assignments.get(name) {
                format_value_for_expression(value)
            } else {
                name.clone()
            }
        }
        Expression::Binary(bin_expr) => {
            let left = substitute_expression_recursive(&bin_expr.left, assignments);
            let right = substitute_expression_recursive(&bin_expr.right, assignments);
            let op = bin_expr.operator.to_string();
            format!("({} {} {})", left, op, right)
        }
        Expression::Unary(unary_expr) => {
            let operand = substitute_expression_recursive(&unary_expr.operand, assignments);
            let op = unary_expr.operator.to_string();
            if unary_expr.is_prefix {
                format!("({}{})", op, operand)
            } else {
                format!("({}{})", operand, op)
            }
        }
        _ => {
            // For other expression types, use the solidity writer
            write_expression_to_string(expr)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio;

    #[tokio::test]
    async fn test_break_invariant_simple_expression() {
        let result = break_invariant("x > 10").await.unwrap();
        assert!(result.success);
        assert!(!result.entries.is_empty());

        // Check that at least one entry has x <= 10
        let has_counterexample = result.entries.iter().any(|entry| {
            if let Some(InvariantBreakerValue::UInt(x)) = entry.variables.get("x") {
                *x <= 10
            } else if let Some(InvariantBreakerValue::Int(x)) = entry.variables.get("x") {
                *x <= 10
            } else {
                false
            }
        });
        assert!(has_counterexample);
    }

    #[tokio::test]
    async fn test_break_invariant_boolean_expression() {
        let result = break_invariant("isActive && hasPermission").await.unwrap();
        assert!(result.success);
        assert!(!result.entries.is_empty());

        // Check that at least one entry has either isActive=false or hasPermission=false
        let has_counterexample = result.entries.iter().any(|entry| {
            let is_active = entry.variables.get("isActive");
            let has_permission = entry.variables.get("hasPermission");

            matches!(is_active, Some(InvariantBreakerValue::Bool(false)))
                || matches!(has_permission, Some(InvariantBreakerValue::Bool(false)))
        });
        assert!(has_counterexample);
    }

    #[tokio::test]
    async fn test_break_invariant_always_true_expression() {
        let result = break_invariant("true").await.unwrap();
        assert!(!result.success);
        assert!(result.error.is_some());
    }

    #[tokio::test]
    async fn test_break_invariant_always_false_expression() {
        let result = break_invariant("false").await.unwrap();
        assert!(result.success);
        assert_eq!(result.entries.len(), 1);
        assert!(result.entries[0].variables.is_empty());
    }

    #[tokio::test]
    async fn test_extract_variables() {
        // Create a simple binary expression: x > y
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Identifier("x".to_string())),
            operator: BinaryOperator::GreaterThan,
            right: Box::new(Expression::Identifier("y".to_string())),
        });

        let variables = extract_variables(&expr);
        assert_eq!(variables, vec!["x", "y"]);
    }

    #[tokio::test]
    async fn test_substitute_variables() {
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(Expression::Identifier("x".to_string())),
            operator: BinaryOperator::GreaterThan,
            right: Box::new(Expression::Identifier("y".to_string())),
        });

        let mut assignments = HashMap::new();
        assignments.insert("x".to_string(), Value::UInt(15));
        assignments.insert("y".to_string(), Value::UInt(10));

        let result = substitute_variables_in_expression(&expr, &assignments).unwrap();
        assert_eq!(result, "(15 > 10)");
    }
}
