//! Solidity Expression Interpreter
//! 
//! A Rust-based interpreter for Solidity expressions with focus on predicate evaluation.
//! Evaluates boolean expressions, comparisons, and logical operations commonly used in smart contracts.
//! 
//! # Features
//! 
//! - **Predicate evaluation**: expressions that return boolean values
//! - **Variable context**: support for variable bindings and lookups  
//! - **Type system**: handle Solidity's basic types (bool, uint, int, string, address, bytes)
//! - **Error handling**: comprehensive error reporting for runtime issues
//! 
//! # Supported Operations
//! 
//! - **Literals**: `true`, `false`, `42`, `"hello"`, `hex"deadbeef"`, `unicode"hello"`
//! - **Binary ops**: `+`, `-`, `*`, `/`, `%`, `**`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `&&`, `||`, `&`, `|`, `^`, `<<`, `>>`, `>>>`
//! - **Unary ops**: `+`, `-`, `!`, `~`
//! - **Conditional**: `condition ? true_expr : false_expr`
//! - **Variables**: variable lookup with context management
//! - **Built-ins**: `keccak256()`, `sha256()`, `ripemd160()`, `ecrecover()` (mock implementations)
//! 
//! # Value Types
//! 
//! - `Bool(bool)`: `true`, `false`
//! - `UInt(u64)`: `0`, `42`, `1000`  
//! - `Int(i64)`: `-1`, `0`, `42`
//! - `String(String)`: `"hello"`, `"world"`
//! - `Address(String)`: `"0x1234..."`
//! - `Bytes(Vec<u8>)`: `[0x12, 0x34, ...]`
//! - `Null`: null/undefined
//! 
//! # Boolean Conversion
//! 
//! All values convert to boolean for predicate evaluation:
//! - `Bool(true)` → `true`, `Bool(false)` → `false`
//! - `UInt(0)` → `false`, `UInt(n)` → `true`
//! - `Int(0)` → `false`, `Int(n)` → `true`  
//! - `String("")` → `false`, `String(s)` → `true`
//! - `Address("0x0000...")` → `false`, other addresses → `true`
//! - `Bytes([])` → `false`, non-empty bytes → `true`
//! - `Null` → `false`
//! 
//! # Limitations
//! 
//! Intentional limitations for predicate evaluation:
//! - No assignment operations (predicate evaluation shouldn 't mutate state)
//! - No object creation (new expressions not supported)
//! - No complex types (arrays/mappings/structs limited)
//! - No member/index access (not yet implemented)
//! 
//! # Architecture
//! 
//! - **Parser integration**: uses existing Solidity parser to generate AST
//! - **Context management**: maintains variable bindings and function definitions
//! - **Type system**: runtime type checking and conversion
//! - **Error propagation**: comprehensive error handling throughout evaluation
//! - **Extensibility**: designed to be easily extended with new operations and types

use crate::ast::*;
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    UInt(u64),
    Int(i64),
    String(String),
    Address(String),
    Bytes(Vec<u8>),
    Null,
}

impl Value {
    pub fn to_bool(&self) -> Result<bool, InterpreterError> {
        match self {
            Value::Bool(b) => Ok(*b),
            Value::UInt(n) => Ok(*n != 0),
            Value::Int(n) => Ok(*n != 0),
            Value::String(s) => Ok(!s.is_empty()),
            Value::Address(addr) => Ok(addr != "0x0000000000000000000000000000000000000000"),
            Value::Bytes(b) => Ok(!b.is_empty()),
            Value::Null => Ok(false),
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Bool(_) => "bool",
            Value::UInt(_) => "uint",
            Value::Int(_) => "int",
            Value::String(_) => "string",
            Value::Address(_) => "address",
            Value::Bytes(_) => "bytes",
            Value::Null => "null",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::UInt(n) => write!(f, "{}", n),
            Value::Int(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Address(addr) => write!(f, "{}", addr),
            Value::Bytes(b) => write!(f, "0x{}", hex::encode(b)),
            Value::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InterpreterError {
    UndefinedVariable(String),
    TypeMismatch { expected: String, found: String },
    UnsupportedOperation(String),
    DivisionByZero,
    InvalidLiteral(String),
    FunctionNotFound(String),
    InvalidArguments(String),
    RuntimeError(String),
}

impl fmt::Display for InterpreterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InterpreterError::UndefinedVariable(name) => write!(f, "Undefined variable: {}", name),
            InterpreterError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {}, found {}", expected, found)
            }
            InterpreterError::UnsupportedOperation(op) => write!(f, "Unsupported operation: {}", op),
            InterpreterError::DivisionByZero => write!(f, "Division by zero"),
            InterpreterError::InvalidLiteral(lit) => write!(f, "Invalid literal: {}", lit),
            InterpreterError::FunctionNotFound(name) => write!(f, "Function not found: {}", name),
            InterpreterError::InvalidArguments(msg) => write!(f, "Invalid arguments: {}", msg),
            InterpreterError::RuntimeError(msg) => write!(f, "Runtime error: {}", msg),
        }
    }
}

impl std::error::Error for InterpreterError {}

#[derive(Debug, Clone)]
pub struct InterpreterContext {
    variables: HashMap<String, Value>,
    functions: HashMap<String, BuiltinFunction>,
}

impl Default for InterpreterContext {
    fn default() -> Self {
        let mut context = Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        };
        context.register_builtin_functions();
        context
    }
}

impl InterpreterContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_variable(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    fn register_builtin_functions(&mut self) {
        self.functions.insert("keccak256".to_string(), BuiltinFunction::Keccak256);
        self.functions.insert("sha256".to_string(), BuiltinFunction::Sha256);
        self.functions.insert("ripemd160".to_string(), BuiltinFunction::Ripemd160);
        self.functions.insert("ecrecover".to_string(), BuiltinFunction::Ecrecover);
    }

    pub fn get_function(&self, name: &str) -> Option<&BuiltinFunction> {
        self.functions.get(name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinFunction {
    Keccak256,
    Sha256,
    Ripemd160,
    Ecrecover,
}

#[derive(Debug)]
pub struct SolidityInterpreter {
    context: InterpreterContext,
}

impl Default for SolidityInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl SolidityInterpreter {
    pub fn new() -> Self {
        Self {
            context: InterpreterContext::new(),
        }
    }

    pub fn with_context(context: InterpreterContext) -> Self {
        Self { context }
    }

    pub fn context_mut(&mut self) -> &mut InterpreterContext {
        &mut self.context
    }

    pub fn context(&self) -> &InterpreterContext {
        &self.context
    }

    pub fn evaluate(&self, expr: &Expression) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Literal(lit) => self.evaluate_literal(lit),
            Expression::Identifier(name) => self.evaluate_identifier(name),
            Expression::Binary(bin_expr) => self.evaluate_binary_expression(bin_expr),
            Expression::Unary(unary_expr) => self.evaluate_unary_expression(unary_expr),
            Expression::FunctionCall(call_expr) => self.evaluate_function_call(call_expr),
            Expression::MemberAccess(member_expr) => self.evaluate_member_access(member_expr),
            Expression::IndexAccess(index_expr) => self.evaluate_index_access(index_expr),
            Expression::Conditional(cond_expr) => self.evaluate_conditional(cond_expr),
            Expression::Tuple(tuple_expr) => self.evaluate_tuple(tuple_expr),
            Expression::Array(array_expr) => self.evaluate_array(array_expr),
            Expression::TypeConversion(conv_expr) => self.evaluate_type_conversion(conv_expr),
            Expression::Assignment(_) => Err(InterpreterError::UnsupportedOperation(
                "Assignment expressions are not supported in predicate evaluation".to_string(),
            )),
            Expression::New(_) => Err(InterpreterError::UnsupportedOperation(
                "New expressions are not supported in predicate evaluation".to_string(),
            )),
        }
    }

    pub fn evaluate_predicate(&self, expr: &Expression) -> Result<bool, InterpreterError> {
        let value = self.evaluate(expr)?;
        value.to_bool()
    }

    fn evaluate_literal(&self, literal: &Literal) -> Result<Value, InterpreterError> {
        match literal {
            Literal::Boolean(b) => Ok(Value::Bool(*b)),
            Literal::Number(num_lit) => self.evaluate_number_literal(num_lit),
            Literal::String(str_lit) => Ok(Value::String(str_lit.value.clone())),
            Literal::HexString(hex_lit) => self.evaluate_hex_string_literal(hex_lit),
            Literal::UnicodeString(unicode_lit) => Ok(Value::String(unicode_lit.value.clone())),
        }
    }

    fn evaluate_number_literal(&self, num_lit: &NumberLiteral) -> Result<Value, InterpreterError> {
        let trimmed_value = num_lit.value.trim();
        if let Ok(uint_val) = trimmed_value.parse::<u64>() {
            Ok(Value::UInt(uint_val))
        } else if let Ok(int_val) = trimmed_value.parse::<i64>() {
            Ok(Value::Int(int_val))
        } else {
            Err(InterpreterError::InvalidLiteral(format!(
                "Cannot parse number: {}",
                num_lit.value
            )))
        }
    }

    fn evaluate_hex_string_literal(&self, hex_lit: &HexStringLiteral) -> Result<Value, InterpreterError> {
        let hex_str = hex_lit.value.strip_prefix("0x").unwrap_or(&hex_lit.value);
        
        // Try to decode as bytes
        match hex::decode(hex_str) {
            Ok(bytes) => Ok(Value::Bytes(bytes)),
            Err(_) => Err(InterpreterError::InvalidLiteral(format!(
                "Invalid hex string: {}",
                hex_lit.value
            ))),
        }
    }

    fn evaluate_identifier(&self, name: &str) -> Result<Value, InterpreterError> {
        self.context
            .get_variable(name)
            .cloned()
            .ok_or_else(|| InterpreterError::UndefinedVariable(name.to_string()))
    }

    fn evaluate_binary_expression(&self, bin_expr: &BinaryExpression) -> Result<Value, InterpreterError> {
        let left = self.evaluate(&bin_expr.left)?;
        let right = self.evaluate(&bin_expr.right)?;

        match &bin_expr.operator {
            BinaryOperator::Add => self.evaluate_add(&left, &right),
            BinaryOperator::Sub => self.evaluate_sub(&left, &right),
            BinaryOperator::Mul => self.evaluate_mul(&left, &right),
            BinaryOperator::Div => self.evaluate_div(&left, &right),
            BinaryOperator::Mod => self.evaluate_mod(&left, &right),
            BinaryOperator::Exp => self.evaluate_exp(&left, &right),
            BinaryOperator::Equal => Ok(Value::Bool(self.values_equal(&left, &right))),
            BinaryOperator::NotEqual => Ok(Value::Bool(!self.values_equal(&left, &right))),
            BinaryOperator::LessThan => self.evaluate_less_than(&left, &right),
            BinaryOperator::LessThanOrEqual => self.evaluate_less_than_or_equal(&left, &right),
            BinaryOperator::GreaterThan => self.evaluate_greater_than(&left, &right),
            BinaryOperator::GreaterThanOrEqual => self.evaluate_greater_than_or_equal(&left, &right),
            BinaryOperator::And => self.evaluate_logical_and(&left, &right),
            BinaryOperator::Or => self.evaluate_logical_or(&left, &right),
            BinaryOperator::BitAnd => self.evaluate_bit_and(&left, &right),
            BinaryOperator::BitOr => self.evaluate_bit_or(&left, &right),
            BinaryOperator::BitXor => self.evaluate_bit_xor(&left, &right),
            BinaryOperator::ShiftLeft => self.evaluate_shift_left(&left, &right),
            BinaryOperator::ShiftRight => self.evaluate_shift_right(&left, &right),
            BinaryOperator::ShiftRightArithmetic => self.evaluate_shift_right_arithmetic(&left, &right),
        }
    }

    fn evaluate_unary_expression(&self, unary_expr: &UnaryExpression) -> Result<Value, InterpreterError> {
        let operand = self.evaluate(&unary_expr.operand)?;

        match &unary_expr.operator {
            UnaryOperator::Plus => Ok(operand), // Unary plus is a no-op
            UnaryOperator::Minus => self.evaluate_unary_minus(&operand),
            UnaryOperator::Not => self.evaluate_logical_not(&operand),
            UnaryOperator::BitNot => self.evaluate_bit_not(&operand),
            UnaryOperator::Increment | UnaryOperator::Decrement => {
                Err(InterpreterError::UnsupportedOperation(
                    "Increment/decrement operators are not supported in predicate evaluation".to_string(),
                ))
            }
            UnaryOperator::Delete => Err(InterpreterError::UnsupportedOperation(
                "Delete operator is not supported in predicate evaluation".to_string(),
            )),
        }
    }

    fn evaluate_function_call(&self, call_expr: &FunctionCallExpression) -> Result<Value, InterpreterError> {
        // For now, we only support built-in functions identified by simple identifiers
        if let Expression::Identifier(func_name) = &*call_expr.function {
            if let Some(builtin_func) = self.context.get_function(func_name) {
                let args: Result<Vec<Value>, InterpreterError> = call_expr
                    .arguments
                    .iter()
                    .map(|arg| self.evaluate(arg))
                    .collect();
                let args = args?;
                return self.evaluate_builtin_function(builtin_func, &args);
            }
        }

        Err(InterpreterError::FunctionNotFound(
            "Complex function calls are not yet supported".to_string(),
        ))
    }

    fn evaluate_member_access(&self, member_expr: &MemberAccessExpression) -> Result<Value, InterpreterError> {
        let _object = self.evaluate(&member_expr.object)?;
        // For now, we don't support member access in predicate evaluation
        Err(InterpreterError::UnsupportedOperation(
            "Member access is not yet supported in predicate evaluation".to_string(),
        ))
    }

    fn evaluate_index_access(&self, index_expr: &IndexAccessExpression) -> Result<Value, InterpreterError> {
        let _object = self.evaluate(&index_expr.object)?;
        // For now, we don't support index access in predicate evaluation
        Err(InterpreterError::UnsupportedOperation(
            "Index access is not yet supported in predicate evaluation".to_string(),
        ))
    }

    fn evaluate_conditional(&self, cond_expr: &ConditionalExpression) -> Result<Value, InterpreterError> {
        let condition = self.evaluate(&cond_expr.condition)?;
        let condition_bool = condition.to_bool()?;

        if condition_bool {
            self.evaluate(&cond_expr.true_expr)
        } else {
            self.evaluate(&cond_expr.false_expr)
        }
    }

    fn evaluate_tuple(&self, tuple_expr: &TupleExpression) -> Result<Value, InterpreterError> {
        // For predicate evaluation, we don't support tuples
        Err(InterpreterError::UnsupportedOperation(
            format!("Tuple expressions are not supported in predicate evaluation (found {} elements)", 
                   tuple_expr.elements.len())
        ))
    }

    fn evaluate_array(&self, array_expr: &ArrayExpression) -> Result<Value, InterpreterError> {
        // For predicate evaluation, we don't support arrays
        Err(InterpreterError::UnsupportedOperation(
            format!("Array expressions are not supported in predicate evaluation (found {} elements)", 
                   array_expr.elements.len())
        ))
    }

    fn evaluate_type_conversion(&self, conv_expr: &TypeConversionExpression) -> Result<Value, InterpreterError> {
        let value = self.evaluate(&conv_expr.expression)?;
        // For now, we don't support type conversions in predicate evaluation
        Err(InterpreterError::UnsupportedOperation(
            format!("Type conversion is not yet supported in predicate evaluation (converting {} to type)", 
                   value.type_name())
        ))
    }

    fn evaluate_add(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a + b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::UInt(a), Value::Int(b)) => Ok(Value::Int(*a as i64 + b)),
            (Value::Int(a), Value::UInt(b)) => Ok(Value::Int(a + *b as i64)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric or string".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_sub(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => {
                if a >= b {
                    Ok(Value::UInt(a - b))
                } else {
                    Ok(Value::Int(*a as i64 - *b as i64))
                }
            }
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::UInt(a), Value::Int(b)) => Ok(Value::Int(*a as i64 - b)),
            (Value::Int(a), Value::UInt(b)) => Ok(Value::Int(a - *b as i64)),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_mul(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a * b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::UInt(a), Value::Int(b)) => Ok(Value::Int(*a as i64 * b)),
            (Value::Int(a), Value::UInt(b)) => Ok(Value::Int(a * *b as i64)),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_div(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => {
                if *b == 0 {
                    Err(InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::UInt(a / b))
                }
            }
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Int(a / b))
                }
            }
            (Value::UInt(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Int(*a as i64 / b))
                }
            }
            (Value::Int(a), Value::UInt(b)) => {
                if *b == 0 {
                    Err(InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Int(a / *b as i64))
                }
            }
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_mod(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => {
                if *b == 0 {
                    Err(InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::UInt(a % b))
                }
            }
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(InterpreterError::DivisionByZero)
                } else {
                    Ok(Value::Int(a % b))
                }
            }
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_exp(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(base), Value::UInt(exp)) => {
                if *exp > 32 {
                    Err(InterpreterError::RuntimeError("Exponent too large".to_string()))
                } else {
                    Ok(Value::UInt(base.pow(*exp as u32)))
                }
            }
            _ => Err(InterpreterError::TypeMismatch {
                expected: "uint".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn values_equal(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::UInt(a), Value::UInt(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::UInt(a), Value::Int(b)) => *a as i64 == *b,
            (Value::Int(a), Value::UInt(b)) => *a == *b as i64,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Address(a), Value::Address(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::Null, Value::Null) => true,
            _ => false,
        }
    }

    fn evaluate_less_than(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::Bool(a < b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
            (Value::UInt(a), Value::Int(b)) => Ok(Value::Bool((*a as i64) < *b)),
            (Value::Int(a), Value::UInt(b)) => Ok(Value::Bool(*a < (*b as i64))),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_less_than_or_equal(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::Bool(a <= b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
            (Value::UInt(a), Value::Int(b)) => Ok(Value::Bool((*a as i64) <= *b)),
            (Value::Int(a), Value::UInt(b)) => Ok(Value::Bool(*a <= (*b as i64))),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_greater_than(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::Bool(a > b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
            (Value::UInt(a), Value::Int(b)) => Ok(Value::Bool((*a as i64) > *b)),
            (Value::Int(a), Value::UInt(b)) => Ok(Value::Bool(*a > (*b as i64))),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_greater_than_or_equal(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::Bool(a >= b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
            (Value::UInt(a), Value::Int(b)) => Ok(Value::Bool((*a as i64) >= *b)),
            (Value::Int(a), Value::UInt(b)) => Ok(Value::Bool(*a >= (*b as i64))),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_logical_and(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        let left_bool = left.to_bool()?;
        let right_bool = right.to_bool()?;
        Ok(Value::Bool(left_bool && right_bool))
    }

    fn evaluate_logical_or(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        let left_bool = left.to_bool()?;
        let right_bool = right.to_bool()?;
        Ok(Value::Bool(left_bool || right_bool))
    }

    fn evaluate_bit_and(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a & b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "integer".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_bit_or(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a | b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "integer".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_bit_xor(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => Ok(Value::UInt(a ^ b)),
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "integer".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_shift_left(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => {
                if *b > 64 {
                    Err(InterpreterError::RuntimeError("Shift amount too large".to_string()))
                } else {
                    Ok(Value::UInt(a << b))
                }
            }
            _ => Err(InterpreterError::TypeMismatch {
                expected: "uint".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_shift_right(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::UInt(a), Value::UInt(b)) => {
                if *b > 64 {
                    Err(InterpreterError::RuntimeError("Shift amount too large".to_string()))
                } else {
                    Ok(Value::UInt(a >> b))
                }
            }
            _ => Err(InterpreterError::TypeMismatch {
                expected: "uint".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_shift_right_arithmetic(&self, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
        match (left, right) {
            (Value::Int(a), Value::UInt(b)) => {
                if *b > 64 {
                    Err(InterpreterError::RuntimeError("Shift amount too large".to_string()))
                } else {
                    Ok(Value::Int(a >> b))
                }
            }
            _ => Err(InterpreterError::TypeMismatch {
                expected: "int and uint".to_string(),
                found: format!("{} and {}", left.type_name(), right.type_name()),
            }),
        }
    }

    fn evaluate_unary_minus(&self, operand: &Value) -> Result<Value, InterpreterError> {
        match operand {
            Value::UInt(n) => Ok(Value::Int(-(*n as i64))),
            Value::Int(n) => Ok(Value::Int(-n)),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "numeric".to_string(),
                found: operand.type_name().to_string(),
            }),
        }
    }

    fn evaluate_logical_not(&self, operand: &Value) -> Result<Value, InterpreterError> {
        let bool_val = operand.to_bool()?;
        Ok(Value::Bool(!bool_val))
    }

    fn evaluate_bit_not(&self, operand: &Value) -> Result<Value, InterpreterError> {
        match operand {
            Value::UInt(n) => Ok(Value::UInt(!n)),
            Value::Int(n) => Ok(Value::Int(!n)),
            _ => Err(InterpreterError::TypeMismatch {
                expected: "integer".to_string(),
                found: operand.type_name().to_string(),
            }),
        }
    }

    fn evaluate_builtin_function(
        &self,
        func: &BuiltinFunction,
        args: &[Value],
    ) -> Result<Value, InterpreterError> {
        match func {
            BuiltinFunction::Keccak256 => {
                if args.len() != 1 {
                    return Err(InterpreterError::InvalidArguments(
                        "keccak256 expects exactly 1 argument".to_string(),
                    ));
                }
                Ok(Value::Bytes(vec![0u8; 32])) // Mock 32-byte hash
            }
            BuiltinFunction::Sha256 => {
                if args.len() != 1 {
                    return Err(InterpreterError::InvalidArguments(
                        "sha256 expects exactly 1 argument".to_string(),
                    ));
                }
                Ok(Value::Bytes(vec![1u8; 32])) // Mock 32-byte hash
            }
            BuiltinFunction::Ripemd160 => {
                if args.len() != 1 {
                    return Err(InterpreterError::InvalidArguments(
                        "ripemd160 expects exactly 1 argument".to_string(),
                    ));
                }
                Ok(Value::Bytes(vec![2u8; 20])) // Mock 20-byte hash
            }
            BuiltinFunction::Ecrecover => {
                if args.len() != 4 {
                    return Err(InterpreterError::InvalidArguments(
                        "ecrecover expects exactly 4 arguments".to_string(),
                    ));
                }
                // For predicate evaluation, we'll return a mock address
                Ok(Value::Address("0x0000000000000000000000000000000000000000".to_string()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_expression; // Added for parsing expressions in tests

    #[test]
    fn test_evaluate_boolean_literal() {
        let interpreter = SolidityInterpreter::new();
        
        let true_expr = Expression::Literal(Literal::Boolean(true));
        let false_expr = Expression::Literal(Literal::Boolean(false));
        
        assert_eq!(interpreter.evaluate(&true_expr).unwrap(), Value::Bool(true));
        assert_eq!(interpreter.evaluate(&false_expr).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_evaluate_number_literal() {
        let interpreter = SolidityInterpreter::new();
        
        let num_expr = Expression::Literal(Literal::Number(NumberLiteral {
            value: "42".to_string(),
            sub_denomination: None,
        }));
        
        assert_eq!(interpreter.evaluate(&num_expr).unwrap(), Value::UInt(42));
    }

    #[test]
    fn test_evaluate_binary_comparison() {
        let interpreter = SolidityInterpreter::new();
        
        let left = Box::new(Expression::Literal(Literal::Number(NumberLiteral {
            value: "10".to_string(),
            sub_denomination: None,
        })));
        
        let right = Box::new(Expression::Literal(Literal::Number(NumberLiteral {
            value: "5".to_string(),
            sub_denomination: None,
        })));
        
        let gt_expr = Expression::Binary(BinaryExpression {
            left: left.clone(),
            operator: BinaryOperator::GreaterThan,
            right: right.clone(),
        });
        
        let lt_expr = Expression::Binary(BinaryExpression {
            left: left.clone(),
            operator: BinaryOperator::LessThan,
            right: right.clone(),
        });
        
        assert!(interpreter.evaluate_predicate(&gt_expr).unwrap());
        assert!(!interpreter.evaluate_predicate(&lt_expr).unwrap());
    }

    #[test]
    fn test_evaluate_logical_and() {
        let interpreter = SolidityInterpreter::new();
        
        let true_expr = Box::new(Expression::Literal(Literal::Boolean(true)));
        let false_expr = Box::new(Expression::Literal(Literal::Boolean(false)));
        
        let and_expr = Expression::Binary(BinaryExpression {
            left: true_expr.clone(),
            operator: BinaryOperator::And,
            right: false_expr.clone(),
        });
        
        assert!(!interpreter.evaluate_predicate(&and_expr).unwrap());
    }

    #[test]
    fn test_evaluate_logical_not() {
        let interpreter = SolidityInterpreter::new();
        
        let true_expr = Box::new(Expression::Literal(Literal::Boolean(true)));
        
        let not_expr = Expression::Unary(UnaryExpression {
            operator: UnaryOperator::Not,
            operand: true_expr,
            is_prefix: true,
        });
        
        assert!(!interpreter.evaluate_predicate(&not_expr).unwrap());
    }

    #[test]
    fn test_evaluate_conditional() {
        let interpreter = SolidityInterpreter::new();
        
        let condition = Box::new(Expression::Literal(Literal::Boolean(true)));
        let true_branch = Box::new(Expression::Literal(Literal::Number(NumberLiteral {
            value: "1".to_string(),
            sub_denomination: None,
        })));
        let false_branch = Box::new(Expression::Literal(Literal::Number(NumberLiteral {
            value: "0".to_string(),
            sub_denomination: None,
        })));
        
        let cond_expr = Expression::Conditional(ConditionalExpression {
            condition,
            true_expr: true_branch,
            false_expr: false_branch,
        });
        
        assert_eq!(interpreter.evaluate(&cond_expr).unwrap(), Value::UInt(1));
    }

    #[test]
    fn test_variable_lookup() {
        let mut interpreter = SolidityInterpreter::new();
        interpreter.context_mut().set_variable("x".to_string(), Value::Bool(true));
        
        let var_expr = Expression::Identifier("x".to_string());
        
        assert!(interpreter.evaluate_predicate(&var_expr).unwrap());
    }

    #[test]
    fn test_undefined_variable() {
        let interpreter = SolidityInterpreter::new();
        
        let var_expr = Expression::Identifier("undefined".to_string());
        
        match interpreter.evaluate(&var_expr) {
            Err(InterpreterError::UndefinedVariable(name)) => {
                assert_eq!(name, "undefined");
            }
            _ => panic!("Expected UndefinedVariable error"),
        }
    }

    #[test]
    fn test_value_to_bool_conversion() {
        assert!(Value::Bool(true).to_bool().unwrap());
        assert!(!Value::Bool(false).to_bool().unwrap());
        assert!(Value::UInt(1).to_bool().unwrap());
        assert!(!Value::UInt(0).to_bool().unwrap());
        assert!(Value::Int(-1).to_bool().unwrap());
        assert!(!Value::Int(0).to_bool().unwrap());
        assert!(Value::String("hello".to_string()).to_bool().unwrap());
        assert!(!Value::String("".to_string()).to_bool().unwrap());
        assert!(!Value::Null.to_bool().unwrap());
    }

    #[test]
    fn test_evaluate_new_value_gt_zero_predicate() {
        let mut interpreter = SolidityInterpreter::new();

        // Construct the expression: _newValue > 0
        let new_value_ident = Expression::Identifier("_newValue".to_string());
        let zero_literal = Expression::Literal(Literal::Number(NumberLiteral {
            value: "0".to_string(),
            sub_denomination: None,
        }));
        let expr = Expression::Binary(BinaryExpression {
            left: Box::new(new_value_ident),
            operator: BinaryOperator::GreaterThan,
            right: Box::new(zero_literal),
        });

        // Case 1: _newValue = 42 (positive)
        // Expected: _newValue > 0  =>  42 > 0  =>  true
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::UInt(42));
        match interpreter.evaluate_predicate(&expr) {
            Ok(result) => assert!(result, "Expected '42 > 0' to be true"),
            Err(e) => panic!("Evaluation failed for _newValue = 42: {}", e),
        }

        // Case 2: _newValue = 0
        // Expected: _newValue > 0  =>  0 > 0  =>  false
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::UInt(0));
        match interpreter.evaluate_predicate(&expr) {
            Ok(result) => assert!(!result, "Expected '0 > 0' to be false"),
            Err(e) => panic!("Evaluation failed for _newValue = 0: {}", e),
        }
        
        // Case 3: _newValue = 1 (positive, edge case near 0)
        // Expected: _newValue > 0  =>  1 > 0  =>  true
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::UInt(1));
        match interpreter.evaluate_predicate(&expr) {
            Ok(result) => assert!(result, "Expected '1 > 0' to be true"),
            Err(e) => panic!("Evaluation failed for _newValue = 1: {}", e),
        }
    }

    #[test]
    fn test_evaluate_parsed_expression_new_value_gt_zero() {
        let mut interpreter = SolidityInterpreter::new();

        // Parse the expression string into an AST
        let expression_str = "_newValue > 0";
        let parsed_expr = match parse_expression(expression_str) {
            Ok(expr) => expr,
            Err(e) => panic!("Failed to parse expression '{}': {}", expression_str, e),
        };

        // Case 1: _newValue = 42 (positive)
        // Expected: _newValue > 0  =>  42 > 0  =>  true
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::UInt(42));
        match interpreter.evaluate_predicate(&parsed_expr) {
            Ok(result) => assert!(result, "Expected '42 > 0' (parsed) to be true"),
            Err(e) => panic!("Parsed evaluation failed for _newValue = 42: {}", e),
        }

        // Case 2: _newValue = 0
        // Expected: _newValue > 0  =>  0 > 0  =>  false
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::UInt(0));
        match interpreter.evaluate_predicate(&parsed_expr) {
            Ok(result) => assert!(!result, "Expected '0 > 0' (parsed) to be false"),
            Err(e) => panic!("Parsed evaluation failed for _newValue = 0: {}", e),
        }
        
        // Case 3: _newValue = 1 (positive, edge case near 0)
        // Expected: _newValue > 0  =>  1 > 0  =>  true
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::UInt(1));
        match interpreter.evaluate_predicate(&parsed_expr) {
            Ok(result) => assert!(result, "Expected '1 > 0' (parsed) to be true"),
            Err(e) => panic!("Parsed evaluation failed for _newValue = 1: {}", e),
        }
    }

    #[test]
    fn test_evaluate_parsed_expression_new_value_gt_zero_signed() {
        let mut interpreter = SolidityInterpreter::new();

        // Parse the expression string into an AST
        let expression_str = "_newValue > 0";
        let parsed_expr = match parse_expression(expression_str) {
            Ok(expr) => expr,
            Err(e) => panic!("Failed to parse expression '{}': {}", expression_str, e),
        };

        // Case 1: _newValue = 42 (positive)
        // Expected: _newValue > 0  =>  42 > 0  =>  true
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::Int(42));
        match interpreter.evaluate_predicate(&parsed_expr) {
            Ok(result) => assert!(result, "Expected '42 > 0' (parsed) to be true"),
            Err(e) => panic!("Parsed evaluation failed for _newValue = 42: {}", e),
        }

        // Case 2: _newValue = 0
        // Expected: _newValue > 0  =>  0 > 0  =>  false
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::Int(0));
        match interpreter.evaluate_predicate(&parsed_expr) {
            Ok(result) => assert!(!result, "Expected '0 > 0' (parsed) to be false"),
            Err(e) => panic!("Parsed evaluation failed for _newValue = 0: {}", e),
        }
        
        // Case 3: _newValue = 1 (positive, edge case near 0)
        // Expected: _newValue > 0  =>  1 > 0  =>  true
        interpreter.context_mut().set_variable("_newValue".to_string(), Value::Int(1));
        match interpreter.evaluate_predicate(&parsed_expr) {
            Ok(result) => assert!(result, "Expected '1 > 0' (parsed) to be true"),
            Err(e) => panic!("Parsed evaluation failed for _newValue = 1: {}", e),
        }
    }


}
