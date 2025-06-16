//! Tests for Solidity parser focusing on expressions and predicates

use crate::parser::{parse_expression, parse_solidity, SolidityParser};
use crate::ast::*;
use pest::Parser;

fn test_parse_expression(input: &str) -> bool {
    match parse_expression(input) {
        Ok(_) => true,
        Err(e) => {
            println!("Parse error for '{}': {}", input, e);
            false
        }
    }
}

fn test_parse_solidity(input: &str) -> bool {
    match parse_solidity(input) {
        Ok(_) => true,
        Err(e) => {
            println!("Parse error for Solidity source: {}", e);
            false
        }
    }
}

#[test]
fn test_basic_arithmetic_expressions() {
    assert!(test_parse_expression("1 + 2"));
    assert!(test_parse_expression("a - b"));
    assert!(test_parse_expression("x * y"));
    assert!(test_parse_expression("total / count"));
    assert!(test_parse_expression("value % 10"));
    assert!(test_parse_expression("base ** exponent"));
}

#[test]
fn test_comparison_expressions() {
    assert!(test_parse_expression("a == b"));
    assert!(test_parse_expression("x != y"));
    assert!(test_parse_expression("value > 0"));
    assert!(test_parse_expression("balance >= amount"));
    assert!(test_parse_expression("count < limit"));
    assert!(test_parse_expression("age <= maxAge"));
}

#[test]
fn test_logical_expressions() {
    assert!(test_parse_expression("isValid && isActive"));
    assert!(test_parse_expression("hasPermission || isOwner"));
    assert!(test_parse_expression("!isLocked"));
    assert!(test_parse_expression("!(a && b)"));
    assert!(test_parse_expression("a && b || c"));
    assert!(test_parse_expression("(a || b) && (c || d)"));
}

#[test]
fn test_bitwise_expressions() {
    assert!(test_parse_expression("flags & mask"));
    assert!(test_parse_expression("value | flag"));
    assert!(test_parse_expression("a ^ b"));
    assert!(test_parse_expression("~bits"));
    assert!(test_parse_expression("value << 2"));
    assert!(test_parse_expression("data >> 8"));
}

#[test]
fn test_assignment_expressions() {
    assert!(test_parse_expression("x = y"));
    assert!(test_parse_expression("balance += amount"));
    assert!(test_parse_expression("count -= 1"));
    assert!(test_parse_expression("total *= factor"));
    assert!(test_parse_expression("value /= divisor"));
    assert!(test_parse_expression("remainder %= modulus"));
    assert!(test_parse_expression("flags |= newFlag"));
    assert!(test_parse_expression("mask &= filter"));
    assert!(test_parse_expression("data ^= key"));
    assert!(test_parse_expression("value <<= shift"));
    assert!(test_parse_expression("data >>= offset"));
}

#[test]
fn test_unary_expressions() {
    assert!(test_parse_expression("++counter"));
    assert!(test_parse_expression("--index"));
    assert!(test_parse_expression("counter++"));
    assert!(test_parse_expression("index--"));
    assert!(test_parse_expression("+value"));
    assert!(test_parse_expression("-amount"));
    assert!(test_parse_expression("!flag"));
    assert!(test_parse_expression("~mask"));
    assert!(test_parse_expression("delete data"));
}

#[test]
fn test_conditional_expressions() {
    assert!(test_parse_expression("condition ? trueValue : falseValue"));
    assert!(test_parse_expression("x > 0 ? x : -x"));
    assert!(test_parse_expression("isValid ? processData() : handleError()"));
    assert!(test_parse_expression("a > b ? a > c ? a : c : b > c ? b : c"));
}

#[test]
fn test_complex_predicates() {
    // Complex boolean expressions that might be used in require statements
    assert!(test_parse_expression("msg.sender == owner"));
    assert!(test_parse_expression("block.timestamp >= startTime"));
    assert!(test_parse_expression("balances[from] >= amount"));
    assert!(test_parse_expression("allowances[from][msg.sender] >= amount"));
    assert!(test_parse_expression("totalSupply + amount <= maxSupply"));
    
    // Multi-condition predicates
    assert!(test_parse_expression("isActive && !isPaused && block.timestamp >= startTime"));
    assert!(test_parse_expression("(msg.sender == owner || hasRole(ADMIN_ROLE, msg.sender)) && amount > 0"));
    assert!(test_parse_expression("balances[account] >= amount && amount > 0 && account != address(0)"));
    
    // Nested conditions
    assert!(test_parse_expression("(a > 0 && b > 0) || (c > 0 && d > 0)"));
    assert!(test_parse_expression("!(paused || locked) && (isWhitelisted || publicSale)"));
}

#[test]
fn test_function_call_expressions() {
    assert!(test_parse_expression("balanceOf(account)"));
    assert!(test_parse_expression("transfer(to, amount)"));
    assert!(test_parse_expression("approve(spender, amount)"));
    assert!(test_parse_expression("keccak256(abi.encodePacked(data))"));
    assert!(test_parse_expression("require(condition, \"Error message\")"));
    
    // Chained calls
    assert!(test_parse_expression("token.balanceOf(msg.sender)"));
    assert!(test_parse_expression("contracts[id].isActive()"));
    
    // Nested function calls
    assert!(test_parse_expression("min(max(value, minValue), maxValue)"));
    assert!(test_parse_expression("sqrt(a * a + b * b)"));
}

#[test]
fn test_member_access_expressions() {
    assert!(test_parse_expression("msg.sender"));
    assert!(test_parse_expression("msg.value"));
    assert!(test_parse_expression("block.timestamp"));
    assert!(test_parse_expression("block.number"));
    assert!(test_parse_expression("tx.origin"));
    assert!(test_parse_expression("address(this).balance"));
    
    // Nested member access
    assert!(test_parse_expression("user.profile.name"));
    assert!(test_parse_expression("settings.config.maxSupply"));
}

#[test]
fn test_array_access_expressions() {
    assert!(test_parse_expression("balances[account]"));
    assert!(test_parse_expression("allowances[owner][spender]"));
    assert!(test_parse_expression("data[index]"));
    assert!(test_parse_expression("matrix[row][col]"));
    
    // Dynamic array access
    assert!(test_parse_expression("users[userCount - 1]"));
    assert!(test_parse_expression("tokens[tokenId]"));
    
    // Complex array access
    assert!(test_parse_expression("mappings[keccak256(key)][subKey]"));
}

#[test]
fn test_type_conversion_expressions() {
    assert!(test_parse_expression("uint256(value)"));
    assert!(test_parse_expression("address(contractAddress)"));
    assert!(test_parse_expression("bytes32(data)"));
    assert!(test_parse_expression("bool(flag)"));
    
    // Complex type conversions
    assert!(test_parse_expression("uint256(bytes32(data))"));
    assert!(test_parse_expression("address(uint160(addressValue))"));
}

#[test]
fn test_literal_expressions() {
    // Number literals
    assert!(test_parse_expression("42"));
    assert!(test_parse_expression("0"));
    assert!(test_parse_expression("1000000"));
    assert!(test_parse_expression("0x1234"));
    assert!(test_parse_expression("0xFF"));
    
    // Number literals with units
    assert!(test_parse_expression("1 ether"));
    assert!(test_parse_expression("1000 wei"));
    assert!(test_parse_expression("1 gwei"));
    assert!(test_parse_expression("30 days"));
    assert!(test_parse_expression("1 hours"));
    assert!(test_parse_expression("60 seconds"));
    
    // String literals
    assert!(test_parse_expression("\"Hello, World!\""));
    assert!(test_parse_expression("\"Error: insufficient balance\""));
    assert!(test_parse_expression("'Single quoted string'"));
    
    // Boolean literals
    assert!(test_parse_expression("true"));
    assert!(test_parse_expression("false"));
    
    // Hex string literals
    assert!(test_parse_expression("hex\"deadbeef\""));
    assert!(test_parse_expression("hex'1234'"));
    
    // Unicode string literals
    assert!(test_parse_expression("unicode\"Hello 🌍\""));
}

#[test]
fn test_operator_precedence() {
    // Test that operators are parsed with correct precedence
    let expr = parse_expression("1 + 2 * 3").unwrap();
    
    // Should parse as: 1 + (2 * 3)
    if let Expression::Binary(binary) = expr {
        assert!(matches!(binary.operator, BinaryOperator::Add));
        if let Expression::Binary(right_binary) = *binary.right {
            assert!(matches!(right_binary.operator, BinaryOperator::Mul));
        } else {
            panic!("Expected multiplication on the right side");
        }
    } else {
        panic!("Expected binary expression");
    }
    
    // Test exponentiation precedence
    let expr = parse_expression("2 ** 3 * 4").unwrap();
    
    // Should parse as: (2 ** 3) * 4
    if let Expression::Binary(binary) = expr {
        assert!(matches!(binary.operator, BinaryOperator::Mul));
        if let Expression::Binary(left_binary) = *binary.left {
            assert!(matches!(left_binary.operator, BinaryOperator::Exp));
        } else {
            panic!("Expected exponentiation on the left side");
        }
    } else {
        panic!("Expected binary expression");
    }
}

#[test]
fn test_parenthesized_expressions() {
    assert!(test_parse_expression("(a + b)"));
    assert!(test_parse_expression("(a + b) * c"));
    assert!(test_parse_expression("a * (b + c)"));
    assert!(test_parse_expression("((a + b) * c) / d"));
    
    // Test that parentheses change precedence
    let expr = parse_expression("(1 + 2) * 3").unwrap();
    
    // Should parse as: (1 + 2) * 3
    if let Expression::Binary(binary) = expr {
        assert!(matches!(binary.operator, BinaryOperator::Mul));
        // The left side should be a parenthesized addition
        // Note: In a real implementation, you might want to handle parentheses explicitly
    } else {
        panic!("Expected binary expression");
    }
}

#[test]
fn test_tuple_expressions() {
    assert!(test_parse_expression("(a, b)"));
    assert!(test_parse_expression("(a, b, c)"));
    assert!(test_parse_expression("(, b, )"));  // Tuple with empty slots
    assert!(test_parse_expression("()"));       // Empty tuple
}

#[test]
fn test_array_literal_expressions() {
    assert!(test_parse_expression("[1, 2, 3]"));
    assert!(test_parse_expression("[a, b, c]"));
    assert!(test_parse_expression("[]"));       // Empty array
    assert!(test_parse_expression("[uint256(1), uint256(2)]"));
}

#[test]
fn test_complex_expressions() {
    // Real-world complex expressions
    assert!(test_parse_expression(
        "balances[from] >= amount && allowances[from][msg.sender] >= amount && to != address(0)"
    ));
    
    assert!(test_parse_expression(
        "block.timestamp >= startTime && block.timestamp <= endTime && !paused"
    ));
    
    assert!(test_parse_expression(
        "totalSupply + amount <= maxSupply && amount > 0 && msg.value >= price * amount"
    ));
    
    assert!(test_parse_expression(
        "(msg.sender == owner || hasRole(MINTER_ROLE, msg.sender)) && amount <= maxMintPerTx"
    ));
    
    // Complex mathematical expressions
    assert!(test_parse_expression(
        "sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2)"
    ));
    
    assert!(test_parse_expression(
        "principal * rate * time / (365 * 24 * 3600 * 10000)"
    ));
}

#[test]
fn test_solidity_require_statements() {
    // Test expressions commonly used in require statements
    assert!(test_parse_expression("msg.sender != address(0)"));
    assert!(test_parse_expression("amount > 0"));
    assert!(test_parse_expression("balance >= amount"));
    assert!(test_parse_expression("block.timestamp >= unlockTime"));
    assert!(test_parse_expression("!paused"));
    assert!(test_parse_expression("isAuthorized[msg.sender]"));
    assert!(test_parse_expression("nonces[owner] == nonce"));
    assert!(test_parse_expression("deadline >= block.timestamp"));
}

#[test]
fn test_solidity_modifier_conditions() {
    // Test expressions commonly used in modifiers
    assert!(test_parse_expression("msg.sender == owner"));
    assert!(test_parse_expression("hasRole(role, account)"));
    assert!(test_parse_expression("!locked"));
    assert!(test_parse_expression("initialized"));
    assert!(test_parse_expression("block.timestamp > lastUpdate + cooldown"));
}

#[test]
fn test_invalid_expressions() {
    // Test that invalid expressions fail to parse
    assert!(!test_parse_expression(""));
    assert!(!test_parse_expression("1 +"));
    // Note: "+ 1" is valid (unary plus)
    // Note: "1 + + 2" is valid (binary plus with unary plus on right)
    assert!(!test_parse_expression("(1 + 2"));
    assert!(!test_parse_expression("1 + 2)"));
    assert!(!test_parse_expression("1 2"));
    assert!(!test_parse_expression("a b"));
}

#[test]
fn test_simple_solidity_contract() {
    let source = r#"
        pragma solidity ^0.8.0;
        
        contract SimpleStorage {
            uint256 public value;
            
            function setValue(uint256 _value) public {
                value = _value;
            }
            
            function getValue() public view returns (uint256) {
                return value;
            }
        }
    "#;
    
    assert!(test_parse_solidity(source));
}

#[test]
fn test_contract_with_complex_expressions() {
    let source = r#"
        pragma solidity ^0.8.0;
        
        contract TokenContract {
            mapping(address => uint256) public balances;
            uint256 public totalSupply;
            address public owner;
            bool public paused;
            
            modifier onlyOwner() {
                require(msg.sender == owner, "Not owner");
                _;
            }
            
            modifier whenNotPaused() {
                require(!paused, "Contract is paused");
                _;
            }
            
            function transfer(address to, uint256 amount) public whenNotPaused returns (bool) {
                require(to != address(0), "Invalid address");
                require(balances[msg.sender] >= amount, "Insufficient balance");
                require(amount > 0, "Amount must be positive");
                
                balances[msg.sender] -= amount;
                balances[to] += amount;
                
                return true;
            }
            
            function mint(address to, uint256 amount) public onlyOwner {
                require(to != address(0), "Invalid address");
                require(totalSupply + amount <= 1000000 * 10**18, "Max supply exceeded");
                
                balances[to] += amount;
                totalSupply += amount;
            }
        }
    "#;
    
    assert!(test_parse_solidity(source));
}

#[test]
fn test_expression_parsing_details() {
    // Test specific expression parsing and verify AST structure
    let expr = parse_expression("a + b * c").unwrap();
    
    // Should parse as: a + (b * c)
    match expr {
        Expression::Binary(binary) => {
            assert!(matches!(binary.operator, BinaryOperator::Add));
            
            // Left side should be identifier 'a'
            if let Expression::Identifier(name) = *binary.left {
                assert_eq!(name, "a");
            } else {
                panic!("Expected identifier 'a' on left side");
            }
            
            // Right side should be multiplication
            if let Expression::Binary(right_binary) = *binary.right {
                assert!(matches!(right_binary.operator, BinaryOperator::Mul));
                
                // Check operands of multiplication
                if let Expression::Identifier(left_name) = *right_binary.left {
                    assert_eq!(left_name, "b");
                } else {
                    panic!("Expected identifier 'b'");
                }
                
                if let Expression::Identifier(right_name) = *right_binary.right {
                    assert_eq!(right_name, "c");
                } else {
                    panic!("Expected identifier 'c'");
                }
            } else {
                panic!("Expected multiplication on right side");
            }
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_comparison_expression_parsing() {
    let expr = parse_expression("balance >= amount").unwrap();
    
    match expr {
        Expression::Binary(binary) => {
            assert!(matches!(binary.operator, BinaryOperator::GreaterThanOrEqual));
            
            if let Expression::Identifier(left_name) = *binary.left {
                assert_eq!(left_name, "balance");
            } else {
                panic!("Expected identifier 'balance'");
            }
            
            if let Expression::Identifier(right_name) = *binary.right {
                assert_eq!(right_name, "amount");
            } else {
                panic!("Expected identifier 'amount'");
            }
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_logical_expression_parsing() {
    let expr = parse_expression("isValid && !isPaused").unwrap();
    
    match expr {
        Expression::Binary(binary) => {
            assert!(matches!(binary.operator, BinaryOperator::And));
            
            if let Expression::Identifier(left_name) = *binary.left {
                assert_eq!(left_name, "isValid");
            } else {
                panic!("Expected identifier 'isValid'");
            }
            
            if let Expression::Unary(unary) = *binary.right {
                assert!(matches!(unary.operator, UnaryOperator::Not));
                assert!(unary.is_prefix);
                
                if let Expression::Identifier(operand_name) = *unary.operand {
                    assert_eq!(operand_name, "isPaused");
                } else {
                    panic!("Expected identifier 'isPaused'");
                }
            } else {
                panic!("Expected unary expression on right side");
            }
        }
        _ => panic!("Expected binary expression"),
    }
}

#[test]
fn test_assignment_expression_parsing() {
    let expr = parse_expression("balance += amount").unwrap();
    
    match expr {
        Expression::Assignment(assignment) => {
            assert!(matches!(assignment.operator, AssignmentOperator::AddAssign));
            
            if let Expression::Identifier(left_name) = *assignment.left {
                assert_eq!(left_name, "balance");
            } else {
                panic!("Expected identifier 'balance'");
            }
            
            if let Expression::Identifier(right_name) = *assignment.right {
                assert_eq!(right_name, "amount");
            } else {
                panic!("Expected identifier 'amount'");
            }
        }
        _ => panic!("Expected assignment expression"),
    }
}

#[test]
fn test_literal_parsing() {
    // Test boolean literal
    let expr = parse_expression("true").unwrap();
    match expr {
        Expression::Literal(Literal::Boolean(value)) => {
            assert!(value);
        }
        _ => panic!("Expected boolean literal"),
    }
    
    // Test number literal
    let expr = parse_expression("42").unwrap();
    match expr {
        Expression::Literal(Literal::Number(number)) => {
            assert_eq!(number.value, "42");
        }
        _ => panic!("Expected number literal"),
    }
    
    // Test string literal
    let expr = parse_expression("\"hello\"").unwrap();
    match expr {
        Expression::Literal(Literal::String(string)) => {
            assert_eq!(string.value, "hello");
        }
        _ => panic!("Expected string literal"),
    }
}

#[test]
fn test_pest_parser_direct() {
    // Test the pest parser directly for specific rules
    use crate::parser::Rule;
    
    let result = SolidityParser::parse(Rule::identifier, "myVariable");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::number_literal, "42");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::boolean_literal, "true");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::string_literal, "\"hello\"");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::additive_expression, "a + b");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::multiplicative_expression, "a * b");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::equality_expression, "a == b");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::relational_expression, "a > b");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::logical_and_expression, "a && b");
    assert!(result.is_ok());
    
    let result = SolidityParser::parse(Rule::logical_or_expression, "a || b");
    assert!(result.is_ok());
}
