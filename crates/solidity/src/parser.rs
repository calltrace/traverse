use crate::ast::*;
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::fmt;

#[derive(Parser)]
#[grammar = "solidity.pest"]
pub struct SolidityParser;

#[derive(Debug)]
pub enum SolidityParseError {
    PestError(String),
    EmptyInput,
    ParseError(String),
}

impl fmt::Display for SolidityParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SolidityParseError::PestError(err) => write!(f, "Parser error: {}", err),
            SolidityParseError::EmptyInput => write!(f, "Input is empty"),
            SolidityParseError::ParseError(err) => write!(f, "Parse error: {}", err),
        }
    }
}

impl std::error::Error for SolidityParseError {}

impl From<pest::error::Error<Rule>> for SolidityParseError {
    fn from(error: pest::error::Error<Rule>) -> Self {
        SolidityParseError::PestError(error.to_string())
    }
}

pub fn parse_solidity(source: &str) -> Result<SourceUnit, SolidityParseError> {
    if source.trim().is_empty() {
        return Err(SolidityParseError::EmptyInput);
    }

    let pairs = SolidityParser::parse(Rule::source_unit, source)?;
    let source_unit = pairs.peek().map(SourceUnit::from).unwrap_or_default();

    Ok(source_unit)
}

pub fn parse_expression(source: &str) -> Result<Expression, SolidityParseError> {
    if source.trim().is_empty() {
        return Err(SolidityParseError::EmptyInput);
    }

    let pairs = SolidityParser::parse(Rule::complete_expression, source)?;
    let expression = pairs
        .peek()
        .map(|pair| {
            pair.into_inner()
                .next()
                .map(Expression::from)
                .unwrap_or(Expression::Literal(Literal::Boolean(false)))
        })
        .ok_or_else(|| {
            SolidityParseError::ParseError("Failed to parse complete expression".to_string())
        })?;

    Ok(expression)
}

impl From<Pair<'_, Rule>> for SourceUnit {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut items = Vec::new();

        for inner_pair in pair.into_inner() {
            if inner_pair.as_rule() == Rule::EOI {
                break;
            }

            if let Some(item) = SourceUnitItem::from_pair(inner_pair) {
                items.push(item);
            }
        }

        SourceUnit { items }
    }
}

impl SourceUnitItem {
    pub fn from_pair(pair: Pair<'_, Rule>) -> Option<Self> {
        match pair.as_rule() {
            Rule::pragma_directive => Some(SourceUnitItem::Pragma(PragmaDirective::from(pair))),
            Rule::contract_definition => {
                Some(SourceUnitItem::Contract(ContractDefinition::from(pair)))
            }
            Rule::struct_definition => Some(SourceUnitItem::Struct(StructDefinition::from(pair))),
            Rule::enum_definition => Some(SourceUnitItem::Enum(EnumDefinition::from(pair))),
            _ => None,
        }
    }
}

impl From<Pair<'_, Rule>> for PragmaDirective {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let tokens = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::pragma_token)
            .map(|p| p.as_str().to_string())
            .collect();

        PragmaDirective { tokens }
    }
}

impl From<Pair<'_, Rule>> for ContractDefinition {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut is_abstract = false;
        let mut name = String::new();
        let inheritance = Vec::new();
        let body = Vec::new();

        if pair.as_str().trim_start().starts_with("abstract") {
            is_abstract = true;
        }

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if name.is_empty() {
                        name = inner_pair.as_str().to_string();
                    }
                }
                _ => {}
            }
        }

        ContractDefinition {
            is_abstract,
            name,
            inheritance,
            body,
        }
    }
}

impl From<Pair<'_, Rule>> for StructDefinition {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut name = String::new();
        let mut members = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if name.is_empty() {
                        name = inner_pair.as_str().to_string();
                    }
                }
                Rule::struct_member => {
                    members.push(StructMember::from(inner_pair));
                }
                _ => {}
            }
        }

        StructDefinition { name, members }
    }
}

impl From<Pair<'_, Rule>> for StructMember {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut type_name = TypeName::Elementary(ElementaryTypeName::Bool);
        let mut name = String::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::type_name => {
                    type_name = TypeName::from(inner_pair);
                }
                Rule::identifier => {
                    name = inner_pair.as_str().to_string();
                }
                _ => {}
            }
        }

        StructMember { type_name, name }
    }
}

impl From<Pair<'_, Rule>> for EnumDefinition {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let mut name = String::new();
        let mut values = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    if name.is_empty() {
                        name = inner_pair.as_str().to_string();
                    } else {
                        values.push(inner_pair.as_str().to_string());
                    }
                }
                _ => {}
            }
        }

        EnumDefinition { name, values }
    }
}

impl From<Pair<'_, Rule>> for TypeName {
    fn from(pair: Pair<'_, Rule>) -> Self {
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::base_type_name => {
                    return TypeName::from(inner_pair);
                }
                Rule::array_suffix => {
                    // This is simplified - in a real implementation you'd handle array types properly
                    return TypeName::Array(
                        Box::new(TypeName::Elementary(ElementaryTypeName::Bool)),
                        None,
                    );
                }
                _ => {}
            }
        }

        TypeName::Elementary(ElementaryTypeName::Bool)
    }
}

impl From<Pair<'_, Rule>> for ElementaryTypeName {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let text = pair.as_str();

        if text == "address" {
            ElementaryTypeName::Address
        } else if text == "bool" {
            ElementaryTypeName::Bool
        } else if text == "string" {
            ElementaryTypeName::String
        } else if text == "bytes" {
            ElementaryTypeName::Bytes
        } else if text.starts_with("uint") {
            let size = if text == "uint" {
                None
            } else {
                text[4..].parse().ok()
            };
            ElementaryTypeName::UnsignedInteger(size)
        } else if text.starts_with("int") {
            let size = if text == "int" {
                None
            } else {
                text[3..].parse().ok()
            };
            ElementaryTypeName::SignedInteger(size)
        } else {
            ElementaryTypeName::Bool // default
        }
    }
}

impl From<Pair<'_, Rule>> for IdentifierPath {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let parts = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::identifier)
            .map(|p| p.as_str().to_string())
            .collect();

        IdentifierPath { parts }
    }
}

fn parse_call_arguments(pair: Pair<'_, Rule>) -> Vec<Expression> {
    let mut arguments = Vec::new();

    for inner_pair in pair.into_inner() {
        match inner_pair.as_rule() {
            Rule::expression_list => {
                for expr_pair in inner_pair.into_inner() {
                    if expr_pair.as_rule() == Rule::expression {
                        arguments.push(Expression::from(expr_pair));
                    }
                }
            }
            Rule::expression => {
                arguments.push(Expression::from(inner_pair));
            }
            _ => {}
        }
    }

    arguments
}

impl From<Pair<'_, Rule>> for Expression {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::expression => {
                if let Some(inner) = pair.into_inner().next() {
                    Expression::from(inner)
                } else {
                    Expression::Literal(Literal::Boolean(false))
                }
            }
            Rule::assignment_expression => {
                let mut inner_pairs = pair.into_inner();
                let first = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                if let Some(op_pair) = inner_pairs.next() {
                    let operator = AssignmentOperator::from(op_pair);
                    let right = inner_pairs
                        .next()
                        .map(Expression::from)
                        .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                    Expression::Assignment(AssignmentExpression {
                        left: Box::new(first),
                        operator,
                        right: Box::new(right),
                    })
                } else {
                    first
                }
            }
            Rule::conditional_expression => {
                let mut inner_pairs = pair.into_inner();
                let condition = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                if inner_pairs.peek().is_some() {
                    let true_expr = inner_pairs
                        .next()
                        .map(Expression::from)
                        .unwrap_or(Expression::Literal(Literal::Boolean(false)));
                    let false_expr = inner_pairs
                        .next()
                        .map(Expression::from)
                        .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                    Expression::Conditional(ConditionalExpression {
                        condition: Box::new(condition),
                        true_expr: Box::new(true_expr),
                        false_expr: Box::new(false_expr),
                    })
                } else {
                    condition
                }
            }
            Rule::additive_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let operator = match op_pair.as_str() {
                            "+" => BinaryOperator::Add,
                            "-" => BinaryOperator::Sub,
                            _ => BinaryOperator::Add,
                        };
                        let right = Expression::from(right_pair);

                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::multiplicative_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let operator = match op_pair.as_str() {
                            "*" => BinaryOperator::Mul,
                            "/" => BinaryOperator::Div,
                            "%" => BinaryOperator::Mod,
                            _ => BinaryOperator::Mul,
                        };
                        let right = Expression::from(right_pair);

                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::relational_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let operator = match op_pair.as_str() {
                            "<" => BinaryOperator::LessThan,
                            ">" => BinaryOperator::GreaterThan,
                            "<=" => BinaryOperator::LessThanOrEqual,
                            ">=" => BinaryOperator::GreaterThanOrEqual,
                            _ => BinaryOperator::GreaterThan,
                        };
                        let right = Expression::from(right_pair);

                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::equality_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let operator = match op_pair.as_str() {
                            "==" => BinaryOperator::Equal,
                            "!=" => BinaryOperator::NotEqual,
                            _ => BinaryOperator::Equal,
                        };
                        let right = Expression::from(right_pair);

                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::logical_and_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(_op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let right = Expression::from(right_pair);
                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator: BinaryOperator::And,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::logical_or_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(_op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let right = Expression::from(right_pair);
                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator: BinaryOperator::Or,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::bitwise_and_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(_op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let right = Expression::from(right_pair);
                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator: BinaryOperator::BitAnd,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::bitwise_xor_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(_op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let right = Expression::from(right_pair);
                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator: BinaryOperator::BitXor,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::bitwise_or_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(_op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let right = Expression::from(right_pair);
                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator: BinaryOperator::BitOr,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::shift_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                while let Some(op_pair) = inner_pairs.next() {
                    if let Some(right_pair) = inner_pairs.next() {
                        let operator = match op_pair.as_str() {
                            "<<" => BinaryOperator::ShiftLeft,
                            ">>" => BinaryOperator::ShiftRight,
                            ">>>" => BinaryOperator::ShiftRightArithmetic,
                            _ => BinaryOperator::ShiftLeft,
                        };
                        let right = Expression::from(right_pair);

                        left = Expression::Binary(BinaryExpression {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        });
                    }
                }

                left
            }
            Rule::exponential_expression => {
                let mut inner_pairs = pair.into_inner();
                let left = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                if let Some(right_pair) = inner_pairs.next() {
                    let right = Expression::from(right_pair);
                    Expression::Binary(BinaryExpression {
                        left: Box::new(left),
                        operator: BinaryOperator::Exp,
                        right: Box::new(right),
                    })
                } else {
                    left
                }
            }
            Rule::unary_expression => {
                let full_text = pair.as_str();
                let mut inner_pairs = pair.into_inner();

                if let Some(operand_pair) = inner_pairs.next() {
                    let operand_text = operand_pair.as_str();

                    // Check if this is a prefix unary expression by comparing texts
                    if full_text != operand_text {
                        // This is a prefix unary expression
                        // Extract the operator by finding what comes before the operand
                        let operator_str = if full_text.starts_with("++")
                            && full_text[2..].trim_start() == operand_text
                        {
                            "++"
                        } else if full_text.starts_with("--")
                            && full_text[2..].trim_start() == operand_text
                        {
                            "--"
                        } else if full_text.starts_with("!")
                            && full_text[1..].trim_start() == operand_text
                        {
                            "!"
                        } else if full_text.starts_with("~")
                            && full_text[1..].trim_start() == operand_text
                        {
                            "~"
                        } else if full_text.starts_with("delete ")
                            && full_text[7..].trim_start() == operand_text
                        {
                            "delete"
                        } else if full_text.starts_with("-")
                            && full_text[1..].trim_start() == operand_text
                        {
                            "-"
                        } else if full_text.starts_with("+")
                            && full_text[1..].trim_start() == operand_text
                        {
                            "+"
                        } else {
                            // Fallback: couldn't determine operator, treat as regular expression
                            return Expression::from(operand_pair);
                        };

                        let operator = match operator_str {
                            "+" => UnaryOperator::Plus,
                            "-" => UnaryOperator::Minus,
                            "!" => UnaryOperator::Not,
                            "~" => UnaryOperator::BitNot,
                            "++" => UnaryOperator::Increment,
                            "--" => UnaryOperator::Decrement,
                            "delete" => UnaryOperator::Delete,
                            _ => UnaryOperator::Plus, // fallback
                        };

                        let operand = Expression::from(operand_pair);

                        Expression::Unary(UnaryExpression {
                            operator,
                            operand: Box::new(operand),
                            is_prefix: true,
                        })
                    } else {
                        // This is not a prefix unary expression, just parse the operand
                        Expression::from(operand_pair)
                    }
                } else {
                    Expression::Literal(Literal::Boolean(false))
                }
            }
            Rule::postfix_expression => {
                let mut inner_pairs = pair.into_inner();
                let mut expr = inner_pairs
                    .next()
                    .map(Expression::from)
                    .unwrap_or(Expression::Literal(Literal::Boolean(false)));

                for suffix_pair in inner_pairs {
                    match suffix_pair.as_rule() {
                        Rule::identifier => {
                            expr = Expression::MemberAccess(MemberAccessExpression {
                                object: Box::new(expr),
                                member: suffix_pair.as_str().to_string(),
                            });
                        }
                        Rule::call_argument_list => {
                            // This is a function call: expr(args)
                            let arguments = parse_call_arguments(suffix_pair);
                            expr = Expression::FunctionCall(FunctionCallExpression {
                                function: Box::new(expr),
                                arguments,
                            });
                        }
                        Rule::expression => {
                            // This is array access: expr[index]
                            let index = Expression::from(suffix_pair);
                            expr = Expression::IndexAccess(IndexAccessExpression {
                                object: Box::new(expr),
                                index: Some(Box::new(index)),
                            });
                        }
                        _ => {
                            match suffix_pair.as_str() {
                                "++" => {
                                    expr = Expression::Unary(UnaryExpression {
                                        operator: UnaryOperator::Increment,
                                        operand: Box::new(expr),
                                        is_prefix: false,
                                    });
                                }
                                "--" => {
                                    expr = Expression::Unary(UnaryExpression {
                                        operator: UnaryOperator::Decrement,
                                        operand: Box::new(expr),
                                        is_prefix: false,
                                    });
                                }
                                _ => {
                                    // Handle other postfix operations
                                    // For now, we'll ignore unknown postfix operations
                                }
                            }
                        }
                    }
                }

                expr
            }
            Rule::primary_expression => {
                let mut inner_pairs = pair.into_inner();

                if let Some(inner_pair) = inner_pairs.next() {
                    match inner_pair.as_rule() {
                        Rule::identifier => Expression::Identifier(inner_pair.as_str().to_string()),
                        Rule::literal => Expression::Literal(Literal::from(inner_pair)),
                        Rule::literal_with_sub_denomination => {
                            Expression::Literal(Literal::from(inner_pair))
                        }
                        Rule::elementary_type_name => {
                            // Type name used as expression (e.g., in type() calls)
                            Expression::Identifier(inner_pair.as_str().to_string())
                        }
                        Rule::tuple_expression => {
                            Expression::Tuple(TupleExpression::from(inner_pair))
                        }
                        Rule::inline_array_expression => {
                            Expression::Array(ArrayExpression::from(inner_pair))
                        }
                        _ => Expression::Literal(Literal::Boolean(false)),
                    }
                } else {
                    Expression::Literal(Literal::Boolean(false))
                }
            }
            Rule::identifier => Expression::Identifier(pair.as_str().to_string()),
            Rule::literal => Expression::Literal(Literal::from(pair)),
            _ => Expression::Literal(Literal::Boolean(false)),
        }
    }
}

impl From<Pair<'_, Rule>> for AssignmentOperator {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.as_str() {
            "=" => AssignmentOperator::Assign,
            "+=" => AssignmentOperator::AddAssign,
            "-=" => AssignmentOperator::SubAssign,
            "*=" => AssignmentOperator::MulAssign,
            "/=" => AssignmentOperator::DivAssign,
            "%=" => AssignmentOperator::ModAssign,
            "&=" => AssignmentOperator::BitAndAssign,
            "|=" => AssignmentOperator::BitOrAssign,
            "^=" => AssignmentOperator::BitXorAssign,
            "<<=" => AssignmentOperator::ShiftLeftAssign,
            ">>=" => AssignmentOperator::ShiftRightAssign,
            ">>>=" => AssignmentOperator::ShiftRightArithmeticAssign,
            _ => AssignmentOperator::Assign,
        }
    }
}

impl From<Pair<'_, Rule>> for UnaryOperator {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.as_str() {
            "+" => UnaryOperator::Plus,
            "-" => UnaryOperator::Minus,
            "!" => UnaryOperator::Not,
            "~" => UnaryOperator::BitNot,
            "++" => UnaryOperator::Increment,
            "--" => UnaryOperator::Decrement,
            "delete" => UnaryOperator::Delete,
            _ => UnaryOperator::Plus,
        }
    }
}

impl From<Pair<'_, Rule>> for TupleExpression {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let elements = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::expression)
            .map(|p| Some(Expression::from(p)))
            .collect();

        TupleExpression { elements }
    }
}

impl From<Pair<'_, Rule>> for ArrayExpression {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let elements = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::expression)
            .map(Expression::from)
            .collect();

        ArrayExpression { elements }
    }
}

impl Literal {
    fn from_specific_literal_rule(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::boolean_literal => Literal::Boolean(pair.as_str() == "true"),
            Rule::number_literal => Literal::Number(NumberLiteral::from(pair)),
            Rule::string_literal => Literal::String(StringLiteral::from(pair)),
            Rule::hex_string_literal => Literal::HexString(HexStringLiteral::from(pair)),
            Rule::unicode_string_literal => {
                Literal::UnicodeString(UnicodeStringLiteral::from(pair))
            }
            _ => {
                eprintln!(
                    "Unexpected rule in from_specific_literal_rule: {:?}",
                    pair.as_rule()
                );
                Literal::Boolean(false)
            }
        }
    }
}

impl From<Pair<'_, Rule>> for Literal {
    fn from(pair: Pair<'_, Rule>) -> Self {
        match pair.as_rule() {
            Rule::literal => {
                let pair_str = pair.as_str();
                if let Some(specific_literal_pair) = pair.into_inner().next() {
                    Literal::from_specific_literal_rule(specific_literal_pair)
                } else {
                    eprintln!("Rule::literal had no inner pair: {:?}", pair_str);
                    Literal::Boolean(false)
                }
            }
            Rule::literal_with_sub_denomination => {
                let mut number_value = String::new();
                let mut sub_denomination = None;

                for inner_pair in pair.into_inner() {
                    match inner_pair.as_rule() {
                        Rule::number_literal => {
                            number_value = inner_pair.as_str().to_string();
                        }
                        Rule::sub_denomination => {
                            sub_denomination = Some(inner_pair.as_str().to_string());
                        }
                        _ => {}
                    }
                }

                Literal::Number(NumberLiteral {
                    value: number_value,
                    sub_denomination,
                })
            }
            Rule::boolean_literal => Literal::Boolean(pair.as_str() == "true"),
            Rule::number_literal => Literal::Number(NumberLiteral::from(pair)),
            Rule::string_literal => Literal::String(StringLiteral::from(pair)),
            Rule::hex_string_literal => Literal::HexString(HexStringLiteral::from(pair)),
            Rule::unicode_string_literal => {
                Literal::UnicodeString(UnicodeStringLiteral::from(pair))
            }
            _ => {
                eprintln!("Unexpected rule in Literal::from: {:?}", pair.as_rule());
                Literal::Boolean(false)
            }
        }
    }
}

impl From<Pair<'_, Rule>> for NumberLiteral {
    fn from(pair: Pair<'_, Rule>) -> Self {
        NumberLiteral {
            value: pair.as_str().to_string(),
            sub_denomination: None,
        }
    }
}

impl From<Pair<'_, Rule>> for StringLiteral {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let value = pair.as_str().trim_matches('"').to_string();
        StringLiteral { value }
    }
}

impl From<Pair<'_, Rule>> for HexStringLiteral {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let value = pair.as_str().to_string();
        HexStringLiteral { value }
    }
}

impl From<Pair<'_, Rule>> for UnicodeStringLiteral {
    fn from(pair: Pair<'_, Rule>) -> Self {
        let value = pair.as_str().to_string();
        UnicodeStringLiteral { value }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_pragma() {
        let source = "pragma solidity ^0.8.0;";
        let result = SolidityParser::parse(Rule::pragma_directive, source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_simple_expression() {
        let source = "a + b";
        let result = parse_expression(source);
        assert!(result.is_ok());

        if let Ok(Expression::Binary(binary)) = result {
            assert!(matches!(binary.operator, BinaryOperator::Add));
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_parse_complex_expression() {
        let source = "a + b * c";
        let result = parse_expression(source);
        assert!(result.is_ok());

        // Should parse as: a + (b * c) due to operator precedence
        if let Ok(Expression::Binary(binary)) = result {
            assert!(matches!(binary.operator, BinaryOperator::Add));
            if let Expression::Binary(right_binary) = *binary.right {
                assert!(matches!(right_binary.operator, BinaryOperator::Mul));
            } else {
                panic!("Expected multiplication on the right side");
            }
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_parse_comparison_expression() {
        let source = "x > 10";
        let result = parse_expression(source);
        assert!(result.is_ok());

        if let Ok(Expression::Binary(binary)) = result {
            assert!(matches!(binary.operator, BinaryOperator::GreaterThan));
        } else {
            panic!("Expected comparison expression");
        }
    }

    #[test]
    fn test_parse_logical_expression() {
        let source = "a && b || c";
        let result = parse_expression(source);
        assert!(result.is_ok());

        // Should parse as: (a && b) || c due to operator precedence
        if let Ok(Expression::Binary(binary)) = result {
            assert!(matches!(binary.operator, BinaryOperator::Or));
        } else {
            panic!("Expected logical expression");
        }
    }

    #[test]
    fn test_parse_assignment_expression() {
        let source = "x = y + z";
        let result = parse_expression(source);
        assert!(result.is_ok());

        if let Ok(Expression::Assignment(assignment)) = result {
            assert!(matches!(assignment.operator, AssignmentOperator::Assign));
        } else {
            panic!("Expected assignment expression");
        }
    }

    #[test]
    fn test_parse_unary_expression() {
        let source = "!flag";
        let result = parse_expression(source);
        assert!(result.is_ok());

        if let Ok(Expression::Unary(unary)) = result {
            assert!(matches!(unary.operator, UnaryOperator::Not));
            assert!(unary.is_prefix);
        } else {
            panic!("Expected unary expression");
        }
    }

    #[test]
    fn test_parse_literal_expressions() {
        // Boolean literal
        let result = parse_expression("true");
        assert!(result.is_ok());
        if let Ok(Expression::Literal(Literal::Boolean(true))) = result {
            // Success
        } else {
            panic!("Expected boolean literal");
        }

        let result = parse_expression("42");
        assert!(result.is_ok());
        if let Ok(Expression::Literal(Literal::Number(_))) = result {
            // Success
        } else {
            panic!("Expected number literal");
        }

        let result = parse_expression("\"hello\"");
        assert!(result.is_ok());
        if let Ok(Expression::Literal(Literal::String(_))) = result {
            // Success
        } else {
            panic!("Expected string literal");
        }
    }

    #[test]
    fn test_parse_identifier_expression() {
        let source = "myVariable";
        let result = parse_expression(source);
        assert!(result.is_ok());

        if let Ok(Expression::Identifier(name)) = result {
            assert_eq!(name, "myVariable");
        } else {
            panic!("Expected identifier expression");
        }
    }

    #[test]
    fn test_operator_precedence() {
        // Test that multiplication has higher precedence than addition
        let source = "1 + 2 * 3";
        let result = parse_expression(source);
        assert!(result.is_ok());

        // Should parse as: 1 + (2 * 3)
        if let Ok(Expression::Binary(binary)) = result {
            assert!(matches!(binary.operator, BinaryOperator::Add));
            if let Expression::Binary(right_binary) = *binary.right {
                assert!(matches!(right_binary.operator, BinaryOperator::Mul));
            } else {
                panic!("Expected multiplication on the right side");
            }
        } else {
            panic!("Expected binary expression");
        }
    }

    #[test]
    fn test_parse_empty_input() {
        let result = parse_expression("");
        assert!(result.is_err());
        assert!(matches!(result, Err(SolidityParseError::EmptyInput)));
    }
}
