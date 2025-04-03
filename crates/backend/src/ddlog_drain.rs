/// # DDlog Drain
///
/// An iterator-based parser and consumer for DDlog output relation lines.
///
/// This module provides functionality to parse and process DDlog output relations
/// in a streaming fashion. It converts textual DDlog output into structured Rust
/// data types that can be easily manipulated in application code.
///
///
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

use nom::{
    branch::alt,
    bytes::complete::{is_not, take_while1},
    character::complete::{char, multispace0, multispace1},
    combinator::{map, map_res, opt, recognize},
    multi::separated_list0,
    sequence::{delimited, preceded},
    IResult, Parser,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeValue {
    String(String),
    Number(i64),
    Path(String),
}

impl fmt::Display for AttributeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AttributeValue::String(s) => write!(f, "{}", s),
            AttributeValue::Number(n) => write!(f, "{}", n),
            AttributeValue::Path(p) => write!(f, "{}", p),
        }
    }
}

impl AttributeValue {
    pub fn as_string(&self) -> Option<&String> {
        match self {
            AttributeValue::String(s) => Some(s),
            AttributeValue::Path(p) => Some(p),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<i64> {
        match self {
            AttributeValue::Number(n) => Some(*n),
            _ => None,
        }
    }

    pub fn as_path(&self) -> Option<&String> {
        match self {
            AttributeValue::Path(p) => Some(p),
            _ => None,
        }
    }

    // For backward compatibility with tests
    pub fn to_string_value(&self) -> String {
        match self {
            AttributeValue::String(s) => s.clone(),
            AttributeValue::Number(n) => n.to_string(),
            AttributeValue::Path(p) => p.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DdlogFact {
    pub relation_name: String,
    pub attributes: HashMap<String, AttributeValue>,
    pub diff: Option<i64>,
}

impl fmt::Display for DdlogFact {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{{", self.relation_name)?;
        for (i, (key, value)) in self.attributes.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, ".{} = {}", key, value)?;
        }
        write!(f, "}}: {}", self.diff.unwrap_or(0))
    }
}

// For backward compatibility with existing code
impl DdlogFact {
    pub fn get_attribute_string(&self, key: &str) -> Option<String> {
        self.attributes.get(key).map(|v| v.to_string_value())
    }
}

// implement

#[derive(Debug)]
pub enum DdlogDrainError {
    ParseError(String),
}

impl fmt::Display for DdlogDrainError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DdlogDrainError::ParseError(e) => write!(f, "DDlog parse error: {}", e),
        }
    }
}

impl Error for DdlogDrainError {}

pub struct DdlogDrain<I: Iterator<Item = String>> {
    lines: I,
}

impl<I> DdlogDrain<I>
where
    I: Iterator<Item = String>,
{
    pub fn new(lines: I) -> Self {
        DdlogDrain { lines }
    }
}

impl<I> Iterator for DdlogDrain<I>
where
    I: Iterator<Item = String>,
{
    type Item = Result<DdlogFact, DdlogDrainError>;

    fn next(&mut self) -> Option<Self::Item> {
        for line in &mut self.lines {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                continue;
            }

            match parse_fact_line(trimmed) {
                Ok((_rest, fact)) => {
                    return Some(Ok(fact));
                }
                Err(e) => {
                    let msg = format!("Failed to parse line {:?}: {:?}", trimmed, e);
                    return Some(Err(DdlogDrainError::ParseError(msg)));
                }
            }
        }
        None
    }
}

fn parse_fact_line(input: &str) -> IResult<&str, DdlogFact> {
    let mut parser = (
        parse_relation_name,
        opt(parse_attribute_block),
        opt(parse_diff),
    );
    let (rest, (relation_name, attrs_opt, diff_opt)) = parser.parse(input)?;

    Ok((
        rest,
        DdlogFact {
            relation_name: relation_name.to_string(),
            attributes: attrs_opt.unwrap_or_default(),
            diff: diff_opt.flatten(), // Flatten Option<Option<i64>> to Option<i64>
        },
    ))
}

fn parse_relation_name(input: &str) -> IResult<&str, &str> {
    let mut parser = recognize(take_while1(|c: char| c.is_alphanumeric() || c == '_'));
    parser.parse(input)
}

fn parse_attribute_block(input: &str) -> IResult<&str, HashMap<String, AttributeValue>> {
    let mut parser = delimited(
        preceded(multispace0, char('{')),
        map(
            separated_list0(parse_comma, parse_one_attribute),
            |pairs: Vec<(String, AttributeValue)>| pairs.into_iter().collect(),
        ),
        preceded(multispace0, char('}')),
    );
    parser.parse(input)
}

fn parse_comma(input: &str) -> IResult<&str, ()> {
    let mut parser = preceded(multispace0, preceded(char(','), multispace0));
    let (rest, _) = parser.parse(input)?;
    Ok((rest, ()))
}

fn parse_one_attribute(input: &str) -> IResult<&str, (String, AttributeValue)> {
    let mut parser = (
        preceded(multispace0, char('.')),
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),
        preceded(multispace0, char('=')),
        preceded(multispace0, parse_attribute_value),
    );

    let (rest, (_, attr_name, _, value)) = parser.parse(input)?;
    Ok((rest, (attr_name.to_string(), value)))
}

fn parse_attribute_value(input: &str) -> IResult<&str, AttributeValue> {
    let mut parser = alt((
        // String values (quoted)
        map(
            delimited(char('"'), opt(is_not("\"")), char('"')),
            |maybe_str: Option<&str>| match maybe_str {
                Some(s) if s.chars().all(|c| c == '.' || c.is_ascii_digit()) => {
                    AttributeValue::Path(s.to_string())
                }
                Some(s) => AttributeValue::String(s.to_string()),
                None => AttributeValue::String("".to_string()),
            },
        ),
        // Number values
        map_res(
            recognize((
                opt(alt((char('+'), char('-')))),
                take_while1(|c: char| c.is_ascii_digit()),
            )),
            |s: &str| -> Result<AttributeValue, String> {
                match s.parse::<i64>() {
                    Ok(n) => Ok(AttributeValue::Number(n)),
                    Err(_) => Ok(AttributeValue::String(s.to_string())), // Fallback
                }
            },
        ),
        // Other values (fallback)
        map(
            take_while1(|c: char| ![',', '}', ' '].contains(&c)),
            |s: &str| AttributeValue::String(s.to_string()),
        ),
    ));
    parser.parse(input)
}

fn parse_diff(input: &str) -> IResult<&str, Option<i64>> {
    let mut parser = preceded(
        delimited(multispace0, char(':'), multispace1),
        opt(map_res(
            recognize((
                opt(alt((char('+'), char('-')))),
                take_while1(|c: char| c.is_ascii_digit()),
            )),
            |s: &str| s.parse::<i64>(),
        )),
    );
    let (rest, maybe_num) = parser.parse(input)?;
    Ok((rest, maybe_num)) // Return None if no number was present
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_parse() {
        let lines = vec![
            "Edge{.from = 92, .to = 93}: +1".into(),
            "EmitMermaidLineActivate:".into(),
            "EmitMermaidLineActivate{.val = \"activate Counter\", .x=42}: -1".into(),
        ];
        let mut dd = DdlogDrain::new(lines.into_iter());

        let fact1 = dd.next().unwrap().unwrap();
        assert_eq!(fact1.relation_name, "Edge");
        assert_eq!(fact1.diff, Some(1));
        assert_eq!(fact1.attributes.len(), 2);
        assert_eq!(fact1.get_attribute_string("from"), Some("92".to_string()));
        assert_eq!(fact1.get_attribute_string("to"), Some("93".to_string()));

        let fact2 = dd.next().unwrap().unwrap();
        assert_eq!(fact2.relation_name, "EmitMermaidLineActivate");
        assert_eq!(fact2.diff, None); // colon was present, but no number => 0 => Some(0) or None
                                      // In the current code, absent digits become 0. If you prefer `None`, just change parse_diff logic.

        let fact3 = dd.next().unwrap().unwrap();
        assert_eq!(fact3.relation_name, "EmitMermaidLineActivate");
        assert_eq!(fact3.diff, Some(-1));
        assert_eq!(
            fact3.get_attribute_string("val"),
            Some("activate Counter".to_string())
        );
        assert_eq!(fact3.get_attribute_string("x"), Some("42".to_string()));

        assert!(dd.next().is_none());
    }

    #[test]
    fn test_mermaid_sequence_diagram_parse() {
        let input = r#"EmitMermaidLineActivate:
EmitMermaidLineActivate{.val = "activate Counter", .ce_id_path = "0.47.49.75.82.83.84.85.86", .callee_contract_path = "0.6"}: +1
EmitMermaidLineActivate{.val = "activate Counter", .ce_id_path = "0.47.49.75.82.91.92.93.94", .callee_contract_path = "0.6"}: +1
EmitMermaidLineCalleeParticipantLine:
EmitMermaidLineCalleeParticipantLine{.val = "participant Counter", .callee_contract_path = "0.6"}: +1
EmitMermaidLineCallerParticipantLine:
EmitMermaidLineCallerParticipantLine{.val = "participant CounterCaller", .caller_contract_path = "0.47"}: +1
EmitMermaidLineSignalLine:
EmitMermaidLineSignalLine{.val = "CounterCaller->>Counter: getCount", .ce_id_path = "0.47.49.75.82.91.92.93.94", .caller_contract_path = "0.47", .callee_contract_path = "0.6", .callee_func_path = "0.6.8.34"}: +1
EmitMermaidLineSignalLine{.val = "CounterCaller->>Counter: increment", .ce_id_path = "0.47.49.75.82.83.84.85.86", .caller_contract_path = "0.47", .callee_contract_path = "0.6", .callee_func_path = "0.6.8.14"}: +1"#;

        let lines = input.lines().map(|s| s.to_string()).collect::<Vec<_>>();
        let mut drain = DdlogDrain::new(lines.into_iter());

        // First fact: EmitMermaidLineActivate with no attributes
        let fact1 = drain.next().unwrap().unwrap();
        assert_eq!(fact1.relation_name, "EmitMermaidLineActivate");
        assert_eq!(fact1.attributes.len(), 0);
        assert_eq!(fact1.diff, None);

        // Second fact: First EmitMermaidLineActivate with attributes
        let fact2 = drain.next().unwrap().unwrap();
        assert_eq!(fact2.relation_name, "EmitMermaidLineActivate");
        assert_eq!(fact2.attributes.len(), 3);
        assert_eq!(
            fact2.get_attribute_string("val"),
            Some("activate Counter".to_string())
        );
        assert_eq!(
            fact2.get_attribute_string("ce_id_path"),
            Some("0.47.49.75.82.83.84.85.86".to_string())
        );
        assert_eq!(
            fact2.get_attribute_string("callee_contract_path"),
            Some("0.6".to_string())
        );
        assert_eq!(fact2.diff, Some(1));

        // Third fact: Second EmitMermaidLineActivate with attributes
        let fact3 = drain.next().unwrap().unwrap();
        assert_eq!(fact3.relation_name, "EmitMermaidLineActivate");
        assert_eq!(fact3.attributes.len(), 3);
        assert_eq!(
            fact3.get_attribute_string("val"),
            Some("activate Counter".to_string())
        );
        assert_eq!(
            fact3.get_attribute_string("ce_id_path"),
            Some("0.47.49.75.82.91.92.93.94".to_string())
        );
        assert_eq!(
            fact3.get_attribute_string("callee_contract_path"),
            Some("0.6".to_string())
        );
        assert_eq!(fact3.diff, Some(1));

        // Fourth fact: EmitMermaidLineCalleeParticipantLine with no attributes
        let fact4 = drain.next().unwrap().unwrap();
        assert_eq!(fact4.relation_name, "EmitMermaidLineCalleeParticipantLine");
        assert_eq!(fact4.attributes.len(), 0);
        assert_eq!(fact4.diff, None);

        // Fifth fact: EmitMermaidLineCalleeParticipantLine with attributes
        let fact5 = drain.next().unwrap().unwrap();
        assert_eq!(fact5.relation_name, "EmitMermaidLineCalleeParticipantLine");
        assert_eq!(fact5.attributes.len(), 2);
        assert_eq!(
            fact5.get_attribute_string("val"),
            Some("participant Counter".to_string())
        );
        assert_eq!(
            fact5.get_attribute_string("callee_contract_path"),
            Some("0.6".to_string())
        );
        assert_eq!(fact5.diff, Some(1));

        // Sixth fact: EmitMermaidLineCallerParticipantLine with no attributes
        let fact6 = drain.next().unwrap().unwrap();
        assert_eq!(fact6.relation_name, "EmitMermaidLineCallerParticipantLine");
        assert_eq!(fact6.attributes.len(), 0);
        assert_eq!(fact6.diff, None);

        // Seventh fact: EmitMermaidLineCallerParticipantLine with attributes
        let fact7 = drain.next().unwrap().unwrap();
        assert_eq!(fact7.relation_name, "EmitMermaidLineCallerParticipantLine");
        assert_eq!(fact7.attributes.len(), 2);
        assert_eq!(
            fact7.get_attribute_string("val"),
            Some("participant CounterCaller".to_string())
        );
        assert_eq!(
            fact7.get_attribute_string("caller_contract_path"),
            Some("0.47".to_string())
        );
        assert_eq!(fact7.diff, Some(1));

        // Eighth fact: EmitMermaidLineSignalLine with no attributes
        let fact8 = drain.next().unwrap().unwrap();
        assert_eq!(fact8.relation_name, "EmitMermaidLineSignalLine");
        assert_eq!(fact8.attributes.len(), 0);
        assert_eq!(fact8.diff, None);

        // Ninth fact: First EmitMermaidLineSignalLine with attributes (getCount)
        let fact9 = drain.next().unwrap().unwrap();
        assert_eq!(fact9.relation_name, "EmitMermaidLineSignalLine");
        assert_eq!(fact9.attributes.len(), 5);
        assert_eq!(
            fact9.get_attribute_string("val"),
            Some("CounterCaller->>Counter: getCount".to_string())
        );
        assert_eq!(
            fact9.get_attribute_string("ce_id_path"),
            Some("0.47.49.75.82.91.92.93.94".to_string())
        );
        assert_eq!(
            fact9.get_attribute_string("caller_contract_path"),
            Some("0.47".to_string())
        );
        assert_eq!(
            fact9.get_attribute_string("callee_contract_path"),
            Some("0.6".to_string())
        );
        assert_eq!(
            fact9.get_attribute_string("callee_func_path"),
            Some("0.6.8.34".to_string())
        );
        assert_eq!(fact9.diff, Some(1));

        // Tenth fact: Second EmitMermaidLineSignalLine with attributes (increment)
        let fact10 = drain.next().unwrap().unwrap();
        assert_eq!(fact10.relation_name, "EmitMermaidLineSignalLine");
        assert_eq!(fact10.attributes.len(), 5);
        assert_eq!(
            fact10.get_attribute_string("val"),
            Some("CounterCaller->>Counter: increment".to_string())
        );
        assert_eq!(
            fact10.get_attribute_string("ce_id_path"),
            Some("0.47.49.75.82.83.84.85.86".to_string())
        );
        assert_eq!(
            fact10.get_attribute_string("caller_contract_path"),
            Some("0.47".to_string())
        );
        assert_eq!(
            fact10.get_attribute_string("callee_contract_path"),
            Some("0.6".to_string())
        );
        assert_eq!(
            fact10.get_attribute_string("callee_func_path"),
            Some("0.6.8.14".to_string())
        );
        assert_eq!(fact10.diff, Some(1));

        // Verify we've consumed all facts
        assert!(drain.next().is_none());
    }

    #[test]
    fn test_attribute_value_type_safety() {
        // Test with a line containing different attribute value types
        let line = "TestRelation{.string_attr = \"hello world\", .number_attr = 42, .path_attr = \"0.1.2.3\"}: +1".to_string();
        let mut drain = DdlogDrain::new(vec![line].into_iter());

        let fact = drain.next().unwrap().unwrap();
        assert_eq!(fact.relation_name, "TestRelation");

        // Test string attribute
        let string_attr = fact.attributes.get("string_attr").unwrap();
        assert!(matches!(string_attr, AttributeValue::String(_)));
        assert_eq!(string_attr.as_string(), Some(&"hello world".to_string()));
        assert_eq!(string_attr.as_number(), None);
        assert_eq!(string_attr.as_path(), None);

        // Test number attribute
        let number_attr = fact.attributes.get("number_attr").unwrap();
        assert!(matches!(number_attr, AttributeValue::Number(_)));
        assert_eq!(number_attr.as_string(), None);
        assert_eq!(number_attr.as_number(), Some(42));
        assert_eq!(number_attr.as_path(), None);

        // Test path attribute
        let path_attr = fact.attributes.get("path_attr").unwrap();
        assert!(matches!(path_attr, AttributeValue::Path(_)));
        assert_eq!(path_attr.as_string(), None);
        assert_eq!(path_attr.as_number(), None);
        assert_eq!(path_attr.as_path(), Some(&"0.1.2.3".to_string()));

        // Test to_string_value() for backward compatibility
        assert_eq!(string_attr.to_string_value(), "hello world");
        assert_eq!(number_attr.to_string_value(), "42");
        assert_eq!(path_attr.to_string_value(), "0.1.2.3");
    }
}
