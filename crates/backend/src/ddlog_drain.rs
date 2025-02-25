/// ddlog_drain: an iterator-based consume for DDlog output relation lines.
///
/// Example lines:
/// ```text
/// Edge{.from = 92, .to = 93}: +1
/// EmitMermaidLineActivate:
/// EmitMermaidLineActivate{.val = "activate", .x = 42}: -1
/// ```
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
    IResult,
    Parser
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DdlogFact {
    pub relation_name: String,
    pub attributes: HashMap<String, String>,
    pub diff: Option<i64>,
}

#[derive(Debug)]
pub enum DdlogDrainError {
    NomError(String),
}

impl fmt::Display for DdlogDrainError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DdlogDrainError::NomError(e) => write!(f, "DDlog parse error: {}", e),
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
                    return Some(Err(DdlogDrainError::NomError(msg)));
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

fn parse_attribute_block(input: &str) -> IResult<&str, HashMap<String, String>> {
    let mut parser = delimited(
        preceded(multispace0, char('{')),
        map(
            separated_list0(parse_comma, parse_one_attribute),
            |pairs: Vec<(String, String)>| pairs.into_iter().collect(),
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

fn parse_one_attribute(input: &str) -> IResult<&str, (String, String)> {
    let mut parser = (
        preceded(multispace0, char('.')),
        take_while1(|c: char| c.is_alphanumeric() || c == '_'),
        preceded(multispace0, char('=')),
        preceded(multispace0, parse_attribute_value),
    );

    let (rest, (_, attr_name, _, value)) = parser.parse(input)?;
    Ok((rest, (attr_name.to_string(), value)))
}

fn parse_attribute_value(input: &str) -> IResult<&str, String> {
    let mut parser = alt((
        map(
            delimited(char('"'), opt(is_not("\"")), char('"')),
            |maybe_str: Option<&str>| format!("\"{}\"", maybe_str.unwrap_or("")), // preserve the quotes
        ),
        map_res(
            recognize((
                opt(alt((char('+'), char('-')))),
                take_while1(|c: char| c.is_ascii_digit()),
            )),
            |s: &str| -> Result<String, String> { Ok(s.to_string()) },
        ),
        map(
            take_while1(|c: char| ![',', '}', ' '].contains(&c)),
            |s: &str| s.to_string(),
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
        assert_eq!(fact1.attributes["from"], "92");
        assert_eq!(fact1.attributes["to"], "93");

        let fact2 = dd.next().unwrap().unwrap();
        assert_eq!(fact2.relation_name, "EmitMermaidLineActivate");
        assert_eq!(fact2.diff, None); // colon was present, but no number => 0 => Some(0) or None
                                      // In the current code, absent digits become 0. If you prefer `None`, just change parse_diff logic.

        let fact3 = dd.next().unwrap().unwrap();
        assert_eq!(fact3.relation_name, "EmitMermaidLineActivate");
        assert_eq!(fact3.diff, Some(-1));
        assert_eq!(fact3.attributes["val"], "\"activate Counter\"");
        assert_eq!(fact3.attributes["x"], "42");

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
        assert_eq!(fact2.attributes["val"], "\"activate Counter\"");
        assert_eq!(fact2.attributes["ce_id_path"], "\"0.47.49.75.82.83.84.85.86\"");
        assert_eq!(fact2.attributes["callee_contract_path"], "\"0.6\"");
        assert_eq!(fact2.diff, Some(1));
        
        // Third fact: Second EmitMermaidLineActivate with attributes
        let fact3 = drain.next().unwrap().unwrap();
        assert_eq!(fact3.relation_name, "EmitMermaidLineActivate");
        assert_eq!(fact3.attributes.len(), 3);
        assert_eq!(fact3.attributes["val"], "\"activate Counter\"");
        assert_eq!(fact3.attributes["ce_id_path"], "\"0.47.49.75.82.91.92.93.94\"");
        assert_eq!(fact3.attributes["callee_contract_path"], "\"0.6\"");
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
        assert_eq!(fact5.attributes["val"], "\"participant Counter\"");
        assert_eq!(fact5.attributes["callee_contract_path"], "\"0.6\"");
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
        assert_eq!(fact7.attributes["val"], "\"participant CounterCaller\"");
        assert_eq!(fact7.attributes["caller_contract_path"], "\"0.47\"");
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
        assert_eq!(fact9.attributes["val"], "\"CounterCaller->>Counter: getCount\"");
        assert_eq!(fact9.attributes["ce_id_path"], "\"0.47.49.75.82.91.92.93.94\"");
        assert_eq!(fact9.attributes["caller_contract_path"], "\"0.47\"");
        assert_eq!(fact9.attributes["callee_contract_path"], "\"0.6\"");
        assert_eq!(fact9.attributes["callee_func_path"], "\"0.6.8.34\"");
        assert_eq!(fact9.diff, Some(1));
        
        // Tenth fact: Second EmitMermaidLineSignalLine with attributes (increment)
        let fact10 = drain.next().unwrap().unwrap();
        assert_eq!(fact10.relation_name, "EmitMermaidLineSignalLine");
        assert_eq!(fact10.attributes.len(), 5);
        assert_eq!(fact10.attributes["val"], "\"CounterCaller->>Counter: increment\"");
        assert_eq!(fact10.attributes["ce_id_path"], "\"0.47.49.75.82.83.84.85.86\"");
        assert_eq!(fact10.attributes["caller_contract_path"], "\"0.47\"");
        assert_eq!(fact10.attributes["callee_contract_path"], "\"0.6\"");
        assert_eq!(fact10.attributes["callee_func_path"], "\"0.6.8.14\"");
        assert_eq!(fact10.diff, Some(1));
        
        // Verify we've consumed all facts
        assert!(drain.next().is_none());
    }
}
