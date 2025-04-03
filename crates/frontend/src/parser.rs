use crate::dsl::{Lval, ProvenanceType};
use crate::syntax::SyntaxHighlighter;
use indexmap::IndexMap;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use std::{
    cmp::Ord,
    fmt::{self, Debug},
    hash::Hash,
    marker::Copy,
    string::ToString,
};

#[derive(Parser)]
#[grammar = "./dsl.pest"]
pub struct Dsl;

fn is_bracket_or_eoi(parsed: &Pair<Rule>) -> bool {
    if parsed.as_rule() == Rule::EOI {
        return true;
    }
    let c = parsed.as_str();
    c == "(" || c == ")" || c == "{" || c == "}"
}

fn read_to_lval(v: &mut Lval, parsed: Pair<Rule>) -> Result<()> {
    for child in parsed.into_inner() {
        if is_bracket_or_eoi(&child) {
            continue;
        }
        Lval::add(v, &*lval_read(child)?)?;
    }
    Ok(())
}

fn lval_read(parsed: Pair<Rule>) -> DslResult {
    match parsed.as_rule() {
        Rule::rulebook => {
            let mut ret = Lval::query();
            read_to_lval(&mut ret, parsed)?;
            Ok(ret)
        }
        Rule::rules_block => {
            let inner = parsed.into_inner();
            let mut rules = vec![];

            for rule in inner {
                if !is_bracket_or_eoi(&rule) {
                    rules.push(lval_read(rule)?);
                }
            }

            Ok(Lval::rules_block(rules))
        }
        Rule::inference => {
            let mut inner = parsed.into_inner();
            let relation = inner.next().unwrap().as_str();
            let mut paths = vec![];
            let mut params: Vec<String> = vec![];
            //
            // Parse parameters
            let parameters = inner.next();
            if parameters.is_some() {
                let parameter_list = parameters.unwrap().into_inner().next().unwrap();
                let params_pair = parameter_list.into_inner();

                params = params_pair
                    .filter(|p| p.as_rule() == Rule::variable)
                    .map(|p| p.as_str().to_string())
                    .collect();

                // Parse inference paths
                for path in inner {
                    if path.as_rule() == Rule::inference_paths {
                        paths.push(lval_read(path)?);
                    }
                }
            }

            let params_option = if params.is_empty() {
                None
            } else {
                Some(params)
            };
            let paths_option = if paths.is_empty() { None } else { Some(paths) };

            Ok(Lval::inference(relation, params_option, paths_option))
        }
        Rule::inference_paths => {
            let mut inner = parsed
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner()
                .next()
                .unwrap()
                .into_inner();

            let mut predicates = vec![];
            let mut computation = None;

            while let Some(element) = inner.next() {
                match element.as_rule() {
                    Rule::predicate_expr => {
                        predicates.push(lval_read(element)?);
                    }
                    Rule::computation => {
                        computation = Some(lval_read(element)?);
                    }
                    _ => {}
                }
            }

            Ok(Lval::inference_path(predicates, computation))
        }

        Rule::predicate_expr => {
            let mut inner = parsed.into_inner();
            let first = inner.next().unwrap();

            match first.as_rule() {
                Rule::prefix_predicate => {
                    let mut prefix_inner = first.into_inner();
                    let prefix_op = prefix_inner.next().unwrap().as_str();
                    let predicate = lval_read(prefix_inner.next().unwrap())?;
                    Ok(Lval::prefix_predicate(prefix_op, predicate))
                }
                Rule::predicate => lval_read(first),
                _ => Err(Error::Parse("Invalid predicate expression".to_string())),
            }
        }

        Rule::predicate => {
            let mut inner = parsed.into_inner();
            let identifier = inner.next().unwrap().as_str();
            let arguments: Vec<String> = inner
                .next()
                .unwrap()
                .into_inner()
                .map(|arg| arg.as_str().to_string())
                .collect();

            Ok(Lval::predicate(identifier, arguments))
        }
        Rule::computation => {
            let mut inner = parsed.into_inner();
            let variable = inner.next().unwrap().as_str();
            let qexpr = lval_read(inner.next().unwrap())?;

            Ok(Lval::computation(variable, qexpr))
        }
        Rule::logical => {
            let mut children = parsed.into_inner();
            let operator = lval_read(children.next().unwrap())?;
            let mut operands = vec![];

            for operand in children {
                operands.push(lval_read(operand)?);
            }

            Ok(Lval::logical(operator, operands))
        }
        Rule::predicate_operator => Ok(Lval::predicate_operator(parsed.as_str())),
        Rule::expr => lval_read(parsed.into_inner().next().unwrap()),
        Rule::sexpr => {
            let mut ret = Lval::sexpr();
            read_to_lval(&mut ret, parsed)?;
            Ok(ret)
        }
        Rule::qexpr => {
            let mut ret = Lval::qexpr();
            read_to_lval(&mut ret, parsed)?;
            Ok(ret)
        }
        Rule::num => Ok(Lval::num(parsed.as_str().parse::<i64>()?)),
        Rule::symbol => Ok(Lval::sym(parsed.as_str())),
        Rule::emit => {
            let mut inner = parsed.into_inner();

            let node_type = inner.next().unwrap().as_str().to_string();
            let mut capture_refs = vec![];
            let mut when: Option<Box<Lval>> = None;
            let mut do_q_exprs: Vec<Box<Lval>> = vec![];
            for child in inner {
                let child = lval_read(child.clone())?;
                if let Lval::Capture(_, _) = *child {
                    capture_refs.push(child);
                } else if let Lval::WhenForm(condition, _) = *child {
                    when = Some(condition);
                } else if let Lval::DoForm(children) = *child {
                    do_q_exprs = children;
                }
            }

            Ok(Box::new(Lval::Emit(
                node_type,
                capture_refs,
                when,
                do_q_exprs
                    .first()
                    .cloned()
                    .and(Some(Box::new(Lval::DoForm(do_q_exprs)))),
            )))
        }
        Rule::when_form => {
            let mut inner = parsed.into_inner();
            let condition = lval_read(inner.next().unwrap())?;
            let mut emits = vec![];
            for child in inner {
                emits.push(lval_read(child)?);
            }
            Ok(Box::new(Lval::WhenForm(condition, emits)))
        }
        /*
        Rule::logical => {
            let mut inner = parsed.into_inner();
            let operator = lval_read(inner.next().unwrap())?;
            let mut operands = vec![];
            for child in inner {
                operands.push(lval_read(child)?);
            }
            Ok(Lval::logical(operator, operands))
        }*/
        Rule::capture => {
            let inner = parsed.into_inner();
            let mut provenance = None;
            let mut capture_name = String::new();

            // Parse the capture components
            for part in inner {
                match part.as_rule() {
                    Rule::provenance_spec => {
                        let mut spec_inner = part.into_inner();
                        if let Some(prov_type) = spec_inner.next() {
                            provenance = Some(match prov_type.as_str() {
                                "path" => ProvenanceType::Path,
                                "downstream" => ProvenanceType::Downstream,
                                "dependency" => ProvenanceType::Dependency,
                                "upstream" => ProvenanceType::Upstream,
                                "span" => ProvenanceType::Span,
                                "full" => ProvenanceType::Full,
                                _ => ProvenanceType::Default,
                            });
                        } else {
                            provenance = Some(ProvenanceType::Default);
                        }
                    }
                    _ => {
                        // This will be the capture name
                        capture_name = part.as_str().to_string();
                    }
                }
            }

            // If no provenance was specified, use None
            Ok(Lval::capture(&capture_name, provenance))
        }
        Rule::capture_form => {
            let mut inner = parsed.into_inner();
            let node_type = inner.next().unwrap().as_str().to_string();

            let mut attributes = IndexMap::new();
            let mut capture_refs = vec![];
            let mut nested_captures: Vec<Box<Lval>> = vec![];
            let mut when: Option<Box<Lval>> = None;
            let mut q_exprs: Vec<Box<Lval>> = vec![];
            for child in inner {
                let lval_child = lval_read(child.clone())?;
                if let Lval::KeyVal(kv) = *lval_child {
                    if let (Some(key), Some(value)) = (kv.first(), kv.get(1)) {
                        if let Lval::Sym(key_str) = &**key {
                            // Dereference twice: first for Box, second for Lval
                            attributes.insert(key_str.clone(), value.clone());
                        }
                    }
                } else if let Lval::Capture(_, _) = *lval_child {
                    capture_refs.push(lval_child);
                } else if let Lval::CaptureForm(_, _, _, _, _, _) = *lval_child {
                    nested_captures.push(lval_child);
                } else if let Lval::WhenForm(condition, _) = *lval_child {
                    when = Some(condition);
                } else if let Lval::DoForm(children) = *lval_read(child)? {
                    q_exprs = children;
                }
            }
            Ok(Box::new(Lval::CaptureForm(
                node_type,
                attributes,
                capture_refs,
                nested_captures.first().cloned(),
                when,
                q_exprs
                    .first()
                    .cloned()
                    .and(Some(Box::new(Lval::DoForm(q_exprs)))),
            )))
        }
        Rule::do_form => {
            let children = parsed.into_inner();
            let mut exprs = vec![];

            for expr in children {
                exprs.push(lval_read(expr)?);
            }
            Ok(Lval::do_form(exprs))
        }
        Rule::key_value => {
            let mut ret = Lval::keyvalue();
            read_to_lval(&mut ret, parsed)?;
            Ok(ret)
        }
        Rule::string_literal => Ok(Lval::string_literal(parsed.as_str())),
        e => {
            println!("Unmatched rule: {:?}", e);
            unreachable!()
        }
    }
}

pub fn parse(s: &str) -> DslResult {
    let parsed = Dsl::parse(Rule::rulebook, s)?.next().unwrap();
    lval_read(parsed)
}

#[derive(Debug)]
pub enum Error {
    DivideByZero,
    EmptyList,
    FunctionFormat,
    NoChildren,
    NotANumber,
    NumArguments(usize, usize),
    Parse(String),
    Readline(String),
    WrongType(String, String),
    UnknownFunction(String),
}

pub type Result<T> = std::result::Result<T, Error>;
pub type DslResult = Result<Box<Lval>>;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Error::{
            DivideByZero, EmptyList, FunctionFormat, NoChildren, NotANumber, NumArguments, Parse,
            Readline, UnknownFunction, WrongType,
        };
        match self {
            DivideByZero => write!(f, "Divide by zero"),
            EmptyList => write!(f, "Empty list"),
            FunctionFormat => write!(
                f,
                "Function format invalid.  Symbol '&' not followed by a single symbol"
            ),
            NoChildren => write!(f, "Lval has no children"),
            NotANumber => write!(f, "NaN"),
            NumArguments(expected, received) => write!(
                f,
                "Wrong number of arguments: expected {expected}, received {received}"
            ),
            Parse(s) => write!(f, "Parse error: {s}"),
            Readline(s) => write!(f, "Readline error: {s}"),
            WrongType(expected, received) => {
                write!(f, "Wrong type: expected {expected}, received {received}")
            }
            UnknownFunction(func_name) => write!(f, "Unknown function {func_name}"),
        }
    }
}

impl<T> From<pest::error::Error<T>> for Error
where
    T: Debug + Ord + Copy + Hash,
{
    fn from(error: pest::error::Error<T>) -> Self {
        Error::Parse(format!("{error}"))
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(_error: std::num::ParseIntError) -> Self {
        Error::NotANumber
    }
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::Parse(error.to_string())
    }
}

impl From<rustyline::error::ReadlineError> for Error {
    fn from(error: rustyline::error::ReadlineError) -> Self {
        Error::Readline(error.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_parse_simple_emit_2() {
        // Given: A simple DSL input with emit and capture
        let input = r#"
            (emit output_type 
                (key1 @var1)
            )
            (capture input_type
                (key2 value2)
            )
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create capture reference for emit
        let capture_var1 = Box::new(Lval::Capture("@var1".to_string(), None));
        let emit_capture_refs = vec![capture_var1];

        // Create attributes for capture form
        let mut capture_attributes = IndexMap::new();
        capture_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Sym("value2".to_string())),
        );

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node
            Box::new(Lval::Emit(
                "output_type".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // Capture node
            Box::new(Lval::CaptureForm(
                "input_type".to_string(),
                capture_attributes,
                vec![],
                None,
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    
    #[test]
    fn test_dependency_provenance() {
        let highlighter = SyntaxHighlighter::default();
        let input = "(emit TestNode (key1 @:dependency:var1))";
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");
        
        // Verify the capture has the correct provenance type
        if let Lval::Rulebook(items) = &*result {
            if let Lval::Emit(_, captures, _, _) = &*items[0] {
                if let Lval::KeyVal(kv) = &*captures[0] {
                    if let (Some(key), Some(value)) = (kv.first(), kv.get(1)) {
                        if let (Lval::Sym(key_str), Lval::Capture(capture_name, provenance)) = (&**key, &**value) {
                            assert_eq!(key_str, "key1");
                            assert_eq!(capture_name, "@var1");
                            assert!(matches!(provenance, Some(ProvenanceType::Dependency)));
                            return;
                        }
                    }
                }
            }
        }
        panic!("Expected capture with dependency provenance not found");
    }
}

    #[test]
    fn test_parse_simple_emit() {
        let input = r#"(emit output_node_type
                         (key1 @some_variable)
                       )
                       (capture input_node_type (key2 some_variable))
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create capture reference for emit
        let capture_var = Box::new(Lval::Capture("@some_variable".to_string(), None));
        let emit_capture_refs = vec![capture_var];

        // Create attributes for capture form
        let mut capture_attributes = IndexMap::new();
        capture_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Sym("some_variable".to_string())),
        );

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // Capture node
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_attributes,
                vec![],
                None,
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_parse_emit_and_nested_capture() {
        let input = r#"(emit output_node_type
                     (key1 @some_variable)
                   )
                   (capture input_node_type 
                     (key2 (key3 @some_other_variable))
                   )
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create capture reference for emit
        let capture_var = Box::new(Lval::Capture("@some_variable".to_string(), None));
        let emit_capture_refs = vec![capture_var];

        // Create nested key-value for capture form
        let mut nested_kv = Lval::keyvalue();
        Lval::add(&mut nested_kv, &Lval::Sym("key3".to_string())).unwrap();
        Lval::add(
            &mut nested_kv,
            &Lval::Capture("@some_other_variable".to_string(), None),
        )
        .unwrap();

        // Create attributes for capture form
        let mut capture_attributes = IndexMap::new();
        capture_attributes.insert("key2".to_string(), nested_kv);

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // Capture node with nested key-value
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_attributes,
                vec![],
                None,
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_parse_nested_emit_and_capture() {
        let input = r#"(emit output_node_type
                     (key1 (key2 @some_nested_variable))
                   )
                   (capture input_node_type 
                     (key3 @some_capture_variable)
                   )
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create nested key-value for emit
        let mut nested_kv = Lval::keyvalue();
        Lval::add(&mut nested_kv, &Lval::Sym("key2".to_string())).unwrap();
        Lval::add(
            &mut nested_kv,
            &Lval::Capture("@some_nested_variable".to_string(), None),
        )
        .unwrap();
        let emit_capture_refs = vec![nested_kv];

        // Create attributes for capture form
        let mut capture_attributes = IndexMap::new();
        capture_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@some_capture_variable".to_string(), None)),
        );

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node with nested key-value
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // Capture node
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_attributes,
                vec![],
                None,
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_parse_emit_with_two_keys_and_multiple_captures() {
        let input = r#"(emit output_node_type
                     (key1 @first_capture_variable)
                     (key2 @second_capture_variable)
                   )
                   (capture input_node_type_1 
                     (key2 @first_capture_variable)
                   )
                   (capture input_node_type_2 
                     (key3 @second_capture_variable)
                   )
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create capture references for emit
        let capture_var1 = Box::new(Lval::Capture("@first_capture_variable".to_string(), None));
        let capture_var2 = Box::new(Lval::Capture("@second_capture_variable".to_string(), None));

        // Create key-value pairs for emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(
            &mut kv1,
            &Lval::Capture("@first_capture_variable".to_string(), None),
        )
        .unwrap();

        let mut kv2 = Lval::keyvalue();
        Lval::add(&mut kv2, &Lval::Sym("key2".to_string())).unwrap();
        Lval::add(
            &mut kv2,
            &Lval::Capture("@second_capture_variable".to_string(), None),
        )
        .unwrap();

        let emit_capture_refs = vec![kv1, kv2];

        // Create attributes for first capture form
        let mut capture1_attributes = IndexMap::new();
        capture1_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@first_capture_variable".to_string(), None)),
        );

        // Create attributes for second capture form
        let mut capture2_attributes = IndexMap::new();
        capture2_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@second_capture_variable".to_string(), None)),
        );

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node with two key-value pairs
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // First capture node
            Box::new(Lval::CaptureForm(
                "input_node_type_1".to_string(),
                capture1_attributes,
                vec![],
                None,
                None,
                None,
            )),
            // Second capture node
            Box::new(Lval::CaptureForm(
                "input_node_type_2".to_string(),
                capture2_attributes,
                vec![],
                None,
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_parse_nested_emit_with_capture_and_capture_block() {
        let input = r#"(emit output_node_type_1
                     (key1 @variable1)
                     (emit output_node_type_2
                        (key2 @variable2)
                     )
                   )
                   (capture input_node_type
                     (key3 @variable3)
                     (key4 @variable1)
                   )
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create key-value pair for outer emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(&mut kv1, &Lval::Capture("@variable1".to_string(), None)).unwrap();

        // Create nested emit
        let nested_capture_var = Box::new(Lval::Capture("@variable2".to_string(), None));
        let mut nested_kv = Lval::keyvalue();
        Lval::add(&mut nested_kv, &Lval::Sym("key2".to_string())).unwrap();
        Lval::add(
            &mut nested_kv,
            &Lval::Capture("@variable2".to_string(), None),
        )
        .unwrap();

        let nested_emit = Box::new(Lval::Emit(
            "output_node_type_2".to_string(),
            vec![nested_kv],
            None,
            None,
        ));

        // Combine for outer emit
        let emit_capture_refs = vec![kv1, nested_emit];

        // Create attributes for capture form
        let mut capture_attributes = IndexMap::new();
        capture_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@variable3".to_string(), None)),
        );
        capture_attributes.insert(
            "key4".to_string(),
            Box::new(Lval::Capture("@variable1".to_string(), None)),
        );

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Outer emit node with nested emit
            Box::new(Lval::Emit(
                "output_node_type_1".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // Capture node
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_attributes,
                vec![],
                None,
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_parse_emit_with_two_keys() {
        let input = r#"
            (emit output_node_type
              (key1 @some_variable)
              (key2 @some_other_variable)
            )
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create key-value pairs for emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(
            &mut kv1,
            &Lval::Capture("@some_variable".to_string(), None),
        )
        .unwrap();

        let mut kv2 = Lval::keyvalue();
        Lval::add(&mut kv2, &Lval::Sym("key2".to_string())).unwrap();
        Lval::add(
            &mut kv2,
            &Lval::Capture("@some_other_variable".to_string(), None),
        )
        .unwrap();

        let emit_capture_refs = vec![kv1, kv2];

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node with two key-value pairs
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_parse_emit_with_constant_value() {
        let input = r#"
            (emit output_node_type
              (key1 @some_variable)
              (key2 some_constant_value)
            )"#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create key-value pairs for emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(
            &mut kv1,
            &Lval::Capture("@some_variable".to_string(), None),
        )
        .unwrap();

        let mut kv2 = Lval::keyvalue();
        Lval::add(&mut kv2, &Lval::Sym("key2".to_string())).unwrap();
        Lval::add(&mut kv2, &Lval::Sym("some_constant_value".to_string())).unwrap();

        let emit_capture_refs = vec![kv1, kv2];

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node with variable and constant value
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }
    #[test]
    fn test_parse_do_form_within_capture() {
        let input = r#"
        (emit output_node_type
            (key1 @variable1)
        )
        (capture input_node_type
            (key2 @variable2)
            (do 
                { execute_task1 }
            )
        )
    "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create key-value pair for emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(&mut kv1, &Lval::Capture("@variable1".to_string(), None)).unwrap();

        let emit_capture_refs = vec![kv1];

        // Create do form for capture
        let mut qexpr = Lval::qexpr();
        Lval::add(&mut qexpr, &Lval::Sym("execute_task1".to_string())).unwrap();
        let do_form = Lval::do_form(vec![qexpr]);

        // Create attributes for capture form
        let mut capture_attributes = IndexMap::new();
        capture_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@variable2".to_string(), None)),
        );

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // Capture node with do form
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_attributes,
                vec![],
                None,
                None,
                Some(do_form),
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_parse_when_form_with_two_variables_and_capturing() {
        let input = r#"
        /* Emit the first output node with a captured variable @var1 */
        (emit output_node_type
            (key1 @var1)
        )
        
        /* Capture a variable @var2 */
        (capture input_node_type
            (key2 @var2)
        )
        
        /* When form with a logical condition involving @var1 and @var2 */
        (when (and @var1 @var2)
            (emit conditional_output1
                (key4 @var1)
            )
            (emit conditional_output2
                (key5 @var2)
            )
        )
    "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create key-value pair for first emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(&mut kv1, &Lval::Capture("@var1".to_string(), None)).unwrap();

        let emit1_capture_refs = vec![kv1];

        // Create attributes for capture form
        let mut capture_attributes = IndexMap::new();
        capture_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@var2".to_string(), None)),
        );

        // Create logical condition for when form
        let logical_op = Lval::predicate_operator("and");
        let operand1 = Box::new(Lval::Capture("@var1".to_string(), None));
        let operand2 = Box::new(Lval::Capture("@var2".to_string(), None));
        let condition = Box::new(Lval::Logical(logical_op, vec![operand1, operand2]));

        // Create emits for when form body
        let mut kv_when1 = Lval::keyvalue();
        Lval::add(&mut kv_when1, &Lval::Sym("key4".to_string())).unwrap();
        Lval::add(&mut kv_when1, &Lval::Capture("@var1".to_string(), None)).unwrap();

        let emit_when1 = Box::new(Lval::Emit(
            "conditional_output1".to_string(),
            vec![kv_when1],
            None,
            None,
        ));

        let mut kv_when2 = Lval::keyvalue();
        Lval::add(&mut kv_when2, &Lval::Sym("key5".to_string())).unwrap();
        Lval::add(&mut kv_when2, &Lval::Capture("@var2".to_string(), None)).unwrap();

        let emit_when2 = Box::new(Lval::Emit(
            "conditional_output2".to_string(),
            vec![kv_when2],
            None,
            None,
        ));

        // Create when form
        let when_form = Box::new(Lval::WhenForm(condition, vec![emit_when1, emit_when2]));

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // First emit node
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit1_capture_refs,
                None,
                None,
            )),
            // Capture node
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_attributes,
                vec![],
                None,
                None,
                None,
            )),
            // When form with conditional emits
            when_form,
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_parse_when_form_with_logical_operand_and_capturing() {
        // Define the input DSL with emits first, followed by capture_form, then when_form.
        let input = r#"
        /* Emit the first output node with a captured variable @var1 */
        (emit output_node_type
            (key1 @var1)
        )
        
        /* Capture a variable @var2 */
        (capture input_node_type
            (key2 @var2)
        )
        
        /* When form with a logical condition involving @var1 and @var2 */
        (when (and @var1 (not @var2))
            (emit conditional_output1
                (key4 @var1)
            )
            (emit conditional_output2
                (key5 @var2)
            )
        )
    "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create key-value pair for first emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(&mut kv1, &Lval::Capture("@var1".to_string(), None)).unwrap();

        let emit1_capture_refs = vec![kv1];

        // Create attributes for capture form
        let mut capture_attributes = IndexMap::new();
        capture_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@var2".to_string(), None)),
        );

        // Create logical condition for when form with nested 'not' operator
        let and_op = Lval::predicate_operator("and");
        let not_op = Lval::predicate_operator("not");
        let operand1 = Box::new(Lval::Capture("@var1".to_string(), None));
        let operand2 = Box::new(Lval::Capture("@var2".to_string(), None));
        
        // Create the nested 'not' logical expression
        let not_expr = Box::new(Lval::Logical(not_op, vec![operand2]));
        
        // Create the top-level 'and' logical expression
        let condition = Box::new(Lval::Logical(and_op, vec![operand1, not_expr]));

        // Create emits for when form body
        let mut kv_when1 = Lval::keyvalue();
        Lval::add(&mut kv_when1, &Lval::Sym("key4".to_string())).unwrap();
        Lval::add(&mut kv_when1, &Lval::Capture("@var1".to_string(), None)).unwrap();

        let emit_when1 = Box::new(Lval::Emit(
            "conditional_output1".to_string(),
            vec![kv_when1],
            None,
            None,
        ));

        let mut kv_when2 = Lval::keyvalue();
        Lval::add(&mut kv_when2, &Lval::Sym("key5".to_string())).unwrap();
        Lval::add(&mut kv_when2, &Lval::Capture("@var2".to_string(), None)).unwrap();

        let emit_when2 = Box::new(Lval::Emit(
            "conditional_output2".to_string(),
            vec![kv_when2],
            None,
            None,
        ));

        // Create when form
        let when_form = Box::new(Lval::WhenForm(condition, vec![emit_when1, emit_when2]));

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // First emit node
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                emit1_capture_refs,
                None,
                None,
            )),
            // Capture node
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_attributes,
                vec![],
                None,
                None,
                None,
            )),
            // When form with conditional emits
            when_form,
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_nested_captures() {
        let input = r#"
            (emit root_node
                (key1 @var1)
            )
            (capture child_node
                (key2 @var2)
                (capture grandchild_node
                    (key3 @var3)
                )
            )
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create key-value pair for emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(&mut kv1, &Lval::Capture("@var1".to_string(), None)).unwrap();

        let emit_capture_refs = vec![kv1];

        // Create attributes for child capture form
        let mut child_attributes = IndexMap::new();
        child_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@var2".to_string(), None)),
        );

        // Create attributes for grandchild capture form
        let mut grandchild_attributes = IndexMap::new();
        grandchild_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@var3".to_string(), None)),
        );

        // Create grandchild capture form
        let grandchild_capture = Box::new(Lval::CaptureForm(
            "grandchild_node".to_string(),
            grandchild_attributes,
            vec![],
            None,
            None,
            None,
        ));

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node
            Box::new(Lval::Emit(
                "root_node".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // Child capture node with nested grandchild capture
            Box::new(Lval::CaptureForm(
                "child_node".to_string(),
                child_attributes,
                vec![],
                Some(grandchild_capture),
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }

    #[test]
    fn test_nested_captures_with_do_on_deepest() {
        let input = r#"
            (emit root_node
                (key1 @var1)
            )
            (capture child_node
                (key2 @var2)
                (capture grandchild_node
                    (key3 @var3)
                    (do { (func1 @var3) })
                )
            )
        "#;

        // When: Parsing the input
        let parsed = Dsl::parse(Rule::rulebook, input)
            .expect("Parse failed")
            .next()
            .unwrap();
        let result = lval_read(parsed).expect("Lval construction failed");

        // Then: Verify the structure matches expectations
        // Create key-value pair for emit
        let mut kv1 = Lval::keyvalue();
        Lval::add(&mut kv1, &Lval::Sym("key1".to_string())).unwrap();
        Lval::add(&mut kv1, &Lval::Capture("@var1".to_string(), None)).unwrap();

        let emit_capture_refs = vec![kv1];

        // Create attributes for child capture form
        let mut child_attributes = IndexMap::new();
        child_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@var2".to_string(), None)),
        );

        // Create attributes for grandchild capture form
        let mut grandchild_attributes = IndexMap::new();
        grandchild_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@var3".to_string(), None)),
        );

        // Create do form for grandchild capture
        let mut sexpr = Lval::sexpr();
        Lval::add(&mut sexpr, &Lval::Sym("func1".to_string())).unwrap();
        Lval::add(&mut sexpr, &Lval::Capture("@var3".to_string(), None)).unwrap();
        
        let mut qexpr = Lval::qexpr();
        Lval::add(&mut qexpr, &*sexpr).unwrap();
        
        let do_form = Lval::do_form(vec![qexpr]);

        // Create grandchild capture form with do form
        let grandchild_capture = Box::new(Lval::CaptureForm(
            "grandchild_node".to_string(),
            grandchild_attributes,
            vec![],
            None,
            None,
            Some(do_form),
        ));

        // Create expected Lval structure
        let expected = Box::new(Lval::Rulebook(vec![
            // Emit node
            Box::new(Lval::Emit(
                "root_node".to_string(),
                emit_capture_refs,
                None,
                None,
            )),
            // Child capture node with nested grandchild capture
            Box::new(Lval::CaptureForm(
                "child_node".to_string(),
                child_attributes,
                vec![],
                Some(grandchild_capture),
                None,
                None,
            )),
        ]));

        assert_eq!(
            result, expected,
            "Parsed result does not match expected structure"
        );
    }
}
