use std::collections::HashMap;

use crate::{ast::Lval, error::Result};
use log::debug;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;

use crate::error::BlisprResult;

#[derive(Parser)]
#[grammar = "./grammar.pest"]
pub struct Grammar;

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

pub(crate) fn lval_read(parsed: Pair<Rule>) -> BlisprResult {
    match parsed.as_rule() {
        Rule::program => {
            let mut ret = Lval::query();
            read_to_lval(&mut ret, parsed)?;
            Ok(ret)
        }
        Rule::logical => {
            let mut children = parsed.into_inner();
            let operator = children.next().unwrap().as_str().to_string(); // "and", "or", "not"
            let mut operands = vec![];

            for operand in children {
                operands.push(lval_read(operand)?);
            }

            Ok(Lval::logical(operator.as_str(), operands))
        }
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

            let mut attributes = HashMap::new();
            for child in inner {
                if let Lval::KeyVal(kv) = *lval_read(child)? {
                    if let (Some(key), Some(value)) = (kv.first(), kv.get(1)) {
                        if let Lval::Sym(key_str) = &**key {
                            // Dereference twice: first for Box, second for Lval
                            attributes.insert(key_str.clone(), value.clone());
                        }
                    }
                }
            }
            Ok(Box::new(Lval::Emit(node_type, attributes)))
        }
        Rule::capture => Ok(Lval::capture(parsed.as_str())),
        Rule::capture_form => { 
            println!("Capture form: {:?}", parsed.as_str());
            Ok(Lval::capture(parsed.as_str()))
        },
        Rule::key_value => {
            let mut ret = Lval::keyvalue();
            read_to_lval(&mut ret, parsed)?;
            Ok(ret)
        }
        e => {
            println!("Unmatched rule: {:?}", e);
            unreachable!()
        }
    }
}

pub fn to_ast(s: &str) -> BlisprResult {
    let parsed = Grammar::parse(Rule::program, s)?.next().unwrap();
    debug!("{}", parsed);
    lval_read(parsed)
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::*;

    #[test]
    fn test_parser() {
        let input = "(+ 1 (- 2 3))";
        let input2 = "(typed_parameter
  (identifier)@name
  type: (type (identifier)@val))";

        let mut parsed = Grammar::parse(Rule::program, input2);
        println!("Parse Tree: {:?}", parsed);

        let mut parsed = to_ast(input2).unwrap();
        println!("Parsed AST: {:?}", parsed);
    }

    #[test]
    fn test_parse_simple_emit() {
        let input = r#"
            (emit output_node_type
              (key1 @some_variable)
            )
        "#;

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();
        println!("Parsed Tree: {:#?}", parsed);

        let mut attributes = HashMap::new();
        attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@some_variable".to_string())),
        );

        let expected = Box::new(Lval::Query(vec![Box::new(Lval::Emit(
            "output_node_type".to_string(),
            attributes,
        ))]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
    }

    #[test]
    fn test_parse_emit_with_two_keys() {
        let input = r#"
            (emit output_node_type
              (key1 @some_variable)
              (key2 @some_other_variable)
            )
        "#;

        // Parse the input
        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();
        println!("Parsed Tree: {:#?}", parsed);

        // Build the expected Lval for comparison
        let mut attributes = HashMap::new();
        attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@some_variable".to_string())),
        );
        attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@some_other_variable".to_string())),
        );

        let expected = Box::new(Lval::Query(vec![Box::new(Lval::Emit(
            "output_node_type".to_string(),
            attributes,
        ))]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);

    }

    #[test]
    fn test_parse_emit_with_constant_value() {
        let input = r#"
            (emit output_node_type
              (key1 @some_variable)
              (key2 some_constant_value)
            )
        "#;

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();
        println!("Parsed Tree: {:#?}", parsed);

        let mut attributes = HashMap::new();
        attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@some_variable".to_string())),
        );
        attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Sym("some_constant_value".to_string())),
        );

        let expected = Box::new(Lval::Query(vec![Box::new(Lval::Emit(
            "output_node_type".to_string(),
            attributes,
        ))]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);

    }

    #[test]
    fn test_emit_with_capture_block() {
        let input = r#"
            (emit output_node_type
              (key1 @some_variable)
              (key2 some_constant_value)
              (capture 
                (key3 some_nested_value)
              )
            )
        "#;

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();
        println!("Parsed Tree: {:#?}", parsed);

        let mut capture_attributes = HashMap::new();
        capture_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Sym("some_nested_value".to_string())),
        );
        capture_attributes.insert(
            "key4".to_string(),
            Box::new(Lval::Capture("@nested_capture".to_string())),
        );

        let capture_block = Box::new(Lval::Sexpr(vec![
            Box::new(Lval::Sym("level1_input_node_type".to_string())),
            Box::new(Lval::Emit(
                "level1_input_node_type".to_string(),
                capture_attributes,
            )),
        ]));

        let mut emit_attributes = HashMap::new();
        emit_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@some_variable".to_string())),
        );
        emit_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Sym("some_constant_value".to_string())),
        );
        emit_attributes.insert("capture".to_string(), capture_block);

        let expected = Box::new(Lval::Query(vec![Box::new(Lval::Emit(
            "output_node_type".to_string(),
            emit_attributes,
        ))]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);

    }
}
