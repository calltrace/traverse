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
            let operator = lval_read(children.next().unwrap())?;
            let mut operands = vec![];

            for operand in children {
                operands.push(lval_read(operand)?);
            }

            Ok(Lval::logical(operator, operands))
        }
        Rule::predicate_operator => {
            Ok(Lval::predicate_operator(parsed.as_str()))
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
            let mut when: Option<Box<Lval>> = None;
            let mut do_q_exprs: Vec<Box<Lval>> = vec![];
            for child in inner {
                let child = lval_read(child.clone())?;

                if let Lval::KeyVal(kv) = *child {
                    if let (Some(key), Some(value)) = (kv.first(), kv.get(1)) {
                        if let Lval::Sym(key_str) = &**key {
                            // Dereference twice: first for Box, second for Lval
                            attributes.insert(key_str.clone(), value.clone());
                        }
                    }
                } else if let Lval::WhenForm(condition,_) = *child {
                    when = Some(condition);
                                    
                } else if let Lval::Emit(_, _, _, _) = *child {
                    attributes.insert("nested_emit".to_string(), child);
                } else if let Lval::DoForm(children) = *child {
                    do_q_exprs = children;
                }
            }

            Ok(Box::new(Lval::Emit(
                node_type,
                attributes,
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
        Rule::capture => Ok(Lval::capture(parsed.as_str())),
        Rule::capture_form => {
            let mut inner = parsed.into_inner();
            let node_type = inner.next().unwrap().as_str().to_string();

            let mut attributes = HashMap::new();
            let mut nested_captures: Vec<Box<Lval>> = vec![];
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
                } else if let Lval::CaptureForm(_, _, _, _) = *lval_child {
                    nested_captures.push(lval_child);
                } else if let Lval::DoForm(children) = *lval_read(child)? {
                    q_exprs = children;
                }
            }
            Ok(Box::new(Lval::CaptureForm(
                node_type,
                attributes,
                nested_captures.first().cloned(),
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
    fn test_parse_simple_emit() {
        let input = r#"(emit output_node_type
                         (key1 @some_variable)
                       )
                       (capture input_node_type (key2 some_variable))
        "#;

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();
        let mut emit_attributes = HashMap::new();
        emit_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@some_variable".to_string())),
        );

        let mut capture_form_attributes = HashMap::new();
        capture_form_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Sym("some_variable".to_string())),
        );

        let expected = Box::new(Lval::Query(vec![
            Box::new(Lval::Emit("output_node_type".to_string(), emit_attributes, None)),
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_form_attributes,
                None,
                None,
            )),
        ]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
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

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();

        // Define emit attributes
        let mut emit_attributes = HashMap::new();
        emit_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@some_variable".to_string())),
        );

        // Define nested attributes for key3
        let mut nested_attributes = HashMap::new();
        nested_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::KeyVal(vec![
                Box::new(Lval::Sym("key3".to_string())),
                Box::new(Lval::Capture("@some_other_variable".to_string())),
            ])),
        );

        // Expected query instantiation
        let expected = Box::new(Lval::Query(vec![
            Box::new(Lval::Emit("output_node_type".to_string(), emit_attributes, None)),
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                nested_attributes,
                None,
                None,
            )),
        ]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
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

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();

        // Define nested attributes for key2 in the emit block
        let mut nested_emit_attributes = HashMap::new();
        nested_emit_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::KeyVal(vec![
                Box::new(Lval::Sym("key2".to_string())),
                Box::new(Lval::Capture("@some_nested_variable".to_string())),
            ])),
        );

        // Define capture attributes
        let mut capture_attributes = HashMap::new();
        capture_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@some_capture_variable".to_string())),
        );

        // Expected query instantiation
        let expected = Box::new(Lval::Query(vec![
            Box::new(Lval::Emit(
                "output_node_type".to_string(),
                nested_emit_attributes,
                None,
            )),
            Box::new(Lval::CaptureForm(
                "input_node_type".to_string(),
                capture_attributes,
                None,
                None,
            )),
        ]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
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

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();

        let mut emit_attributes = HashMap::new();
        emit_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@first_capture_variable".to_string())),
        );
        emit_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@second_capture_variable".to_string())),
        );

        let mut capture_1_attributes = HashMap::new();
        capture_1_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@first_capture_variable".to_string())),
        );

        let mut capture_2_attributes = HashMap::new();
        capture_2_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@second_capture_variable".to_string())),
        );

        let expected = Box::new(Lval::Query(vec![
            Box::new(Lval::Emit("output_node_type".to_string(), emit_attributes, None)),
            Box::new(Lval::CaptureForm(
                "input_node_type_1".to_string(),
                capture_1_attributes,
                None,
                None,
            )),
            Box::new(Lval::CaptureForm(
                "input_node_type_2".to_string(),
                capture_2_attributes,
                None,
                None,
            )),
        ]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
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

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();

        // Attributes for the inner emit
        let mut inner_emit_attributes = HashMap::new();
        inner_emit_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@variable2".to_string())),
        );

        let inner_emit = Box::new(Lval::Emit(
            "output_node_type_2".to_string(),
            inner_emit_attributes,
            None,
        ));

        // Attributes for the outer emit
        let mut outer_emit_attributes = HashMap::new();
        outer_emit_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@variable1".to_string())),
        );
        outer_emit_attributes.insert("nested_emit".to_string(), inner_emit);

        let outer_emit = Box::new(Lval::Emit(
            "output_node_type_1".to_string(),
            outer_emit_attributes,
            None
        ));

        // Attributes for the capture block
        let mut capture_attributes = HashMap::new();
        capture_attributes.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@variable3".to_string())),
        );
        capture_attributes.insert(
            "key4".to_string(),
            Box::new(Lval::Capture("@variable1".to_string())),
        );

        let capture_block = Box::new(Lval::CaptureForm(
            "input_node_type".to_string(),
            capture_attributes,
            None,
            None,
        ));

        let expected = Box::new(Lval::Query(vec![outer_emit, capture_block]));

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
            None,
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
            )"#;

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();

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
            None,
        ))]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
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

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();

        // Define emit attributes
        let mut emit_attributes = HashMap::new();
        emit_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@variable1".to_string())),
        );

        let emit = Box::new(Lval::Emit("output_node_type".to_string(), emit_attributes, None));

        let mut capture_attributes = HashMap::new();
        capture_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@variable2".to_string())),
        );

        // Define the do form with two code blocks
        let do_form = Box::new(Lval::DoForm(vec![Box::new(Lval::Qexpr(vec![Box::new(
            Lval::Sym("execute_task1".to_string()),
        )]))]));

        // Define the capture form with attributes and the do form
        let capture_form = Box::new(Lval::CaptureForm(
            "input_node_type".to_string(),
            capture_attributes,
            None,
            Some(Box::new(Lval::DoForm(vec![Box::new(Lval::Qexpr(vec![
                Box::new(Lval::Sym("execute_task1".to_string())),
            ]))]))),
        ));

        // Expected Lval structure
        let expected = Box::new(Lval::Query(vec![emit, capture_form]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
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

        // Parse the input using the defined Pest grammar.
        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        // Unwrap the parsed result.
        let parsed = parse_result.unwrap();

        let mut emit1_attributes = HashMap::new();
        emit1_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@var1".to_string())),
        );

        let emit1 = Box::new(Lval::Emit("output_node_type".to_string(), emit1_attributes, None));

        let mut capture_attributes = HashMap::new();
        capture_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@var2".to_string())),
        );

        let capture_form = Box::new(Lval::CaptureForm(
            "input_node_type".to_string(),
            capture_attributes,
            None,
            None, // No `do_form` associated
        ));

        let var1_capture = Box::new(Lval::Capture("@var1".to_string()));
        let var2_capture = Box::new(Lval::Capture("@var2".to_string()));
        let logical_condition = Box::new(Lval::Logical(
            "and".to_string(),
            vec![var1_capture, var2_capture],
        ));

        let mut emit2_attributes = HashMap::new();
        emit2_attributes.insert(
            "key4".to_string(),
            Box::new(Lval::Capture("@var1".to_string())),
        );

        let emit2 = Box::new(Lval::Emit(
            "conditional_output1".to_string(),
            emit2_attributes,
            None,
        ));

        let mut emit3_attributes = HashMap::new();
        emit3_attributes.insert(
            "key5".to_string(),
            Box::new(Lval::Capture("@var2".to_string())),
        );

        let emit3 = Box::new(Lval::Emit(
            "conditional_output2".to_string(),
            emit3_attributes,
            None,
        ));

        // 10. Create the `when_form` Lval with the logical condition and conditional emits.
        let when_form = Box::new(Lval::WhenForm(logical_condition, vec![emit2, emit3]));

        // 11. Construct the expected top-level `Query` Lval containing the `emit1`, `capture_form`, and `when_form`.
        let expected = Box::new(Lval::Query(vec![emit1, capture_form, when_form]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
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

        // Parse the input using the defined Pest grammar.
        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Failed to parse input: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();

        let mut emit1_attributes = HashMap::new();
        emit1_attributes.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@var1".to_string())),
        );

        let emit1 = Box::new(Lval::Emit("output_node_type".to_string(), emit1_attributes, None));

        // 3. Define attributes for the `capture_form`.
        let mut capture_attributes = HashMap::new();
        capture_attributes.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@var2".to_string())),
        );

        // 4. Create the `capture_form` Lval.
        let capture_form = Box::new(Lval::CaptureForm(
            "input_node_type".to_string(),
            capture_attributes,
            None,
            None, // No `do_form` associated
        ));

        let var1_capture = Box::new(Lval::Capture("@var1".to_string()));
        let var2_capture = Box::new(Lval::Capture("@var2".to_string()));
        let not_var2 = Box::new(Lval::Logical("not".to_string(), vec![var2_capture]));
        let logical_condition = Box::new(Lval::Logical(
            "and".to_string(),
            vec![var1_capture, not_var2],
        ));

        let mut emit2_attributes = HashMap::new();
        emit2_attributes.insert(
            "key4".to_string(),
            Box::new(Lval::Capture("@var1".to_string())),
        );

        let emit2 = Box::new(Lval::Emit(
            "conditional_output1".to_string(),
            emit2_attributes,
            None,
        ));

        let mut emit3_attributes = HashMap::new();
        emit3_attributes.insert(
            "key5".to_string(),
            Box::new(Lval::Capture("@var2".to_string())),
        );

        // 9. Create the second conditional `emit` Lval.
        let emit3 = Box::new(Lval::Emit(
            "conditional_output2".to_string(),
            emit3_attributes,
            None,
        ));

        let when_form = Box::new(Lval::WhenForm(logical_condition, vec![emit2, emit3]));

        let expected = Box::new(Lval::Query(vec![emit1, capture_form, when_form]));

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        assert_eq!(parsed_lval, expected);
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

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Grammar::parse failed unexpectedly: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();

        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        let mut root_emit_attrs = HashMap::new();
        root_emit_attrs.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@var1".to_string())),
        );
        let emit_root = Lval::Emit("root_node".to_string(), root_emit_attrs, None);

        let mut grandchild_attrs = HashMap::new();
        grandchild_attrs.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@var3".to_string())),
        );
        let grandchild_capture =
            Lval::CaptureForm("grandchild_node".to_string(), grandchild_attrs, None, None);

        // 4c) Create attributes for the child_node
        let mut child_attrs = HashMap::new();
        child_attrs.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@var2".to_string())),
        );
        let child_capture = Lval::CaptureForm(
            "child_node".to_string(),
            child_attrs,
            Some(Box::new(grandchild_capture)),
            None,
        );

        let expected_lval = Box::new(Lval::Query(vec![
            Box::new(emit_root),
            Box::new(child_capture),
        ]));

        assert_eq!(
            parsed_lval, expected_lval,
            "Parsed Lval does not match the expected nested capture structure"
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

        let parse_result = Grammar::parse(Rule::program, input);
        assert!(
            parse_result.is_ok(),
            "Grammar::parse failed unexpectedly: {:?}",
            parse_result
        );

        let parsed = parse_result.unwrap();
        let parsed_lval =
            lval_read(parsed.into_iter().next().unwrap()).expect("Failed to build Lval");

        let mut root_emit_attrs = HashMap::new();
        root_emit_attrs.insert(
            "key1".to_string(),
            Box::new(Lval::Capture("@var1".to_string())),
        );
        let emit_root = Lval::Emit("root_node".to_string(), root_emit_attrs, None);

        let mut grandchild_attrs = HashMap::new();
        grandchild_attrs.insert(
            "key3".to_string(),
            Box::new(Lval::Capture("@var3".to_string())),
        );

        let sexpr_in_qexpr = Lval::Sexpr(vec![
            Box::new(Lval::Sym("func1".to_string())),
            Box::new(Lval::Capture("@var3".to_string())),
        ]);
        let qexpr_inner = Lval::Qexpr(vec![Box::new(sexpr_in_qexpr)]);
        let do_form_for_grandchild = Lval::DoForm(vec![Box::new(qexpr_inner)]);

        let grandchild_capture = Lval::CaptureForm(
            "grandchild_node".to_string(),
            grandchild_attrs,
            None,
            Some(Box::new(do_form_for_grandchild)),
        );

        let mut child_attrs = HashMap::new();
        child_attrs.insert(
            "key2".to_string(),
            Box::new(Lval::Capture("@var2".to_string())),
        );
        let child_capture = Lval::CaptureForm(
            "child_node".to_string(),
            child_attrs,
            Some(Box::new(grandchild_capture)),
            None,
        );

        let expected_lval = Box::new(Lval::Query(vec![
            Box::new(emit_root),
            Box::new(child_capture),
        ]));

        assert_eq!(
            parsed_lval, expected_lval,
            "Parsed Lval does not match expected nested capture with do block on deepest level"
        );
    }
}
