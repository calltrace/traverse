use crate::{error::Result, ast::Lval};
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

fn lval_read(parsed: Pair<Rule>) -> BlisprResult {
    match parsed.as_rule() {
        Rule::program => {
            let mut ret = Lval::query();
            read_to_lval(&mut ret, parsed)?;
            Ok(ret)
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
        Rule::capture => Ok(Lval::capture(parsed.as_str())),
        Rule::key_value => {
            let mut ret = Lval::keyvalue();
            read_to_lval(&mut ret, parsed)?;
            Ok(ret)
        },
        _ => unreachable!(),

            
    }
}

pub fn to_ast(s: &str) -> BlisprResult {
    let parsed = Grammar::parse(Rule::program, s)?.next().unwrap();
    debug!("{}", parsed);
    lval_read(parsed)
}


#[cfg(test)]
mod tests {
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
}
