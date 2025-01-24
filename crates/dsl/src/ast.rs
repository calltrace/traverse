// Lisp AST types and primitives heavily inspired by https://github.com/deciduously/blispr/blob/master/src/lval.rs
use crate::error::{BlisprResult, Error, Result};
use std::{collections::HashMap, fmt};

type LvalChildren = Vec<Box<Lval>>;
pub type LBuiltin = fn(&mut Lval) -> BlisprResult;

#[derive(Clone)]
pub enum Func {
    Builtin(String, LBuiltin),
    Lambda(HashMap<String, Box<Lval>>, Box<Lval>, Box<Lval>), // (environment(?), formals, body), both should be Qexpr // TODO these should both be Rc<T>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lval {
    Query(LvalChildren),
    Fun(Func),
    Num(i64),
    String(String),
    Sym(String),
    Sexpr(LvalChildren),
    Qexpr(LvalChildren),
    DoForm(LvalChildren),
    Logical(Box<Lval>, LvalChildren),
    PredicateOperator(String),
    Emit(String, HashMap<String, Box<Lval>>, Option<Box<Lval>>, Option<Box<Lval>>),
    Capture(String),
    CaptureForm(
        String,
        HashMap<String, Box<Lval>>,
        Option<Box<Lval>>,
        Option<Box<Lval>>,
    ),
    WhenForm(Box<Lval>, LvalChildren),
    KeyVal(LvalChildren),
}

impl Lval {
    fn lval_expr_print(cell: &[Box<Lval>]) -> String {
        let mut ret = String::new();
        for i in 0..cell.len() {
            ret.push_str(&format!("{}", cell[i]));
            if i < cell.len() - 1 {
                ret.push(' ');
            }
        }
        ret
    }

    pub fn query() -> Box<Lval> {
        Box::new(Lval::Query(Vec::new()))
    }

    pub fn builtin(f: LBuiltin, name: &str) -> Box<Lval> {
        Box::new(Lval::Fun(Func::Builtin(name.to_string(), f)))
    }

    pub fn lambda(
        env: HashMap<String, Box<Lval>>,
        formals: Box<Lval>,
        body: Box<Lval>,
    ) -> Box<Lval> {
        Box::new(Lval::Fun(Func::Lambda(env, formals, body)))
    }

    pub fn num(n: i64) -> Box<Lval> {
        Box::new(Lval::Num(n))
    }

    pub fn sym(s: &str) -> Box<Lval> {
        Box::new(Lval::Sym(s.into()))
    }

    pub fn logical(op: Box<Lval>, operands: Vec<Box<Lval>>) -> Box<Lval> {
        Box::new(Lval::Logical(op, operands))
    }

    pub fn predicate_operator(operator: &str) -> Box<Lval> {
        Box::new(Lval::PredicateOperator(operator.into()))
    }

    pub fn capture(s: &str) -> Box<Lval> {
        Box::new(Lval::Capture(s.into()))
    }

    pub fn sexpr() -> Box<Lval> {
        Box::new(Lval::Sexpr(Vec::new()))
    }

    pub fn qexpr() -> Box<Lval> {
        Box::new(Lval::Qexpr(Vec::new()))
    }

    pub fn emit(
        node_type: &str,
        attributes: HashMap<String, Box<Lval>>,
        when_form: Option<Box<Lval>>,
        do_form: Option<Box<Lval>>,
    ) -> Box<Lval> {
        Box::new(Lval::Emit(node_type.to_string(), attributes, when_form, do_form))
    }

    pub fn capture_form(
        node_type: &str,
        attributes: HashMap<String, Box<Lval>>,
        nested_captures: Option<Box<Lval>>,
        q_expr: Option<Box<Lval>>,
    ) -> Box<Lval> {
        Box::new(Lval::CaptureForm(
            node_type.to_string(),
            attributes,
            nested_captures,
            q_expr,
        ))
    }

    pub fn do_form(exprs: LvalChildren) -> Box<Lval> {
        Box::new(Lval::DoForm(exprs))
    }

    pub fn add_to_emit(emit: &mut Lval, key: &str, value: Box<Lval>) {
        if let Lval::Emit(_, ref mut attributes, _, _) = emit {
            attributes.insert(key.to_string(), value);
        }
    }
    pub fn keyvalue() -> Box<Lval> {
        Box::new(Lval::KeyVal(Vec::new()))
    }

    pub fn string_literal(s: &str) -> Box<Lval> {
        Box::new(Lval::String(s.into()))
    }

    pub fn add(v: &mut Lval, x: &Lval) -> Result<()> {
        match *v {
            Lval::Sexpr(ref mut children)
            | Lval::Qexpr(ref mut children)
            | Lval::Query(ref mut children)
            | Lval::KeyVal(ref mut children) => {
                children.push(Box::new(x.clone()));
            }
            _ => return Err(Error::NoChildren),
        }
        Ok(())
    }

    pub fn pop(v: &mut Lval, i: usize) -> BlisprResult {
        match *v {
            Lval::Sexpr(ref mut children)
            | Lval::Qexpr(ref mut children)
            | Lval::Query(ref mut children)
            | Lval::KeyVal(ref mut children) => {
                let ret = children[i].clone();
                children.remove(i);
                Ok(ret)
            }
            _ => Err(Error::NoChildren),
        }
    }

    pub fn join(x: &mut Lval, mut y: Box<Lval>) -> Result<()> {
        while y.len()? > 0 {
            Lval::add(x, &*Lval::pop(&mut y, 0)?)?;
        }
        Ok(())
    }
    pub fn as_num(&self) -> Result<i64> {
        match *self {
            Lval::Num(n_num) => Ok(n_num),
            _ => Err(Error::NotANumber),
        }
    }
    pub fn as_string(&self) -> Result<String> {
        match self {
            Lval::Sym(s) => Ok(s.to_string()),
            _ => Err(Error::WrongType("symbol".to_string(), format!("{self}"))),
        }
    }
    pub fn len(&self) -> Result<usize> {
        match *self {
            Lval::Sexpr(ref children) | Lval::Qexpr(ref children) | Lval::Query(ref children) => {
                Ok(children.len())
            }
            _ => Err(Error::NoChildren),
        }
    }
}

impl fmt::Debug for Func {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Func::Builtin(name, _) => write!(f, "Builtin({name})"),
            Func::Lambda(env, formals, body) => {
                write!(f, "Lambda({{{env:?}}},{{{formals}}},{{{body}}})")
            }
        }
    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Func) -> bool {
        match self {
            Func::Builtin(name, _) => match other {
                Func::Builtin(other_name, _) => name == other_name,
                Func::Lambda(..) => false,
            },
            Func::Lambda(env, formals, body) => match other {
                Func::Lambda(other_env, other_f, other_b) => {
                    formals == other_f && body == other_b && env == other_env
                }
                Func::Builtin(..) => false,
            },
        }
    }
}

impl fmt::Display for Lval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Lval::Query(_cells) => write!(f, "<toplevel>"),
            Lval::Fun(lf) => match lf {
                Func::Builtin(name, _) => write!(f, "<builtin: {name}>"),
                Func::Lambda(_, formals, body) => write!(f, "(\\ {formals} {body})"),
            },
            Lval::Num(n) => write!(f, "{n}"),
            Lval::Sym(s) => write!(f, "{s}"),
            Lval::String(s) => write!(f , "\"{s}\""),
            Lval::Logical(operator, operands) => {
                write!(f, "({} {})", operator, format_children(operands))
            }
            Lval::PredicateOperator(operator) => write!(f, "{}", operator),
            Lval::Emit(node_type, attributes, when_block, do_block) => {
                let formatted_attrs = attributes
                    .iter()
                    .map(|(key, value)| format!("({} {})", key, value))
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(
                    f,
                    "(emit {} {} {} {})",
                    node_type,
                    formatted_attrs,
                    when_block.as_ref().map(|b| b.to_string()).unwrap_or_default(),
                    do_block.as_ref().map(|b| b.to_string()).unwrap_or_default()
                )
            }
            Lval::WhenForm(condition, body) => {
                write!(f, "(when {} {})", condition, format_children(body))
            }
            Lval::CaptureForm(node_type, attributes, nested_captures, do_block) => {
                let formatted_attrs = attributes
                    .iter()
                    .map(|(key, value)| format!("({} {})", key, value))
                    .collect::<Vec<_>>()
                    .join(" ");
                write!(
                    f,
                    "(emit {} {} {} {})",
                    node_type,
                    formatted_attrs,
                    nested_captures
                        .clone()
                        .map(|n| Lval::lval_expr_print(&[n]))
                        .unwrap_or_default(),
                    do_block.as_ref().map(|b| b.to_string()).unwrap_or_default()
                )
            }
            Lval::Capture(capture) => {
                write!(f, "@{}", capture)
            }
            Lval::DoForm(cell) => write!(f, "(do {})", Lval::lval_expr_print(cell)),
            Lval::Sexpr(cell) => write!(f, "({})", Lval::lval_expr_print(cell)),
            Lval::Qexpr(cell) => write!(f, "{{{}}}", Lval::lval_expr_print(cell)),
            Lval::KeyVal(cell) => write!(f, "({})", Lval::lval_expr_print(cell)),
        }
    }
}

fn format_children(children: &[Box<Lval>]) -> String {
    children
        .iter()
        .map(|child| child.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}
