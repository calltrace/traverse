// Heavily inspired by https://github.com/deciduously/blispr/blob/master/src/lval.rs 
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
    Sym(String),
    Sexpr(LvalChildren),
    Qexpr(LvalChildren),
    Capture(String),
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

    pub fn capture(s: &str) -> Box<Lval> {
        Box::new(Lval::Capture(s.into()))
    }

    pub fn sexpr() -> Box<Lval> {
        Box::new(Lval::Sexpr(Vec::new()))
    }

    pub fn qexpr() -> Box<Lval> {
        Box::new(Lval::Qexpr(Vec::new()))
    }

    pub fn keyvalue() -> Box<Lval> {
        Box::new(Lval::KeyVal(Vec::new()))
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
            Lval::Capture(c) => write!(f, "{c}"),
            Lval::Sexpr(cell) => write!(f, "({})", Lval::lval_expr_print(cell)),
            Lval::Qexpr(cell) => write!(f, "{{{}}}", Lval::lval_expr_print(cell)),
            Lval::KeyVal(cell) => write!(f, "({})", Lval::lval_expr_print(cell)),
        }
    }
}
