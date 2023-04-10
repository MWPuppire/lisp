use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use thiserror::Error;
use crate::env::LispEnv;

#[derive(Clone)]
pub enum LispValue {
    Symbol(String),
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    List(Vec<LispValue>),
    BuiltinFunc(fn(&[LispValue], &mut LispEnv) -> Result<LispValue, LispError>),
    Atom(Rc<RefCell<LispValue>>),
    Func {
        args: Vec<String>,
        body: Box<LispValue>,
        env: LispEnv,
    },
}

impl std::cmp::PartialEq for LispValue {
    fn eq(&self, other: &LispValue) -> bool {
        match (self, other) {
            (LispValue::Symbol(a), LispValue::Symbol(b)) => a == b,
            (LispValue::String(a), LispValue::String(b)) => a == b,
            (LispValue::Number(a), LispValue::Number(b)) => a == b,
            (LispValue::Bool(a), LispValue::Bool(b)) => a == b,
            (LispValue::Nil, LispValue::Nil) => true,
            (LispValue::List(a), LispValue::List(b)) => a == b,
            (LispValue::Atom(a), LispValue::Atom(b)) => *a == *b,
            _ => false,
        }
    }
}

impl fmt::Display for LispValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LispValue::Symbol(s) => write!(f, "Symbol({})", s),
            LispValue::String(s) => write!(f, r#""{}""#, s),
            LispValue::Number(n) => write!(f, "{}", n),
            LispValue::Bool(b) => write!(f, "{}", b),
            LispValue::Nil => write!(f, "nil"),
            LispValue::List(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.to_string()).collect();
                write!(f, "({})", xs.join(" "))
            },
            LispValue::BuiltinFunc(_) => write!(f, "#<native function>"),
            LispValue::Atom(x) => write!(f, "Atom({})", x.borrow()),
            LispValue::Func { .. } => write!(f, "#<function>"),
        }
    }
}

#[derive(Error, Debug)]
pub enum LispError {
    #[error("syntax error at line {0}, column {1}")]
    SyntaxError(usize, usize),
    #[error("unbalanced parentheses (missing {0})")]
    UnbalancedParens(usize),
    #[error("undefined variable {0}")]
    UndefinedVariable(String),
    #[error("invalid data type. expected {0}, received {1}")]
    InvalidDataType(&'static str, &'static str),
    #[error("unexpected arguments. expected {0}, received {1}")]
    IncorrectArguments(usize, usize),
}
