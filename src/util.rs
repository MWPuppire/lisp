use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use thiserror::Error;
use crate::env::LispEnv;

#[macro_export]
macro_rules! expect {
    ($cond:expr, $err:expr) => {
        if !$cond {
            return Err($err);
        }
    }
}

pub type Result<T> = std::result::Result<T, LispError>;

#[derive(Clone)]
pub enum LispValue {
    Symbol(String),
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    List(Vec<LispValue>),
    BuiltinFunc(fn(&[LispValue], &mut LispEnv) -> Result<LispValue>),
    Atom(Rc<RefCell<LispValue>>),
    Func {
        args: Vec<String>,
        body: Box<LispValue>,
        env: LispEnv,
    },
}

impl LispValue {
    pub fn type_of(&self) -> &'static str {
        match self {
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::BuiltinFunc(_) => "function",
            Self::String(_) => "string",
            Self::Number(_) => "number",
            Self::Bool(_) => "bool",
            Self::Nil => "nil",
            Self::Func { .. } => "function",
            Self::Atom(_) => "atom",
        }
    }

    pub fn expect_symbol(&self) -> Result<&str> {
        match self {
            Self::Symbol(s) => Ok(s),
            _ => Err(LispError::InvalidDataType("symbol", self.type_of())),
        }
    }
    pub fn expect_list(&self) -> Result<&[LispValue]> {
        match self {
            Self::List(l) => Ok(&l),
            _ => Err(LispError::InvalidDataType("list", self.type_of())),
        }
    }
    pub fn expect_atom(&self) -> Result<Rc<RefCell<LispValue>>> {
        match self {
            Self::Atom(x) => Ok(x.clone()),
            _ => Err(LispError::InvalidDataType("atom", self.type_of())),
        }
    }
    pub fn expect_number(&self) -> Result<f64> {
        match self {
            Self::Number(f) => Ok(*f),
            _ => Err(LispError::InvalidDataType("number", self.type_of())),
        }
    }
    pub fn expect_bool(&self) -> Result<bool> {
        match self {
            Self::Bool(b) => Ok(*b),
            _ => Err(LispError::InvalidDataType("bool", self.type_of())),
        }
    }
    pub fn expect_string(&self) -> Result<&str> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(LispError::InvalidDataType("string", self.type_of())),
        }
    }
    pub fn inspect(&self) -> String {
        match self {
            LispValue::Symbol(s) => format!("{}", s),
            LispValue::String(s) => format!(r#""{}""#, s.escape_default()),
            LispValue::Number(n) => format!("{}", n),
            LispValue::Bool(b) => format!("{}", b),
            LispValue::Nil => format!("nil"),
            LispValue::List(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect()).collect();
                format!("'({})", xs.join(" "))
            },
            LispValue::BuiltinFunc(_) => format!("#<native function>"),
            LispValue::Atom(x) => format!("(atom {})", x.borrow().inspect()),
            LispValue::Func { .. } => format!("#<function>")
        }
    }
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
            (LispValue::Atom(a), LispValue::Atom(b)) => a == b,
            _ => false,
        }
    }
}

impl fmt::Display for LispValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LispValue::Symbol(s) => write!(f, "{}", s),
            LispValue::String(s) => write!(f, "{}", s),
            LispValue::Number(n) => write!(f, "{}", n),
            LispValue::Bool(b) => write!(f, "{}", b),
            LispValue::Nil => write!(f, "nil"),
            LispValue::List(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.to_string()).collect();
                write!(f, "({})", xs.join(" "))
            },
            LispValue::BuiltinFunc(_) => write!(f, "#<native function>"),
            LispValue::Atom(x) => write!(f, "{}", x.borrow()),
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
    #[error("error calling into native function")]
    OSFailure(#[from] std::io::Error),
    #[error("`unquote` and `splice-unquote` can only be used inside `quasiquote`")]
    OnlyInQuasiquote,
}
