use std::fmt;
use std::sync::{Arc, RwLock};
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
pub struct LispFunc(pub fn(&[LispValue], &mut LispEnv) -> Result<LispValue>);
impl fmt::Debug for LispFunc {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum LispValue {
    Symbol(String),
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    List(Vec<LispValue>),
    Atom(Arc<RwLock<LispValue>>),
    BuiltinFunc {
        name: &'static str,
        f: LispFunc,
    },
    Func {
        args: Vec<String>,
        body: Box<LispValue>,
        env: LispEnv,
        is_macro: bool,
    },
}

impl LispValue {
    pub fn type_of(&self) -> &'static str {
        match self {
            Self::Symbol(_) => "symbol",
            Self::List(_) => "list",
            Self::BuiltinFunc { .. } => "function",
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
    pub fn expect_atom(&self) -> Result<Arc<RwLock<LispValue>>> {
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
    pub fn expect_string(&self) -> Result<&str> {
        match self {
            Self::String(s) => Ok(s),
            _ => Err(LispError::InvalidDataType("string", self.type_of())),
        }
    }
    pub fn is_nil(&self) -> bool {
        match self {
            Self::Nil => true,
            _ => false,
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
            LispValue::BuiltinFunc { name, .. } => format!("{}", name),
            LispValue::Atom(x) => format!("(atom {})", x.read().unwrap().inspect()),
            LispValue::Func { args, body, is_macro, .. } => format!(
                "({} ({}) {})",
                if *is_macro { "#<macro-fn>" } else { "fn*" },
                args.join(" "),
                body.to_string()
            ),
        }
    }
    pub fn truthiness(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Bool(b) => *b,
            Self::Atom(a) => a.read().unwrap().truthiness(),
            _ => true,
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
            (LispValue::Atom(a), LispValue::Atom(b)) => *a.read().unwrap() == *b.read().unwrap(),
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
            LispValue::BuiltinFunc { .. } => write!(f, "#<native function>"),
            LispValue::Atom(x) => write!(f, "{}", x.read().unwrap()),
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
    #[error("undefined variable `{0}`")]
    UndefinedVariable(String),
    #[error("invalid data type. expected {0}, received {1}")]
    InvalidDataType(&'static str, &'static str),
    #[error("unexpected arguments. expected {0}, received {1}")]
    IncorrectArguments(usize, usize),
    #[error("error calling into native function")]
    OSFailure(#[from] std::io::Error),
    #[error("`unquote` and `splice-unquote` can only be used inside `quasiquote`")]
    OnlyInQuasiquote,
    #[error("index {0} out of range")]
    IndexOutOfRange(usize),
    #[error("missing a value for a `let` declaration binding")]
    MissingBinding,
    #[error("uncaught exception: {0}")]
    UncaughtException(LispValue),
    #[error("`catch` can only be used inside `try`")]
    OnlyInTry,
    #[error("missing `catch` block for a `try`")]
    TryNoCatch,
}
