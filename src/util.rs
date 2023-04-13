use std::fmt;
use std::hash;
use std::sync::{Arc, RwLock};
use thiserror::Error;
use im::{HashMap, Vector};
use crate::env::{LispEnv, LispClosure};

#[macro_export]
macro_rules! expect {
    ($cond:expr, $err:expr) => {
        if !$cond {
            return Err($err);
        }
    }
}

pub type Result<T> = std::result::Result<T, LispError>;

#[derive(Clone, Debug, Hash, PartialEq)]
pub struct LispFunc {
    pub args: Vec<String>,
    pub body: LispValue,
    pub closure: LispClosure,
    pub variadic: bool,
    pub is_macro: bool,
    // used for self-recursive functions
    pub name: Option<String>,
}

#[derive(Clone, Debug)]
pub enum LispValue {
    Symbol(String),
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    List(Vector<LispValue>),
    Atom(Arc<RwLock<LispValue>>),
    BuiltinFunc {
        name: &'static str,
        f: fn(Vector<LispValue>, LispEnv) -> Result<(LispValue, LispEnv, bool)>,
    },
    Func(Box<LispFunc>),
    Keyword(String),
    Map(HashMap<LispValue, LispValue>),
    Vector(Vec<LispValue>),
    VariadicSymbol(String),
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
            Self::Keyword(_) => "keyword",
            Self::Map(_) => "map",
            Self::Vector(_) => "vector",
            Self::VariadicSymbol(_) => "symbol",
        }
    }

    pub fn expect_symbol(&self) -> Result<&str> {
        match self {
            Self::Symbol(s) => Ok(s),
            Self::VariadicSymbol(s) => Ok(s),
            _ => Err(LispError::InvalidDataType("symbol", self.type_of())),
        }
    }
    pub fn expect_hashmap(&self) -> Result<&HashMap<LispValue, LispValue>> {
        match self {
            Self::Map(m) => Ok(m),
            _ => Err(LispError::InvalidDataType("map", self.type_of())),
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
            LispValue::Symbol(s) => format!("'{}", s),
            LispValue::List(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect_inner()).collect();
                format!("'({})", xs.join(" "))
            },
            LispValue::Map(m) => {
                let xs: Vec<String> = m.iter().map(|(key, val)|
                    key.inspect_inner() + " '" + &val.inspect_inner()
                ).collect();
                format!("'{{{}}}", xs.join(" "))
            },
            LispValue::Vector(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect_inner()).collect();
                format!("'[{}]", xs.join(" "))
            },
            _ => self.inspect_inner(),
        }
    }
    fn inspect_inner(&self) -> String {
        match self {
            LispValue::Symbol(s) => format!("{}", s),
            LispValue::String(s) => format!(r#""{}""#, s.escape_default()),
            LispValue::Number(n) => format!("{}", n),
            LispValue::Bool(b) => format!("{}", b),
            LispValue::Nil => format!("nil"),
            LispValue::List(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect_inner()).collect();
                format!("({})", xs.join(" "))
            },
            LispValue::BuiltinFunc { name, .. } => format!("{}", name),
            LispValue::Atom(x) => format!("(atom {})", x.read().unwrap().inspect()),
            LispValue::Func(f) => format!(
                "({} ({}) {})",
                if f.is_macro { "#<macro-fn>" } else { "fn*" },
                f.args.join(" "),
                f.body.to_string()
            ),
            LispValue::Keyword(s) => format!(":{}", s),
            LispValue::Map(m) => {
                let xs: Vec<String> = m.iter().map(|(key, val)|
                    key.inspect_inner() + " " + &val.inspect_inner()
                ).collect();
                format!("{{{}}}", xs.join(" "))
            },
            LispValue::Vector(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect_inner()).collect();
                format!("[{}]", xs.join(" "))
            },
            LispValue::VariadicSymbol(s) => format!("&{}", s),
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
    pub fn into_list(self) -> Result<Vector<LispValue>> {
        match self {
            Self::List(l) => Ok(l),
            Self::Vector(l) => Ok(Vector::from(l)),
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        }
    }
    pub fn into_symbol(self) -> Result<String> {
        match self {
            Self::Symbol(s) => Ok(s),
            Self::VariadicSymbol(s) => Ok(s),
            x => Err(LispError::InvalidDataType("symbol", x.type_of())),
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
            (LispValue::BuiltinFunc { name: a_name, .. }, LispValue::BuiltinFunc { name: b_name, .. }) => a_name == b_name,
            (LispValue::Vector(a), LispValue::Vector(b)) => a == b,
            (LispValue::Keyword(a), LispValue::Keyword(b)) => a == b,
            (LispValue::Map(a), LispValue::Map(b)) => a == b,
            (LispValue::VariadicSymbol(a), LispValue::VariadicSymbol(b)) => a == b,
            (LispValue::Symbol(a), LispValue::VariadicSymbol(b)) => a == b,
            (LispValue::VariadicSymbol(a), LispValue::Symbol(b)) => a == b,
            (LispValue::Func(a), LispValue::Func(b)) => a == b,
            (LispValue::List(a), LispValue::Vector(b)) => a == &Vector::from(b),
            (LispValue::Vector(a), LispValue::List(b)) => &Vector::from(a) == b,
            _ => false,
        }
    }
}
impl std::cmp::Eq for LispValue { }

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
            LispValue::Vector(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.to_string()).collect();
                write!(f, "[{}]", xs.join(" "))
            },
            LispValue::Map(m) => {
                let xs: Vec<String> = m.iter().map(|(key, val)|
                    key.to_string() + " " + &val.to_string()
                ).collect();
                write!(f, "{{{}}}", xs.join(" "))
            },
            LispValue::Keyword(s) => write!(f, ":{}", s),
            LispValue::VariadicSymbol(s) => write!(f, "{}", s),
        }
    }
}

impl hash::Hash for LispValue {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        match self {
            LispValue::Symbol(s) => {
                state.write_u8(b'_');
                s.hash(state)
            },
            LispValue::String(s) => {
                state.write_u8(b'"');
                s.hash(state)
            },
            LispValue::Number(n) => state.write_u64(n.to_bits()),
            LispValue::Bool(b) => b.hash(state),
            LispValue::List(l) => {
                state.write_u8(0x7F);
                l.hash(state)
            },
            LispValue::BuiltinFunc { name, .. } => name.hash(state),
            LispValue::Atom(x) => x.read().unwrap().hash(state),
            LispValue::Func(f) => f.hash(state),
            LispValue::Vector(l) => {
                state.write_u8(0xFF);
                l.hash(state)
            },
            LispValue::Map(m) => m.hash(state),
            LispValue::Keyword(s) => {
                state.write_u8(b':');
                s.hash(state)
            },
            LispValue::Nil => state.write_u64(0),
            LispValue::VariadicSymbol(s) => {
                state.write_u8(b'_');
                s.hash(state)
            },
        }
    }
}

#[derive(Error, Debug)]
pub enum LispError {
    #[error("syntax error at line {0}, column {1}")]
    SyntaxError(usize, usize),
    #[error("missing delimiter '{1}' (expected {0} more)")]
    UnbalancedDelim(usize, &'static str),
    #[error("undefined variable `{0}`")]
    UndefinedVariable(String),
    #[error("invalid data type. expected {0}, received {1}")]
    InvalidDataType(&'static str, &'static str),
    #[error("unexpected arguments. expected {0}, received {1}")]
    IncorrectArguments(usize, usize),
    #[error("error calling into native function: {0}")]
    OSFailure(#[from] std::io::Error),
    #[error("`unquote` and `splice-unquote` can only be used inside `quasiquote`")]
    OnlyInQuasiquote,
    #[error("index {0} out of range")]
    IndexOutOfRange(usize),
    #[error("missing a value for a `let` or `hash-map` binding")]
    MissingBinding,
    #[error("uncaught exception: {0}")]
    UncaughtException(LispValue),
    #[error("`catch` can only be used inside `try`")]
    OnlyInTry,
    #[error("missing `catch` block for a `try`")]
    TryNoCatch,
    #[error("cannot redefine variable in current scope")]
    AlreadyExists,
    #[error("attempt to read while no tokens were waiting")]
    ParseNoTokens,
}
