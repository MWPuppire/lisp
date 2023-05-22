use std::fmt;
use std::sync::Arc;
use std::ops::Deref;
use std::convert::Infallible;
use thiserror::Error;
use ordered_float::OrderedFloat;
use im::{HashMap, Vector};
use by_address::ByAddress;
use phf::phf_map;
use parking_lot::RwLock;
use crate::env::{LispEnv, LispClosure, LispSymbol};

cfg_if::cfg_if! {
    if #[cfg(feature = "async")] {
        use std::pin::Pin;
        use futures::prelude::*;
        use futures::future::{Shared, BoxFuture};
        use futures::task::{Context, Poll};
    }
}

macro_rules! __expect__ {
    ($cond:expr, $err:expr) => {
        if !$cond {
            Err($err)?;
        }
    }
}
pub(crate) use __expect__ as expect;

// criteria for becoming a special form are somewhat nebulous
// `deref`, for example, is only a special form because of the `@atom` syntax,
//    so making it a special form prevents changing what `@` does syntatically
// `catch*`, `unquote`, and `splice-unquote` are special forms because they're
//    handled by separate functions that expect a specific name (and handle the
//    behavior internally), so redefining the functions won't work as expected
//    (redefined, they would function differently only outside their intended
//    scope; redefining `catch*` won't change anything within a `try*`, which is
//    confusing)
// `eval`, `apply`, `do`, `cond`, and `if` are here mainly for TCO benefits
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum LispSpecialForm {
    Def,
    Defmacro,
    Let,
    Quote,
    Quasiquote,
    Unquote,
    SpliceUnquote,
    Macroexpand,
    Try,
    Catch,
    Do,
    If,
    Fn,
    Deref,
    Eval,
    Apply,
    Cond,
}

impl fmt::Display for LispSpecialForm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Def => write!(f, "def!"),
            Self::Defmacro => write!(f, "defmacro!"),
            Self::Let => write!(f, "let*"),
            Self::Quote => write!(f, "quote"),
            Self::Quasiquote => write!(f, "quasiquote"),
            Self::Unquote => write!(f, "unquote"),
            Self::SpliceUnquote => write!(f, "splice-unquote"),
            Self::Macroexpand => write!(f, "macroexpand"),
            Self::Try => write!(f, "try*"),
            Self::Catch => write!(f, "catch*"),
            Self::Do => write!(f, "do"),
            Self::If => write!(f, "if"),
            Self::Fn => write!(f, "fn*"),
            Self::Deref => write!(f, "deref"),
            Self::Eval => write!(f, "eval"),
            Self::Apply => write!(f, "apply"),
            Self::Cond => write!(f, "cond"),
        }
    }
}

static LISP_SPECIAL_FORMS: phf::Map<&'static str, LispValue> = phf_map! {
    "def!" => LispValue::Special(LispSpecialForm::Def),
    "defmacro!" => LispValue::Special(LispSpecialForm::Defmacro),
    "let*" => LispValue::Special(LispSpecialForm::Let),
    "quote" => LispValue::Special(LispSpecialForm::Quote),
    "quasiquote" => LispValue::Special(LispSpecialForm::Quasiquote),
    "unquote" => LispValue::Special(LispSpecialForm::Unquote),
    "splice-unquote" => LispValue::Special(LispSpecialForm::SpliceUnquote),
    "macroexpand" => LispValue::Special(LispSpecialForm::Macroexpand),
    "try*" => LispValue::Special(LispSpecialForm::Try),
    "catch*" => LispValue::Special(LispSpecialForm::Catch),
    "do" => LispValue::Special(LispSpecialForm::Do),
    "if" => LispValue::Special(LispSpecialForm::If),
    "fn*" => LispValue::Special(LispSpecialForm::Fn),
    "deref" => LispValue::Special(LispSpecialForm::Deref),
    "eval" => LispValue::Special(LispSpecialForm::Eval),
    "apply" => LispValue::Special(LispSpecialForm::Apply),
    "cond" => LispValue::Special(LispSpecialForm::Cond),
    "nil" => LispValue::Nil,
    "true" => LispValue::Bool(true),
    "false" => LispValue::Bool(false),
};

pub type Result<T> = std::result::Result<T, LispError>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LispFunc {
    pub(crate) args: Vec<LispSymbol>,
    pub(crate) body: LispValue,
    pub(crate) closure: LispClosure,
    pub(crate) variadic: bool,
}

pub type LispBuiltinFunc = fn(Vector<LispValue>, &LispEnv) -> Result<LispValue>;

cfg_if::cfg_if! {
    if #[cfg(feature = "async")] {
        #[derive(Clone, Debug)]
        #[repr(transparent)]
        pub struct LispAsyncValue(Shared<BoxFuture<'static, Result<LispValue>>>);

        impl PartialEq for LispAsyncValue {
            fn eq(&self, other: &Self) -> bool {
                self.0.ptr_eq(&other.0)
            }
        }
        impl Eq for LispAsyncValue { }

        impl std::hash::Hash for LispAsyncValue {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.0.ptr_hash(state);
            }
        }

        impl Future for LispAsyncValue {
            type Output = Result<LispValue>;
            fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                Pin::new(&mut self.0).poll(cx)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ObjectValue {
    List(Vector<LispValue>),
    BuiltinFunc {
        name: &'static str,
        f: LispBuiltinFunc,
    },
    Func(LispFunc),
    Macro(LispFunc),
    Map(HashMap<LispValue, LispValue>),
    Vector(Vec<LispValue>),
    String(String),
    Keyword(String),
}

impl ObjectValue {
    pub fn type_of(&self) -> &'static str {
        match self {
            Self::List(_) => "list",
            Self::BuiltinFunc { .. } => "function",
            Self::Func(_) => "function",
            Self::Macro(_) => "function",
            Self::Map(_) => "map",
            Self::Vector(_) => "vector",
            Self::String(_) => "string",
            Self::Keyword(_) => "keyword",
        }
    }
    pub fn inspect(&self) -> String {
        match self {
            Self::List(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect_inner()).collect();
                format!("'({})", xs.join(" "))
            },
            Self::Map(m) => {
                let xs: Vec<String> = m.iter().map(|(key, val)|
                    key.inspect_inner() + " " + &val.inspect()
                ).collect();
                format!("'{{{}}}", xs.join(" "))
            },
            Self::Vector(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect_inner()).collect();
                format!("'[{}]", xs.join(" "))
            },
            _ => self.inspect_inner(),
        }
    }
    fn inspect_inner(&self) -> String {
        match self {
            Self::List(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect_inner()).collect();
                format!("({})", xs.join(" "))
            }
            Self::BuiltinFunc { name, .. } => name.to_string(),
            Self::Func(f) => format!(
                "({} ({}) {})",
                "fn*",
                f.args.iter().map(|x| LispEnv::symbol_string(*x).unwrap()).collect::<Vec<&str>>().join(" "),
                f.body.inspect()
            ),
            Self::Macro(f) => format!(
                "({} ({}) {})",
                "#<macro-fn*>",
                f.args.iter().map(|x| LispEnv::symbol_string(*x).unwrap()).collect::<Vec<&str>>().join(" "),
                f.body.inspect()
            ),
            Self::Map(m) => {
                let xs: Vec<String> = m.iter().map(|(key, val)|
                    key.inspect_inner() + " " + &val.inspect()
                ).collect();
                format!("{{{}}}", xs.join(" "))
            },
            Self::Vector(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.inspect_inner()).collect();
                format!("'[{}]", xs.join(" "))
            },
            Self::String(s) => format!(r#""{}""#, s.escape_default()),
            Self::Keyword(s) => format!(":{}", s),
        }
    }
}

impl fmt::Display for ObjectValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::List(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.to_string()).collect();
                write!(f, "({})", xs.join(" "))
            },
            Self::BuiltinFunc { .. } => write!(f, "#<native function>"),
            Self::Func(_) => write!(f, "#<function>"),
            Self::Macro(_) => write!(f, "#<macro>"),
            Self::Vector(l) => {
                let xs: Vec<String> = l.iter().map(|x| x.to_string()).collect();
                write!(f, "[{}]", xs.join(" "))
            },
            Self::Map(m) => {
                let xs: Vec<String> = m.iter().map(|(key, val)|
                    key.to_string() + " " + &val.to_string()
                ).collect();
                write!(f, "{{{}}}", xs.join(" "))
            },
            Self::String(s) => write!(f, "{}", s),
            Self::Keyword(s) => write!(f, ":{}", s),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum LispValue {
    Symbol(LispSymbol),
    VariadicSymbol(LispSymbol),
    Number(OrderedFloat<f64>),
    Bool(bool),
    Nil,
    Atom(ByAddress<Arc<RwLock<LispValue>>>),
    Object(Arc<ObjectValue>),
    Special(LispSpecialForm),

    #[cfg(feature = "async")]
    Future(LispAsyncValue),
}

impl LispValue {
    pub fn symbol_for(s: &str) -> Self {
        if let Some(val) = LISP_SPECIAL_FORMS.get(s) {
            val.clone()
        } else {
            Self::Symbol(LispEnv::symbol_for(s))
        }
    }
    pub fn symbol_for_static(s: &'static str) -> Self {
        if let Some(val) = LISP_SPECIAL_FORMS.get(s) {
            val.clone()
        } else {
            Self::Symbol(LispEnv::symbol_for_static(s))
        }
    }

    pub fn string_for(s: String) -> Self {
        Self::Object(Arc::new(
            ObjectValue::String(s.to_owned())
        ))
    }

    pub fn keyword_for(s: String) -> Self {
        Self::Object(Arc::new(
            ObjectValue::Keyword(s.to_owned())
        ))
    }

    pub fn type_of(&self) -> &'static str {
        match self {
            Self::Symbol(_) => "symbol",
            Self::VariadicSymbol(_) => "symbol",
            Self::Number(_) => "number",
            Self::Bool(_) => "bool",
            Self::Nil => "nil",
            Self::Atom(_) => "atom",
            Self::Object(obj) => obj.type_of(),
            Self::Special(_) => "function",

            #[cfg(feature = "async")]
            Self::Future(_) => "future",
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            Self::Symbol(s) => format!("'{}", LispEnv::symbol_string(*s).unwrap()),
            Self::Object(o) => o.inspect(),
            _ => self.inspect_inner(),
        }
    }
    fn inspect_inner(&self) -> String {
        match self {
            Self::Symbol(s) => format!("{}", LispEnv::symbol_string(*s).unwrap()),
            Self::VariadicSymbol(s) => format!("&{}", LispEnv::symbol_string(*s).unwrap()),
            Self::Number(n) => format!("{}", n),
            Self::Bool(b) => format!("{}", b),
            Self::Nil => "nil".to_owned(),
            Self::Atom(x) => format!("(atom {})", x.read().inspect()),
            Self::Object(o) => o.inspect_inner(),
            Self::Special(s) => format!("{}", s),

            #[cfg(feature = "async")]
            Self::Future(_) => format!("(promise ...)"),
        }
    }

    pub fn truthiness(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Bool(b) => *b,
            Self::Atom(a) => a.read().truthiness(),
            _ => true,
        }
    }
    pub fn is_nil(&self) -> bool {
        matches!(self, Self::Nil)
    }

    pub fn expect_symbol(&self) -> Result<LispSymbol> {
        match self {
            Self::Symbol(s) => Ok(*s),
            Self::VariadicSymbol(s) => Ok(*s),
            _ => Err(LispError::InvalidDataType("symbol", self.type_of())),
        }
    }
    pub fn expect_number(&self) -> Result<OrderedFloat<f64>> {
        match self {
            Self::Number(f) => Ok(*f),
            _ => Err(LispError::InvalidDataType("number", self.type_of())),
        }
    }
    pub fn expect_func(&self) -> Result<&LispFunc> {
        match self {
            Self::Object(o) => match o.deref() {
                ObjectValue::Func(f) => Ok(f),
                ObjectValue::Macro(f) => Ok(f),
                _ => Err(LispError::InvalidDataType("function", o.type_of())),
            },
            _ => Err(LispError::InvalidDataType("function", self.type_of())),
        }
    }
    pub fn into_list(self) -> Result<Vector<LispValue>> {
        if let Self::Object(o) = self {
            if matches!(o.deref(), ObjectValue::List(_) | ObjectValue::Vector(_)) {
                // `matches!` before the `match` to avoid potentially cloning
                // non-lists; whether or not this is necessary remains to be
                // seen, since these `expect_` functions do expect to receive a
                // value of the correct type, so incorrect types should be a
                // rare case
                let cloned = Arc::unwrap_or_clone(o);
                match cloned {
                    ObjectValue::List(l) => Ok(l),
                    ObjectValue::Vector(l) => Ok(Vector::from(l)),
                    _ => unreachable!(),
                }
            } else {
                Err(LispError::InvalidDataType("list", o.type_of()))
            }
        } else {
            Err(LispError::InvalidDataType("list", self.type_of()))
        }
    }
    pub fn into_hashmap(self) -> Result<HashMap<LispValue, LispValue>> {
        if let Self::Object(o) = self {
            if matches!(o.deref(), ObjectValue::Map(_)) {
                // `matches!` before the `match` explained in `into_list`
                let cloned = Arc::unwrap_or_clone(o);
                match cloned {
                    ObjectValue::Map(m) => Ok(m),
                    _ => unreachable!(),
                }
            } else {
                Err(LispError::InvalidDataType("map", o.type_of()))
            }
        } else {
            Err(LispError::InvalidDataType("map", self.type_of()))
        }
    }
    pub fn into_atom(self) -> Result<Arc<RwLock<LispValue>>> {
        match self {
            Self::Atom(x) => Ok(x.0),
            _ => Err(LispError::InvalidDataType("atom", self.type_of())),
        }
    }
    pub fn expect_string(&self) -> Result<&str> {
        match self {
            Self::Object(o) => match o.deref() {
                ObjectValue::String(s) => Ok(s),
                _ => Err(LispError::InvalidDataType("string", self.type_of())),
            },
            _ => Err(LispError::InvalidDataType("string", self.type_of())),
        }
    }

    #[cfg(feature = "async")]
    pub fn into_future(self) -> Result<LispAsyncValue> {
        match self {
            Self::Future(f) => Ok(f),
            _ => Err(LispError::InvalidDataType("future", self.type_of())),
        }
    }

    // recursively transforms vectors to lists; used for testing equality,
    // since the spec requires lists and vectors containing the same elements
    // to compare equal
    pub fn vector_to_list(self) -> Self {
        if let Self::Object(o) = self {
            if matches!(o.deref(), ObjectValue::List(_) | ObjectValue::Vector(_)) {
                // `matches!` before the `match` explained in `into_list`
                let cloned = Arc::unwrap_or_clone(o);
                let list: Vector<LispValue> = match cloned {
                    ObjectValue::List(l) => l.into_iter().map(Self::vector_to_list).collect(),
                    ObjectValue::Vector(l) => l.into_iter().map(Self::vector_to_list).collect(),
                    _ => unreachable!(),
                };
                Self::Object(Arc::new(ObjectValue::List(list)))
            } else {
                Self::Object(o)
            }
        } else {
            self
        }
    }

    pub fn list_from<T: IntoIterator<Item = Self>>(iter: T) -> Self {
        let list = ObjectValue::List(iter.into_iter().collect());
        Self::Object(Arc::new(list))
    }

    pub fn vector_from<T: IntoIterator<Item = Self>>(iter: T) -> Self {
        let list = ObjectValue::Vector(iter.into_iter().collect());
        Self::Object(Arc::new(list))
    }

    pub fn map_from<T: IntoIterator<Item = (Self, Self)>>(iter: T) -> Self {
        let list = ObjectValue::Map(iter.into_iter().collect());
        Self::Object(Arc::new(list))
    }
}

impl From<f64> for LispValue {
    fn from(item: f64) -> Self {
        Self::Number(OrderedFloat(item))
    }
}
impl From<OrderedFloat<f64>> for LispValue {
    fn from(item: OrderedFloat<f64>) -> Self {
        Self::Number(item)
    }
}
impl From<bool> for LispValue {
    fn from(item: bool) -> Self {
        Self::Bool(item)
    }
}
impl From<String> for LispValue {
    fn from(item: String) -> Self {
        Self::Object(Arc::new(ObjectValue::String(item)))
    }
}
impl From<LispSymbol> for LispValue {
    fn from(item: LispSymbol) -> Self {
        Self::Symbol(item)
    }
}
#[cfg(feature = "async")]
impl From<BoxFuture<'static, Result<LispValue>>> for LispValue {
    fn from(item: BoxFuture<'static, Result<LispValue>>) -> Self {
        let wrapper = LispAsyncValue(item.shared());
        Self::Future(wrapper)
    }
}

impl fmt::Display for LispValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Symbol(s) => write!(f, "{}", LispEnv::symbol_string(*s).unwrap()),
            Self::VariadicSymbol(s) => write!(f, "{}", LispEnv::symbol_string(*s).unwrap()),
            Self::Number(n) => write!(f, "{}", n),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Nil => write!(f, "nil"),
            Self::Atom(_) => write!(f, "#<atom>"),
            Self::Object(o) => o.fmt(f),
            Self::Special(s) => s.fmt(f),
            #[cfg(feature = "async")]
            Self::Future(_) => write!(f, "#<future>"),
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
    UndefinedVariable(&'static str),
    #[error("invalid data type. expected {0}, received {1}")]
    InvalidDataType(&'static str, &'static str),
    #[error("unexpected arguments. expected {0}, received {1}")]
    IncorrectArguments(usize, usize),
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
    #[error("prefix symbol `{0}` not followed by any tokens")]
    MissingToken(&'static str),
    #[error("`meta` and `with-meta` are not implemented for this version")]
    NoMeta,
    #[error("odd number of arguments passed to cond")]
    OddCondArguments,
    #[error("cannot redefine special form `{0}`")]
    CannotRedefineSpecialForm(LispSpecialForm),

    #[cfg(feature = "io-stdlib")]
    #[error("error calling into native function: {0}")]
    OSFailure(#[from] std::io::Error),
}
impl<F: Into<LispError>> From<std::result::Result<Infallible, F>> for LispError {
    #[inline]
    fn from(item: std::result::Result<Infallible, F>) -> Self {
        match item {
            Err(e) => e.into(),
            Ok(_) => unreachable!(),
        }
    }
}
impl Clone for LispError {
    fn clone(&self) -> Self {
        match self {
            // HACK to get around io::Error being non-cloneable
            Self::OSFailure(x) => Self::UncaughtException(
                format!("error calling into native function: {}", x).into()
            ),
            x => x.clone(),
        }
    }
}
