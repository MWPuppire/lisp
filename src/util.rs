use crate::env::{hash, LispClosure, LispEnv, LispSymbol};
use by_address::ByAddress;
use im::{HashMap, Vector};
use ordered_float::OrderedFloat;
use parking_lot::RwLock;
use phf::phf_map;
use std::fmt;
use std::sync::Arc;
use thiserror::Error;

// Utility macro, raise `err` if `cond` is false
macro_rules! assert_or_err {
    ($cond:expr, $err:expr) => {
        if !$cond {
            Err($err)?;
        }
    };
}
pub(crate) use assert_or_err;

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

// the map includes any reserved words (identifiers with special meanings)
// this includes the built-in special forms, but also `true`, `false`, and
// `nil`, which have the unique property of being handled by the parser rather
// than the evaluator (e.g. `parse("true")` returns the boolean `true`, but
// `parse("x")` returns the identifier `x` for any non-reserved `x`)
static LISP_SPECIAL_FORMS: phf::Map<&'static str, LispValue> = phf_map! {
    "def!" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Def,
        quoted: false,
    }),
    "defmacro!" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Defmacro,
        quoted: false,
    }),
    "let*" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Let,
        quoted: false,
    }),
    "quote" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Quote,
        quoted: false,
    }),
    "quasiquote" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Quasiquote,
        quoted: false,
    }),
    "unquote" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Unquote,
        quoted: false,
    }),
    "splice-unquote" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::SpliceUnquote,
        quoted: false,
    }),
    "macroexpand" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Macroexpand,
        quoted: false,
    }),
    "try*" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Try,
        quoted: false,
    }),
    "catch*" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Catch,
        quoted: false,
    }),
    "do" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Do,
        quoted: false,
    }),
    "if" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::If,
        quoted: false,
    }),
    "fn*" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Fn,
        quoted: false,
    }),
    "deref" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Deref,
        quoted: false,
    }),
    "eval" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Eval,
        quoted: false,
    }),
    "apply" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Apply,
        quoted: false,
    }),
    "cond" => LispValue::new(InnerValue::Special {
        form: LispSpecialForm::Cond,
        quoted: false,
    }),
    "nil" => LispValue::nil(),
    "true" => LispValue::new(InnerValue::Bool(true)),
    "false" => LispValue::new(InnerValue::Bool(false)),
};

// convenience type
pub type Result<T> = std::result::Result<T, LispError>;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(crate) struct LispFunc {
    pub(crate) args: Vec<LispSymbol>,
    pub(crate) body: LispValue,
    pub(crate) closure: LispClosure,
    pub(crate) variadic: bool,
}

#[derive(Clone, Copy)]
pub(crate) struct LispBuiltinFunc {
    pub name: &'static str,
    pub body: fn(Vector<LispValue>, &LispEnv) -> Result<LispValue>,
}
impl PartialEq for LispBuiltinFunc {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for LispBuiltinFunc {}
impl std::hash::Hash for LispBuiltinFunc {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}
impl fmt::Debug for LispBuiltinFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LispBuiltinFunc")
            .field("name", &self.name)
            .field(
                "body",
                &self.body as &fn(Vector<LispValue>, &'static LispEnv) -> Result<LispValue>,
            )
            .finish()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum InnerObjectValue {
    List(Vector<LispValue>),
    BuiltinFunc(LispBuiltinFunc),
    Func(LispFunc),
    Macro(LispFunc),
    Map(HashMap<LispValue, LispValue>),
    Vector(Vec<LispValue>),
    String(String),
    Keyword(String),
}

impl InnerObjectValue {
    pub fn type_of(&self) -> &'static str {
        match self {
            Self::List(_) => "list",
            Self::BuiltinFunc(_) => "function",
            Self::Func(_) => "function",
            Self::Macro(_) => "function",
            Self::Map(_) => "map",
            Self::Vector(_) => "vector",
            Self::String(_) => "string",
            Self::Keyword(_) => "keyword",
        }
    }
}

#[derive(Clone, Debug, Derivative)]
#[derivative(PartialEq, Eq, Hash)]
pub(crate) struct ObjectValue {
    pub(crate) val: InnerObjectValue,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub(crate) meta: Option<LispValue>,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    pub(crate) quoted: bool,
}

impl ObjectValue {
    pub fn type_of(&self) -> &'static str {
        self.val.type_of()
    }

    // outputs the value fully-quoted
    fn inspect_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.val {
            InnerObjectValue::List(l) => {
                write!(f, "'(")?;
                l.iter()
                    .take(l.len().saturating_sub(1))
                    .try_for_each(|x| write!(f, "{} ", x))?;
                // take the last value separate to avoid printing an extra space
                // before the closing parenthesis
                if let Some(last) = l.last() {
                    write!(f, "{})", last)
                } else {
                    write!(f, ")")
                }
            }
            InnerObjectValue::Map(m) => {
                write!(f, "'{{")?;
                let mut iter = m.iter();
                // just take the first one; Lisp maps are unordered, and there's
                // no easy way to extract the last item from a hashmap like with
                // a vector
                let first = iter.next();
                iter.try_for_each(|(key, val)| write!(f, "{} {:#} ", key, val))?;
                if let Some((key, val)) = first {
                    write!(f, "{} {:#}}}", key, val)
                } else {
                    write!(f, "}}")
                }
            }
            InnerObjectValue::Vector(l) => {
                write!(f, "'[")?;
                l.iter()
                    .take(l.len().saturating_sub(1))
                    .try_for_each(|x| write!(f, "{} ", x))?;
                if let Some(last) = l.last() {
                    write!(f, "{}]", last)
                } else {
                    write!(f, "]")
                }
            }
            _ => self.inspect_quoted_fmt(f),
        }
    }
    // outputs the value assuming it is already quoted
    fn inspect_quoted_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.val {
            InnerObjectValue::List(l) => {
                write!(f, "(")?;
                l.iter()
                    .take(l.len().saturating_sub(1))
                    .try_for_each(|x| write!(f, "{} ", x))?;
                if let Some(last) = l.last() {
                    write!(f, "{})", last)
                } else {
                    write!(f, ")")
                }
            }
            InnerObjectValue::BuiltinFunc(func) => write!(f, "{}", func.name),
            InnerObjectValue::Func(func) => {
                write!(f, "(fn* (")?;
                func.args
                    .iter()
                    .take(func.args.len().saturating_sub(1))
                    .try_for_each(|x| write!(f, "\\{} ", *x))?;
                if let Some(last) = func.args.last() {
                    write!(f, "\\{}", *last)?;
                }
                write!(f, ") ({})", func.body)
            }
            InnerObjectValue::Macro(func) => {
                // because macros have to be defined to a variable before they
                // can be used, this is the only form that cannot be inspected
                // properly (and hence uses "#<macro-fn*>")
                write!(f, "(#<macro-fn*> (")?;
                func.args
                    .iter()
                    .take(func.args.len().saturating_sub(1))
                    .try_for_each(|x| write!(f, "\\{} ", *x))?;
                if let Some(last) = func.args.last() {
                    write!(f, "\\{}", *last)?;
                }
                write!(f, ") ({})", func.body)
            }
            InnerObjectValue::Map(m) => {
                // code explained above, in `inspect_fmt`
                write!(f, "{{")?;
                let mut iter = m.iter();
                let first = iter.next();
                iter.try_for_each(|(key, val)| write!(f, "{} {:#} ", key, val))?;
                if let Some((key, val)) = first {
                    write!(f, "{} {:#}}}", key, val)
                } else {
                    write!(f, "}}")
                }
            }
            InnerObjectValue::Vector(l) => {
                write!(f, "[")?;
                l.iter()
                    .take(l.len().saturating_sub(1))
                    .try_for_each(|x| write!(f, "{} ", x))?;
                if let Some(last) = l.last() {
                    write!(f, "{}]", last)
                } else {
                    write!(f, "]")
                }
            }
            InnerObjectValue::String(s) => write!(f, r#""{}""#, s.escape_default()),
            InnerObjectValue::Keyword(s) => write!(f, ":{}", s),
        }
    }
}

impl fmt::Display for ObjectValue {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.inspect_fmt(f)
        } else {
            self.inspect_quoted_fmt(f)
        }
    }
}

#[derive(Clone, Debug, Default, Derivative)]
#[derivative(PartialEq, Eq, Hash)]
pub(crate) enum InnerValue {
    Symbol {
        sym: LispSymbol,
        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        quoted: bool,
        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        variadic: bool,
    },
    Number(OrderedFloat<f64>),
    Bool(bool),
    #[default]
    Nil,
    Atom(ByAddress<Arc<RwLock<LispValue>>>),
    Object(Arc<ObjectValue>),
    Special {
        form: LispSpecialForm,
        #[derivative(PartialEq = "ignore", Hash = "ignore")]
        quoted: bool,
    },
}

impl InnerValue {
    pub fn type_of(&self) -> &'static str {
        match self {
            Self::Symbol { .. } => "symbol",
            Self::Number(_) => "number",
            Self::Bool(_) => "bool",
            Self::Nil => "nil",
            Self::Atom(_) => "atom",
            Self::Object(obj) => obj.type_of(),
            Self::Special { .. } => "function",
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash)]
pub struct LispValue {
    pub(crate) val: InnerValue,
}

impl LispValue {
    #[inline]
    pub(crate) const fn new(val: InnerValue) -> Self {
        LispValue { val }
    }

    #[inline]
    pub const fn nil() -> Self {
        Self::new(InnerValue::Nil)
    }

    pub fn symbol_for(s: &str) -> Self {
        if let Some(val) = LISP_SPECIAL_FORMS.get(s) {
            val.clone()
        } else {
            Self::new(InnerValue::Symbol {
                sym: hash(s),
                quoted: false,
                variadic: false,
            })
        }
    }

    #[inline]
    pub fn string_for(s: String) -> Self {
        Self::new(InnerValue::Object(Arc::new(ObjectValue {
            val: InnerObjectValue::String(s),
            meta: None,
            quoted: false,
        })))
    }

    #[inline]
    pub fn keyword_for(s: String) -> Self {
        Self::new(InnerValue::Object(Arc::new(ObjectValue {
            val: InnerObjectValue::Keyword(s),
            meta: None,
            quoted: false,
        })))
    }

    pub fn type_of(&self) -> &'static str {
        self.val.type_of()
    }

    #[inline]
    pub fn inspect(&self) -> String {
        format!("{:#}", self)
    }
    fn inspect_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.val {
            InnerValue::Symbol { sym, .. } => write!(f, "'\\{}", *sym),
            InnerValue::Object(o) => o.inspect_fmt(f),
            _ => self.inspect_quoted_fmt(f),
        }
    }
    fn inspect_quoted_fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.val {
            InnerValue::Symbol { sym, .. } => write!(f, "\\{}", *sym),
            InnerValue::Number(n) => write!(f, "{}", n),
            InnerValue::Bool(b) => write!(f, "{}", b),
            InnerValue::Nil => write!(f, "nil"),
            InnerValue::Atom(x) => {
                write!(f, "(atom ")?;
                x.read().inspect_fmt(f)?;
                write!(f, ")")
            }
            InnerValue::Object(o) => o.inspect_quoted_fmt(f),
            InnerValue::Special { form, .. } => write!(f, "{}", form),
        }
    }

    pub fn truthiness(&self) -> bool {
        match &self.val {
            InnerValue::Nil => false,
            InnerValue::Bool(b) => *b,
            InnerValue::Atom(a) => a.read().truthiness(),
            _ => true,
        }
    }
    #[inline]
    pub fn is_nil(&self) -> bool {
        matches!(self.val, InnerValue::Nil)
    }

    pub fn try_into_iter(self) -> Result<std::vec::IntoIter<LispValue>> {
        if let InnerValue::Object(o) = self.val {
            if matches!(
                o.val,
                InnerObjectValue::List(_) | InnerObjectValue::Vector(_)
            ) {
                // `matches!` before the `match` to avoid potentially cloning a
                // non-list object
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let vec = match cloned.val {
                    InnerObjectValue::List(l) => l.into_iter().collect(),
                    InnerObjectValue::Vector(l) => l,
                    _ => unreachable!(),
                };
                Ok(vec.into_iter())
            } else {
                Err(LispError::InvalidDataType("list", o.type_of()))
            }
        } else {
            Err(LispError::InvalidDataType("list", self.type_of()))
        }
    }

    pub fn expect_string(&self) -> Result<&str> {
        match &self.val {
            InnerValue::Object(o) => match &o.val {
                InnerObjectValue::String(s) => Ok(s),
                _ => Err(LispError::InvalidDataType("string", self.type_of())),
            },
            _ => Err(LispError::InvalidDataType("string", self.type_of())),
        }
    }

    // recursively transforms vectors to lists; used for testing equality,
    // since the spec requires lists and vectors containing the same elements
    // to compare equal
    pub(crate) fn vector_to_list(self) -> Self {
        if let InnerValue::Object(o) = self.val {
            if matches!(
                o.val,
                InnerObjectValue::List(_) | InnerObjectValue::Vector(_)
            ) {
                // `matches!` before the `match` explained in `try_into_iter`
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let list: Vector<LispValue> = match cloned.val {
                    InnerObjectValue::List(l) => l.into_iter().map(Self::vector_to_list).collect(),
                    InnerObjectValue::Vector(l) => {
                        l.into_iter().map(Self::vector_to_list).collect()
                    }
                    _ => unreachable!(),
                };
                Self::new(InnerValue::Object(Arc::new(ObjectValue {
                    val: InnerObjectValue::List(list),
                    meta: None,
                    quoted: false,
                })))
            } else {
                Self::new(InnerValue::Object(o))
            }
        } else {
            self
        }
    }

    #[inline]
    pub fn vector_from<T: IntoIterator<Item = Self>>(iter: T) -> Self {
        let vector = InnerObjectValue::Vector(iter.into_iter().collect());
        Self::new(InnerValue::Object(Arc::new(ObjectValue {
            val: vector,
            meta: None,
            quoted: false,
        })))
    }

    pub fn is_quoted(&self) -> bool {
        match &self.val {
            InnerValue::Object(o) => o.quoted,
            InnerValue::Symbol { quoted, .. } => *quoted,
            InnerValue::Special { quoted, .. } => *quoted,
            _ => false,
        }
    }

    pub fn quote(self) -> Self {
        Self::new(match self.val {
            InnerValue::Object(mut o) => {
                Arc::make_mut(&mut o).quoted = true;
                InnerValue::Object(o)
            }
            InnerValue::Symbol { sym, variadic, .. } => InnerValue::Symbol {
                sym,
                quoted: true,
                variadic,
            },
            InnerValue::Special { form, .. } => InnerValue::Special { form, quoted: true },
            x => x,
        })
    }

    pub fn unquote(self) -> Self {
        Self::new(match self.val {
            InnerValue::Object(mut o) => {
                Arc::make_mut(&mut o).quoted = false;
                InnerValue::Object(o)
            }
            InnerValue::Symbol { sym, variadic, .. } => InnerValue::Symbol {
                sym,
                quoted: false,
                variadic,
            },
            InnerValue::Special { form, .. } => InnerValue::Special {
                form,
                quoted: false,
            },
            x => x,
        })
    }
}

impl From<f64> for LispValue {
    #[inline]
    fn from(item: f64) -> Self {
        Self::new(InnerValue::Number(OrderedFloat(item)))
    }
}
impl From<OrderedFloat<f64>> for LispValue {
    #[inline]
    fn from(item: OrderedFloat<f64>) -> Self {
        Self::new(InnerValue::Number(item))
    }
}
impl From<bool> for LispValue {
    #[inline]
    fn from(item: bool) -> Self {
        Self::new(InnerValue::Bool(item))
    }
}
impl From<String> for LispValue {
    #[inline]
    fn from(item: String) -> Self {
        Self::new(InnerValue::Object(Arc::new(ObjectValue {
            val: InnerObjectValue::String(item),
            meta: None,
            quoted: false,
        })))
    }
}
impl From<LispSymbol> for LispValue {
    #[inline]
    fn from(sym: LispSymbol) -> Self {
        Self::new(InnerValue::Symbol {
            sym,
            quoted: false,
            variadic: false,
        })
    }
}
impl From<Vector<LispValue>> for LispValue {
    #[inline]
    fn from(item: Vector<LispValue>) -> Self {
        Self::new(InnerValue::Object(Arc::new(ObjectValue {
            val: InnerObjectValue::List(item),
            meta: None,
            quoted: false,
        })))
    }
}
impl From<HashMap<LispValue, LispValue>> for LispValue {
    #[inline]
    fn from(item: HashMap<LispValue, LispValue>) -> Self {
        Self::new(InnerValue::Object(Arc::new(ObjectValue {
            val: InnerObjectValue::Map(item),
            meta: None,
            quoted: false,
        })))
    }
}
impl FromIterator<LispValue> for LispValue {
    #[inline]
    fn from_iter<T: IntoIterator<Item = LispValue>>(iter: T) -> Self {
        let list = InnerObjectValue::List(iter.into_iter().collect());
        Self::new(InnerValue::Object(Arc::new(ObjectValue {
            val: list,
            meta: None,
            quoted: false,
        })))
    }
}

impl FromIterator<(LispValue, LispValue)> for LispValue {
    #[inline]
    fn from_iter<T: IntoIterator<Item = (LispValue, LispValue)>>(iter: T) -> Self {
        let map = InnerObjectValue::Map(iter.into_iter().collect());
        Self::new(InnerValue::Object(Arc::new(ObjectValue {
            val: map,
            meta: None,
            quoted: false,
        })))
    }
}

impl TryFrom<LispValue> for LispSymbol {
    type Error = LispError;
    #[inline]
    fn try_from(item: LispValue) -> Result<Self> {
        match item.val {
            InnerValue::Symbol { sym, .. } => Ok(sym),
            _ => Err(LispError::InvalidDataType("symbol", item.type_of())),
        }
    }
}
impl TryFrom<LispValue> for f64 {
    type Error = LispError;
    #[inline]
    fn try_from(item: LispValue) -> Result<Self> {
        match item.val {
            InnerValue::Number(f) => Ok(f.0),
            _ => Err(LispError::InvalidDataType("number", item.type_of())),
        }
    }
}
impl TryFrom<LispValue> for OrderedFloat<f64> {
    type Error = LispError;
    #[inline]
    fn try_from(item: LispValue) -> Result<Self> {
        match item.val {
            InnerValue::Number(f) => Ok(f),
            _ => Err(LispError::InvalidDataType("number", item.type_of())),
        }
    }
}
impl TryFrom<LispValue> for Vector<LispValue> {
    type Error = LispError;
    #[inline]
    fn try_from(item: LispValue) -> Result<Self> {
        if let InnerValue::Object(o) = item.val {
            // `matches!` instead of a `match` or `if let` guard to avoid
            // potentially cloning a non-list object
            if matches!(o.val, InnerObjectValue::List(_)) {
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let InnerObjectValue::List(l) = cloned.val else { unreachable!() };
                Ok(l)
            } else {
                Err(LispError::InvalidDataType("list", o.type_of()))
            }
        } else {
            Err(LispError::InvalidDataType("list", item.type_of()))
        }
    }
}
impl TryFrom<LispValue> for HashMap<LispValue, LispValue> {
    type Error = LispError;
    #[inline]
    fn try_from(item: LispValue) -> Result<Self> {
        if let InnerValue::Object(o) = item.val {
            // `matches!` explained in `TryFrom` for `Vector`
            if matches!(o.val, InnerObjectValue::Map(_)) {
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let InnerObjectValue::Map(l) = cloned.val else { unreachable!() };
                Ok(l)
            } else {
                Err(LispError::InvalidDataType("map", o.type_of()))
            }
        } else {
            Err(LispError::InvalidDataType("map", item.type_of()))
        }
    }
}
impl TryFrom<LispValue> for Arc<RwLock<LispValue>> {
    type Error = LispError;
    #[inline]
    fn try_from(item: LispValue) -> Result<Self> {
        match item.val {
            InnerValue::Atom(x) => Ok(x.0),
            _ => Err(LispError::InvalidDataType("atom", item.type_of())),
        }
    }
}

impl fmt::Display for LispValue {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            self.inspect_fmt(f)
        } else {
            self.inspect_quoted_fmt(f)
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
    #[error("odd number of arguments passed to cond")]
    OddCondArguments,
    #[error("cannot redefine special form `{0}`")]
    CannotRedefineSpecialForm(LispSpecialForm),

    #[cfg(feature = "io-stdlib")]
    #[error("error calling into native function: {0}")]
    OSFailure(#[from] std::io::Error),
}
