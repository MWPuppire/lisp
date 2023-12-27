use crate::{LispValue, value::LispSpecialForm};
use thiserror::Error;

// Utility macro, raise `err` if `cond` is false
macro_rules! assert_or_err {
    ($cond:expr, $err:expr $(,)?) => {
        if !$cond {
            Err($err)?;
        }
    };
}
pub(crate) use assert_or_err;

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

// convenience type
pub type Result<T> = std::result::Result<T, LispError>;
