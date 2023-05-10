extern crate cfg_if;
extern crate lazy_static;
extern crate thiserror;
extern crate im;
extern crate string_interner;
extern crate ordered_float;
extern crate nom;

pub mod util;
pub use crate::util::{LispError, LispValue, Result};
pub mod parser;
pub use crate::parser::LispParser;
pub mod eval;
pub use crate::eval::{eval, eval_top};
pub mod env;
pub use crate::env::LispEnv;
mod builtins;
