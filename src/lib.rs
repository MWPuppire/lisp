#![feature(arc_unwrap_or_clone)]

extern crate cfg_if;
extern crate lazy_static;
extern crate thiserror;
extern crate im;
extern crate string_interner;
extern crate ordered_float;
extern crate nom;
extern crate by_address;
extern crate phf;

pub mod util;
pub use crate::util::{LispError, LispValue, Result};
pub mod parser;
pub use crate::parser::LispParser;
pub mod eval;
pub use crate::eval::eval;
pub mod env;
pub use crate::env::LispEnv;
mod builtins;
mod specials;
