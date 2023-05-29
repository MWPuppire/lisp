// #![forbid(unsafe_code)]

extern crate cfg_if;
extern crate lazy_static;
extern crate thiserror;
// TODO `im` currently seems to violate Stacked Borrows
// (I get test fails under Miri, and someone else has a similar problem:
// https://github.com/bodil/im-rs/issues/207)
// I could look for a different crate, or I could expect the developer to fix it
// at some point, but I should decide what I'm going to do
// the hardest part is finding a `HashMap` that also implements `Hash`, since
// Mal allows (or encourages allowing) hashmaps to be keys in other hashmaps
extern crate by_address;
extern crate im;
extern crate itertools;
extern crate nom;
extern crate ordered_float;
extern crate parking_lot;
extern crate phf;
extern crate string_interner;

// Also with Miri test fails, Miri reports a memory leak in the test suite
// (starting at step 4 tests)
// I think this probably has to be a cycle somewhere with `Arc`s, but I'm not
// sure the best way to handle that (weakening every `Arc` in an object value
// would avoid memory leaks, but would cause other significant problems)
// I could use a garbage collector crate, but none of the ones I'd investigated
// earler quite seemed to fit:
// `gc` has a single allocator per thread, while I would like the option of
// creating a `LispContext` type including the allocator (I really need to do
// this for the symbol interner, too)
// `gc-arena` looks good, but its `Gc` pointers don't derive `PartialEq`, `Eq`,
// `Hash`, etc., and I'd rather avoid manually implementing those on `LispValue`
// I haven't looked enough into `zerogc`, but it might be promising

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
