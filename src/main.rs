extern crate lazy_static;
extern crate regex;
extern crate thiserror;
extern crate anyhow;

use std::io;
use std::io::Write;

pub mod util;
pub use crate::util::{LispError, LispValue};
pub mod parser;
use crate::parser::LispParser;
pub mod eval;
use crate::eval::eval;
pub mod env;
use crate::env::LispEnv;
pub mod builtins;

fn main() -> Result<(), anyhow::Error> {
    let mut stdout = io::stdout();
    let stdin = io::stdin();

    let mut parser = LispParser::new();
    let mut env = LispEnv::new_builtin();
    let mut complete = true;
    let mut buffer = String::new();

    loop {
        if complete {
            print!("> ");
        } else {
            print!("... ");
        }
        stdout.flush()?;
        let size = stdin.read_line(&mut buffer)?;
        if size == 0 {
            // eof
            break Ok(());
        }
        parser.add_tokenize(&buffer);
        complete = parser.is_complete();
        if complete {
            let out = eval(&parser.next()?, &mut env);
            match out {
                Ok(out) => println!("{}", out),
                Err(err) => println!("Err: {}", err),
            }
        }
        buffer.clear();
    }
}
