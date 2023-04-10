extern crate lazy_static;
extern crate regex;
extern crate thiserror;
extern crate anyhow;
extern crate rustyline;

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
    let mut rl = rustyline::DefaultEditor::new()?;

    let mut parser = LispParser::new();
    let mut env = LispEnv::new_builtin();
    let mut complete = true;
    let mut buffer = String::new();

    loop {
        if let Ok(line) = rl.readline(if complete { "> " } else { "... " }) {
            parser.add_tokenize(&line);
            complete = parser.is_complete();
            if complete {
                rl.add_history_entry(buffer + &line)?;
                buffer = String::new();
                let out = eval(&parser.next()?, &mut env);
                match out {
                    Ok(out) => println!("{}", out),
                    Err(err) => println!("Err: {}", err),
                }
            } else {
                buffer = buffer + &line;
            }
        } else {
            // eof
            break Ok(());
        }
    }
}
