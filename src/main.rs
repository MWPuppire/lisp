extern crate lazy_static;
extern crate regex;
extern crate thiserror;
extern crate anyhow;
extern crate rustyline;
extern crate unescape;

use std::fs::File;
use std::io::prelude::*;

pub mod util;
pub use crate::util::{LispError, LispValue, Result};
pub mod parser;
use crate::parser::LispParser;
pub mod eval;
use crate::eval::eval;
pub mod env;
use crate::env::LispEnv;
pub mod builtins;

fn main() -> std::result::Result<(), anyhow::Error> {
    let mut rl = rustyline::DefaultEditor::new()?;

    let mut parser = LispParser::new();
    let mut env = LispEnv::new_stdlib();
    let mut complete = true;
    let mut buffer = String::new();

    let args: Vec<String> = std::env::args().collect();
    let lisp_argv = args.iter().map(|x| LispValue::String(x.clone())).collect();
    env.set("*ARGV*".to_owned(), LispValue::List(lisp_argv));

    if args.len() > 1 && &args[1] != "--" {
        let mut file = File::open(&args[1])?;
        file.read_to_string(&mut buffer)?;
        parser.add_tokenize(&buffer);
        while parser.has_tokens() {
            eval(&parser.next()?, &mut env)?;
        }
        return Ok(());
    }

    loop {
        if let Ok(line) = rl.readline(if complete { "> " } else { "... " }) {
            parser.add_tokenize(&line);
            complete = parser.is_complete();
            if complete {
                rl.add_history_entry(buffer + &line)?;
                buffer = String::new();
                let out = eval(&parser.next()?, &mut env);
                match out {
                    Ok(out) => println!("{}", out.inspect()),
                    Err(err) => println!("Err: {}", err),
                }
            } else {
                buffer = buffer + &line + "\n";
            }
        } else {
            // eof
            break Ok(());
        }
    }
}
