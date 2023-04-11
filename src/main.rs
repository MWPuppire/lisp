extern crate lazy_static;
extern crate regex;
extern crate thiserror;
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

fn main() -> Result<()> {
    let mut rl = rustyline::DefaultEditor::new().unwrap();

    let mut parser = LispParser::new();
    let mut env = LispEnv::new_stdlib();
    let mut complete = true;
    let mut buffer = String::new();

    let args: Vec<String> = std::env::args().collect();
    let lisp_argv = args.iter().map(|x| LispValue::String(x.clone())).collect();
    env.set("*ARGV*".to_owned(), LispValue::List(lisp_argv));
    env.set("*host-language*".to_owned(), LispValue::String("Rust".to_owned()));
    parser.add_tokenize("(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))");
    eval(&parser.next()?, &mut env)?;

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
                rl.add_history_entry(buffer + &line).unwrap();
                buffer = String::new();
                while parser.has_tokens() {
                    match parser.next() {
                        Ok(tok) => match eval(&tok, &mut env) {
                            Ok(out) => println!("{}", out.inspect()),
                            Err(err) => println!("Err: {}", err),
                        },
                        Err(err) => println!("Err: {}", err),
                    }
                }
                parser.next_line();
            } else {
                buffer = buffer + &line + "\n";
            }
        } else {
            // eof
            break Ok(());
        }
    }
}
