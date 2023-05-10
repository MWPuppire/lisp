extern crate rustyline;

use std::fs::File;
use std::io::prelude::*;
use im::Vector;

use lisp::{Result, LispParser, LispEnv, LispValue, eval_top};

fn main() -> Result<()> {
    let mut rl = rustyline::DefaultEditor::new().unwrap();

    let mut parser = LispParser::new();
    let mut env = LispEnv::new_stdlib();
    let mut complete = true;
    let mut buffer = String::new();

    let args: Vec<String> = std::env::args().collect();
    // *ARGV* symbol doesn't include the program name
    let mut lisp_argv: Vector<LispValue> = args.iter().skip(1).map(|x| LispValue::String(x.clone())).collect();
    lisp_argv.push_front(LispValue::Symbol(LispEnv::symbol_for("list")));
    env.set_by_str("*ARGV*", LispValue::List(lisp_argv));
    env.set_by_str("*host-language*", LispValue::String("Rust".to_owned()));
    let cond = "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))";
    eval_top(&LispParser::parse(cond).unwrap().unwrap(), &mut env)?;

    if args.len() > 1 && &args[1] != "--" {
        let mut file = File::open(&args[1])?;
        file.read_to_string(&mut buffer)?;
        parser.add_tokenize(&buffer)?;
        for val in parser {
            eval_top(&val?, &mut env)?;
        }
        return Ok(());
    }

    loop {
        if let Ok(line) = rl.readline(if complete { "> " } else { "... " }) {
            if let Err(err) = parser.add_tokenize(&line) {
                println!("Err: {}", err);
                parser.clear_tokens();
                parser.advance_line();
                continue;
            }
            complete = parser.is_parse_complete();
            if complete {
                rl.add_history_entry(buffer + &line).unwrap();
                buffer = String::new();
                for val in &mut parser {
                    match val {
                        Ok(tok) => match eval_top(&tok, &mut env) {
                            Ok(out) => println!("{}", out.inspect()),
                            Err(err) => println!("Err: {}", err),
                        },
                        Err(err) => println!("Err: {}", err),
                    }
                }
                parser.advance_line();
            } else {
                buffer = buffer + &line + "\n";
            }
        } else {
            // eof
            break Ok(());
        }
    }
}
