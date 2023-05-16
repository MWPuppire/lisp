extern crate rustyline;

use std::fs::File;
use std::io::prelude::*;

use lisp::{Result, LispParser, LispEnv, eval};

fn main() -> Result<()> {
    let mut rl = rustyline::DefaultEditor::new().unwrap();

    let mut parser = LispParser::new();
    let mut env = LispEnv::default();
    let mut buffer = String::new();

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 && &args[1] != "--" {
        let mut file = File::open(&args[1]).unwrap();
        file.read_to_string(&mut buffer).unwrap();
        parser.add_tokenize(&buffer)?;
        for val in parser {
            eval(val?, &mut env)?;
        }
        return Ok(());
    }

    loop {
        if let Ok(line) = rl.readline("> ") {
            if let Err(err) = parser.add_tokenize(&line) {
                println!("Err: {}", err);
                parser.clear_tokens();
                parser.advance_line();
                continue;
            }
            rl.add_history_entry(buffer + &line).unwrap();
            buffer = String::new();
            for val in &mut parser {
                match val {
                    Ok(tok) => match eval(tok, &mut env) {
                        Ok(out) => println!("{}", out.inspect()),
                        Err(err) => println!("Err: {}", err),
                    },
                    Err(err) => println!("Err: {}", err),
                }
            }
            parser.advance_line();
        } else {
            // eof
            break Ok(());
        }
    }
}
