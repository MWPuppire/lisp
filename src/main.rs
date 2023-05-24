extern crate rustyline;

use std::fs::File;
use std::io::prelude::*;
use std::ops::DerefMut;

use lisp::{Result, LispParser, LispEnv, eval};

fn main() -> Result<()> {
    let mut rl = rustyline::DefaultEditor::new().unwrap();

    let mut parser = LispParser::new();
    #[cfg(feature = "io-stdlib")]
    let env = LispEnv::new_stdlib();
    #[cfg(not(feature = "io-stdlib"))]
    let env = LispEnv::new_stdlib_protected();
    let mut buffer = String::new();

    let mut env_writer = env.write();

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 && &args[1] != "--" {
        let mut file = File::open(&args[1]).unwrap();
        file.read_to_string(&mut buffer).unwrap();
        parser.add_tokenize(&buffer)?;
        for val in parser {
            eval(val?, env_writer.deref_mut())?;
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
                    Ok(tok) => match eval(tok, env_writer.deref_mut()) {
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
