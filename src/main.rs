extern crate rustyline;

use std::error::Error;
use std::fs;

use lisp::{eval, LispEnv, LispParser};

fn main() -> Result<(), Box<dyn Error>> {
    let mut rl = rustyline::DefaultEditor::new().unwrap();

    let mut parser = LispParser::new();
    #[cfg(feature = "io-stdlib")]
    let env = LispEnv::new_stdlib();
    #[cfg(not(feature = "io-stdlib"))]
    let env = LispEnv::new_stdlib_protected();
    let mut env_writer = env.write();

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 && &args[1] != "--" {
        let file = fs::read_to_string(&args[1])?;
        parser.add_tokenize(&file)?;
        for val in parser {
            eval(val?, &mut env_writer)?;
        }
        return Ok(());
    }

    loop {
        if let Ok(line) = rl.readline("> ") {
            rl.add_history_entry(&line).unwrap();
            if let Err(err) = parser.add_tokenize(&line) {
                println!("Err: {}", err);
                parser.clear_tokens();
                parser.advance_line();
                continue;
            }
            for val in &mut parser {
                match val {
                    Ok(tok) => match eval(tok, &mut env_writer) {
                        Ok(out) => println!("{:#}", out),
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
