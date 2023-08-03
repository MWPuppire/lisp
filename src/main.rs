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

    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 && &args[1] != "--" {
        let path = std::path::Path::new(&args[1]);
        if !path.exists() {
            return Err(format!("file {} not found", path.display()).into());
        } else if path.is_dir() {
            return Err(format!("{} is a directory", path.display()).into());
        }
        let file = if let Some(dir) = path.parent() {
            // so the script can `load-file` relative paths
            std::env::set_current_dir(dir).unwrap();
            fs::read_to_string(path.file_name().unwrap())?
        } else {
            fs::read_to_string(path)?
        };
        parser.add_tokenize(&file)?;
        for val in parser {
            eval(val?, &env)?;
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
                    Ok(tok) => match eval(tok, &env) {
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
