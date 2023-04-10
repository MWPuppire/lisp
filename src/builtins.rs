use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::{LispValue, LispError, expect, env::LispEnv, eval::eval, parser::LispParser};

fn eval_list_to_numbers(args: &[LispValue], env: &mut LispEnv) -> Result<Vec<f64>, LispError> {
    args.iter().map(|x| eval(x, env)?.expect_number()).collect()
}

pub fn lisp_plus(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(0.0, |acc, x| acc + x)))
}

pub fn lisp_minus(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let first = eval(&args[0], env)?.expect_number()?;
    let nums = eval_list_to_numbers(&args[1..], env)?;
    Ok(LispValue::Number(first - nums.iter().fold(0.0, |acc, x| acc + x)))
}

pub fn lisp_times(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(1.0, |acc, x| acc * x)))
}

pub fn lisp_divide(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(&args[0], env)?.expect_number()?;
    let denominator = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Number(numerator / denominator))
}

pub fn lisp_int_divide(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(&args[0], env)?.expect_number()?;
    let denominator = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Number((numerator / denominator).trunc()))
}

pub fn lisp_def(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let val = eval(&args[1], env)?;
    env.set(name.to_owned(), val.clone());
    Ok(val)
}

pub fn lisp_let(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut new_env = env.new_nested();
    let list = args[0].expect_list()?;
    let name = list[0].expect_symbol()?;
    new_env.set(name.to_owned(), list[1].clone());
    eval(&args[1], &mut new_env)
}

pub fn lisp_if(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 1 && args.len() < 4, LispError::IncorrectArguments(2, args.len()));
    let pred = eval(&args[0], env)?.expect_bool()?;
    if pred {
        eval(&args[1], env)
    } else {
        if args.len() == 3 {
            eval(&args[2], env)
        } else {
            Ok(LispValue::Nil)
        }
    }
}

pub fn lisp_do(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let mut last = eval(&args[0], env)?;
    for val in args.iter().skip(1) {
        last = eval(val, env)?;
    }
    Ok(last)
}

pub fn lisp_fn(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let params = args[0].expect_list()?;
    let param_names = params.iter().map(|x| x.expect_symbol().map(|s| s.to_owned())).collect::<Result<Vec<String>, LispError>>()?;
    let body = Box::new(args[1].clone());
    let new_env = env.new_nested();
    Ok(LispValue::Func {
        args: param_names,
        body: body,
        env: new_env,
    })
}

pub fn lisp_equals(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?;
    let y = eval(&args[1], env)?;
    Ok(LispValue::Bool(x == y))
}

pub fn lisp_prn(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    for val in args.iter() {
        let val = eval(val, env)?;
        print!("{} ", val);
    }
    println!();
    Ok(LispValue::Nil)
}

pub fn lisp_list(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    let vals: Vec<LispValue> = args.iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>, LispError>>()?;
    Ok(LispValue::List(vals))
}

pub fn lisp_listq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(match val {
        LispValue::List(_) => true,
        _ => false,
    }))
}

pub fn lisp_emptyq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(val.expect_list()?.len() == 0))
}

pub fn lisp_count(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Number(val.expect_list()?.len() as f64))
}

pub fn lisp_lt(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x < y))
}

pub fn lisp_lte(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x <= y))
}

pub fn lisp_gt(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x > y))
}

pub fn lisp_gte(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x >= y))
}

pub fn lisp_not(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?.expect_bool()?;
    Ok(LispValue::Bool(!x))
}

pub fn lisp_atom(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    Ok(LispValue::Atom(Rc::new(RefCell::new(x))))
}

pub fn lisp_atomq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(match val {
        LispValue::Atom(_) => true,
        _ => false,
    }))
}

pub fn lisp_deref(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    let atom = val.expect_atom()?;
    let out = atom.borrow().clone();
    Ok(out)
}

pub fn lisp_reset(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let val = eval(&args[0], env)?;
    let atom = val.expect_atom()?;
    let val = eval(&args[1], env)?;
    *atom.borrow_mut() = val.clone();
    Ok(val)
}

pub fn lisp_swap(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let val = eval(&args[0], env)?;
    let atom = val.expect_atom()?;
    let val = eval(&args[1], env)?;
    let list = LispValue::List(vec![
        val,
        LispValue::List(vec![
            LispValue::Symbol("deref".to_owned()),
            LispValue::Atom(atom.clone()),
        ]),
    ]);
    let out_val = eval(&list, env)?;
    *atom.borrow_mut() = out_val.clone();
    Ok(out_val)
}

pub fn lisp_eval(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    eval(&arg, env)
}

pub fn lisp_readline(_args: &[LispValue], _env: &mut LispEnv) -> Result<LispValue, LispError> {
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).map_err(|x| LispError::OSFailure(x))?;
    buffer.pop(); // remove newline
    Ok(LispValue::String(buffer))
}

pub fn lisp_read_string(args: &[LispValue], _env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let mut parser = LispParser::new();
    parser.add_tokenize(args[0].expect_string()?);
    parser.next()
}

pub fn lisp_str(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let mut buffer = String::new();
    for arg in args.iter() {
        let x = eval(arg, env)?;
        buffer.push_str(x.expect_string()?);
    }
    Ok(LispValue::String(buffer))
}

pub fn create_builtins() -> HashMap<String, LispValue> {
    HashMap::from([
        ("true".to_owned(), LispValue::Bool(true)),
        ("false".to_owned(), LispValue::Bool(false)),
        ("nil".to_owned(), LispValue::Nil),
        ("+".to_owned(), LispValue::BuiltinFunc(lisp_plus)),
        ("-".to_owned(), LispValue::BuiltinFunc(lisp_minus)),
        ("*".to_owned(), LispValue::BuiltinFunc(lisp_times)),
        ("/".to_owned(), LispValue::BuiltinFunc(lisp_divide)),
        ("//".to_owned(), LispValue::BuiltinFunc(lisp_int_divide)),
        ("def!".to_owned(), LispValue::BuiltinFunc(lisp_def)),
        ("let*".to_owned(), LispValue::BuiltinFunc(lisp_let)),
        ("if".to_owned(), LispValue::BuiltinFunc(lisp_if)),
        ("do".to_owned(), LispValue::BuiltinFunc(lisp_do)),
        ("fn*".to_owned(), LispValue::BuiltinFunc(lisp_fn)),
        ("=".to_owned(), LispValue::BuiltinFunc(lisp_equals)),
        ("prn".to_owned(), LispValue::BuiltinFunc(lisp_prn)),
        ("list".to_owned(), LispValue::BuiltinFunc(lisp_list)),
        ("list?".to_owned(), LispValue::BuiltinFunc(lisp_listq)),
        ("empty?".to_owned(), LispValue::BuiltinFunc(lisp_emptyq)),
        ("count".to_owned(), LispValue::BuiltinFunc(lisp_count)),
        ("<".to_owned(), LispValue::BuiltinFunc(lisp_lt)),
        ("<=".to_owned(), LispValue::BuiltinFunc(lisp_lte)),
        (">".to_owned(), LispValue::BuiltinFunc(lisp_gt)),
        (">=".to_owned(), LispValue::BuiltinFunc(lisp_gte)),
        ("not".to_owned(), LispValue::BuiltinFunc(lisp_not)),
        ("atom".to_owned(), LispValue::BuiltinFunc(lisp_atom)),
        ("atomq".to_owned(), LispValue::BuiltinFunc(lisp_atomq)),
        ("deref".to_owned(), LispValue::BuiltinFunc(lisp_deref)),
        ("reset!".to_owned(), LispValue::BuiltinFunc(lisp_reset)),
        ("swap!".to_owned(), LispValue::BuiltinFunc(lisp_swap)),
        ("eval".to_owned(), LispValue::BuiltinFunc(lisp_eval)),
        ("readline".to_owned(), LispValue::BuiltinFunc(lisp_readline)),
        ("read-string".to_owned(), LispValue::BuiltinFunc(lisp_read_string)),
        ("str".to_owned(), LispValue::BuiltinFunc(lisp_str)),
    ])
}
