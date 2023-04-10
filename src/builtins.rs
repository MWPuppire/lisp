use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::{LispValue, LispError, env::LispEnv, eval::{eval, eval_to_number, eval_to_bool}};

macro_rules! expect {
    ($cond:expr , $err:expr) => {
        if !$cond {
            return Err($err);
        }
    }
}

fn eval_list_to_numbers(args: &[LispValue], env: &mut LispEnv) -> Result<Vec<f64>, LispError> {
    args.iter().map(|x| eval_to_number(x, env)).collect()
}

fn expect_symbol(arg: &LispValue) -> Result<&str, LispError> {
    match arg {
        LispValue::Symbol(s) => Ok(s),
        LispValue::List(_) => Err(LispError::InvalidDataType("symbol", "list")),
        LispValue::BuiltinFunc(_) => Err(LispError::InvalidDataType("symbol", "function")),
        LispValue::String(_) => Err(LispError::InvalidDataType("symbol", "string")),
        LispValue::Number(_) => Err(LispError::InvalidDataType("symbol", "number")),
        LispValue::Bool(_) => Err(LispError::InvalidDataType("symbol", "bool")),
        LispValue::Nil => Err(LispError::InvalidDataType("symbol", "nil")),
        LispValue::Func { .. } => Err(LispError::InvalidDataType("symbol", "function")),
        LispValue::Atom(_) => Err(LispError::InvalidDataType("symbol", "atom")),
    }
}

fn expect_list(arg: &LispValue) -> Result<&[LispValue], LispError> {
    match arg {
        LispValue::Symbol(_) => Err(LispError::InvalidDataType("list", "symbol")),
        LispValue::List(l) => Ok(&l),
        LispValue::BuiltinFunc(_) => Err(LispError::InvalidDataType("list", "function")),
        LispValue::String(_) => Err(LispError::InvalidDataType("list", "string")),
        LispValue::Number(_) => Err(LispError::InvalidDataType("list", "number")),
        LispValue::Bool(_) => Err(LispError::InvalidDataType("list", "bool")),
        LispValue::Nil => Err(LispError::InvalidDataType("list", "nil")),
        LispValue::Func { .. } => Err(LispError::InvalidDataType("list", "function")),
        LispValue::Atom(_) => Err(LispError::InvalidDataType("list", "atom")),
    }
}

fn expect_atom(arg: &LispValue) -> Result<Rc<RefCell<LispValue>>, LispError> {
    match arg {
        LispValue::Symbol(_) => Err(LispError::InvalidDataType("atom", "symbol")),
        LispValue::List(_) => Err(LispError::InvalidDataType("atom", "list")),
        LispValue::BuiltinFunc(_) => Err(LispError::InvalidDataType("atom", "function")),
        LispValue::String(_) => Err(LispError::InvalidDataType("atom", "string")),
        LispValue::Number(_) => Err(LispError::InvalidDataType("atom", "number")),
        LispValue::Bool(_) => Err(LispError::InvalidDataType("atom", "bool")),
        LispValue::Nil => Err(LispError::InvalidDataType("atom", "nil")),
        LispValue::Func { .. } => Err(LispError::InvalidDataType("atom", "function")),
        LispValue::Atom(x) => Ok(x.clone()),
    }
}

pub fn lisp_plus(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(0.0, |acc, x| acc + x)))
}

pub fn lisp_minus(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let first = eval_to_number(&args[0], env)?;
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
    let numerator = eval_to_number(&args[0], env)?;
    let denominator = eval_to_number(&args[1], env)?;
    Ok(LispValue::Number(numerator / denominator))
}

pub fn lisp_int_divide(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval_to_number(&args[0], env)?;
    let denominator = eval_to_number(&args[1], env)?;
    Ok(LispValue::Number((numerator / denominator).trunc()))
}

pub fn lisp_def(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = expect_symbol(&args[0])?;
    let val = eval(&args[1], env)?;
    env.set(name.to_owned(), val.clone());
    Ok(val)
}

pub fn lisp_let(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut new_env = env.new_nested();
    let list = expect_list(&args[0])?;
    let name = expect_symbol(&list[0])?;
    new_env.set(name.to_owned(), list[1].clone());
    eval(&args[1], &mut new_env)
}

pub fn lisp_if(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() > 1 && args.len() < 4, LispError::IncorrectArguments(2, args.len()));
    let pred = eval_to_bool(&args[0], env)?;
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
    let params = expect_list(&args[0])?;
    let param_names = params.iter().map(|x| expect_symbol(x).map(|s| s.to_owned())).collect::<Result<Vec<String>, LispError>>()?;
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
    Ok(LispValue::Bool(expect_list(&val)?.len() == 0))
}

pub fn lisp_count(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Number(expect_list(&val)?.len() as f64))
}

pub fn lisp_lt(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_to_number(&args[0], env)?;
    let y = eval_to_number(&args[1], env)?;
    Ok(LispValue::Bool(x < y))
}

pub fn lisp_lte(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_to_number(&args[0], env)?;
    let y = eval_to_number(&args[1], env)?;
    Ok(LispValue::Bool(x <= y))
}

pub fn lisp_gt(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_to_number(&args[0], env)?;
    let y = eval_to_number(&args[1], env)?;
    Ok(LispValue::Bool(x > y))
}

pub fn lisp_gte(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_to_number(&args[0], env)?;
    let y = eval_to_number(&args[1], env)?;
    Ok(LispValue::Bool(x >= y))
}

pub fn lisp_not(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval_to_bool(&args[0], env)?;
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
    let atom = expect_atom(&val)?;
    let out = atom.borrow().clone();
    Ok(out)
}

pub fn lisp_reset(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let val = eval(&args[0], env)?;
    let atom = expect_atom(&val)?;
    let val = eval(&args[1], env)?;
    *atom.borrow_mut() = val.clone();
    Ok(val)
}

pub fn lisp_swap(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let val = eval(&args[0], env)?;
    let atom = expect_atom(&val)?;
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

pub fn lisp_read_string(_args: &[LispValue], _env: &mut LispEnv) -> Result<LispValue, LispError> {
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).map_err(|x| LispError::OSFailure(x))?;
    buffer.pop(); // remove newline
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
        ("read-string".to_owned(), LispValue::BuiltinFunc(lisp_read_string)),
    ])
}
