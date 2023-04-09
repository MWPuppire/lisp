use crate::{LispValue, LispError, env::LispEnv, eval::{eval, eval_to_number, eval_to_bool}};

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
    }
}

pub fn lisp_plus(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() < 1 {
        return Err(LispError::IncorrectArguments(1, 0));
    }
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(0.0, |acc, x| acc + x)))
}

pub fn lisp_minus(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() < 1 {
        return Err(LispError::IncorrectArguments(1, 0));
    }
    let first = eval_to_number(&args[0], env)?;
    let nums = eval_list_to_numbers(&args[1..], env)?;
    Ok(LispValue::Number(first - nums.iter().fold(0.0, |acc, x| acc + x)))
}

pub fn lisp_times(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() < 1 {
        return Err(LispError::IncorrectArguments(1, 0));
    }
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(1.0, |acc, x| acc * x)))
}

pub fn lisp_divide(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() != 2 {
        return Err(LispError::IncorrectArguments(2, args.len()));
    }
    let numerator = eval_to_number(&args[0], env)?;
    let denominator = eval_to_number(&args[1], env)?;
    Ok(LispValue::Number(numerator / denominator))
}

pub fn lisp_int_divide(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() != 2 {
        return Err(LispError::IncorrectArguments(2, args.len()));
    }
    let numerator = eval_to_number(&args[0], env)?;
    let denominator = eval_to_number(&args[1], env)?;
    Ok(LispValue::Number((numerator / denominator).trunc()))
}

pub fn lisp_def(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() != 2 {
        return Err(LispError::IncorrectArguments(2, args.len()));
    }
    let name = expect_symbol(&args[0])?;
    let val = eval(&args[1], env)?;
    env.set(name.to_owned(), val.clone());
    Ok(val)
}

pub fn lisp_let(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() != 2 {
        return Err(LispError::IncorrectArguments(2, args.len()));
    }
    let mut new_env = env.new_nested();
    let list = expect_list(&args[0])?;
    let name = expect_symbol(&list[0])?;
    new_env.set(name.to_owned(), list[1].clone());
    eval(&args[1], &mut new_env)
}

pub fn lisp_if(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() < 2 || args.len() > 3 {
        return Err(LispError::IncorrectArguments(2, args.len()));
    }
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
    if args.len() < 1 {
        return Err(LispError::IncorrectArguments(1, 0));
    }
    let mut last = eval(&args[0], env);
    for val in args.iter().skip(1) {
        last = eval(val, env);
    }
    last
}

pub fn lisp_fn(args: &[LispValue], _env: &mut LispEnv) -> Result<LispValue, LispError> {
    if args.len() < 2 {
        return Err(LispError::IncorrectArguments(1, 0));
    }
    let params = expect_list(&args[0])?;
    let param_names = params.iter().map(|x| expect_symbol(x).map(|s| s.to_owned())).collect::<Result<Vec<String>, LispError>>()?;
    let body = Box::new(args[1].clone());
    // let new_env = env.new_nested();
    Ok(LispValue::Func {
        args: param_names,
        body: body,
        // env: new_env,
    })
}
