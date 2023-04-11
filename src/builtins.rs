use std::sync::{Arc, RwLock};
use std::fs::File;
use std::io::prelude::*;
use im::{hashmap, HashMap};
use lazy_static::lazy_static;
use crate::{LispValue, LispError, Result, expect};
use crate::env::LispEnv;
use crate::eval::{eval, expand_macros};
use crate::parser::LispParser;
use crate::util::{LispFunc, ExternLispFunc};

fn eval_list_to_numbers(args: &[LispValue], env: &mut LispEnv) -> Result<Vec<f64>> {
    args.iter().map(|x| eval(x, env)?.expect_number()).collect()
}

fn lisp_plus(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(0.0, |acc, x| acc + x)))
}

fn lisp_minus(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let first = eval(&args[0], env)?.expect_number()?;
    let nums = eval_list_to_numbers(&args[1..], env)?;
    Ok(LispValue::Number(first - nums.iter().fold(0.0, |acc, x| acc + x)))
}

fn lisp_times(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(1.0, |acc, x| acc * x)))
}

fn lisp_divide(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(&args[0], env)?.expect_number()?;
    let denominator = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Number(numerator / denominator))
}

fn lisp_int_divide(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(&args[0], env)?.expect_number()?;
    let denominator = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Number((numerator / denominator).trunc()))
}

fn lisp_def(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let val = eval(&args[1], env)?;
    if !env.set(name.to_owned(), val.clone()) {
        Err(LispError::AlreadyExists)
    } else {
        Ok(val)
    }
}

fn lisp_let(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut new_env = env.new_nested();
    let list = args[0].expect_list()?;
    expect!(list.len() & 1 == 0, LispError::MissingBinding);
    for pair in list.chunks_exact(2) {
        let name = pair[0].expect_symbol()?;
        let val = eval(&pair[1], env)?;
        new_env.set(name.to_owned(), val);
    }
    eval(&args[1], &mut new_env)
}

fn lisp_if(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 1 && args.len() < 4, LispError::IncorrectArguments(2, args.len()));
    let pred = eval(&args[0], env)?.truthiness();
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

fn lisp_do(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let mut last = eval(&args[0], env)?;
    for val in args.iter().skip(1) {
        last = eval(val, env)?;
    }
    Ok(last)
}

fn lisp_fn(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let args_list = args[0].expect_list()?;
    let variadic = if args_list.len() > 0 {
        let last = &args_list[args_list.len() - 1];
        match last {
            LispValue::VariadicSymbol(_) => Ok(true),
            LispValue::Symbol(_) => Ok(false),
            _ => Err(LispError::InvalidDataType("symbol", last.type_of())),
        }?
    } else {
        false
    };
    let arg_names = args_list.iter().map(|x| x.expect_symbol().map(|x| x.to_owned())).collect::<Result<Vec<String>>>()?;
    let body = args[1].clone();
    let closure = env.make_closure();
    Ok(LispValue::Func(Box::new(LispFunc {
        args: arg_names,
        body,
        closure,
        variadic,
        is_macro: false,
    })))
}

fn lisp_equals(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?;
    let y = eval(&args[1], env)?;
    Ok(LispValue::Bool(x == y))
}

fn lisp_prn(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    // args.len() != 0, so `split_list` isn't `None`
    let (last, args) = unsafe { args.split_last().unwrap_unchecked() };
    for val in args.iter() {
        let val = eval(val, env)?;
        print!("{} ", val);
    }
    println!("{}", eval(last, env)?);
    Ok(LispValue::Nil)
}

fn lisp_list(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    let vals: Vec<LispValue> = args.iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?;
    Ok(LispValue::List(vals))
}

fn lisp_listq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(match val {
        LispValue::List(_) => true,
        _ => false,
    }))
}

fn lisp_emptyq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(val.expect_list_or_vec()?.len() == 0))
}

fn lisp_count(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    if val.is_nil() {
        Ok(LispValue::Number(0.0))
    } else {
        Ok(LispValue::Number(val.expect_list_or_vec()?.len() as f64))
    }
}

fn lisp_lt(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x < y))
}

fn lisp_lte(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x <= y))
}

fn lisp_gt(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x > y))
}

fn lisp_gte(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x >= y))
}

fn lisp_not(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?.truthiness();
    Ok(LispValue::Bool(!x))
}

fn lisp_atom(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    Ok(LispValue::Atom(Arc::new(RwLock::new(x))))
}

fn lisp_atomq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(match val {
        LispValue::Atom(_) => true,
        _ => false,
    }))
}

fn lisp_deref(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    let atom = val.expect_atom()?;
    let out = atom.read().unwrap().clone();
    Ok(out)
}

fn lisp_reset(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let val = eval(&args[0], env)?;
    let atom = val.expect_atom()?;
    let val = eval(&args[1], env)?;
    *atom.write().unwrap() = val.clone();
    Ok(val)
}

fn lisp_swap(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let val = eval(&args[0], env)?;
    let atom = val.expect_atom()?;
    let val = eval(&args[1], env)?;
    let mut list = vec![
        val,
        LispValue::List(vec![
            LispValue::Symbol("deref".to_owned()),
            LispValue::Atom(atom.clone()),
        ]),
    ];
    list.extend_from_slice(&args[2..]);
    let out_val = eval(&LispValue::List(list), env)?;
    *atom.write().unwrap() = out_val.clone();
    Ok(out_val)
}

fn lisp_eval(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    eval(&arg, env)
}

fn lisp_readline(_args: &[LispValue], _env: &mut LispEnv) -> Result<LispValue> {
    let mut buffer = String::new();
    let len = std::io::stdin().read_line(&mut buffer)?;
    if len == 0 {
        // eof
        Ok(LispValue::Nil)
    } else {
        buffer.pop(); // remove newline
        Ok(LispValue::String(buffer))
    }
}

fn lisp_read_string(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let mut parser = LispParser::new();
    let arg = eval(&args[0], env)?;
    parser.add_tokenize(arg.expect_string()?);
    parser.next()
}

fn lisp_str(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let mut buffer = String::new();
    for arg in args.iter() {
        let x = eval(arg, env)?;
        buffer.push_str(&x.to_string());
    }
    Ok(LispValue::String(buffer))
}

fn lisp_slurp(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    let mut buffer = String::new();
    let mut f = File::open(x.expect_string()?)?;
    f.read_to_string(&mut buffer)?;
    Ok(LispValue::String(buffer))
}

fn lisp_load_file(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    let mut buffer = String::new();
    let mut f = File::open(x.expect_string()?)?;
    let mut parser = LispParser::new();
    f.read_to_string(&mut buffer)?;
    parser.add_tokenize(&buffer);
    let mut last = LispValue::Nil;
    while parser.has_tokens() {
        last = eval(&parser.next()?, env)?;
    }
    Ok(last)
}

fn lisp_typeof(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    Ok(LispValue::String(x.type_of().to_owned()))
}

fn lisp_quote(args: &[LispValue], _env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    Ok(args[0].clone())
}

fn inner_quasiquote(arg: &LispValue, env: &mut LispEnv) -> Result<(LispValue, bool)> {
    match arg {
        LispValue::List(l) => {
            if l.len() == 0 {
                Ok((arg.clone(), false))
            } else if l[0] == LispValue::Symbol("unquote".to_owned()) {
                expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                Ok((eval(&l[1], env)?, false))
            } else if l[0] == LispValue::Symbol("splice-unquote".to_owned()) {
                expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                Ok((eval(&l[1], env)?, true))
            } else {
                let mut out = vec![];
                for val in l {
                    let (new_val, inplace) = inner_quasiquote(val, env)?;
                    if inplace {
                        out.extend_from_slice(new_val.expect_list()?);
                    } else {
                        out.push(new_val);
                    }
                }
                Ok((LispValue::List(out), false))
            }
        },
        _ => Ok((arg.clone(), false)),
    }
}

fn lisp_quasiquote(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    if let Ok(list) = args[0].expect_list() {
        if list.len() == 0 {
            return Ok(LispValue::List(vec![]));
        }
        if list[0] == LispValue::Symbol("unquote".to_owned()) || list[0] == LispValue::Symbol("splice-unquote".to_owned()) {
            expect!(list.len() == 2, LispError::IncorrectArguments(1, list.len() - 1));
            return eval(&list[1], env);
        }
        let mut out = vec![];
        for val in list {
            let (new_val, inplace) = inner_quasiquote(val, env)?;
            if inplace {
                out.extend_from_slice(new_val.expect_list()?);
            } else {
                out.push(new_val);
            }
        }
        Ok(LispValue::List(out))
    } else {
        Ok(args[0].clone())
    }
}

// `lisp_quasiquote` handles the `unquote` and `splice-unquote` methods itself,
// so this can only be called outside `quasiquote` (which is an error)
fn lisp_unquote(_args: &[LispValue], _env: &mut LispEnv) -> Result<LispValue> {
    Err(LispError::OnlyInQuasiquote)
}

fn lisp_cons(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(&args[0], env)?;
    let arg2 = eval(&args[1], env)?;
    let list = arg2.expect_list_or_vec()?;
    let mut new_list = vec![arg1];
    new_list.extend_from_slice(list);
    Ok(LispValue::List(new_list))
}

fn lisp_concat(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    let mut out = vec![];
    for arg in args {
        let evaled = eval(arg, env)?;
        let list = evaled.expect_list_or_vec()?;
        out.extend_from_slice(list);
    }
    Ok(LispValue::List(out))
}

fn lisp_nth(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(&args[0], env)?;
    let arg2 = eval(&args[1], env)?;
    let list = arg1.expect_list_or_vec()?;
    let idx = arg2.expect_number()? as usize;
    expect!(list.len() > idx, LispError::IndexOutOfRange(idx));
    Ok(list[idx].clone())
}

fn lisp_first(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    if arg.is_nil() {
        return Ok(LispValue::Nil);
    }
    let list = arg.expect_list_or_vec()?;
    if list.len() == 0 {
        Ok(LispValue::Nil)
    } else {
        Ok(list[0].clone())
    }
}

fn lisp_rest(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    if arg.is_nil() {
        return Ok(LispValue::List(vec![]));
    }
    let list = arg.expect_list_or_vec()?;
    if list.len() < 2 {
        Ok(LispValue::List(vec![]))
    } else {
        Ok(LispValue::List(list[1..].to_owned()))
    }
}

fn lisp_macroq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    match arg {
        LispValue::Func(f) => Ok(LispValue::Bool(f.is_macro)),
        _ => Ok(LispValue::Bool(false)),
    }
}

fn lisp_defmacro(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let mut val = eval(&args[1], env)?;
    match val {
        LispValue::Func(ref mut f) => {
            f.is_macro = true;
        },
        _ => return Err(LispError::InvalidDataType("function", val.type_of())),
    }
    if !env.set(name.to_owned(), val.clone()) {
        Err(LispError::AlreadyExists)
    } else {
        Ok(val)
    }
}

fn lisp_macroexpand(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    expand_macros(&args[0], env)
}

fn lisp_inspect(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    // args.len() != 0, so `split_list` isn't `None`
    let (last, args) = unsafe { args.split_last().unwrap_unchecked() };
    for val in args.iter() {
        let val = eval(val, env)?;
        print!("{} ", val.inspect());
    }
    println!("{}", eval(last, env)?.inspect());
    Ok(LispValue::Nil)
}

fn lisp_try(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    if let Ok(catch) = args[1].expect_list() {
        expect!(catch.len() == 3, LispError::IncorrectArguments(2, args.len() - 1));
        if catch[0] != LispValue::Symbol("catch*".to_owned()) {
            return Err(LispError::TryNoCatch);
        }
        let err_name = catch[1].expect_symbol()?;
        let mut caught_env = env.new_nested();
        match eval(&args[0], env) {
            Ok(x) => Ok(x),
            Err(LispError::UncaughtException(except)) => {
                caught_env.set(err_name.to_owned(), except);
                eval(&catch[2], &mut caught_env)
            },
            Err(err) => {
                let s = err.to_string();
                caught_env.set(err_name.to_owned(), LispValue::String(s));
                eval(&catch[2], &mut caught_env)
            }
        }
    } else {
        Err(LispError::TryNoCatch)
    }
}

// `lisp_try` handles `catch`, which can't be used outside `try`,
// so this is an error
fn lisp_catch(_args: &[LispValue], _env: &mut LispEnv) -> Result<LispValue> {
    Err(LispError::OnlyInTry)
}

fn lisp_throw(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Err(LispError::UncaughtException(arg))
}

fn lisp_nilq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(arg.is_nil()))
}

fn lisp_trueq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Bool(b) => b,
        _ => false,
    }))
}

fn lisp_falseq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Bool(b) => !b,
        _ => false,
    }))
}

fn lisp_symbolq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Symbol(_) => true,
        _ => false,
    }))
}

fn lisp_symbol(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    let s = arg.expect_string()?;
    Ok(LispValue::Symbol(s.to_owned()))
}

fn lisp_vector(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    let vals: Vec<LispValue> = args.iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?;
    Ok(LispValue::Vector(vals))
}

fn lisp_vectorq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Vector(_) => true,
        _ => false,
    }))
}

fn lisp_keyword(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    let s = arg.expect_string()?;
    Ok(LispValue::Keyword(s.to_owned()))
}

fn lisp_keywordq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Keyword(_) => true,
        _ => false,
    }))
}

fn lisp_hashmap(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() & 1 == 0, LispError::MissingBinding);
    let pairs = args.chunks_exact(2).map(|x| {
        let k = eval(&x[0], env)?;
        let v = eval(&x[1], env)?;
        Ok((k, v))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    Ok(LispValue::Map(pairs.into()))
}

fn lisp_mapq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Map(_) => true,
        _ => false,
    }))
}

fn lisp_sequentialq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::List(_) => true,
        LispValue::Vector(_) => true,
        _ => false,
    }))
}

fn lisp_assoc(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() & 1 == 1, LispError::MissingBinding);
    let base_map = eval(&args[0], env)?;
    let base_map = base_map.expect_hashmap()?;
    let pairs = args[1..].chunks_exact(2).map(|x| {
        let k = eval(&x[0], env)?;
        let v = eval(&x[1], env)?;
        Ok((k, v))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    let new_map: HashMap<LispValue, LispValue> = pairs.into();
    Ok(LispValue::Map(new_map.union(base_map.clone())))
}

fn lisp_dissoc(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let base_map = eval(&args[0], env)?;
    let base_map = base_map.expect_hashmap()?;
    let keys = args[1..].iter().map(|x| {
        Ok((eval(&x, env)?, LispValue::Nil))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    Ok(LispValue::Map(base_map.clone().difference(keys.into())))
}

fn lisp_get(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval(&args[0], env)?;
    let map = map.expect_hashmap()?;
    let key = eval(&args[1], env)?;
    if let Some(val) = map.get(&key) {
        Ok(val.clone())
    } else {
        Ok(LispValue::Nil)
    }
}

fn lisp_containsq(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval(&args[0], env)?;
    let map = map.expect_hashmap()?;
    let key = eval(&args[1], env)?;
    Ok(LispValue::Bool(map.contains_key(&key)))
}

fn lisp_keys(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval(&args[0], env)?;
    let map = map.expect_hashmap()?;
    let keys = map.keys().map(|x| x.clone()).collect();
    Ok(LispValue::List(keys))
}

fn lisp_vals(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval(&args[0], env)?;
    let map = map.expect_hashmap()?;
    let vals = map.values().map(|x| x.clone()).collect();
    Ok(LispValue::List(vals))
}

fn lisp_apply(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let f = eval(&args[0], env)?;
    let last_idx = args.len() - 1;
    let list = eval(&args[last_idx], env)?;
    let list = list.expect_list_or_vec()?;
    let mut full_list = vec![f];
    full_list.extend_from_slice(&args[1..last_idx]);
    full_list.extend_from_slice(list);
    eval(&LispValue::List(full_list), env)
}

fn lisp_map(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let f = eval(&args[0], env)?;
    let list = eval(&args[1], env)?;
    let list = list.expect_list_or_vec()?;
    let out = list.iter().map(|x| {
        let list = LispValue::List(vec![f.clone(), x.clone()]);
        eval(&list, env)
    }).collect::<Result<Vec<LispValue>>>()?;
    Ok(LispValue::List(out))
}

macro_rules! lisp_func {
    ($name:expr, $f:expr) => { LispValue::BuiltinFunc { name: $name, f: ExternLispFunc($f) } }
}
lazy_static! {
    pub static ref BUILTINS: HashMap<String, LispValue> = {
        hashmap!{
            "+".to_owned() => lisp_func!("+", lisp_plus),
            "-".to_owned() => lisp_func!("-", lisp_minus),
            "*".to_owned() => lisp_func!("*", lisp_times),
            "/".to_owned() => lisp_func!("/", lisp_divide),
            "//".to_owned() => lisp_func!("//", lisp_int_divide),
            "def!".to_owned() => lisp_func!("def!", lisp_def),
            "let*".to_owned() => lisp_func!("let*", lisp_let),
            "if".to_owned() => lisp_func!("if", lisp_if),
            "do".to_owned() => lisp_func!("do", lisp_do),
            "fn*".to_owned() => lisp_func!("fn*", lisp_fn),
            "=".to_owned() => lisp_func!("=", lisp_equals),
            "prn".to_owned() => lisp_func!("prn", lisp_prn),
            "list".to_owned() => lisp_func!("list", lisp_list),
            "list?".to_owned() => lisp_func!("list?", lisp_listq),
            "empty?".to_owned() => lisp_func!("empty?", lisp_emptyq),
            "count".to_owned() => lisp_func!("count", lisp_count),
            "<".to_owned() => lisp_func!("<", lisp_lt),
            "<=".to_owned() => lisp_func!("<=", lisp_lte),
            ">".to_owned() => lisp_func!(">", lisp_gt),
            ">=".to_owned() => lisp_func!(">=", lisp_gte),
            "not".to_owned() => lisp_func!("not", lisp_not),
            "atom".to_owned() => lisp_func!("atom", lisp_atom),
            "atom?".to_owned() => lisp_func!("atom?", lisp_atomq),
            "deref".to_owned() => lisp_func!("deref", lisp_deref),
            "reset!".to_owned() => lisp_func!("reset!", lisp_reset),
            "swap!".to_owned() => lisp_func!("swap!", lisp_swap),
            "eval".to_owned() => lisp_func!("eval", lisp_eval),
            "readline".to_owned() => lisp_func!("readline", lisp_readline),
            "read-string".to_owned() => lisp_func!("read-string", lisp_read_string),
            "str".to_owned() => lisp_func!("str", lisp_str),
            "slurp".to_owned() => lisp_func!("slurp", lisp_slurp),
            "load-file".to_owned() => lisp_func!("load-file", lisp_load_file),
            "typeof".to_owned() => lisp_func!("typeof", lisp_typeof),
            "quote".to_owned() => lisp_func!("quote", lisp_quote),
            "quasiquote".to_owned() => lisp_func!("quasiquote", lisp_quasiquote),
            "unquote".to_owned() => lisp_func!("unquote", lisp_unquote),
            "splice-unquote".to_owned() => lisp_func!("splice-unquote", lisp_unquote),
            "cons".to_owned() => lisp_func!("cons", lisp_cons),
            "concat".to_owned() => lisp_func!("concat", lisp_concat),
            "nth".to_owned() => lisp_func!("nth", lisp_nth),
            "first".to_owned() => lisp_func!("first", lisp_first),
            "head".to_owned() => lisp_func!("head", lisp_first),
            "rest".to_owned() => lisp_func!("rest", lisp_rest),
            "tail".to_owned() => lisp_func!("tail", lisp_rest),
            "macro?".to_owned() => lisp_func!("macro?", lisp_macroq),
            "defmacro!".to_owned() => lisp_func!("defmacro!", lisp_defmacro),
            "macroexpand".to_owned() => lisp_func!("macroexpand", lisp_macroexpand),
            "inspect".to_owned() => lisp_func!("inspect", lisp_inspect),
            "try*".to_owned() => lisp_func!("try*", lisp_try),
            "catch*".to_owned() => lisp_func!("catch*", lisp_catch),
            "throw".to_owned() => lisp_func!("throw", lisp_throw),
            "nil?".to_owned() => lisp_func!("nil?", lisp_nilq),
            "true?".to_owned() => lisp_func!("true?", lisp_trueq),
            "false?".to_owned() => lisp_func!("false?", lisp_falseq),
            "symbol?".to_owned() => lisp_func!("symbol?", lisp_symbolq),
            "symbol".to_owned() => lisp_func!("symbol", lisp_symbol),
            "vector".to_owned() => lisp_func!("vector", lisp_vector),
            "vector?".to_owned() => lisp_func!("vector?", lisp_vectorq),
            "keyword".to_owned() => lisp_func!("keyword", lisp_keyword),
            "keyword?".to_owned() => lisp_func!("keyword?", lisp_keywordq),
            "hash-map".to_owned() => lisp_func!("hash-map", lisp_hashmap),
            "map?".to_owned() => lisp_func!("map?", lisp_mapq),
            "sequential?".to_owned() => lisp_func!("sequential?", lisp_sequentialq),
            "assoc".to_owned() => lisp_func!("assoc", lisp_assoc),
            "dissoc".to_owned() => lisp_func!("dissoc", lisp_dissoc),
            "get".to_owned() => lisp_func!("get", lisp_get),
            "contains?".to_owned() => lisp_func!("contains?", lisp_containsq),
            "keys".to_owned() => lisp_func!("keys", lisp_keys),
            "vals".to_owned() => lisp_func!("vals", lisp_vals),
            "apply".to_owned() => lisp_func!("apply", lisp_apply),
            "map".to_owned() => lisp_func!("map", lisp_map),
        }
    };
}
