use std::sync::{Arc, RwLock};
use std::fs::File;
use std::io::prelude::*;
use crate::{LispValue, LispError, Result, expect, env::LispEnv, eval::{eval, expand_macros}, parser::LispParser};

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
    env.set(name.to_owned(), val.clone());
    Ok(val)
}

fn lisp_let(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut new_env = env.closure();
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
    let params = args[0].expect_list()?;
    let param_names = params.iter().map(|x| x.expect_symbol().map(|s| s.to_owned())).collect::<Result<Vec<String>>>()?;
    let body = Box::new(args[1].clone());
    let new_env = env.closure();
    Ok(LispValue::Func {
        args: param_names,
        body: body,
        env: new_env,
        is_macro: false,
    })
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
    Ok(LispValue::Bool(val.expect_list()?.len() == 0))
}

fn lisp_count(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    if val.is_nil() {
        Ok(LispValue::Number(0.0))
    } else {
        Ok(LispValue::Number(val.expect_list()?.len() as f64))
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
    std::io::stdin().read_line(&mut buffer)?;
    buffer.pop(); // remove newline
    Ok(LispValue::String(buffer))
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
    let list = arg2.expect_list()?;
    let mut new_list = vec![arg1];
    new_list.extend_from_slice(list);
    Ok(LispValue::List(new_list))
}

fn lisp_concat(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    let mut out = vec![];
    for arg in args {
        let evaled = eval(arg, env)?;
        let list = evaled.expect_list()?;
        out.extend_from_slice(list);
    }
    Ok(LispValue::List(out))
}

fn lisp_nth(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(&args[0], env)?;
    let arg2 = eval(&args[1], env)?;
    let list = arg1.expect_list()?;
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
    let list = arg.expect_list()?;
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
    let list = arg.expect_list()?;
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
        LispValue::Func { is_macro, .. } => Ok(LispValue::Bool(is_macro)),
        _ => Ok(LispValue::Bool(false)),
    }
}

fn lisp_defmacro(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let val = eval(&args[1], env)?;
    let val = match val {
        LispValue::Func { args, body, env, .. } => Ok(LispValue::Func {
            args, body, env, is_macro: true
        }),
        _ => Err(LispError::InvalidDataType("function", val.type_of())),
    }?;
    env.set(name.to_owned(), val.clone());
    Ok(val)
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

/*
fn lisp_try(args: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    if let Ok(catch) = args[1].expect_list() {
        expect!(catch.len() == 3, LispError::IncorrectArguments(2, args.len() - 1));
        if catch[0] != LispValue::Symbol("catch*".to_owned()) {
            return Err(LispError::TryNoCatch);
        }
        let err_name = catch[1].expect_symbol()?;
        let expr = eval(&args[0], env);
        if let Err(LispError::UncaughtException(except)) = expr {
            let mut caught_env = env.closure();
            caught_env.set(err_name.to_owned(), except);
            eval(&catch[2], &mut caught_env)
        } else {
            expr
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
*/

pub fn add_builtins(env: &mut LispEnv) {
    env.bind_func("+", lisp_plus);
    env.bind_func("-", lisp_minus);
    env.bind_func("*", lisp_times);
    env.bind_func("/", lisp_divide);
    env.bind_func("//", lisp_int_divide);
    env.bind_func("def!", lisp_def);
    env.bind_func("let*", lisp_let);
    env.bind_func("if", lisp_if);
    env.bind_func("do", lisp_do);
    env.bind_func("fn*", lisp_fn);
    env.bind_func("=", lisp_equals);
    env.bind_func("prn", lisp_prn);
    env.bind_func("list", lisp_list);
    env.bind_func("list?", lisp_listq);
    env.bind_func("empty?", lisp_emptyq);
    env.bind_func("count", lisp_count);
    env.bind_func("<", lisp_lt);
    env.bind_func("<=", lisp_lte);
    env.bind_func(">", lisp_gt);
    env.bind_func(">=", lisp_gte);
    env.bind_func("not", lisp_not);
    env.bind_func("atom", lisp_atom);
    env.bind_func("atom?", lisp_atomq);
    env.bind_func("deref", lisp_deref);
    env.bind_func("reset!", lisp_reset);
    env.bind_func("swap!", lisp_swap);
    env.bind_func("eval", lisp_eval);
    env.bind_func("readline", lisp_readline);
    env.bind_func("read-string", lisp_read_string);
    env.bind_func("str", lisp_str);
    env.bind_func("slurp", lisp_slurp);
    env.bind_func("load-file", lisp_load_file);
    env.bind_func("typeof", lisp_typeof);
    env.bind_func("quote", lisp_quote);
    env.bind_func("quasiquote", lisp_quasiquote);
    env.bind_func("unquote", lisp_unquote);
    env.bind_func("splice-unquote", lisp_unquote);
    env.bind_func("cons", lisp_cons);
    env.bind_func("concat", lisp_concat);
    env.bind_func("nth", lisp_nth);
    env.bind_func("first", lisp_first);
    env.bind_func("head", lisp_first);
    env.bind_func("rest", lisp_rest);
    env.bind_func("tail", lisp_rest);
    env.bind_func("macro?", lisp_macroq);
    env.bind_func("defmacro!", lisp_defmacro);
    env.bind_func("macroexpand", lisp_macroexpand);
    env.bind_func("inspect", lisp_inspect);
//    env.bind_func("try*", lisp_try);
//    env.bind_func("catch*", lisp_catch);
//    env.bind_func("throw", lisp_throw);
}
