use std::sync::{Arc, RwLock};
use std::fs::File;
use std::io::prelude::*;
use std::time::Instant;
use std::collections::VecDeque;
use im::{hashmap, HashMap};
use lazy_static::lazy_static;
use crate::{LispValue, LispError, Result, expect};
use crate::env::LispEnv;
use crate::eval::{eval, expand_macros};
use crate::parser::LispParser;
use crate::util::{LispFunc, ExternLispFunc};

fn eval_list_to_numbers(args: &VecDeque<LispValue>, env: &mut LispEnv) -> Result<Vec<f64>> {
    args.iter().map(|x| eval(x, env)?.expect_number()).collect()
}

fn lisp_plus(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(0.0, |acc, x| acc + x)))
}

fn lisp_minus(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let first = eval(&first, env)?.expect_number()?;
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(first - nums.iter().fold(0.0, |acc, x| acc + x)))
}

fn lisp_times(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, env)?;
    Ok(LispValue::Number(nums.iter().fold(1.0, |acc, x| acc * x)))
}

fn lisp_divide(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(&args[0], env)?.expect_number()?;
    let denominator = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Number(numerator / denominator))
}

fn lisp_int_divide(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(&args[0], env)?.expect_number()?;
    let denominator = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Number((numerator / denominator).trunc()))
}

fn lisp_def(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let val = eval(&args[1], env)?;
    let val = match val {
        LispValue::Func(mut f) => {
            f.name = Some(name.to_owned());
            LispValue::Func(f)
        },
        x => x,
    };
    if !env.set(name.to_owned(), val.clone()) {
        Err(LispError::AlreadyExists)
    } else {
        Ok(val)
    }
}

fn lisp_let(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut new_env = env.new_nested();
    let Some(list) = args.pop_front() else { unreachable!() };
    let mut list = list.into_list()?;
    expect!(list.len() & 1 == 0, LispError::MissingBinding);
    let slice = list.make_contiguous();
    for pair in slice.chunks_exact(2) {
        let name = pair[0].expect_symbol()?;
        let val = eval(&pair[1], &mut new_env)?;
        new_env.set(name.to_owned(), val);
    }
    eval(&args[0], &mut new_env)
}

fn lisp_if(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
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

fn lisp_do(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let mut last = eval(&args[0], env)?;
    for val in args.iter().skip(1) {
        last = eval(val, env)?;
    }
    Ok(last)
}

fn lisp_fn(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let Some(args_list) = args.pop_front() else { unreachable!() };
    let args_list = args_list.into_list()?;
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
    let arg_names = args_list.into_iter().map(|x| x.into_symbol()).collect::<Result<Vec<String>>>()?;
    let Some(body) = args.pop_front() else { unreachable!() };
    let closure = env.make_closure();
    Ok(LispValue::Func(Box::new(LispFunc {
        args: arg_names,
        body,
        closure,
        variadic,
        is_macro: false,
        name: None,
    })))
}

fn lisp_equals(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?;
    let y = eval(&args[1], env)?;
    Ok(LispValue::Bool(x == y))
}

fn lisp_prn(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    if let Some(last) = args.pop_back() {
        for val in args.iter() {
            let val = eval(val, env)?;
            print!("{} ", val.inspect());
        }
        let last_val = eval(&last, env)?;
        println!("{}", last_val.inspect());
    } else {
        println!();
    }
    Ok(LispValue::Nil)
}

fn lisp_list(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let vals = args.iter().map(|x| eval(x, env)).collect::<Result<VecDeque<LispValue>>>()?;
    Ok(LispValue::List(vals))
}

fn lisp_listq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(match val {
        LispValue::List(_) => true,
        _ => false,
    }))
}

fn lisp_emptyq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(match val {
        LispValue::List(l) => Ok(l.len() == 0),
        LispValue::Vector(l) => Ok(l.len() == 0),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }?))
}

fn lisp_count(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    if val.is_nil() {
        Ok(LispValue::Number(0.0))
    } else {
        Ok(LispValue::Number(match val {
            LispValue::List(l) => Ok(l.len() as f64),
            LispValue::Vector(l) => Ok(l.len() as f64),
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        }?))
    }
}

fn lisp_lt(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x < y))
}

fn lisp_lte(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x <= y))
}

fn lisp_gt(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x > y))
}

fn lisp_gte(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], env)?.expect_number()?;
    let y = eval(&args[1], env)?.expect_number()?;
    Ok(LispValue::Bool(x >= y))
}

fn lisp_not(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?.truthiness();
    Ok(LispValue::Bool(!x))
}

fn lisp_atom(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    Ok(LispValue::Atom(Arc::new(RwLock::new(x))))
}

fn lisp_atomq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::Bool(match val {
        LispValue::Atom(_) => true,
        _ => false,
    }))
}

fn lisp_deref(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    let atom = val.expect_atom()?;
    let out = atom.read().unwrap().clone();
    Ok(out)
}

fn lisp_reset(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let val = eval(&args[0], env)?;
    let atom = val.expect_atom()?;
    let val = eval(&args[1], env)?;
    *atom.write().unwrap() = val.clone();
    Ok(val)
}

fn lisp_swap(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let Some(first) = args.pop_front() else { unreachable!() };
    let Some(second) = args.pop_front() else { unreachable!() };
    let val = eval(&first, env)?;
    let atom = val.expect_atom()?;
    let val = eval(&second, env)?;
    let mut list = VecDeque::from([
        val,
        LispValue::List(VecDeque::from([
            LispValue::Symbol("deref".to_owned()),
            LispValue::Atom(atom.clone()),
        ])),
    ]);
    list.append(&mut args);
    let out_val = eval(&LispValue::List(list), env)?;
    *atom.write().unwrap() = out_val.clone();
    Ok(out_val)
}

fn lisp_eval(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    let mut global = env.global();
    eval(&arg, &mut global)
}

fn lisp_readline(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    if args.len() > 0 {
        let val = eval(&args[0], env)?;
        let s = val.expect_string()?;
        print!("{}", s);
        std::io::stdout().flush()?;
    }
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

fn lisp_read_string(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let mut parser = LispParser::new();
    let arg = eval(&args[0], env)?;
    parser.add_tokenize(arg.expect_string()?);
    if parser.has_tokens() {
        parser.next()
    } else {
        Ok(LispValue::Nil)
    }
}

fn lisp_str(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let mut buffer = String::new();
    for arg in args.iter() {
        let x = eval(arg, env)?;
        buffer.push_str(&x.to_string());
    }
    Ok(LispValue::String(buffer))
}

fn lisp_slurp(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    let mut buffer = String::new();
    let mut f = File::open(x.expect_string()?)?;
    f.read_to_string(&mut buffer)?;
    Ok(LispValue::String(buffer))
}

fn lisp_load_file(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    let mut buffer = String::new();
    let mut f = File::open(x.expect_string()?)?;
    let mut parser = LispParser::new();
    f.read_to_string(&mut buffer)?;
    parser.add_tokenize(&buffer);
    let mut global = env.global();
    while parser.has_tokens() {
        eval(&parser.next()?, &mut global)?;
    }
    Ok(LispValue::Nil)
}

fn lisp_typeof(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], env)?;
    Ok(LispValue::String(x.type_of().to_owned()))
}

fn lisp_quote(mut args: VecDeque<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    if let Some(out) = args.pop_front() {
        Ok(out)
    } else {
        unreachable!();
    }
}

fn inner_quasiquote(arg: LispValue, env: &mut LispEnv) -> Result<(LispValue, bool)> {
    match arg {
        LispValue::List(l) => {
            if l.len() == 0 {
                Ok((LispValue::List(l), false))
            } else if l[0] == LispValue::Symbol("unquote".to_owned()) {
                expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                Ok((eval(&l[1], env)?, false))
            } else if l[0] == LispValue::Symbol("splice-unquote".to_owned()) {
                expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                Ok((eval(&l[1], env)?, true))
            } else {
                let mut out = VecDeque::new();
                for val in l.into_iter() {
                    let (new_val, inplace) = inner_quasiquote(val, env)?;
                    if inplace {
                        out.append(&mut new_val.into_list()?);
                    } else {
                        out.push_back(new_val);
                    }
                }
                Ok((LispValue::List(out), false))
            }
        },
        _ => Ok((arg, false)),
    }
}

fn lisp_quasiquote(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let Some(front) = args.pop_front() else { unreachable!() };
    match front {
        LispValue::List(list) => {
            if list.len() == 0 {
                return Ok(LispValue::List(VecDeque::new()));
            }
            if list[0] == LispValue::Symbol("unquote".to_owned()) || list[0] == LispValue::Symbol("splice-unquote".to_owned()) {
                expect!(list.len() == 2, LispError::IncorrectArguments(1, list.len() - 1));
                return eval(&list[1], env);
            }
            let mut out = VecDeque::new();
            for val in list {
                let (new_val, inplace) = inner_quasiquote(val, env)?;
                if inplace {
                    out.append(&mut new_val.into_list()?);
                } else {
                    out.push_back(new_val);
                }
            }
            Ok(LispValue::List(out))
        },
        x => Ok(x),
    }
}

// `lisp_quasiquote` handles the `unquote` and `splice-unquote` methods itself,
// so this can only be called outside `quasiquote` (which is an error)
fn lisp_unquote(_args: VecDeque<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
    Err(LispError::OnlyInQuasiquote)
}

fn lisp_cons(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(&args[0], env)?;
    let arg2 = eval(&args[1], env)?;
    let mut list = arg2.into_list()?;
    list.push_front(arg1);
    Ok(LispValue::List(list))
}

fn lisp_concat(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let mut out = VecDeque::new();
    for arg in args.iter() {
        let arg = eval(&arg, env)?;
        let mut list = arg.into_list()?;
        out.append(&mut list);
    }
    Ok(LispValue::List(out))
}

fn lisp_nth(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(&args[0], env)?;
    let arg2 = eval(&args[1], env)?;
    let mut list = arg1.into_list()?;
    let idx = arg2.expect_number()? as usize;
    if let Some(item) = list.remove(idx) {
        Ok(item)
    } else {
        Err(LispError::IndexOutOfRange(idx))
    }
}

fn lisp_first(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    if arg.is_nil() {
        return Ok(LispValue::Nil);
    }
    let mut list = arg.into_list()?;
    if let Some(item) = list.pop_front() {
        Ok(item)
    } else {
        Ok(LispValue::Nil)
    }
}

fn lisp_rest(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    if arg.is_nil() {
        return Ok(LispValue::List(VecDeque::new()));
    }
    let mut list = arg.into_list()?;
    list.pop_front();
    Ok(LispValue::List(list))
}

fn lisp_macroq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    match arg {
        LispValue::Func(f) => Ok(LispValue::Bool(f.is_macro)),
        _ => Ok(LispValue::Bool(false)),
    }
}

fn lisp_defmacro(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let mut val = eval(&args[1], env)?;
    match val {
        LispValue::Func(ref mut f) => {
            f.name = Some(name.to_owned());
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

fn lisp_macroexpand(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    expand_macros(&args[0], env)
}

fn lisp_inspect(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], env)?;
    Ok(LispValue::String(val.inspect()))
}

fn lisp_try(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let Some(catch) = args.pop_back() else { unreachable!() };
    if let Ok(mut catch) = catch.into_list() {
        expect!(catch.len() == 3, LispError::IncorrectArguments(2, args.len() - 1));
        let Some(catch_sym) = catch.pop_front() else { unreachable!() };
        if catch_sym != LispValue::Symbol("catch*".to_owned()) {
            return Err(LispError::TryNoCatch);
        }
        let Some(err_name) = catch.pop_front() else { unreachable!() };
        let err_name = err_name.into_symbol()?;
        let mut caught_env = env.new_nested();
        match eval(&args[0], env) {
            Ok(x) => Ok(x),
            Err(LispError::UncaughtException(except)) => {
                caught_env.set(err_name, except);
                eval(&catch[0], &mut caught_env)
            },
            Err(err) => {
                let s = err.to_string();
                caught_env.set(err_name, LispValue::String(s));
                eval(&catch[0], &mut caught_env)
            }
        }
    } else {
        Err(LispError::TryNoCatch)
    }
}

// `lisp_try` handles `catch`, which can't be used outside `try`,
// so this is an error
fn lisp_catch(_args: VecDeque<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
    Err(LispError::OnlyInTry)
}

fn lisp_throw(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Err(LispError::UncaughtException(arg))
}

fn lisp_nilq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(arg.is_nil()))
}

fn lisp_trueq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Bool(b) => b,
        _ => false,
    }))
}

fn lisp_falseq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Bool(b) => !b,
        _ => false,
    }))
}

fn lisp_symbolq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Symbol(_) => true,
        _ => false,
    }))
}

fn lisp_symbol(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    let s = arg.expect_string()?;
    Ok(LispValue::Symbol(s.to_owned()))
}

fn lisp_vector(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let vals: Vec<LispValue> = args.iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?;
    Ok(LispValue::Vector(vals))
}

fn lisp_vectorq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Vector(_) => true,
        _ => false,
    }))
}

fn lisp_keyword(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    let s = arg.expect_string()?;
    Ok(LispValue::Keyword(s.to_owned()))
}

fn lisp_keywordq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Keyword(_) => true,
        _ => false,
    }))
}

fn lisp_hashmap(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() & 1 == 0, LispError::MissingBinding);
    let slice = args.make_contiguous();
    let pairs = slice.chunks_exact(2).map(|x| {
        let k = eval(&x[0], env)?;
        let v = eval(&x[1], env)?;
        Ok((k, v))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    Ok(LispValue::Map(pairs.into()))
}

fn lisp_mapq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Map(_) => true,
        _ => false,
    }))
}

fn lisp_sequentialq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::List(_) => true,
        LispValue::Vector(_) => true,
        _ => false,
    }))
}

fn lisp_assoc(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() & 1 == 1, LispError::MissingBinding);
    let Some(first) = args.pop_front() else { unreachable!() };
    let base_map = eval(&first, env)?;
    let base_map = base_map.expect_hashmap()?;
    let slice = args.make_contiguous();
    let pairs = slice.chunks_exact(2).map(|x| {
        let k = eval(&x[0], env)?;
        let v = eval(&x[1], env)?;
        Ok((k, v))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    let new_map: HashMap<LispValue, LispValue> = pairs.into();
    Ok(LispValue::Map(new_map.union(base_map.clone())))
}

fn lisp_dissoc(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let base_map = eval(&first, env)?;
    let base_map = base_map.expect_hashmap()?;
    let keys = args.iter().map(|x| {
        Ok((eval(&x, env)?, LispValue::Nil))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    Ok(LispValue::Map(base_map.clone().difference(keys.into())))
}

fn lisp_get(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval(&args[0], env)?;
    let key = eval(&args[1], env)?;
    if map.is_nil() {
        return Ok(LispValue::Nil);
    } else {
        let map = map.expect_hashmap()?;
        if let Some(val) = map.get(&key) {
            Ok(val.clone())
        } else {
            Ok(LispValue::Nil)
        }
    }
}

fn lisp_containsq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval(&args[0], env)?;
    let map = map.expect_hashmap()?;
    let key = eval(&args[1], env)?;
    Ok(LispValue::Bool(map.contains_key(&key)))
}

fn lisp_keys(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval(&args[0], env)?;
    let map = map.expect_hashmap()?;
    let keys = map.keys().map(|x| x.clone()).collect();
    Ok(LispValue::List(keys))
}

fn lisp_vals(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval(&args[0], env)?;
    let map = map.expect_hashmap()?;
    let vals = map.values().map(|x| x.clone()).collect();
    Ok(LispValue::List(vals))
}

fn lisp_apply(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let Some(first) = args.pop_front() else { unreachable!() };
    let f = eval(&first, env)?;
    let Some(last) = args.pop_back() else { unreachable!() };
    let list = eval(&last, env)?;
    let mut list = list.into_list()?;
    let mut full_list = VecDeque::from([f]);
    full_list.append(&mut args);
    full_list.append(&mut list);
    eval(&LispValue::List(full_list), env)
}

fn lisp_map(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let f = eval(&args[0], env)?;
    let list = eval(&args[1], env)?;
    let list = list.into_list()?;
    let out = list.into_iter().map(|x| {
        let list = LispValue::List(VecDeque::from([f.clone(), x]));
        eval(&list, env)
    }).collect::<Result<VecDeque<LispValue>>>()?;
    Ok(LispValue::List(out))
}

fn lisp_pr_str(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let full_str = args.iter().map(|x| {
        Ok(eval(x, env)?.inspect())
    }).collect::<Result<Vec<String>>>()?;
    Ok(LispValue::String(full_str.join(" ")))
}

fn lisp_println(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    // args.len() != 0, so `split_list` isn't `None`
    let Some(last) = args.pop_back() else { unreachable!() };
    for val in args.iter() {
        let val = eval(val, env)?;
        print!("{} ", val);
    }
    println!("{}", eval(&last, env)?);
    Ok(LispValue::Nil)
}

fn lisp_fnq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Func(_) => true,
        LispValue::BuiltinFunc { .. } => true,
        _ => false,
    }))
}

fn lisp_stringq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::String(_) => true,
        _ => false,
    }))
}

fn lisp_numberq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Number(_) => true,
        _ => false,
    }))
}

fn lisp_vec(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    match arg {
        LispValue::Vector(l) => Ok(LispValue::Vector(l)),
        LispValue::List(l) => Ok(LispValue::Vector(l.into())),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_time_ms(_args: VecDeque<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
    Ok(LispValue::Number(START_TIME.elapsed().as_secs_f64() * 1000.0))
}

fn lisp_seq(args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], env)?;
    match arg {
        LispValue::Vector(l) => {
            Ok(if l.len() == 0 {
                LispValue::Nil
            } else {
                LispValue::List(l.into())
            })
        },
        LispValue::List(l) => {
            Ok(if l.len() == 0 {
                LispValue::Nil
            } else {
                LispValue::List(l)
            })
        },
        LispValue::String(s) => {
            Ok(if s.len() == 0 {
                LispValue::Nil
            } else {
                LispValue::List(s.chars().map(|x| LispValue::String(x.to_string())).collect())
            })
        },
        LispValue::Nil => Ok(LispValue::Nil),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_conj(mut args: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    expect!(args.len() > 0, LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let first = eval(&first, env)?;
    match first {
        LispValue::Vector(mut l) => {
            let mut args = args.iter().map(|x| {
                eval(x, env)
            }).collect::<Result<Vec<LispValue>>>()?;
            l.append(&mut args);
            Ok(LispValue::Vector(l))
        },
        LispValue::List(mut l) => {
            let mut new_list = args.iter().rev().map(|x| {
                eval(x, env)
            }).collect::<Result<VecDeque<LispValue>>>()?;
            new_list.append(&mut l);
            Ok(LispValue::List(new_list))
        },
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_meta(_args: VecDeque<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
    unimplemented!();
}

fn lisp_with_meta(_args: VecDeque<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
    unimplemented!();
}

macro_rules! lisp_func {
    ($name:expr, $f:expr) => { LispValue::BuiltinFunc { name: $name, f: ExternLispFunc($f) } }
}
lazy_static! {
    static ref START_TIME: Instant = {
        Instant::now()
    };
    pub static ref BUILTINS: HashMap<String, LispValue> = {
        // dummy to make `time-ms` count from environment (so probably program)
        // initialization rather than since the first `time-ms` call
        let _ = *START_TIME;
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
            "pr-str".to_owned() => lisp_func!("pr-str", lisp_pr_str),
            "println".to_owned() => lisp_func!("println", lisp_println),
            "fn?".to_owned() => lisp_func!("fn?", lisp_fnq),
            "string?".to_owned() => lisp_func!("string?", lisp_stringq),
            "number?".to_owned() => lisp_func!("number?", lisp_numberq),
            "vec".to_owned() => lisp_func!("vec", lisp_vec),
            "time-ms".to_owned() => lisp_func!("time-ms", lisp_time_ms),
            "seq".to_owned() => lisp_func!("seq", lisp_seq),
            "conj".to_owned() => lisp_func!("conj", lisp_conj),
            "meta".to_owned() => lisp_func!("meta", lisp_meta),
            "with-meta".to_owned() => lisp_func!("with-meta", lisp_with_meta),
        }
    };
}
