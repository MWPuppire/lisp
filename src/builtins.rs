use std::sync::{Arc, RwLock};
use im::{hashmap, HashMap, vector, Vector};
use lazy_static::lazy_static;
use ordered_float::OrderedFloat;
use crate::{LispValue, LispError, Result, expect};
use crate::env::{LispEnv, LispSymbol};
use crate::eval::{eval, eval_top, expand_macros};
use crate::parser::LispParser;
use crate::util::{LispFunc, LispBuiltinResult};

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        use std::fs::File;
        use std::io::prelude::*;
        use std::time::{SystemTime, UNIX_EPOCH};
    }
}

fn eval_list_to_numbers(args: Vector<LispValue>, env: &mut LispEnv) -> Result<Vec<OrderedFloat<f64>>> {
    args.into_iter().map(|x| eval(x, env)?.expect_number()).collect()
}

fn lisp_plus(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(args, env)?;
    LispBuiltinResult::Done(LispValue::Number(
        nums.iter().fold(OrderedFloat(0.0), |acc, x| acc + x)
    ))
}

fn lisp_minus(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let first = eval(first, env)?.expect_number()?;
    let nums = eval_list_to_numbers(args, env)?;
    LispBuiltinResult::Done(LispValue::Number(
        first - nums.iter().fold(OrderedFloat(0.0), |acc, x| acc + x)
    ))
}

fn lisp_times(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(args, env)?;
    LispBuiltinResult::Done(LispValue::Number(
        nums.iter().fold(OrderedFloat(1.0), |acc, x| acc * x)
    ))
}

fn lisp_divide(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    let denominator = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Number(numerator / denominator))
}

fn lisp_int_divide(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    let denominator = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Number(OrderedFloat(
        (numerator / denominator).trunc()
    )))
}

fn lisp_def(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let val = eval(args.pop_back().unwrap(), env)?;
    env.set(name, val.clone());
    LispBuiltinResult::Done(val)
}

fn lisp_let(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut new_env = env.new_nested();
    let Some(list) = args.pop_front() else { unreachable!() };
    let list = list.into_list()?;
    expect!(list.len() & 1 == 0, LispError::MissingBinding);
    let mut list_iter = list.into_iter();
    while let Some(name) = list_iter.next() {
        let Some(val_expr) = list_iter.next() else { unreachable!() };
        let name = name.expect_symbol()?;
        let val = eval(val_expr, &mut new_env)?;
        new_env.set(name, val);
    }
    let Some(head) = args.pop_front() else { unreachable!() };
    LispBuiltinResult::ContinueIn(head, new_env)
}

fn lisp_if(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() > 1 && args.len() < 4, LispError::IncorrectArguments(2, args.len()));
    let Some(pred) = args.pop_front() else { unreachable!() };
    let pred = eval(pred, env)?.truthiness();
    if pred {
        let Some(then) = args.pop_front() else { unreachable!() };
        LispBuiltinResult::Continue(then)
    } else if args.len() > 1 {
        let Some(arg) = args.pop_back() else { unreachable!() };
        LispBuiltinResult::Continue(arg)
    } else {
        LispBuiltinResult::Done(LispValue::Nil)
    }
}

fn lisp_do(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let Some(last_eval) = args.pop_back() else { unreachable!() };
    for val in args.into_iter() {
        eval(val, env)?;
    }
    LispBuiltinResult::Continue(last_eval)
}

fn lisp_fn(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let Some(args_list) = args.pop_front() else { unreachable!() };
    let args_list = args_list.into_list()?;
    let variadic = if !args_list.is_empty() {
        let last = &args_list[args_list.len() - 1];
        match last {
            LispValue::VariadicSymbol(_) => Ok(true),
            LispValue::Symbol(_) => Ok(false),
            _ => Err(LispError::InvalidDataType("symbol", last.type_of())),
        }?
    } else {
        false
    };
    let arg_names = args_list.into_iter().map(|x| x.expect_symbol()).collect::<Result<Vec<LispSymbol>>>()?;
    let Some(body) = args.pop_front() else { unreachable!() };
    let closure = Some(env.make_closure());
    LispBuiltinResult::Done(LispValue::Func(Box::new(LispFunc {
        args: arg_names,
        body,
        closure,
        variadic,
        is_macro: false,
    })))
}

fn lisp_equals(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?;
    let y = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(x == y))
}

#[cfg(feature = "io-stdlib")]
fn lisp_prn(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    if let Some(last) = args.pop_back() {
        for val in args.into_iter() {
            let val = eval(val, env)?;
            print!("{} ", val.inspect());
        }
        let last_val = eval(last, env)?;
        println!("{}", last_val.inspect());
    } else {
        println!();
    }
    LispBuiltinResult::Done(LispValue::Nil)
}

fn lisp_list(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    let vals = args.into_iter().map(|x| eval(x, env)).collect::<Result<Vector<LispValue>>>()?;
    LispBuiltinResult::Done(LispValue::List(vals))
}

fn lisp_listq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(val, LispValue::List(_))))
}

fn lisp_emptyq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(match val {
        LispValue::List(l) => Ok(l.is_empty()),
        LispValue::Vector(l) => Ok(l.is_empty()),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }?))
}

fn lisp_count(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(args.pop_front().unwrap(), env)?;
    if val.is_nil() {
        LispBuiltinResult::Done(LispValue::Number(OrderedFloat(0.0)))
    } else {
        LispBuiltinResult::Done(LispValue::Number(OrderedFloat(match val {
            LispValue::List(l) => Ok(l.len() as f64),
            LispValue::Vector(l) => Ok(l.len() as f64),
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        }?)))
    }
}

fn lisp_lt(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    let y = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Bool(x < y))
}

fn lisp_lte(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    let y = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Bool(x <= y))
}

fn lisp_gt(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    let y = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Bool(x > y))
}

fn lisp_gte(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    let y = eval(args.pop_front().unwrap(), env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Bool(x >= y))
}

fn lisp_not(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?.truthiness();
    LispBuiltinResult::Done(LispValue::Bool(!x))
}

fn lisp_atom(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Atom(Arc::new(RwLock::new(x))))
}

fn lisp_atomq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(val, LispValue::Atom(_))))
}

fn lisp_deref(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(args.pop_front().unwrap(), env)?;
    let atom = val.into_atom()?;
    let out = atom.read().unwrap().clone();
    LispBuiltinResult::Done(out)
}

fn lisp_reset(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let val = eval(args.pop_front().unwrap(), env)?;
    let atom = val.into_atom()?;
    let val = eval(args.pop_front().unwrap(), env)?;
    *atom.write().unwrap() = val.clone();
    LispBuiltinResult::Done(val)
}

fn lisp_swap(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let Some(first) = args.pop_front() else { unreachable!() };
    let Some(second) = args.pop_front() else { unreachable!() };
    let val = eval(first, env)?;
    let atom = val.into_atom()?;
    let val = eval(second, env)?;
    let derefed = atom.read().unwrap().clone();
    let mut list = vector![
        val,
        derefed,
    ];
    list.append(args);
    let out_val = eval(LispValue::List(list), env)?;
    *atom.write().unwrap() = out_val.clone();
    LispBuiltinResult::Done(out_val)
}

fn lisp_eval(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    let global = env.global();
    LispBuiltinResult::ContinueIn(arg, global)
}

#[cfg(feature = "io-stdlib")]
fn lisp_readline(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    if !args.is_empty() {
        let val = eval(args.pop_front().unwrap(), env)?;
        let s = val.into_string()?;
        print!("{}", s);
        std::io::stdout().flush()?;
    }
    let mut buffer = String::new();
    let len = std::io::stdin().read_line(&mut buffer)?;
    if len == 0 {
        // eof
        LispBuiltinResult::Done(LispValue::Nil)
    } else {
        buffer.pop(); // remove newline
        LispBuiltinResult::Done(LispValue::String(buffer))
    }
}

fn lisp_read_string(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    let code_expr = arg.into_string()?;
    let parsed = LispParser::parse(&code_expr);
    if let Some(parsed) = parsed {
        LispBuiltinResult::Done(parsed?)
    } else {
        LispBuiltinResult::Done(LispValue::Nil)
    }
}

fn lisp_str(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    let mut buffer = String::new();
    for arg in args.into_iter() {
        let x = eval(arg, env)?;
        buffer.push_str(&x.to_string());
    }
    LispBuiltinResult::Done(LispValue::String(buffer))
}

#[cfg(feature = "io-stdlib")]
fn lisp_slurp(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?;
    let file_name = x.into_string()?;
    let mut f = File::open(file_name)?;
    let mut buffer = String::new();
    f.read_to_string(&mut buffer)?;
    LispBuiltinResult::Done(LispValue::String(buffer))
}

#[cfg(feature = "io-stdlib")]
fn lisp_load_file(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?;
    let file_name = x.into_string()?;
    let mut f = File::open(file_name)?;
    let mut buffer = String::new();
    f.read_to_string(&mut buffer)?;

    let mut parser = LispParser::new();
    parser.add_tokenize(&buffer)?;
    let mut global = env.global();
    for val in parser {
        eval(val?, &mut global)?;
    }
    LispBuiltinResult::Done(LispValue::Nil)
}

fn lisp_typeof(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::String(x.type_of().to_owned()))
}

fn lisp_quote(mut args: Vector<LispValue>, _env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    if let Some(out) = args.pop_front() {
        LispBuiltinResult::Done(out)
    } else {
        unreachable!();
    }
}

fn inner_quasiquote(arg: LispValue, env: &mut LispEnv) -> Result<(LispValue, bool)> {
    let unquote_sym = LispEnv::symbol_for_static("unquote");
    let splice_sym = LispEnv::symbol_for_static("splice-unquote");
    match arg {
        LispValue::List(mut l) => {
            if l.is_empty() {
                Ok((LispValue::List(l), false))
            } else if l[0] == LispValue::Symbol(unquote_sym) {
                expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                Ok((eval_top(l.pop_back().unwrap(), env)?, false))
            } else if l[0] == LispValue::Symbol(splice_sym) {
                expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                Ok((eval_top(l.pop_back().unwrap(), env)?, true))
            } else {
                let mut out = Vector::new();
                for val in l.into_iter() {
                    let (new_val, inplace) = inner_quasiquote(val, env)?;
                    if inplace {
                        out.append(new_val.into_list()?);
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

fn lisp_quasiquote(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let Some(front) = args.pop_front() else { unreachable!() };
    match front {
        LispValue::List(mut list) => {
            if list.is_empty() {
                return LispBuiltinResult::Done(LispValue::List(Vector::new()));
            }
            let unquote_sym = LispEnv::symbol_for_static("unquote");
            let splice_sym = LispEnv::symbol_for_static("splice-unquote");
            if list[0] == LispValue::Symbol(unquote_sym) || list[0] == LispValue::Symbol(splice_sym) {
                expect!(list.len() == 2, LispError::IncorrectArguments(1, list.len() - 1));
                let Some(val) = list.pop_back() else { unreachable!() };
                return LispBuiltinResult::Continue(val);
            }
            let mut out = Vector::new();
            for val in list {
                let (new_val, inplace) = inner_quasiquote(val, env)?;
                if inplace {
                    out.append(new_val.into_list()?);
                } else {
                    out.push_back(new_val);
                }
            }
            LispBuiltinResult::Done(LispValue::List(out))
        },
        x => LispBuiltinResult::Done(x),
    }
}

// `lisp_quasiquote` handles the `unquote` and `splice-unquote` methods itself,
// so this can only be called outside `quasiquote` (which is an error)
fn lisp_unquote(_args: Vector<LispValue>, _env: &mut LispEnv) -> LispBuiltinResult {
    LispBuiltinResult::Error(LispError::OnlyInQuasiquote)
}

fn lisp_cons(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(args.pop_front().unwrap(), env)?;
    let arg2 = eval(args.pop_front().unwrap(), env)?;
    let mut list = arg2.into_list()?;
    list.push_front(arg1);
    LispBuiltinResult::Done(LispValue::List(list))
}

fn lisp_concat(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    let mut out = Vector::new();
    for arg in args.into_iter() {
        let arg = eval(arg, env)?;
        let list = arg.into_list()?;
        out.append(list);
    }
    LispBuiltinResult::Done(LispValue::List(out))
}

fn lisp_nth(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(args.pop_front().unwrap(), env)?;
    let arg2 = eval(args.pop_front().unwrap(), env)?;
    let mut list = arg1.into_list()?;
    let idx = arg2.expect_number()?.into_inner() as usize;
    expect!(idx < list.len(), LispError::IndexOutOfRange(idx));
    LispBuiltinResult::Done(list.remove(idx))
}

fn lisp_first(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    if arg.is_nil() {
        return LispBuiltinResult::Done(LispValue::Nil);
    }
    let mut list = arg.into_list()?;
    if let Some(item) = list.pop_front() {
        LispBuiltinResult::Done(item)
    } else {
        LispBuiltinResult::Done(LispValue::Nil)
    }
}

fn lisp_rest(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    if arg.is_nil() {
        return LispBuiltinResult::Done(LispValue::List(Vector::new()));
    }
    let mut list = arg.into_list()?;
    list.pop_front();
    LispBuiltinResult::Done(LispValue::List(list))
}

fn lisp_macroq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(match arg {
        LispValue::Func(f) => LispValue::Bool(f.is_macro),
        _ => LispValue::Bool(false),
    })
}

fn lisp_defmacro(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let mut val = eval(args.pop_back().unwrap(), env)?;
    match val {
        LispValue::Func(ref mut f) => {
            f.is_macro = true;
        },
        _ => return LispBuiltinResult::Error(LispError::InvalidDataType("function", val.type_of())),
    }
    env.set(name, val.clone());
    LispBuiltinResult::Done(val)
}

fn lisp_macroexpand(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    LispBuiltinResult::Done(expand_macros(args.pop_front().unwrap(), env)?)
}

fn lisp_inspect(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::String(val.inspect()))
}

fn lisp_try(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let Some(catch) = args.pop_back() else { unreachable!() };
    if let Ok(mut catch) = catch.into_list() {
        expect!(catch.len() == 3, LispError::IncorrectArguments(2, args.len() - 1));
        let Some(catch_sym) = catch.pop_front() else { unreachable!() };
        let base_catch_sym = LispValue::Symbol(LispEnv::symbol_for_static("catch*"));
        if catch_sym != base_catch_sym {
            return LispBuiltinResult::Error(LispError::TryNoCatch);
        }
        let Some(err_name) = catch.pop_front() else { unreachable!() };
        let err_name = err_name.expect_symbol()?;
        let mut caught_env = env.new_nested();
        let Some(catch) = catch.pop_front() else { unreachable!() };
        match eval(args.pop_front().unwrap(), env) {
            Ok(x) => LispBuiltinResult::Done(x),
            Err(LispError::UncaughtException(except)) => {
                caught_env.set(err_name, except);
                LispBuiltinResult::ContinueIn(catch, caught_env)
            },
            Err(err) => {
                let s = err.to_string();
                caught_env.set(err_name, LispValue::String(s));
                LispBuiltinResult::ContinueIn(catch, caught_env)
            }
        }
    } else {
        LispBuiltinResult::Error(LispError::TryNoCatch)
    }
}

// `lisp_try` handles `catch`, which can't be used outside `try`,
// so this is an error
fn lisp_catch(_args: Vector<LispValue>, _env: &mut LispEnv) -> LispBuiltinResult {
    LispBuiltinResult::Error(LispError::OnlyInTry)
}

fn lisp_throw(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Error(LispError::UncaughtException(arg))
}

fn lisp_nilq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(arg.is_nil()))
}

fn lisp_trueq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Bool(true))))
}

fn lisp_falseq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Bool(false))))
}

fn lisp_symbolq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Symbol(_))))
}

fn lisp_symbol(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    let s = arg.into_string()?;
    LispBuiltinResult::Done(LispValue::Symbol(LispEnv::symbol_for(&s)))
}

fn lisp_vector(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    let vals: Vec<LispValue> = args.into_iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?;
    LispBuiltinResult::Done(LispValue::Vector(vals))
}

fn lisp_vectorq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Vector(_))))
}

fn lisp_keyword(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(match arg {
        LispValue::Keyword(s) => Ok(LispValue::Keyword(s)),
        LispValue::String(s) => Ok(LispValue::Keyword(s)),
        LispValue::Symbol(s) => Ok(LispValue::Keyword(LispEnv::symbol_string(s).unwrap().to_owned())),
        x => Err(LispError::InvalidDataType("string", x.type_of())),
    }?)
}

fn lisp_keywordq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Keyword(_))))
}

fn lisp_hashmap(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() & 1 == 0, LispError::MissingBinding);
    let mut pairs = vec![];
    let mut arg_iter = args.into_iter();
    while let Some(key_expr) = arg_iter.next() {
        let Some(val_expr) = arg_iter.next() else { unreachable!() };
        let k = eval(key_expr, env)?;
        let v = eval(val_expr, env)?;
        pairs.push((k, v))
    }
    LispBuiltinResult::Done(LispValue::Map(pairs.into()))
}

fn lisp_mapq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Map(_))))
}

fn lisp_sequentialq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::List(_) | LispValue::Vector(_))))
}

fn lisp_assoc(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() & 1 == 1, LispError::MissingBinding);
    let Some(first) = args.pop_front() else { unreachable!() };
    let map = eval(first, env)?;
    let mut map = map.into_hashmap()?;
    let mut arg_iter = args.into_iter();
    while let Some(key_expr) = arg_iter.next() {
        let Some(val_expr) = arg_iter.next() else { unreachable!() };
        let k = eval(key_expr, env)?;
        let v = eval(val_expr, env)?;
        map.insert(k, v);
    }
    LispBuiltinResult::Done(LispValue::Map(map))
}

fn lisp_dissoc(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let base_map = eval(first, env)?;
    let base_map = base_map.into_hashmap()?;
    let keys = args.into_iter().map(|x| {
        Ok((eval(x, env)?, LispValue::Nil))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    LispBuiltinResult::Done(LispValue::Map(base_map.difference(keys.into())))
}

fn lisp_get(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval(args.pop_front().unwrap(), env)?;
    let key = eval(args.pop_front().unwrap(), env)?;
    if map.is_nil() {
        LispBuiltinResult::Done(LispValue::Nil)
    } else {
        let map = map.into_hashmap()?;
        if let Some(val) = map.get(&key) {
            LispBuiltinResult::Done(val.clone())
        } else {
            LispBuiltinResult::Done(LispValue::Nil)
        }
    }
}

fn lisp_containsq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval(args.pop_front().unwrap(), env)?;
    let map = map.into_hashmap()?;
    let key = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(map.contains_key(&key)))
}

fn lisp_keys(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval(args.pop_front().unwrap(), env)?;
    let map = map.into_hashmap()?;
    let keys = map.keys().cloned().collect();
    LispBuiltinResult::Done(LispValue::List(keys))
}

fn lisp_vals(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval(args.pop_front().unwrap(), env)?;
    let map = map.into_hashmap()?;
    let vals = map.values().cloned().collect();
    LispBuiltinResult::Done(LispValue::List(vals))
}

fn lisp_apply(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let Some(first) = args.pop_front() else { unreachable!() };
    let f = eval(first, env)?;
    let Some(last) = args.pop_back() else { unreachable!() };
    let list = eval(last, env)?;
    let list = list.into_list()?;
    let mut full_list = vector![f];
    full_list.append(args);
    full_list.append(list);
    LispBuiltinResult::Continue(LispValue::List(full_list))
}

fn lisp_map(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let f = eval(args.pop_front().unwrap(), env)?;
    let list = eval(args.pop_front().unwrap(), env)?;
    let list = list.into_list()?;
    let out = list.into_iter().map(|x| {
        let list = LispValue::List(vector![f.clone(), x]);
        eval(list, env)
    }).collect::<Result<Vector<LispValue>>>()?;
    LispBuiltinResult::Done(LispValue::List(out))
}

fn lisp_pr_str(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    let full_str = args.into_iter().map(|x| {
        Ok(eval(x, env)?.inspect())
    }).collect::<Result<Vec<String>>>()?;
    LispBuiltinResult::Done(LispValue::String(full_str.join(" ")))
}

#[cfg(feature = "io-stdlib")]
fn lisp_println(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    if let Some(last) = args.pop_back() {
        for val in args.into_iter() {
            let val = eval(val, env)?;
            print!("{} ", val);
        }
        println!("{}", eval(last, env)?);
    } else {
        println!();
    }
    LispBuiltinResult::Done(LispValue::Nil)
}

fn lisp_fnq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Func(_) | LispValue::BuiltinFunc { .. })))
}

fn lisp_stringq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::String(_))))
}

fn lisp_numberq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Number(_))))
}

fn lisp_vec(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    match arg {
        LispValue::Vector(l) => LispBuiltinResult::Done(LispValue::Vector(l)),
        LispValue::List(l) => LispBuiltinResult::Done(LispValue::Vector(l.into_iter().collect())),
        x => LispBuiltinResult::Error(LispError::InvalidDataType("list", x.type_of())),
    }
}

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        fn lisp_time_ms(_args: Vector<LispValue>, _env: &mut LispEnv) -> LispBuiltinResult {
            LispBuiltinResult::Done(LispValue::Number(OrderedFloat(
                match SystemTime::now().duration_since(UNIX_EPOCH) {
                    Ok(d) => d.as_secs_f64() * 1000.0,
                    Err(d) => d.duration().as_secs_f64() * 1000.0,
                }
            )))
        }
    }
}

fn lisp_seq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(args.pop_front().unwrap(), env)?;
    match arg {
        LispValue::Vector(l) => {
            LispBuiltinResult::Done(if l.is_empty() {
                LispValue::Nil
            } else {
                LispValue::List(l.into())
            })
        },
        LispValue::List(l) => {
            LispBuiltinResult::Done(if l.is_empty() {
                LispValue::Nil
            } else {
                LispValue::List(l)
            })
        },
        LispValue::String(s) => {
            LispBuiltinResult::Done(if s.is_empty() {
                LispValue::Nil
            } else {
                LispValue::List(s.chars().map(|x| x.to_string().into()).collect())
            })
        },
        LispValue::Nil => LispBuiltinResult::Done(LispValue::Nil),
        x => LispBuiltinResult::Error(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_conj(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let first = eval(first, env)?;
    match first {
        LispValue::Vector(mut l) => {
            let mut args = args.into_iter().map(|x| {
                eval(x, env)
            }).collect::<Result<Vec<LispValue>>>()?;
            l.append(&mut args);
            LispBuiltinResult::Done(LispValue::Vector(l))
        },
        LispValue::List(l) => {
            let mut new_list = args.into_iter().rev().map(|x| {
                eval(x, env)
            }).collect::<Result<Vector<LispValue>>>()?;
            new_list.append(l);
            LispBuiltinResult::Done(LispValue::List(new_list))
        },
        x => LispBuiltinResult::Error(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_meta(_args: Vector<LispValue>, _env: &mut LispEnv) -> LispBuiltinResult {
    LispBuiltinResult::Error(LispError::NoMeta)
}

fn lisp_with_meta(_args: Vector<LispValue>, _env: &mut LispEnv) -> LispBuiltinResult {
    LispBuiltinResult::Error(LispError::NoMeta)
}

macro_rules! make_lisp_funcs {
    ($interner:expr, $($name:literal => $f:path,)*) => {
        hashmap! {
            $($interner.get_or_intern_static($name) => LispValue::BuiltinFunc {
                name: $name,
                f: $f,
            }),*
        }
    }
}

const COND_BODY_STR: &str = "(if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))";
lazy_static! {
    pub static ref BUILTINS_NO_IO: HashMap<LispSymbol, LispValue> = {
        let mut strs = LispEnv::interner_mut();
        let mut funcs = make_lisp_funcs!(strs,
            "+" => lisp_plus,
            "-" => lisp_minus,
            "*" => lisp_times,
            "/" => lisp_divide,
            "//" => lisp_int_divide,
            "def!" => lisp_def,
            "let*" => lisp_let,
            "if" => lisp_if,
            "do" => lisp_do,
            "fn*" => lisp_fn,
            "=" => lisp_equals,
            "list" => lisp_list,
            "list?" => lisp_listq,
            "empty?" => lisp_emptyq,
            "count" => lisp_count,
            "<" => lisp_lt,
            "<=" => lisp_lte,
            ">" => lisp_gt,
            ">=" => lisp_gte,
            "not" => lisp_not,
            "atom" => lisp_atom,
            "atom?" => lisp_atomq,
            "deref" => lisp_deref,
            "reset!" => lisp_reset,
            "swap!" => lisp_swap,
            "eval" => lisp_eval,
            "read-string" => lisp_read_string,
            "str" => lisp_str,
            "typeof" => lisp_typeof,
            "quote" => lisp_quote,
            "quasiquote" => lisp_quasiquote,
            "unquote" => lisp_unquote,
            "splice-unquote" => lisp_unquote,
            "cons" => lisp_cons,
            "concat" => lisp_concat,
            "nth" => lisp_nth,
            "first" => lisp_first,
            "head" => lisp_first,
            "rest" => lisp_rest,
            "tail" => lisp_rest,
            "macro?" => lisp_macroq,
            "defmacro!" => lisp_defmacro,
            "macroexpand" => lisp_macroexpand,
            "inspect" => lisp_inspect,
            "try*" => lisp_try,
            "catch*" => lisp_catch,
            "throw" => lisp_throw,
            "nil?" => lisp_nilq,
            "true?" => lisp_trueq,
            "false?" => lisp_falseq,
            "symbol?" => lisp_symbolq,
            "symbol" => lisp_symbol,
            "vector" => lisp_vector,
            "vector?" => lisp_vectorq,
            "keyword" => lisp_keyword,
            "keyword?" => lisp_keywordq,
            "hash-map" => lisp_hashmap,
            "map?" => lisp_mapq,
            "sequential?" => lisp_sequentialq,
            "assoc" => lisp_assoc,
            "dissoc" => lisp_dissoc,
            "get" => lisp_get,
            "contains?" => lisp_containsq,
            "keys" => lisp_keys,
            "vals" => lisp_vals,
            "apply" => lisp_apply,
            "map" => lisp_map,
            "pr-str" => lisp_pr_str,
            "fn?" => lisp_fnq,
            "string?" => lisp_stringq,
            "number?" => lisp_numberq,
            "vec" => lisp_vec,
            "seq" => lisp_seq,
            "conj" => lisp_conj,
            "meta" => lisp_meta,
            "with-meta" => lisp_with_meta,
        );

        let xs_symbol = strs.get_or_intern_static("xs");
        let cond_name = strs.get_or_intern_static("cond");
        let parsed_cond_body = LispParser::parse_with_interner(
            &mut strs,
            COND_BODY_STR,
        ).unwrap().unwrap();
        let cond = LispValue::Func(Box::new(LispFunc {
            args: vec![xs_symbol],
            body: parsed_cond_body,
            closure: None,
            variadic: true,
            is_macro: true,
        }));
        funcs.insert(cond_name, cond);
        funcs.insert(
            strs.get_or_intern_static("*host-language*"),
            LispValue::String("Rust".to_owned()),
        );

        funcs
    };
}

#[cfg(feature = "io-stdlib")]
lazy_static! {
    pub static ref BUILTINS: HashMap<LispSymbol, LispValue> = {
        let base = BUILTINS_NO_IO.clone();
        let mut strs = LispEnv::interner_mut();
        let mut ext = make_lisp_funcs!(strs,
            "prn" => lisp_prn,
            "readline" => lisp_readline,
            "slurp" => lisp_slurp,
            "load-file" => lisp_load_file,
            "println" => lisp_println,
            "time-ms" => lisp_time_ms,
        );

        let args: Vec<String> = std::env::args().collect();
        let mut lisp_argv: Vector<LispValue> = args.into_iter()
            // Lisp *ARGV* doesn't include the interpreter name
            .skip(1)
            .map(LispValue::String)
            .collect();
        lisp_argv.push_front(LispValue::Symbol(strs.get_or_intern_static("list")));
        ext.insert(
            strs.get_or_intern_static("*ARGV*"),
            LispValue::List(lisp_argv),
        );

        base.union(ext)
    };
}
