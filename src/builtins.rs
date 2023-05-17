use std::sync::{Arc, RwLock};
use std::ops::Deref;
use im::{hashmap, HashMap, vector, Vector};
use lazy_static::lazy_static;
use ordered_float::OrderedFloat;
use by_address::ByAddress;
use crate::{LispValue, LispError, Result, expect};
use crate::env::{LispEnv, LispSymbol};
use crate::eval::{eval, expand_macros};
use crate::parser::LispParser;
use crate::util::{LispFunc, ObjectValue, LispBuiltinResult};

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        use std::fs;
        use std::io::prelude::*;
        use std::time::{SystemTime, UNIX_EPOCH};
    }
}
cfg_if::cfg_if! {
    if #[cfg(feature = "async")] {
        use futures::prelude::*;
        use futures::future;
        use crate::util::LispAsyncValue;
    }
}

cfg_if::cfg_if! {
    if #[cfg(debug_assertions)] {
        macro_rules! pop_head {
            ($args:expr) => {
                $args.pop_front().unwrap()
            }
        }
        macro_rules! pop_tail {
            ($args:expr) => {
                $args.pop_back().unwrap()
            }
        }
        fn unreachable() -> ! {
            unreachable!()
        }
    } else {
        macro_rules! pop_head {
            ($args:expr) => {
                unsafe { $args.pop_front().unwrap_unchecked() }
            }
        }
        macro_rules! pop_tail {
            ($args:expr) => {
                unsafe { $args.pop_back().unwrap_unchecked() }
            }
        }
        fn unreachable() -> ! {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }
}
macro_rules! eval_head {
    ($list:expr, $env:expr) => {
        eval(pop_head!($list), $env)
    }
}
macro_rules! eval_tail {
    ($list:expr, $env:expr) => {
        eval(pop_tail!($list), $env)
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
    let first = eval_head!(args, env)?.expect_number()?;
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
    let numerator = eval_head!(args, env)?.expect_number()?;
    let denominator = eval_head!(args, env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Number(numerator / denominator))
}

fn lisp_int_divide(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval_head!(args, env)?.expect_number()?;
    let denominator = eval_head!(args, env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Number(OrderedFloat(
        (numerator / denominator).trunc()
    )))
}

fn lisp_def(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let val = eval_tail!(args, env)?;
    env.set(name, val.clone());
    LispBuiltinResult::Done(val)
}

fn lisp_let(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut new_env = env.new_nested();
    let list = pop_head!(args).into_list()?;
    expect!(list.len() & 1 == 0, LispError::MissingBinding);
    let mut list_iter = list.into_iter();
    while let Some(name) = list_iter.next() {
        let Some(val_expr) = list_iter.next() else { unreachable() };
        let name = name.expect_symbol()?;
        let val = eval(val_expr, &mut new_env)?;
        new_env.set(name, val);
    }
    let head = pop_head!(args);
    LispBuiltinResult::ContinueIn(head, new_env)
}

fn lisp_if(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() > 1 && args.len() < 4, LispError::IncorrectArguments(2, args.len()));
    let pred = eval_head!(args, env)?.truthiness();
    if pred {
        let then = pop_head!(args);
        LispBuiltinResult::Continue(then)
    } else if args.len() > 1 {
        let otherwise = pop_tail!(args);
        LispBuiltinResult::Continue(otherwise)
    } else {
        LispBuiltinResult::Done(LispValue::Nil)
    }
}

fn lisp_do(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let last_eval = pop_tail!(args);
    for val in args.into_iter() {
        eval(val, env)?;
    }
    LispBuiltinResult::Continue(last_eval)
}

fn lisp_fn(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let args_list = pop_head!(args).into_list()?;
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
    let body = pop_head!(args);
    let closure = env.make_closure();
    let inner = ObjectValue::Func(LispFunc {
        args: arg_names,
        body,
        closure,
        variadic,
        is_macro: false,
    });
    LispBuiltinResult::Done(LispValue::Object(Arc::new(inner)))
}

fn lisp_equals(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_head!(args, env)?.vector_to_list();
    let y = eval_head!(args, env)?.vector_to_list();
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
    LispBuiltinResult::Done(LispValue::list_from(vals))
}

fn lisp_listq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(match val {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::List(_)),
        _ => false,
    }))
}

fn lisp_emptyq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval_head!(args, env)?;
    if val.is_nil() {
        LispBuiltinResult::Done(LispValue::Bool(true))
    } else {
        let list = val.into_list()?;
        LispBuiltinResult::Done(LispValue::Bool(list.is_empty()))
    }
}

fn lisp_count(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval_head!(args, env)?;
    if val.is_nil() {
        LispBuiltinResult::Done(LispValue::Number(OrderedFloat(0.0)))
    } else {
        let list = val.into_list()?;
        LispBuiltinResult::Done(LispValue::Number(OrderedFloat(list.len() as f64)))
    }
}

fn lisp_lt(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_head!(args, env)?.expect_number()?;
    let y = eval_head!(args, env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Bool(x < y))
}

fn lisp_lte(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_head!(args, env)?.expect_number()?;
    let y = eval_head!(args, env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Bool(x <= y))
}

fn lisp_gt(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_head!(args, env)?.expect_number()?;
    let y = eval_head!(args, env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Bool(x > y))
}

fn lisp_gte(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval_head!(args, env)?.expect_number()?;
    let y = eval_head!(args, env)?.expect_number()?;
    LispBuiltinResult::Done(LispValue::Bool(x >= y))
}

fn lisp_not(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval_head!(args, env)?.truthiness();
    LispBuiltinResult::Done(LispValue::Bool(!x))
}

fn lisp_atom(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Atom(ByAddress(Arc::new(RwLock::new(x)))))
}

fn lisp_atomq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(val, LispValue::Atom(_))))
}

fn lisp_deref(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let atom = eval_head!(args, env)?.into_atom()?;
    let out = atom.read().unwrap().clone();
    LispBuiltinResult::Done(out)
}

fn lisp_reset(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let atom = eval_head!(args, env)?.into_atom()?;
    let val = eval_head!(args, env)?;
    *atom.write().unwrap() = val.clone();
    LispBuiltinResult::Done(val)
}

fn lisp_swap(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let atom = eval_head!(args, env)?.into_atom()?;
    let val = eval_head!(args, env)?;
    let derefed = atom.read().unwrap().clone();
    let mut list = vector![
        val,
        derefed,
    ];
    list.append(args);
    let out_val = eval(LispValue::list_from(list), env)?;
    *atom.write().unwrap() = out_val.clone();
    LispBuiltinResult::Done(out_val)
}

fn lisp_eval(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    let global = env.global();
    LispBuiltinResult::ContinueIn(arg, global)
}

#[cfg(feature = "io-stdlib")]
fn lisp_readline(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    if !args.is_empty() {
        let s = eval_head!(args, env)?.into_string()?;
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
    let code_expr = eval_head!(args, env)?.into_string()?;
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
    let file_name = eval_head!(args, env)?.into_string()?;
    let contents = fs::read_to_string(file_name)?;
    LispBuiltinResult::Done(LispValue::String(contents))
}

#[cfg(feature = "io-stdlib")]
fn lisp_load_file(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let file_name = eval_head!(args, env)?.into_string()?;
    let contents = fs::read_to_string(file_name)?;

    let mut parser = LispParser::new();
    parser.add_tokenize(&contents)?;
    let mut global = env.global();
    for val in parser {
        eval(val?, &mut global)?;
    }
    LispBuiltinResult::Done(LispValue::Nil)
}

fn lisp_typeof(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::String(x.type_of().to_owned()))
}

fn lisp_quote(mut args: Vector<LispValue>, _env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    LispBuiltinResult::Done(pop_head!(args))
}

fn inner_quasiquote(arg: LispValue, env: &mut LispEnv) -> Result<(LispValue, bool)> {
    let unquote_sym = LispEnv::symbol_for_static("unquote");
    let splice_sym = LispEnv::symbol_for_static("splice-unquote");
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(l) => {
                if l.is_empty() {
                    return Ok((LispValue::Object(o.clone()), false));
                }
                let ObjectValue::List(mut l) = Arc::unwrap_or_clone(o) else { unreachable!() };
                // established non-empty just prior
                if l[0] == LispValue::Symbol(unquote_sym) {
                    expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                    Ok((eval_tail!(l, env)?, false))
                } else if l[0] == LispValue::Symbol(splice_sym) {
                    expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                    Ok((eval_tail!(l, env)?, true))
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
                    Ok((LispValue::list_from(out), false))
                }
            },
            _ => Ok((LispValue::Object(o.clone()), false)),
        },
        x => Ok((x, false)),
    }
}

fn lisp_quasiquote(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let front = pop_head!(args);
    match front {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(list) => {
                if list.is_empty() {
                    return LispBuiltinResult::Done(LispValue::Object(o.clone()));
                }
                let ObjectValue::List(mut list) = Arc::unwrap_or_clone(o) else { unreachable!() };
                let unquote_sym = LispEnv::symbol_for_static("unquote");
                let splice_sym = LispEnv::symbol_for_static("splice-unquote");
                if list[0] == LispValue::Symbol(unquote_sym) || list[0] == LispValue::Symbol(splice_sym) {
                    expect!(list.len() == 2, LispError::IncorrectArguments(1, list.len() - 1));
                    return LispBuiltinResult::Continue(pop_tail!(list));
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
                LispBuiltinResult::Done(LispValue::list_from(out))
            },
            _ => LispBuiltinResult::Done(LispValue::Object(o.clone())),
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
    let cons = eval_head!(args, env)?;
    let mut list = eval_head!(args, env)?.into_list()?;
    list.push_front(cons);
    LispBuiltinResult::Done(LispValue::list_from(list))
}

fn lisp_concat(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    let mut out = Vector::new();
    for arg in args.into_iter() {
        let arg = eval(arg, env)?;
        let list = arg.into_list()?;
        out.append(list);
    }
    LispBuiltinResult::Done(LispValue::list_from(out))
}

fn lisp_nth(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut list = eval_head!(args, env)?.into_list()?;
    let idx = eval_head!(args, env)?.expect_number()?.into_inner() as usize;
    expect!(idx < list.len(), LispError::IndexOutOfRange(idx));
    LispBuiltinResult::Done(list.remove(idx))
}

fn lisp_first(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
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
    let arg = eval_head!(args, env)?;
    if arg.is_nil() {
        return LispBuiltinResult::Done(LispValue::list_from(vector![]));
    }
    let mut list = arg.into_list()?;
    list.pop_front();
    LispBuiltinResult::Done(LispValue::list_from(list))
}

fn lisp_macroq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    if let Ok(f) = arg.expect_func() {
        LispBuiltinResult::Done(LispValue::Bool(f.is_macro))
    } else {
        LispBuiltinResult::Done(LispValue::Bool(false))
    }
}

fn lisp_defmacro(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let val = eval_tail!(args, env)?;
    let val = match val {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::Func(f) => {
                let mut f = f.clone();
                f.is_macro = true;
                LispValue::Object(Arc::new(ObjectValue::Func(f)))
            },
            x => return LispBuiltinResult::Error(LispError::InvalidDataType("function", x.type_of())),
        }
        x => return LispBuiltinResult::Error(LispError::InvalidDataType("function", x.type_of())),
    };
    env.set(name, val.clone());
    LispBuiltinResult::Done(val)
}

fn lisp_macroexpand(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    LispBuiltinResult::Done(expand_macros(pop_head!(args), env)?)
}

fn lisp_inspect(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::String(val.inspect()))
}

fn lisp_try(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let catch = pop_tail!(args);
    if let Ok(mut catch) = catch.into_list() {
        expect!(catch.len() == 3, LispError::IncorrectArguments(2, catch.len() - 1));
        let catch_sym = pop_head!(catch);
        let base_catch_sym = LispValue::Symbol(LispEnv::symbol_for_static("catch*"));
        if catch_sym != base_catch_sym {
            return LispBuiltinResult::Error(LispError::TryNoCatch);
        }
        let err_name = pop_head!(catch).expect_symbol()?;
        let mut caught_env = env.new_nested();
        let catch = pop_head!(catch);
        match eval_head!(args, env) {
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
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Error(LispError::UncaughtException(arg))
}

fn lisp_nilq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(arg.is_nil()))
}

fn lisp_trueq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Bool(true))))
}

fn lisp_falseq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Bool(false))))
}

fn lisp_symbolq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Symbol(_))))
}

fn lisp_symbol(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let s = eval_head!(args, env)?.into_string()?;
    LispBuiltinResult::Done(LispValue::Symbol(LispEnv::symbol_for(&s)))
}

fn lisp_vector(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    let vals: Vec<LispValue> = args.into_iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?;
    LispBuiltinResult::Done(LispValue::vector_from(vals))
}

fn lisp_vectorq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::Vector(_)),
        _ => false,
    }))
}

fn lisp_keyword(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(match arg {
        LispValue::Keyword(s) => Ok(LispValue::Keyword(s)),
        LispValue::String(s) => Ok(LispValue::Keyword(s)),
        LispValue::Symbol(s) => Ok(LispValue::Keyword(LispEnv::symbol_string(s).unwrap().to_owned())),
        x => Err(LispError::InvalidDataType("string", x.type_of())),
    }?)
}

fn lisp_keywordq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Keyword(_))))
}

fn lisp_hashmap(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() & 1 == 0, LispError::MissingBinding);
    let mut pairs = vec![];
    let mut arg_iter = args.into_iter();
    while let Some(key_expr) = arg_iter.next() {
        let Some(val_expr) = arg_iter.next() else { unreachable() };
        let k = eval(key_expr, env)?;
        let v = eval(val_expr, env)?;
        pairs.push((k, v))
    }
    LispBuiltinResult::Done(LispValue::map_from(pairs))
}

fn lisp_mapq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::Map(_)),
        _ => false,
    }))
}

fn lisp_sequentialq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::List(_) | ObjectValue::Vector(_)),
        _ => false,
    }))
}

fn lisp_assoc(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() & 1 == 1, LispError::MissingBinding);
    let mut map = eval_head!(args, env)?.into_hashmap()?;
    let mut arg_iter = args.into_iter();
    while let Some(key_expr) = arg_iter.next() {
        let Some(val_expr) = arg_iter.next() else { unreachable() };
        let k = eval(key_expr, env)?;
        let v = eval(val_expr, env)?;
        map.insert(k, v);
    }
    LispBuiltinResult::Done(LispValue::map_from(map))
}

fn lisp_dissoc(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let base_map = eval_head!(args, env)?.into_hashmap()?;
    let keys = args.into_iter().map(|x| {
        Ok((eval(x, env)?, LispValue::Nil))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    LispBuiltinResult::Done(LispValue::map_from(base_map.difference(keys.into())))
}

fn lisp_get(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval_head!(args, env)?;
    let key = eval_head!(args, env)?;
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
    let map = eval_head!(args, env)?.into_hashmap()?;
    let key = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(map.contains_key(&key)))
}

fn lisp_keys(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval_head!(args, env)?.into_hashmap()?;
    let keys = map.keys().cloned();
    LispBuiltinResult::Done(LispValue::list_from(keys))
}

fn lisp_vals(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval_head!(args, env)?.into_hashmap()?;
    let vals = map.values().cloned();
    LispBuiltinResult::Done(LispValue::list_from(vals))
}

fn lisp_apply(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let f = eval_head!(args, env)?;
    let list = eval_tail!(args, env)?.into_list()?;
    let mut full_list = vector![f];
    full_list.append(args);
    full_list.append(list);
    LispBuiltinResult::Continue(LispValue::list_from(full_list))
}

fn lisp_map(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let f = eval_head!(args, env)?;
    let list = eval_head!(args, env)?.into_list()?;
    let out = list.into_iter().map(|x| {
        let list = LispValue::list_from(vector![f.clone(), x]);
        eval(list, env)
    }).collect::<Result<Vector<LispValue>>>()?;
    LispBuiltinResult::Done(LispValue::list_from(out))
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
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::Func(_) | ObjectValue::BuiltinFunc { .. }),
        _ => false,
    }))
}

fn lisp_stringq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::String(_))))
}

fn lisp_numberq(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    LispBuiltinResult::Done(LispValue::Bool(matches!(arg, LispValue::Number(_))))
}

fn lisp_vec(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval_head!(args, env)?;
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::Vector(_) => {
                LispBuiltinResult::Done(LispValue::Object(o.clone()))
            },
            ObjectValue::List(l) => {
                LispBuiltinResult::Done(
                    LispValue::vector_from(
                        l.iter().cloned()
                    )
                )
            }
            x => LispBuiltinResult::Error(LispError::InvalidDataType("list", x.type_of())),
        },
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
    let arg = eval_head!(args, env)?;
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(l) => LispBuiltinResult::Done(if l.is_empty() {
                LispValue::Nil
            } else {
                LispValue::Object(o.clone())
            }),
            ObjectValue::Vector(l) => LispBuiltinResult::Done(if l.is_empty() {
                LispValue::Nil
            } else {
                LispValue::list_from(l.iter().cloned())
            }),
            x => LispBuiltinResult::Error(LispError::InvalidDataType("list", x.type_of())),
        },
        LispValue::String(s) => {
            LispBuiltinResult::Done(if s.is_empty() {
                LispValue::Nil
            } else {
                LispValue::list_from(s.chars().map(|x| x.to_string().into()))
            })
        },
        LispValue::Nil => LispBuiltinResult::Done(LispValue::Nil),
        x => LispBuiltinResult::Error(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_conj(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let first = eval_head!(args, env)?;
    let args = args.into_iter().map(|x| eval(x, env));
    match first {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::Vector(l) => {
                let mut args = args.collect::<Result<Vec<LispValue>>>()?;
                if l.is_empty() {
                    return LispBuiltinResult::Done(LispValue::vector_from(args));
                }
                let ObjectValue::Vector(mut l) = Arc::unwrap_or_clone(o) else {
                    unreachable!()
                };
                l.append(&mut args);
                LispBuiltinResult::Done(LispValue::vector_from(l))
            },
            ObjectValue::List(l) => {
                let mut args = args.rev().collect::<Result<Vector<LispValue>>>()?;
                if l.is_empty() {
                    return LispBuiltinResult::Done(LispValue::list_from(args));
                }
                let ObjectValue::List(l) = Arc::unwrap_or_clone(o) else {
                    unreachable!()
                };
                args.append(l);
                LispBuiltinResult::Done(LispValue::list_from(args))
            },
            x => LispBuiltinResult::Error(LispError::InvalidDataType("list", x.type_of())),
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

cfg_if::cfg_if! {
    if #[cfg(feature = "async")] {
        fn lisp_promise(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
            expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
            let expr = pop_head!(args);
            let mut env = env.clone();
            let fut = future::lazy(move |_| eval(expr, &mut env)).boxed();
            LispBuiltinResult::Done(fut.into())
        }

        fn lisp_resolve(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
            expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
            let result = eval_head!(args, env)?;
            let fut = future::ready(Ok(result)).boxed();
            LispBuiltinResult::Done(fut.into())
        }

        fn lisp_reject(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
            expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
            let result = eval_head!(args, env)?;
            let fut = future::ready(Err(LispError::UncaughtException(result))).boxed();
            LispBuiltinResult::Done(fut.into())
        }

        fn lisp_await(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
            expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
            let fut = eval_head!(args, env)?.into_future()?;
            futures::executor::block_on(fut).into()
        }

        fn lisp_join(args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
            expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
            let futs = args.into_iter()
                .map(|x| eval(x, env)?.into_future())
                .collect::<Result<Vec<LispAsyncValue>>>()?;
            let joined = future::join_all(futs);
            LispBuiltinResult::Done(joined.map(|x| {
                Ok(LispValue::list_from(x.into_iter().collect::<Result<Vector<LispValue>>>()?))
            }).boxed().into())
        }
    }
}

#[cfg(feature = "async-io")]
fn lisp_readline_async(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    let mut env = env.clone();
    LispBuiltinResult::Done(async move {
        if !args.is_empty() {
            let s = eval_head!(args, &mut env)?.into_string()?;
            print!("{}", s);
            async_std::io::stdout().flush().await?;
        }
        let mut buffer = String::new();
        let len = async_std::io::stdin().read_line(&mut buffer).await?;
        if len == 0 {
            // eof
            Ok(LispValue::Nil)
        } else {
            buffer.pop(); // remove newline
            Ok(LispValue::String(buffer))
        }
    }.boxed().into())
}

#[cfg(feature = "async-io")]
fn lisp_slurp_async(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let mut env = env.clone();
    LispBuiltinResult::Done(async move {
        let file_name = eval_head!(args, &mut env)?.into_string()?;
        let contents = async_std::fs::read_to_string(file_name).await?;
        Ok(LispValue::String(contents))
    }.boxed().into())
}

fn lisp_cond(mut args: Vector<LispValue>, env: &mut LispEnv) -> LispBuiltinResult {
    expect!(args.len() & 1 == 0, LispError::OddCondArguments);
    while let Some(pred) = args.pop_front() {
        if eval(pred, env)?.truthiness() {
            return LispBuiltinResult::Continue(pop_head!(args));
        } else {
            pop_head!(args);
        }
    }
    LispBuiltinResult::Done(LispValue::Nil)
}

macro_rules! make_lisp_funcs {
    ($interner:expr, $($name:literal => $f:path,)*) => {
        hashmap! {
            $($interner.get_or_intern_static($name) => LispValue::Object(
                    Arc::new(ObjectValue::BuiltinFunc {
                        name: $name,
                        f: $f,
                    })
                )
            ),*
        }
    }
}

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
            "cond" => lisp_cond,
        );

        funcs.insert(
            strs.get_or_intern_static("*host-language*"),
            LispValue::String("Rust".to_owned()),
        );

        cfg_if::cfg_if! {
            if #[cfg(feature = "async")] {
                let async_funcs = make_lisp_funcs!(strs,
                    "promise" => lisp_promise,
                    "resolve" => lisp_resolve,
                    "reject" => lisp_reject,
                    "await" => lisp_await,
                    "join" => lisp_join,
                );
                funcs.union(async_funcs)
            } else {
                funcs
            }
        }
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
            LispValue::list_from(lisp_argv),
        );

        cfg_if::cfg_if! {
            if #[cfg(feature = "async-io")] {
                let async_funcs = make_lisp_funcs!(strs,
                    "readline-async" => lisp_readline_async,
                    "slurp-async" => lisp_slurp_async,
                );
                base.union(ext).union(async_funcs)
            } else {
                base.union(ext)
            }
        }
    };
}
