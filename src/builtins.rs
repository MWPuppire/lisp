use std::sync::{Arc, RwLock};
use im::{hashmap, HashMap, vector, Vector};
use lazy_static::lazy_static;
use ordered_float::OrderedFloat;
use crate::{LispValue, LispError, Result, expect};
use crate::env::{LispEnv, LispSymbol};
use crate::eval::{eval, eval_top, expand_macros};
use crate::parser::LispParser;
use crate::util::LispFunc;

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        use std::fs::File;
        use std::io::prelude::*;
        use std::time::Instant;
    }
}

fn eval_list_to_numbers(args: &Vector<LispValue>, env: &mut LispEnv) -> Result<Vec<OrderedFloat<f64>>> {
    args.iter().map(|x| eval(x, env)?.expect_number()).collect()
}

fn lisp_plus(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, &mut env)?;
    Ok((LispValue::Number(
        nums.iter().fold(OrderedFloat(0.0), |acc, x| acc + x)
    ), env, false))
}

fn lisp_minus(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let first = eval(&first, &mut env)?.expect_number()?;
    let nums = eval_list_to_numbers(&args, &mut env)?;
    Ok((LispValue::Number(
        first - nums.iter().fold(OrderedFloat(0.0), |acc, x| acc + x)
    ), env, false))
}

fn lisp_times(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(&args, &mut env)?;
    Ok((LispValue::Number(
        nums.iter().fold(OrderedFloat(1.0), |acc, x| acc * x)
    ), env, false))
}

fn lisp_divide(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(&args[0], &mut env)?.expect_number()?;
    let denominator = eval(&args[1], &mut env)?.expect_number()?;
    Ok((LispValue::Number(numerator / denominator), env, false))
}

fn lisp_int_divide(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let numerator = eval(&args[0], &mut env)?.expect_number()?;
    let denominator = eval(&args[1], &mut env)?.expect_number()?;
    Ok((LispValue::Number(OrderedFloat(
        (numerator / denominator).trunc()
    )), env, false))
}

fn lisp_def(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let val = eval(&args[1], &mut env)?;
    let val = match val {
        LispValue::Func(mut f) => {
            f.name = Some(name);
            LispValue::Func(f)
        },
        x => x,
    };
    if !env.set(name, val.clone()) {
        Err(LispError::AlreadyExists(LispEnv::symbol_string(name).unwrap()))
    } else {
        Ok((val, env, false))
    }
}

fn lisp_let(mut args: Vector<LispValue>, env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let mut new_env = env.new_nested();
    let Some(list) = args.pop_front() else { unreachable!() };
    let list = list.into_list()?;
    expect!(list.len() & 1 == 0, LispError::MissingBinding);
    let mut list_iter = list.iter();
    while let Some(name) = list_iter.next() {
        let Some(val_expr) = list_iter.next() else { unreachable!() };
        let name = name.expect_symbol()?;
        let val = eval(val_expr, &mut new_env)?;
        new_env.set(name, val);
    }
    let Some(head) = args.pop_front() else { unreachable!() };
    Ok((head, new_env, true))
}

fn lisp_if(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() > 1 && args.len() < 4, LispError::IncorrectArguments(2, args.len()));
    let Some(pred) = args.pop_front() else { unreachable!() };
    let pred = eval(&pred, &mut env)?.truthiness();
    if pred {
        let Some(then) = args.pop_front() else { unreachable!() };
        Ok((then, env, true))
    } else if args.len() > 1 {
        let Some(arg) = args.pop_back() else { unreachable!() };
        Ok((arg, env, true))
    } else {
        Ok((LispValue::Nil, env, false))
    }
}

fn lisp_do(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let Some(last_eval) = args.pop_back() else { unreachable!() };
    for val in args.iter() {
        eval(val, &mut env)?;
    }
    Ok((last_eval, env, true))
}

fn lisp_fn(mut args: Vector<LispValue>, env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
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
    let closure = env.make_closure();
    Ok((LispValue::Func(Box::new(LispFunc {
        args: arg_names,
        body,
        closure,
        variadic,
        is_macro: false,
        name: None,
    })), env, false))
}

fn lisp_equals(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], &mut env)?;
    let y = eval(&args[1], &mut env)?;
    Ok((LispValue::Bool(x == y), env, false))
}

#[cfg(feature = "io-stdlib")]
fn lisp_prn(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    if let Some(last) = args.pop_back() {
        for val in args.iter() {
            let val = eval(val, &mut env)?;
            print!("{} ", val.inspect());
        }
        let last_val = eval(&last, &mut env)?;
        println!("{}", last_val.inspect());
    } else {
        println!();
    }
    Ok((LispValue::Nil, env, false))
}

fn lisp_list(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    let vals = args.iter().map(|x| eval(x, &mut env)).collect::<Result<Vector<LispValue>>>()?;
    Ok((LispValue::List(vals), env, false))
}

fn lisp_listq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(val, LispValue::List(_))), env, false))
}

fn lisp_emptyq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(match val {
        LispValue::List(l) => Ok(l.is_empty()),
        LispValue::Vector(l) => Ok(l.is_empty()),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }?), env, false))
}

fn lisp_count(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], &mut env)?;
    if val.is_nil() {
        Ok((LispValue::Number(OrderedFloat(0.0)), env, false))
    } else {
        Ok((LispValue::Number(OrderedFloat(match val {
            LispValue::List(l) => Ok(l.len() as f64),
            LispValue::Vector(l) => Ok(l.len() as f64),
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        }?)), env, false))
    }
}

fn lisp_lt(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], &mut env)?.expect_number()?;
    let y = eval(&args[1], &mut env)?.expect_number()?;
    Ok((LispValue::Bool(x < y), env, false))
}

fn lisp_lte(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], &mut env)?.expect_number()?;
    let y = eval(&args[1], &mut env)?.expect_number()?;
    Ok((LispValue::Bool(x <= y), env, false))
}

fn lisp_gt(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], &mut env)?.expect_number()?;
    let y = eval(&args[1], &mut env)?.expect_number()?;
    Ok((LispValue::Bool(x > y), env, false))
}

fn lisp_gte(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let x = eval(&args[0], &mut env)?.expect_number()?;
    let y = eval(&args[1], &mut env)?.expect_number()?;
    Ok((LispValue::Bool(x >= y), env, false))
}

fn lisp_not(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?.truthiness();
    Ok((LispValue::Bool(!x), env, false))
}

fn lisp_atom(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?;
    Ok((LispValue::Atom(Arc::new(RwLock::new(x))), env, false))
}

fn lisp_atomq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(val, LispValue::Atom(_))), env, false))
}

fn lisp_deref(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], &mut env)?;
    let atom = val.expect_atom()?;
    let out = atom.read().unwrap().clone();
    Ok((out, env, false))
}

fn lisp_reset(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let val = eval(&args[0], &mut env)?;
    let atom = val.expect_atom()?;
    let val = eval(&args[1], &mut env)?;
    *atom.write().unwrap() = val.clone();
    Ok((val, env, false))
}

fn lisp_swap(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let Some(first) = args.pop_front() else { unreachable!() };
    let Some(second) = args.pop_front() else { unreachable!() };
    let val = eval(&first, &mut env)?;
    let atom = val.expect_atom()?;
    let val = eval(&second, &mut env)?;
    let deref_sym = LispEnv::symbol_for_static("deref");
    let mut list = vector![
        val,
        LispValue::List(vector![
            LispValue::Symbol(deref_sym),
            LispValue::Atom(atom.clone()),
        ]),
    ];
    list.append(args);
    let out_val = eval(&LispValue::List(list), &mut env)?;
    *atom.write().unwrap() = out_val.clone();
    Ok((out_val, env, false))
}

fn lisp_eval(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    let global = env.global();
    Ok((arg, global, true))
}

#[cfg(feature = "io-stdlib")]
fn lisp_readline(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    if !args.is_empty() {
        let val = eval(&args[0], &mut env)?;
        let s = val.expect_string()?;
        print!("{}", s);
        std::io::stdout().flush()?;
    }
    let mut buffer = String::new();
    let len = std::io::stdin().read_line(&mut buffer)?;
    if len == 0 {
        // eof
        Ok((LispValue::Nil, env, false))
    } else {
        buffer.pop(); // remove newline
        Ok((LispValue::String(buffer), env, false))
    }
}

fn lisp_read_string(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    let code_expr = arg.expect_string()?;
    let parsed = LispParser::parse(code_expr);
    if let Some(parsed) = parsed {
        Ok((parsed?, env, false))
    } else {
        Ok((LispValue::Nil, env, false))
    }
}

fn lisp_str(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    let mut buffer = String::new();
    for arg in args.iter() {
        let x = eval(arg, &mut env)?;
        buffer.push_str(&x.to_string());
    }
    Ok((LispValue::String(buffer), env, false))
}

#[cfg(feature = "io-stdlib")]
fn lisp_slurp(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?;
    let file_name = x.expect_string()?;
    let mut f = File::open(file_name)?;
    let mut buffer = String::new();
    f.read_to_string(&mut buffer)?;
    Ok((LispValue::String(buffer), env, false))
}

#[cfg(feature = "io-stdlib")]
fn lisp_load_file(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?;
    let file_name = x.expect_string()?;
    let mut f = File::open(file_name)?;
    let mut buffer = String::new();
    f.read_to_string(&mut buffer)?;

    let mut parser = LispParser::new();
    parser.add_tokenize(&buffer);
    let mut global = env.global();
    for val in parser {
        eval(&val?, &mut global)?;
    }
    Ok((LispValue::Nil, env, false))
}

fn lisp_typeof(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?;
    Ok((LispValue::String(x.type_of().to_owned()), env, false))
}

fn lisp_quote(mut args: Vector<LispValue>, env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    if let Some(out) = args.pop_front() {
        Ok((out, env, false))
    } else {
        unreachable!();
    }
}

fn inner_quasiquote(arg: LispValue, env: &mut LispEnv) -> Result<(LispValue, bool)> {
    let unquote_sym = LispEnv::symbol_for_static("unquote");
    let splice_sym = LispEnv::symbol_for_static("splice-unquote");
    match arg {
        LispValue::List(l) => {
            if l.is_empty() {
                Ok((LispValue::List(l), false))
            } else if l[0] == LispValue::Symbol(unquote_sym) {
                expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                Ok((eval_top(&l[1], env)?, false))
            } else if l[0] == LispValue::Symbol(splice_sym) {
                expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                Ok((eval_top(&l[1], env)?, true))
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

fn lisp_quasiquote(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let Some(front) = args.pop_front() else { unreachable!() };
    match front {
        LispValue::List(mut list) => {
            if list.is_empty() {
                return Ok((LispValue::List(Vector::new()), env, false));
            }
            let unquote_sym = LispEnv::symbol_for_static("unquote");
            let splice_sym = LispEnv::symbol_for_static("splice-unquote");
            if list[0] == LispValue::Symbol(unquote_sym) || list[0] == LispValue::Symbol(splice_sym) {
                expect!(list.len() == 2, LispError::IncorrectArguments(1, list.len() - 1));
                let Some(val) = list.pop_back() else { unreachable!() };
                return Ok((val, env, true));
            }
            let mut out = Vector::new();
            for val in list {
                let (new_val, inplace) = inner_quasiquote(val, &mut env)?;
                if inplace {
                    out.append(new_val.into_list()?);
                } else {
                    out.push_back(new_val);
                }
            }
            Ok((LispValue::List(out), env, false))
        },
        x => Ok((x, env, false)),
    }
}

// `lisp_quasiquote` handles the `unquote` and `splice-unquote` methods itself,
// so this can only be called outside `quasiquote` (which is an error)
fn lisp_unquote(_args: Vector<LispValue>, _env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    Err(LispError::OnlyInQuasiquote)
}

fn lisp_cons(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(&args[0], &mut env)?;
    let arg2 = eval(&args[1], &mut env)?;
    let mut list = arg2.into_list()?;
    list.push_front(arg1);
    Ok((LispValue::List(list), env, false))
}

fn lisp_concat(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    let mut out = Vector::new();
    for arg in args.iter() {
        let arg = eval(arg, &mut env)?;
        let list = arg.into_list()?;
        out.append(list);
    }
    Ok((LispValue::List(out), env, false))
}

fn lisp_nth(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let arg1 = eval(&args[0], &mut env)?;
    let arg2 = eval(&args[1], &mut env)?;
    let mut list = arg1.into_list()?;
    let idx = arg2.expect_number()?.into_inner() as usize;
    expect!(idx < list.len(), LispError::IndexOutOfRange(idx));
    Ok((list.remove(idx), env, false))
}

fn lisp_first(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    if arg.is_nil() {
        return Ok((LispValue::Nil, env, false));
    }
    let mut list = arg.into_list()?;
    if let Some(item) = list.pop_front() {
        Ok((item, env, false))
    } else {
        Ok((LispValue::Nil, env, false))
    }
}

fn lisp_rest(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    if arg.is_nil() {
        return Ok((LispValue::List(Vector::new()), env, false));
    }
    let mut list = arg.into_list()?;
    list.pop_front();
    Ok((LispValue::List(list), env, false))
}

fn lisp_macroq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((match arg {
        LispValue::Func(f) => LispValue::Bool(f.is_macro),
        _ => LispValue::Bool(false),
    }, env, false))
}

fn lisp_defmacro(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let name = args[0].expect_symbol()?;
    let mut val = eval(&args[1], &mut env)?;
    match val {
        LispValue::Func(ref mut f) => {
            f.name = Some(name.to_owned());
            f.is_macro = true;
        },
        _ => return Err(LispError::InvalidDataType("function", val.type_of())),
    }
    if !env.set(name, val.clone()) {
        Err(LispError::AlreadyExists(LispEnv::symbol_string(name).unwrap()))
    } else {
        Ok((val, env, false))
    }
}

fn lisp_macroexpand(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    Ok((expand_macros(&args[0], &mut env)?, env, false))
}

fn lisp_inspect(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let val = eval(&args[0], &mut env)?;
    Ok((LispValue::String(val.inspect()), env, false))
}

fn lisp_try(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let Some(catch) = args.pop_back() else { unreachable!() };
    if let Ok(mut catch) = catch.into_list() {
        expect!(catch.len() == 3, LispError::IncorrectArguments(2, args.len() - 1));
        let Some(catch_sym) = catch.pop_front() else { unreachable!() };
        let base_catch_sym = LispValue::Symbol(LispEnv::symbol_for_static("catch*"));
        if catch_sym != base_catch_sym {
            return Err(LispError::TryNoCatch);
        }
        let Some(err_name) = catch.pop_front() else { unreachable!() };
        let err_name = err_name.expect_symbol()?;
        let mut caught_env = env.new_nested();
        let Some(catch) = catch.pop_front() else { unreachable!() };
        match eval(&args[0], &mut env) {
            Ok(x) => Ok((x, env, false)),
            Err(LispError::UncaughtException(except)) => {
                caught_env.set(err_name, except);
                Ok((catch, caught_env, true))
            },
            Err(err) => {
                let s = err.to_string();
                caught_env.set(err_name, LispValue::String(s));
                Ok((catch, caught_env, true))
            }
        }
    } else {
        Err(LispError::TryNoCatch)
    }
}

// `lisp_try` handles `catch`, which can't be used outside `try`,
// so this is an error
fn lisp_catch(_args: Vector<LispValue>, _env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    Err(LispError::OnlyInTry)
}

fn lisp_throw(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Err(LispError::UncaughtException(arg))
}

fn lisp_nilq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(arg.is_nil()), env, false))
}

fn lisp_trueq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::Bool(true))), env, false))
}

fn lisp_falseq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::Bool(false))), env, false))
}

fn lisp_symbolq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::Symbol(_))), env, false))
}

fn lisp_symbol(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    let s = arg.expect_string()?;
    Ok((LispValue::Symbol(LispEnv::symbol_for(s)), env, false))
}

fn lisp_vector(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    let vals: Vec<LispValue> = args.iter().map(|x| eval(x, &mut env)).collect::<Result<Vec<LispValue>>>()?;
    Ok((LispValue::Vector(vals), env, false))
}

fn lisp_vectorq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::Vector(_))), env, false))
}

fn lisp_keyword(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((match arg {
        LispValue::Keyword(s) => Ok(LispValue::Keyword(s)),
        LispValue::String(s) => Ok(LispValue::Keyword(s)),
        LispValue::Symbol(s) => Ok(LispValue::Keyword(LispEnv::symbol_string(s).unwrap().to_owned())),
        x => Err(LispError::InvalidDataType("string", x.type_of())),
    }?, env, false))
}

fn lisp_keywordq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::Keyword(_))), env, false))
}

fn lisp_hashmap(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() & 1 == 0, LispError::MissingBinding);
    let mut pairs = vec![];
    let mut arg_iter = args.iter();
    while let Some(key_expr) = arg_iter.next() {
        let Some(val_expr) = arg_iter.next() else { unreachable!() };
        let k = eval(key_expr, &mut env)?;
        let v = eval(val_expr, &mut env)?;
        pairs.push((k, v))
    }
    Ok((LispValue::Map(pairs.into()), env, false))
}

fn lisp_mapq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::Map(_))), env, false))
}

fn lisp_sequentialq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::List(_) | LispValue::Vector(_))), env, false))
}

fn lisp_assoc(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() & 1 == 1, LispError::MissingBinding);
    let Some(first) = args.pop_front() else { unreachable!() };
    let map = eval(&first, &mut env)?;
    let mut map = map.into_hashmap()?;
    let mut arg_iter = args.iter();
    while let Some(key_expr) = arg_iter.next() {
        let Some(val_expr) = arg_iter.next() else { unreachable!() };
        let k = eval(key_expr, &mut env)?;
        let v = eval(val_expr, &mut env)?;
        map.insert(k, v);
    }
    Ok((LispValue::Map(map), env, false))
}

fn lisp_dissoc(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let base_map = eval(&first, &mut env)?;
    let base_map = base_map.into_hashmap()?;
    let keys = args.iter().map(|x| {
        Ok((eval(x, &mut env)?, LispValue::Nil))
    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?;
    Ok((LispValue::Map(base_map.difference(keys.into())), env, false))
}

fn lisp_get(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval(&args[0], &mut env)?;
    let key = eval(&args[1], &mut env)?;
    if map.is_nil() {
        Ok((LispValue::Nil, env, false))
    } else {
        let map = map.into_hashmap()?;
        if let Some(val) = map.get(&key) {
            Ok((val.clone(), env, false))
        } else {
            Ok((LispValue::Nil, env, false))
        }
    }
}

fn lisp_containsq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let map = eval(&args[0], &mut env)?;
    let map = map.into_hashmap()?;
    let key = eval(&args[1], &mut env)?;
    Ok((LispValue::Bool(map.contains_key(&key)), env, false))
}

fn lisp_keys(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval(&args[0], &mut env)?;
    let map = map.into_hashmap()?;
    let keys = map.keys().cloned().collect();
    Ok((LispValue::List(keys), env, false))
}

fn lisp_vals(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let map = eval(&args[0], &mut env)?;
    let map = map.into_hashmap()?;
    let vals = map.values().cloned().collect();
    Ok((LispValue::List(vals), env, false))
}

fn lisp_apply(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let Some(first) = args.pop_front() else { unreachable!() };
    let f = eval(&first, &mut env)?;
    let Some(last) = args.pop_back() else { unreachable!() };
    let list = eval(&last, &mut env)?;
    let list = list.into_list()?;
    let mut full_list = vector![f];
    full_list.append(args);
    full_list.append(list);
    Ok((LispValue::List(full_list), env, true))
}

fn lisp_map(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 2, LispError::IncorrectArguments(2, args.len()));
    let f = eval(&args[0], &mut env)?;
    let list = eval(&args[1], &mut env)?;
    let list = list.into_list()?;
    let out = list.into_iter().map(|x| {
        let list = LispValue::List(vector![f.clone(), x]);
        eval(&list, &mut env)
    }).collect::<Result<Vector<LispValue>>>()?;
    Ok((LispValue::List(out), env, false))
}

fn lisp_pr_str(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    let full_str = args.iter().map(|x| {
        Ok(eval(x, &mut env)?.inspect())
    }).collect::<Result<Vec<String>>>()?;
    Ok((LispValue::String(full_str.join(" ")), env, false))
}

#[cfg(feature = "io-stdlib")]
fn lisp_println(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    if let Some(last) = args.pop_back() {
        for val in args.iter() {
            let val = eval(val, &mut env)?;
            print!("{} ", val);
        }
        println!("{}", eval(&last, &mut env)?);
    } else {
        println!();
    }
    Ok((LispValue::Nil, env, false))
}

fn lisp_fnq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::Func(_) | LispValue::BuiltinFunc { .. })), env, false))
}

fn lisp_stringq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::String(_))), env, false))
}

fn lisp_numberq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    Ok((LispValue::Bool(matches!(arg, LispValue::Number(_))), env, false))
}

fn lisp_vec(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    match arg {
        LispValue::Vector(l) => Ok((LispValue::Vector(l), env, false)),
        LispValue::List(l) => Ok((LispValue::Vector(l.into_iter().collect()), env, false)),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        lazy_static! {
            static ref START_TIME: Instant = Instant::now();
        }
        fn lisp_time_ms(_args: Vector<LispValue>, env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
            Ok((LispValue::Number(OrderedFloat(
                START_TIME.elapsed().as_secs_f64() * 1000.0
            )), env, false))
        }
    }
}

fn lisp_seq(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let arg = eval(&args[0], &mut env)?;
    match arg {
        LispValue::Vector(l) => {
            Ok((if l.is_empty() {
                LispValue::Nil
            } else {
                LispValue::List(l.into())
            }, env, false))
        },
        LispValue::List(l) => {
            Ok((if l.is_empty() {
                LispValue::Nil
            } else {
                LispValue::List(l)
            }, env, false))
        },
        LispValue::String(s) => {
            Ok((if s.is_empty() {
                LispValue::Nil
            } else {
                LispValue::List(s.chars().map(|x| x.to_string().into()).collect())
            }, env, false))
        },
        LispValue::Nil => Ok((LispValue::Nil, env, false)),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_conj(mut args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let Some(first) = args.pop_front() else { unreachable!() };
    let first = eval(&first, &mut env)?;
    match first {
        LispValue::Vector(mut l) => {
            let mut args = args.iter().map(|x| {
                eval(x, &mut env)
            }).collect::<Result<Vec<LispValue>>>()?;
            l.append(&mut args);
            Ok((LispValue::Vector(l), env, false))
        },
        LispValue::List(l) => {
            let mut new_list = args.iter().rev().map(|x| {
                eval(x, &mut env)
            }).collect::<Result<Vector<LispValue>>>()?;
            new_list.append(l);
            Ok((LispValue::List(new_list), env, false))
        },
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_meta(_args: Vector<LispValue>, _env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    Err(LispError::NoMeta)
}

fn lisp_with_meta(_args: Vector<LispValue>, _env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    Err(LispError::NoMeta)
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

lazy_static! {
    pub static ref BUILTINS_NO_IO: HashMap<LispSymbol, LispValue> = {
        let mut strs = LispEnv::interner_mut();
        make_lisp_funcs!(strs,
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
        )
    };
}

#[cfg(feature = "io-stdlib")]
lazy_static! {
    pub static ref BUILTINS: HashMap<LispSymbol, LispValue> = {
        // dummy to make `time-ms` count from environment (so probably program)
        // initialization rather than since the first `time-ms` call
        let _ = *START_TIME;

        let base = BUILTINS_NO_IO.clone();
        let mut strs = LispEnv::interner_mut();
        let ext = make_lisp_funcs!(strs,
            "prn" => lisp_prn,
            "readline" => lisp_readline,
            "slurp" => lisp_slurp,
            "load-file" => lisp_load_file,
            "println" => lisp_println,
            "time-ms" => lisp_time_ms,
        );
        base.union(ext)
    };
}
