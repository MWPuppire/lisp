use crate::env::{LispEnv, LispSymbol};
use crate::eval::eval;
use crate::parser::LispParser;
use crate::util::{assert_or_err, LispBuiltinFunc, ObjectValue};
use crate::{LispError, LispValue, Result};
use by_address::ByAddress;
use im::{hashmap, vector, HashMap, Vector};
use itertools::Itertools;
use lazy_static::lazy_static;
use ordered_float::OrderedFloat;
use parking_lot::RwLock;
use std::iter;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        use std::fs;
        use std::io::prelude::*;
        use std::time::{SystemTime, UNIX_EPOCH};
    }
}

macro_rules! pop_head {
    ($args:expr) => {
        $args.pop_front().unwrap()
    };
}
macro_rules! eval_head {
    ($list:expr, $env:expr) => {
        eval(pop_head!($list), $env)
    };
}

#[inline]
fn eval_list_to_numbers<I: IntoIterator<Item = LispValue>>(
    args: I,
    env: &mut LispEnv,
) -> Result<Vec<OrderedFloat<f64>>> {
    args.into_iter().map(|x| eval(x, env)?.try_into()).collect()
}

// note: differs from Mal spec by allowing multiple parameters
fn lisp_plus(args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(args, env)?;
    Ok(LispValue::Number(
        nums.iter().fold(OrderedFloat(0.0), |acc, x| acc + x),
    ))
}

// note: differs from Mal spec by allowing multiple parameters
fn lisp_minus(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let first: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let nums = eval_list_to_numbers(args, env)?;
    Ok(LispValue::Number(
        first - nums.iter().fold(OrderedFloat(0.0), |acc, x| acc + x),
    ))
}

// note: differs from Mal spec by allowing multiple parameters
fn lisp_times(args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(args, env)?;
    Ok(LispValue::Number(
        nums.iter().fold(OrderedFloat(1.0), |acc, x| acc * x),
    ))
}

fn lisp_divide(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let numerator: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let denominator: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok(LispValue::Number(numerator / denominator))
}

// new function (not in Mal)
fn lisp_int_divide(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let numerator: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let denominator: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok(LispValue::Number(OrderedFloat(
        (numerator / denominator).trunc(),
    )))
}

fn lisp_equals(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x = eval_head!(args, env)?.vector_to_list();
    let y = eval_head!(args, env)?.vector_to_list();
    Ok(LispValue::Bool(x == y))
}

#[cfg(feature = "io-stdlib")]
fn lisp_prn(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
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
    Ok(LispValue::Nil)
}

fn lisp_list(args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    args.into_iter().map(|x| eval(x, env)).try_collect()
}

fn lisp_listq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    Ok(LispValue::Bool(match val {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::List(_)),
        _ => false,
    }))
}

fn lisp_emptyq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    if val.is_nil() {
        Ok(LispValue::Bool(true))
    } else {
        let list = val.try_into_iter()?;
        // `is_empty()` is unstable for iterators
        Ok(LispValue::Bool(list.len() == 0))
    }
}

fn lisp_count(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    if val.is_nil() {
        Ok(LispValue::Number(OrderedFloat(0.0)))
    } else {
        let list = val.try_into_iter()?;
        Ok(LispValue::Number(OrderedFloat(list.len() as f64)))
    }
}

fn lisp_lt(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let y: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok(LispValue::Bool(x < y))
}

fn lisp_lte(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let y: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok(LispValue::Bool(x <= y))
}

fn lisp_gt(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let y: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok(LispValue::Bool(x > y))
}

fn lisp_gte(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let y: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok(LispValue::Bool(x >= y))
}

fn lisp_not(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let x = eval_head!(args, env)?.truthiness();
    Ok(LispValue::Bool(!x))
}

fn lisp_atom(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let x = eval_head!(args, env)?;
    Ok(LispValue::Atom(ByAddress(Arc::new(RwLock::new(x)))))
}

fn lisp_atomq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    Ok(LispValue::Bool(matches!(val, LispValue::Atom(_))))
}

fn lisp_reset(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let atom: Arc<RwLock<LispValue>> = eval_head!(args, env)?.try_into()?;
    let val = eval_head!(args, env)?;
    *atom.write() = val.clone();
    Ok(val)
}

fn lisp_swap(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(args.len() > 1, LispError::IncorrectArguments(2, args.len()));
    let atom: Arc<RwLock<LispValue>> = eval_head!(args, env)?.try_into()?;
    let val = eval_head!(args, env)?;
    let derefed = atom.read().clone();
    let mut list = vector![val, derefed,];
    list.append(args);
    let out_val = eval(list.into(), env)?;
    *atom.write() = out_val.clone();
    Ok(out_val)
}

#[cfg(feature = "io-stdlib")]
fn lisp_readline(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    if !args.is_empty() {
        let s_owner = eval_head!(args, env)?;
        let s = s_owner.expect_string()?;
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
        Ok(LispValue::string_for(buffer))
    }
}

fn lisp_read_string(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let code_value = eval_head!(args, env)?;
    let code_expr = code_value.expect_string()?;
    let parsed = LispParser::parse(code_expr);
    if let Some(parsed) = parsed {
        Ok(parsed?)
    } else {
        Ok(LispValue::Nil)
    }
}

fn lisp_str(args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let mut buffer = String::new();
    for arg in args.into_iter() {
        let x = eval(arg, env)?;
        buffer.push_str(&x.to_string());
    }
    Ok(LispValue::string_for(buffer))
}

#[cfg(feature = "io-stdlib")]
fn lisp_slurp(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let file_value = eval_head!(args, env)?;
    let file_name = file_value.expect_string()?;
    let contents = fs::read_to_string(file_name)?;
    Ok(LispValue::string_for(contents))
}

#[cfg(feature = "io-stdlib")]
fn lisp_load_file(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let file_value = eval_head!(args, env)?;
    let file_name = file_value.expect_string()?;
    let contents = fs::read_to_string(file_name)?;

    let mut parser = LispParser::new();
    parser.add_tokenize(&contents)?;
    let global = env.global();
    // This is NOT an optimization. This is required to avoid a dead-lock;
    // attempting to lock the same environment calling a function will always
    // result in a dead-lock, so built-in functions which lock an environment
    // need to be aware of this and handle the case accordingly.
    if Arc::ptr_eq(&global, &env.clone_arc()) {
        for val in parser {
            eval(val?, env)?;
        }
    } else {
        let mut lock = global.write();
        for val in parser {
            eval(val?, lock.deref_mut())?;
        }
    }
    Ok(LispValue::Nil)
}

// new function (not in Mal)
fn lisp_typeof(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let x = eval_head!(args, env)?;
    Ok(LispValue::string_for(x.type_of().to_owned()))
}

fn lisp_cons(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let cons = eval_head!(args, env)?;
    let iter = eval_head!(args, env)?.try_into_iter()?;
    let iter = iter::once(cons).chain(iter);
    Ok(iter.collect())
}

fn lisp_concat(args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    args.into_iter()
        .map(|arg| eval(arg, env)?.try_into_iter())
        .flatten_ok()
        .try_collect()
}

fn lisp_nth(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let mut iter = eval_head!(args, env)?.try_into_iter()?;
    let floating: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let idx = floating.into_inner() as usize;
    iter.nth(idx).ok_or(LispError::IndexOutOfRange(idx))
}

// additionally aliased to `head`, which is not in Mal
fn lisp_first(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    if arg.is_nil() {
        return Ok(LispValue::Nil);
    }
    let mut list = arg.try_into_iter()?;
    if let Some(item) = list.next() {
        Ok(item)
    } else {
        Ok(LispValue::Nil)
    }
}

// additionally aliased to `tail`, which is not in Mal
fn lisp_rest(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    if arg.is_nil() {
        return Ok(vector![].into());
    }
    let mut iter = arg.try_into_iter()?;
    // drop the first item
    let _ = iter.next();
    Ok(iter.collect())
}

fn lisp_macroq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::Macro(_)),
        _ => false,
    }))
}

// new function (not in Mal)
fn lisp_inspect(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    Ok(LispValue::string_for(val.inspect()))
}

fn lisp_throw(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Err(LispError::UncaughtException(arg))
}

fn lisp_nilq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(arg.is_nil()))
}

fn lisp_trueq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(matches!(arg, LispValue::Bool(true))))
}

fn lisp_falseq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(matches!(arg, LispValue::Bool(false))))
}

fn lisp_symbolq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(matches!(arg, LispValue::Symbol(_))))
}

fn lisp_symbol(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let s_owner = eval_head!(args, env)?;
    let s = s_owner.expect_string()?;
    Ok(LispValue::symbol_for(s))
}

fn lisp_vector(args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let vals: Vec<LispValue> = args.into_iter().map(|x| eval(x, env)).try_collect()?;
    Ok(LispValue::vector_from(vals))
}

fn lisp_vectorq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::Vector(_)),
        _ => false,
    }))
}

fn lisp_keyword(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::Keyword(_) => Ok(LispValue::Object(o.clone())),
            ObjectValue::String(_) => {
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let ObjectValue::String(inner) = cloned else { unreachable!() };
                Ok(LispValue::Object(Arc::new(ObjectValue::Keyword(inner))))
            }
            _ => Err(LispError::InvalidDataType("string", o.type_of())),
        },
        LispValue::Symbol(s) => Ok(LispValue::keyword_for(LispEnv::symbol_string(s).unwrap())),
        x => Err(LispError::InvalidDataType("string", x.type_of())),
    }
}

fn lisp_keywordq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::Keyword(_)),
        _ => false,
    }))
}

fn lisp_hashmap(args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(args.len() & 1 == 0, LispError::MissingBinding);
    args.into_iter()
        .tuples()
        .map(|(key, val)| Ok((eval(key, env)?, eval(val, env)?)))
        .try_collect()
}

fn lisp_mapq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::Map(_)),
        _ => false,
    }))
}

fn lisp_sequentialq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(o.deref(), ObjectValue::List(_) | ObjectValue::Vector(_)),
        _ => false,
    }))
}

fn lisp_assoc(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(args.len() & 1 == 1, LispError::MissingBinding);
    let map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    args.into_iter()
        .tuples()
        .try_fold(map, |mut map, (key, val)| {
            let k = eval(key, env)?;
            let v = eval(val, env)?;
            map.insert(k, v);
            Ok(map)
        })
        .map(LispValue::from)
}

fn lisp_dissoc(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let base_map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let keys: Vec<(LispValue, LispValue)> = args
        .into_iter()
        .map(|x| Ok::<_, LispError>((eval(x, env)?, LispValue::Nil)))
        .try_collect()?;
    Ok(base_map.difference(keys.into()).into())
}

fn lisp_get(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let map = eval_head!(args, env)?;
    let key = eval_head!(args, env)?;
    if map.is_nil() {
        Ok(LispValue::Nil)
    } else {
        let map: HashMap<LispValue, LispValue> = map.try_into()?;
        if let Some(val) = map.get(&key) {
            Ok(val.clone())
        } else {
            Ok(LispValue::Nil)
        }
    }
}

fn lisp_containsq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let key = eval_head!(args, env)?;
    Ok(LispValue::Bool(map.contains_key(&key)))
}

fn lisp_keys(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let keys = map.keys().cloned();
    Ok(keys.collect())
}

fn lisp_vals(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let vals = map.values().cloned();
    Ok(vals.collect())
}

fn lisp_map(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let f = eval_head!(args, env)?;
    let iter = eval_head!(args, env)?.try_into_iter()?;
    iter.map(|x| {
        let list = vector![f.clone(), x].into();
        eval(list, env)
    })
    .try_collect()
}

fn lisp_pr_str(args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let full_str: Vec<String> = args
        .into_iter()
        .map(|x| Ok::<_, LispError>(eval(x, env)?.inspect()))
        .try_collect()?;
    Ok(LispValue::string_for(full_str.join(" ")))
}

#[cfg(feature = "io-stdlib")]
fn lisp_println(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    if let Some(last) = args.pop_back() {
        for val in args.into_iter() {
            let val = eval(val, env)?;
            print!("{} ", val);
        }
        println!("{}", eval(last, env)?);
    } else {
        println!();
    }
    Ok(LispValue::Nil)
}

fn lisp_fnq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(match arg {
        LispValue::Object(o) => matches!(
            o.deref(),
            ObjectValue::Func(_) | ObjectValue::BuiltinFunc { .. }
        ),
        _ => false,
    }))
}

fn lisp_stringq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(if arg.expect_string().is_ok() {
        LispValue::Bool(true)
    } else {
        LispValue::Bool(false)
    })
}

fn lisp_numberq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(LispValue::Bool(matches!(arg, LispValue::Number(_))))
}

fn lisp_vec(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::Vector(_) => Ok(LispValue::Object(o.clone())),
            ObjectValue::List(l) => Ok(LispValue::vector_from(l.iter().cloned())),
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        },
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        fn lisp_time_ms(_args: Vector<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
            Ok(LispValue::Number(OrderedFloat(
                match SystemTime::now().duration_since(UNIX_EPOCH) {
                    Ok(d) => d.as_secs_f64() * 1000.0,
                    Err(d) => d.duration().as_secs_f64() * 1000.0,
                }
            )))
        }
    }
}

fn lisp_seq(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(l) => Ok(if l.is_empty() {
                LispValue::Nil
            } else {
                LispValue::Object(o.clone())
            }),
            ObjectValue::Vector(l) => Ok(if l.is_empty() {
                LispValue::Nil
            } else {
                l.iter().cloned().collect()
            }),
            ObjectValue::String(s) => Ok(if s.is_empty() {
                LispValue::Nil
            } else {
                s.chars().map(|x| LispValue::from(x.to_string())).collect()
            }),
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        },
        LispValue::Nil => Ok(LispValue::Nil),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_conj(mut args: Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let first = eval_head!(args, env)?;
    let args = args.into_iter().map(|x| eval(x, env));
    match first {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::Vector(l) => {
                let mut args: Vec<LispValue> = args.try_collect()?;
                if l.is_empty() {
                    return Ok(LispValue::vector_from(args));
                }
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let ObjectValue::Vector(mut l) = cloned else { unreachable!() };
                l.append(&mut args);
                Ok(LispValue::vector_from(l))
            }
            ObjectValue::List(l) => {
                let mut args: Vector<LispValue> = args.rev().try_collect()?;
                if l.is_empty() {
                    return Ok(args.into());
                }
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let ObjectValue::List(l) = cloned else { unreachable!() };
                args.append(l);
                Ok(args.into())
            }
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        },
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_meta(_args: Vector<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
    Err(LispError::NoMeta)
}

fn lisp_with_meta(_args: Vector<LispValue>, _env: &mut LispEnv) -> Result<LispValue> {
    Err(LispError::NoMeta)
}

macro_rules! make_lisp_funcs {
    ($interner:expr, $($name:literal => $f:path,)*) => {
        hashmap! {
            $($interner.get_or_intern_static($name) => LispValue::Object(
                    Arc::new(ObjectValue::BuiltinFunc(LispBuiltinFunc {
                        name: $name,
                        body: $f,
                    }))
                )
            ),*
        }
    }
}

lazy_static! {
    pub(crate) static ref BUILTINS_NO_IO: HashMap<LispSymbol, LispValue> = {
        let mut strs = LispEnv::interner_mut();
        let mut funcs = make_lisp_funcs!(strs,
            "+" => lisp_plus,
            "-" => lisp_minus,
            "*" => lisp_times,
            "/" => lisp_divide,
            "//" => lisp_int_divide,
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
            "reset!" => lisp_reset,
            "swap!" => lisp_swap,
            "read-string" => lisp_read_string,
            "str" => lisp_str,
            "typeof" => lisp_typeof,
            "cons" => lisp_cons,
            "concat" => lisp_concat,
            "nth" => lisp_nth,
            "first" => lisp_first,
            "head" => lisp_first,
            "rest" => lisp_rest,
            "tail" => lisp_rest,
            "macro?" => lisp_macroq,
            "inspect" => lisp_inspect,
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

        funcs.insert(
            strs.get_or_intern_static("*host-language*"),
            LispValue::string_for("Rust".to_owned()),
        );

        funcs
    };
}

#[cfg(feature = "io-stdlib")]
lazy_static! {
    pub(crate) static ref BUILTINS: HashMap<LispSymbol, LispValue> = {
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
            .map(LispValue::string_for)
            .collect();
        lisp_argv.push_front(LispValue::Symbol(strs.get_or_intern_static("list")));
        ext.insert(
            strs.get_or_intern_static("*ARGV*"),
            lisp_argv.into(),
        );

        base.union(ext)
    };
}
