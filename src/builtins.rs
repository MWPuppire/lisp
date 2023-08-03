use crate::env::{hash, LispEnv, LispSymbol};
use crate::eval::eval;
use crate::parser::LispParser;
use crate::util::{assert_or_err, InnerObjectValue, InnerValue, LispBuiltinFunc, ObjectValue};
use crate::{LispError, LispValue, Result};
use by_address::ByAddress;
use im::{hashmap, vector, HashMap, Vector};
use itertools::Itertools;
use once_cell::sync::Lazy;
use ordered_float::OrderedFloat;
use parking_lot::RwLock;
use std::iter;
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
    env: &LispEnv,
) -> Result<Vec<OrderedFloat<f64>>> {
    args.into_iter().map(|x| eval(x, env)?.try_into()).collect()
}

#[inline]
fn eval_list_to_bools<I: IntoIterator<Item = LispValue>>(
    args: I,
    env: &LispEnv,
) -> Result<Vec<bool>> {
    args.into_iter()
        .map(|x| Ok(eval(x, env)?.truthiness()))
        .collect()
}

fn printable_value(val: LispValue) -> String {
    if let Ok(s) = val.expect_string() {
        s.to_owned()
    } else {
        val.to_string()
    }
}

// note: differs from Mal spec by allowing multiple parameters
fn lisp_plus(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(args, env)?;
    Ok(LispValue::new(InnerValue::Number(
        nums.iter().fold(OrderedFloat(0.0), |acc, x| acc + x),
    )))
}

// note: differs from Mal spec by allowing multiple parameters
fn lisp_minus(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let first: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    if args.is_empty() {
        Ok(LispValue::new(InnerValue::Number(-first)))
    } else {
        let nums = eval_list_to_numbers(args, env)?;
        Ok(LispValue::new(InnerValue::Number(
            first - nums.iter().fold(OrderedFloat(0.0), |acc, x| acc + x),
        )))
    }
}

// note: differs from Mal spec by allowing multiple parameters
fn lisp_times(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let nums = eval_list_to_numbers(args, env)?;
    Ok(LispValue::new(InnerValue::Number(
        nums.iter().fold(OrderedFloat(1.0), |acc, x| acc * x),
    )))
}

fn lisp_divide(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let numerator: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let denominator: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok(LispValue::new(InnerValue::Number(numerator / denominator)))
}

// new function (not in Mal)
fn lisp_int_divide(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let numerator: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let denominator: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok(LispValue::new(InnerValue::Number(OrderedFloat(
        (numerator / denominator).trunc(),
    ))))
}

fn lisp_equals(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x = eval_head!(args, env)?.vector_to_list();
    let y = eval_head!(args, env)?.vector_to_list();
    Ok((x == y).into())
}

#[cfg(feature = "io-stdlib")]
fn lisp_prn(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    if let Some(last) = args.pop_back() {
        for val in args.into_iter() {
            let val = eval(val, env)?;
            print!("{} ", val);
        }
        let last_val = eval(last, env)?;
        println!("{}", last_val);
    } else {
        println!();
    }
    Ok(LispValue::nil())
}

#[cfg(feature = "io-stdlib")]
fn lisp_println(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    if let Some(last) = args.pop_back() {
        for val in args.into_iter() {
            let val = eval(val, env)?;
            print!("{} ", printable_value(val));
        }
        let last_val = eval(last, env)?;
        println!("{}", printable_value(last_val));
    } else {
        println!();
    }
    Ok(LispValue::nil())
}

fn lisp_list(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    args.into_iter().map(|x| eval(x, env)).try_collect()
}

fn lisp_listq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    Ok(match val.val {
        InnerValue::Object(o) => matches!(o.val, InnerObjectValue::List(_)),
        _ => false,
    }
    .into())
}

fn lisp_emptyq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    if val.is_nil() {
        Ok(true.into())
    } else if let Ok(s) = val.expect_string() {
        Ok(s.is_empty().into())
    } else {
        let list = val.try_into_iter()?;
        // `is_empty()` is unstable for iterators
        Ok((list.len() == 0).into())
    }
}

fn lisp_count(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    if val.is_nil() {
        Ok(0.0.into())
    } else if let Ok(s) = val.expect_string() {
        Ok((s.len() as f64).into())
    } else {
        let list = val.try_into_iter()?;
        Ok((list.len() as f64).into())
    }
}

fn lisp_lt(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let y: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok((x < y).into())
}

fn lisp_lte(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let y: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok((x <= y).into())
}

fn lisp_gt(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let y: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok((x > y).into())
}

fn lisp_gte(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let x: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let y: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    Ok((x >= y).into())
}

fn lisp_not(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let x = eval_head!(args, env)?.truthiness();
    Ok((!x).into())
}

fn lisp_atom(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let x = eval_head!(args, env)?;
    Ok(LispValue::new(InnerValue::Atom(ByAddress(Arc::new(
        RwLock::new(x),
    )))))
}

fn lisp_atomq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    Ok(matches!(val.val, InnerValue::Atom(_)).into())
}

fn lisp_reset(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let atom: Arc<RwLock<LispValue>> = eval_head!(args, env)?.try_into()?;
    let val = eval_head!(args, env)?;
    *atom.write() = val.clone();
    Ok(val)
}

fn lisp_swap(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
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
fn lisp_readline(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    if !args.is_empty() {
        let s = eval_head!(args, env)?;
        print!("{}", printable_value(s));
        std::io::stdout().flush()?;
    }
    let mut buffer = String::new();
    let len = std::io::stdin().read_line(&mut buffer)?;
    if len == 0 {
        // eof
        Ok(LispValue::nil())
    } else {
        buffer.pop(); // remove newline
        Ok(LispValue::string_for(buffer))
    }
}

fn lisp_read_string(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
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
        Ok(LispValue::nil())
    }
}

fn lisp_str(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    let mut buffer = String::new();
    for arg in args.into_iter() {
        let x = eval(arg, env)?;
        buffer.push_str(&printable_value(x));
    }
    Ok(LispValue::string_for(buffer))
}

#[cfg(feature = "io-stdlib")]
fn lisp_slurp(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
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
fn lisp_load_file(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
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
    for val in parser {
        eval(val?, &global)?;
    }
    Ok(LispValue::nil())
}

// new function (not in Mal)
fn lisp_typeof(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let x = eval_head!(args, env)?;
    Ok(LispValue::string_for(x.type_of().to_owned()))
}

fn lisp_cons(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let cons = eval_head!(args, env)?;
    let iter = eval_head!(args, env)?.try_into_iter()?;
    let iter = iter::once(cons).chain(iter);
    Ok(iter.collect())
}

fn lisp_concat(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    args.into_iter()
        .map(|arg| eval(arg, env)?.try_into_iter())
        .flatten_ok()
        .try_collect()
}

fn lisp_nth(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let list = eval_head!(args, env)?;
    let floating: OrderedFloat<f64> = eval_head!(args, env)?.try_into()?;
    let idx = floating.into_inner() as usize;
    if let Ok(s) = list.expect_string() {
        s.chars()
            .nth(idx)
            .map(|ch| LispValue::string_for(ch.into()))
            .ok_or(LispError::IndexOutOfRange(idx))
    } else {
        let mut iter = list.try_into_iter()?;
        iter.nth(idx).ok_or(LispError::IndexOutOfRange(idx))
    }
}

// additionally aliased to `head`, which is not in Mal
fn lisp_first(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    if arg.is_nil() {
        return Ok(LispValue::nil());
    }
    if let Ok(s) = arg.expect_string() {
        if let Some(ch) = s.chars().next() {
            Ok(LispValue::string_for(ch.into()))
        } else {
            Ok(LispValue::nil())
        }
    } else {
        let mut list = arg.try_into_iter()?;
        if let Some(item) = list.next() {
            Ok(item)
        } else {
            Ok(LispValue::nil())
        }
    }
}

// additionally aliased to `tail`, which is not in Mal
fn lisp_rest(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    if arg.is_nil() {
        return Ok(vector![].into());
    }
    if let Ok(s) = arg.expect_string() {
        let mut chars = s.chars();
        let _ = chars.next();
        Ok(LispValue::string_for(chars.as_str().to_owned()))
    } else {
        let mut iter = arg.try_into_iter()?;
        // drop the first item
        let _ = iter.next();
        Ok(iter.collect())
    }
}

fn lisp_macroq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(match arg.val {
        InnerValue::Object(o) => matches!(o.val, InnerObjectValue::Macro(_)),
        _ => false,
    }
    .into())
}

// new function (not in Mal)
fn lisp_inspect(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let val = eval_head!(args, env)?;
    Ok(LispValue::string_for(val.inspect()))
}

fn lisp_throw(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Err(LispError::UncaughtException(arg))
}

fn lisp_nilq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(arg.is_nil().into())
}

fn lisp_trueq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(matches!(arg.val, InnerValue::Bool(true)).into())
}

fn lisp_falseq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(matches!(arg.val, InnerValue::Bool(false)).into())
}

fn lisp_symbolq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(matches!(arg.val, InnerValue::Symbol { .. }).into())
}

fn lisp_symbol(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let s_owner = eval_head!(args, env)?;
    let s = s_owner.expect_string()?;
    Ok(LispValue::symbol_for(s))
}

fn lisp_vector(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    let vals: Vec<LispValue> = args.into_iter().map(|x| eval(x, env)).try_collect()?;
    Ok(LispValue::vector_from(vals))
}

fn lisp_vectorq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(match arg.val {
        InnerValue::Object(o) => matches!(o.val, InnerObjectValue::Vector(_)),
        _ => false,
    }
    .into())
}

fn lisp_keyword(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    match arg.val {
        InnerValue::Object(o) => match &o.val {
            InnerObjectValue::Keyword(_) => Ok(LispValue::new(InnerValue::Object(o.clone()))),
            InnerObjectValue::String(_) => {
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let InnerObjectValue::String(inner) = cloned.val else { unreachable!() };
                Ok(LispValue::new(InnerValue::Object(Arc::new(ObjectValue {
                    val: InnerObjectValue::Keyword(inner),
                    meta: None,
                    quoted: false,
                }))))
            }
            _ => Err(LispError::InvalidDataType("string", o.type_of())),
        },
        x => Err(LispError::InvalidDataType("string", x.type_of())),
    }
}

fn lisp_keywordq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(match arg.val {
        InnerValue::Object(o) => matches!(o.val, InnerObjectValue::Keyword(_)),
        _ => false,
    }
    .into())
}

fn lisp_hashmap(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(args.len() & 1 == 0, LispError::MissingBinding);
    args.into_iter()
        .tuples()
        .map(|(key, val)| Ok((eval(key, env)?, eval(val, env)?)))
        .try_collect()
}

fn lisp_mapq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(match arg.val {
        InnerValue::Object(o) => matches!(o.val, InnerObjectValue::Map(_)),
        _ => false,
    }
    .into())
}

fn lisp_sequentialq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(match arg.val {
        InnerValue::Object(o) => matches!(
            o.val,
            InnerObjectValue::List(_) | InnerObjectValue::Vector(_)
        ),
        _ => false,
    }
    .into())
}

fn lisp_assoc(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
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

fn lisp_dissoc(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let base_map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let keys: Vec<(LispValue, LispValue)> = args
        .into_iter()
        .map(|x| Ok::<_, LispError>((eval(x, env)?, LispValue::nil())))
        .try_collect()?;
    Ok(base_map.difference(keys.into()).into())
}

fn lisp_get(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let map = eval_head!(args, env)?;
    let key = eval_head!(args, env)?;
    if map.is_nil() {
        Ok(LispValue::nil())
    } else {
        let map: HashMap<LispValue, LispValue> = map.try_into()?;
        if let Some(val) = map.get(&key) {
            Ok(val.clone())
        } else {
            Ok(LispValue::nil())
        }
    }
}

fn lisp_containsq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let key = eval_head!(args, env)?;
    Ok(map.contains_key(&key).into())
}

fn lisp_keys(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let keys = map.keys().cloned();
    Ok(keys.collect())
}

fn lisp_vals(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let vals = map.values().cloned();
    Ok(vals.collect())
}

fn lisp_map(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
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

fn lisp_pr_str(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    let full_str: Vec<String> = args
        .into_iter()
        .map(|x| Ok::<_, LispError>(eval(x, env)?.to_string()))
        .try_collect()?;
    Ok(LispValue::string_for(full_str.join(" ")))
}

fn lisp_fnq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(match arg.val {
        InnerValue::Object(o) => {
            matches!(
                o.val,
                InnerObjectValue::Func(_) | InnerObjectValue::BuiltinFunc(_)
            )
        }
        InnerValue::Special { quoted, .. } => !quoted,
        _ => false,
    }
    .into())
}

fn lisp_stringq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(if arg.expect_string().is_ok() {
        true.into()
    } else {
        false.into()
    })
}

fn lisp_numberq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    Ok(matches!(arg.val, InnerValue::Number(_)).into())
}

fn lisp_vec(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    match arg.val {
        InnerValue::Object(o) => match &o.val {
            InnerObjectValue::Vector(_) => Ok(LispValue::new(InnerValue::Object(o.clone()))),
            InnerObjectValue::List(l) => Ok(LispValue::vector_from(l.iter().cloned())),
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        },
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        fn lisp_time_ms(_args: Vector<LispValue>, _env: &LispEnv) -> Result<LispValue> {
            Ok(OrderedFloat(
                match SystemTime::now().duration_since(UNIX_EPOCH) {
                    Ok(d) => d.as_secs_f64() * 1000.0,
                    Err(d) => d.duration().as_secs_f64() * 1000.0,
                }
            ).into())
        }
    }
}

fn lisp_seq(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    match arg.val {
        InnerValue::Object(o) => match &o.val {
            InnerObjectValue::List(l) => Ok(if l.is_empty() {
                LispValue::nil()
            } else {
                LispValue::new(InnerValue::Object(o.clone()))
            }),
            InnerObjectValue::Vector(l) => Ok(if l.is_empty() {
                LispValue::nil()
            } else {
                l.iter().cloned().collect()
            }),
            InnerObjectValue::String(s) => Ok(if s.is_empty() {
                LispValue::nil()
            } else {
                s.chars().map(|x| LispValue::from(x.to_string())).collect()
            }),
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        },
        InnerValue::Nil => Ok(LispValue::nil()),
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_conj(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let first = eval_head!(args, env)?;
    let args = args.into_iter().map(|x| eval(x, env));
    match first.val {
        InnerValue::Object(o) => match &o.val {
            InnerObjectValue::Vector(l) => {
                let mut args: Vec<LispValue> = args.try_collect()?;
                if l.is_empty() {
                    return Ok(LispValue::vector_from(args));
                }
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let InnerObjectValue::Vector(mut l) = cloned.val else { unreachable!() };
                l.append(&mut args);
                Ok(LispValue::vector_from(l))
            }
            InnerObjectValue::List(l) => {
                let mut args: Vector<LispValue> = args.rev().try_collect()?;
                if l.is_empty() {
                    return Ok(args.into());
                }
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let InnerObjectValue::List(l) = cloned.val else { unreachable!() };
                args.append(l);
                Ok(args.into())
            }
            x => Err(LispError::InvalidDataType("list", x.type_of())),
        },
        x => Err(LispError::InvalidDataType("list", x.type_of())),
    }
}

fn lisp_meta(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let obj = eval_head!(args, env)?;
    match obj.val {
        InnerValue::Object(o) => Ok(o.meta.clone().unwrap_or(LispValue::nil())),
        x => Err(LispError::InvalidDataType("object", x.type_of())),
    }
}

fn lisp_with_meta(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let obj = eval_head!(args, env)?;
    assert_or_err!(
        matches!(obj.val, InnerValue::Object(_)),
        LispError::InvalidDataType("object", obj.type_of())
    );
    let meta = eval_head!(args, env)?;
    let InnerValue::Object(mut o) = obj.val else { unreachable!() };
    let inner = Arc::make_mut(&mut o);
    inner.meta = Some(meta);
    Ok(LispValue::new(InnerValue::Object(o)))
}

// new function (not in Mal)
fn lisp_dump_env(_args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    Ok(LispValue::string_for(env.dump()))
}

// new function (not in Mal)
fn lisp_pairs(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let map: HashMap<LispValue, LispValue> = eval_head!(args, env)?.try_into()?;
    let pairs = map
        .iter()
        .map(|x| LispValue::from(vector![x.0.clone(), x.1.clone()]));
    Ok(pairs.collect())
}

// new function (not in Mal)
fn lisp_join(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let iter = eval_head!(args, env)?.try_into_iter()?;
    let join_with = printable_value(eval_head!(args, env)?);
    Ok(LispValue::string_for(iter.map(String::try_from).fold_ok(
        "".to_owned(),
        |acc, s| if acc.is_empty() { acc } else { acc + &join_with } + &s,
    )?))
}

// new function (not in Mal)
fn lisp_and(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let bools = eval_list_to_bools(args, env)?;
    Ok(LispValue::new(InnerValue::Bool(
        bools.iter().fold(true, |acc, x| acc & x),
    )))
}

// new function (not in Mal)
fn lisp_or(args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(!args.is_empty(), LispError::IncorrectArguments(1, 0));
    let bools = eval_list_to_bools(args, env)?;
    Ok(LispValue::new(InnerValue::Bool(
        bools.iter().fold(false, |acc, x| acc | x),
    )))
}

// new function (not in Mal)
fn lisp_bool(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let x = eval_head!(args, env)?.truthiness();
    Ok(x.into())
}

// new function (not in Mal)
fn lisp_reduce(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 2,
        LispError::IncorrectArguments(2, args.len())
    );
    let f = eval_head!(args, env)?;
    let mut iter = eval_head!(args, env)?.try_into_iter()?;
    if let Some(head) = iter.next() {
        iter.fold_while(Ok(head), |acc, x| {
            let list = vector![
                f.clone(),
                acc.expect("An error should have ended the function")
                    .quote(),
                x
            ]
            .into();
            match eval(list, env) {
                Ok(x) => itertools::FoldWhile::Continue(Ok(x)),
                Err(err) => itertools::FoldWhile::Done(Err(err)),
            }
        })
        .into_inner()
    } else {
        // empty list returns `nil`
        Ok(LispValue::new(InnerValue::Nil))
    }
}

// new function (not in Mal)
fn lisp_last(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    if arg.is_nil() {
        return Ok(LispValue::nil());
    }
    if let Ok(s) = arg.expect_string() {
        if let Some(ch) = s.chars().last() {
            Ok(LispValue::string_for(ch.into()))
        } else {
            Ok(LispValue::nil())
        }
    } else {
        let list = arg.try_into_iter()?;
        if let Some(item) = list.last() {
            Ok(item)
        } else {
            Ok(LispValue::nil())
        }
    }
}

// new function (not in Mal)
fn lisp_foldr(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 3,
        LispError::IncorrectArguments(3, args.len())
    );
    let f = eval_head!(args, env)?;
    let init = eval_head!(args, env)?;
    let mut iter = eval_head!(args, env)?.try_into_iter()?;
    iter.fold_while(Ok(init), |acc, x| {
        let list = vector![
            f.clone(),
            acc.expect("An error should have ended the function")
                .quote(),
            x
        ]
        .into();
        match eval(list, env) {
            Ok(x) => itertools::FoldWhile::Continue(Ok(x)),
            Err(err) => itertools::FoldWhile::Done(Err(err)),
        }
    })
    .into_inner()
}

// new function (not in Mal)
fn lisp_rev(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    if arg.is_nil() {
        return Ok(LispValue::nil());
    }
    let list = arg.try_into_iter()?;
    Ok(list.rev().collect())
}

// new function (not in Mal)
fn lisp_flatten(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    if arg.is_nil() {
        return Ok(LispValue::nil());
    }
    let list = arg.try_into_iter()?;
    Ok(list
        .flat_map(|arg| {
            arg.clone()
                .try_into_iter()
                .unwrap_or_else(|_| vec![arg].into_iter())
        })
        .collect())
}

// new function (not in Mal)
fn lisp_sign(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    let num = OrderedFloat::try_from(arg)?;
    Ok(if num >= OrderedFloat(0.0) {
        1.0.into()
    } else {
        (-1.0).into()
    })
}

// new function (not in Mal)
fn lisp_trunc(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    let num = OrderedFloat::try_from(arg)?;
    Ok(num.trunc().into())
}

// new function (not in Mal)
fn lisp_round(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    let num = OrderedFloat::try_from(arg)?;
    Ok(num.round().into())
}

// new function (not in Mal)
fn lisp_floor(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    let num = OrderedFloat::try_from(arg)?;
    Ok(num.floor().into())
}

// new function (not in Mal)
fn lisp_ceil(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    assert_or_err!(
        args.len() == 1,
        LispError::IncorrectArguments(1, args.len())
    );
    let arg = eval_head!(args, env)?;
    let num = OrderedFloat::try_from(arg)?;
    Ok(num.ceil().into())
}

// Note that these don't currently pass the entire test suite.
#[cfg(feature = "self-implemented")]
pub(crate) static SELF_IMPLEMENTED: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    hashmap! {
        "//" => r#"(fn* (num den) (
                trunc (/ num den))
            )"#,
        "list" => r#"(fn* (&args) args)"#,
        "list?" => r#"(fn* (x) (
                = (typeof x) "list"
            ))"#,
        "empty?" => r#"(fn* (ls) (
                ; can't use `or`, since `or` uses `reduce` which uses `empty?`
                if (= ls ())
                    true
                    (if (= ls "")
                        true
                        (if (= ls [])
                            true
                            (nil? ls)
                        )
                    )
            ))"#,
        "count" => r#"(fn* (ls) (
                if (empty? ls)
                    0
                    (+ 1 (count (rest ls)))
            ))"#,
        "<=" => r#"(fn* (x y) (
                not (< y x)
            ))"#,
        ">" => r#"(fn* (x y) (
                < y x
            ))"#,
        ">=" => r#"(fn* (x y) (
                not (< x y)
            ))"#,
        "not" => r#"(fn* (bool) (
                if bool false true
            ))"#,
        "atom?" => r#"(fn* (x) (
                = (typeof x) "atom"
            ))"#,
        "swap!" => r#"(fn* (atom fn &args) (
                reset! atom (apply fn @atom args)
            ))"#,
        "cons" => r#"(fn* (item ls) (
                apply list item ls
            ))"#,
        "concat" => r#"(fn* (&lists) (
                foldr (fn* (acc list) (
                    foldr (fn* (acc item) (cons item acc)) acc (rev list)
                )) () (rev lists)
            ))"#,
        "nth" => r#"(fn* (ls n) (
                if (empty? ls)
                    (throw (str "index " n " out of range"))
                    (if (= n 0)
                        (first ls)
                        (nth (rest ls) (- n 1))
                    )
            ))"#,
        "head" => "first",
        "tail" => "rest",
        "macro?" => r#"(fn* (x) (
                = (typeof x) "macro"
            ))"#,
        "nil?" => r#"(fn* (x) (
                = x nil
            ))"#,
        "true?" => r#"(fn* (x) (
                = x true
            ))"#,
        "false?" => r#"(fn* (x) (
                = x false
            ))"#,
        "symbol?" => r#"(fn* (x) (
                = (typeof x) "symbol"
            ))"#,
        "vector?" => r#"(fn* (x) (
                = (typeof x) "vector"
            ))"#,
        "keyword?" => r#"(fn* (x) (
                = (typeof x) "keyword"
            ))"#,
        "hashmap" => r#"(fn* (&pairs) (
                apply assoc {} (flatten pairs)
            ))"#,
        "map?" => r#"(fn* (x) (
                = (typeof x) "map"
            ))"#,
        "sequential?" => r#"(fn* (x) (
                or (list? x) (vector? x)
            ))"#,
        "keys" => r#"(fn* (mp) (
                map (fn* (pair) (first pair)) (pairs mp)
            ))"#,
        "vals" => r#"(fn* (mp) (
                map (fn* (pair) (
                    first (rest pair)
                )) (pairs mp)
            ))"#,
        "map" => r#"(fn* (transformer ls) (
                if (empty? ls)
                    ()
                    (cons (transformer (first ls)) (map transformer (rest ls)))
            ))"#,
        "pr-str" => r#"(fn* (&strs) (
                if (empty? strs)
                    ""
                    (str (reduce (fn* (acc x) (str acc " " x)) strs))
            ))"#,
        "fn?" => r#"(fn* (x) (
                = (typeof x) "function"
            ))"#,
        "string?" => r#"(fn* (x) (
                = (typeof x) "string"
            ))"#,
        "number?" => r#"(fn* (x) (
                = (typeof x) "number"
            ))"#,
        "vec" => r#"(fn* (ls) (
                apply vector ls
            ))"#,
        "seq" => r#"(fn* (ls) (
                if (empty? ls)
                    nil
                    (cons (first ls) (let* (tmp (seq (rest ls)))
                        (if (nil? tmp) () tmp)
                    ))
            ))"#,
        "conj" => r#"(fn* (ls &args) (
                if (list? ls)
                    (concat args ls)
                    (vec (concat ls args))
            ))"#,
        "join" => r#"(fn* (strs sep) (
                if (empty? strs)
                    ""
                    (str (reduce (fn* (acc x) (
                        str acc sep x
                    )) strs))
            ))"#,
        "and" => r#"(fn* (&args) (
                bool (foldr (fn* (acc x) (
                    if acc (bool x) false
                )) true args)
            ))"#,
        "or" => r#"(fn* (&args) (
                bool (foldr (fn* (acc x) (
                    if acc true (bool x)
                )) false args)
            ))"#,
        "bool" => r#"(fn* (bool) (
                if bool true false
            ))"#,
        "reduce" => r#"(fn* (reducer ls) (
                let* (acc (first ls) ls (rest ls)) (
                    if (empty? ls)
                        acc
                        (reduce reducer (
                            cons (reducer acc (first ls)) (rest ls)
                        ))
                )
            ))"#,
        "last" => r#"(fn* (ls) (
                reduce (fn* (_ item) item) ls
            ))"#,
        "foldr" => r#"(fn* (reducer acc ls) (
                if (empty? ls)
                    acc
                    (foldr reducer (reducer acc (first ls)) (rest ls))
            ))"#,
        "rev" => r#"(fn* (ls) (
                foldr (fn* (acc next) (
                    cons next acc
                )) () ls
            ))"#,
        "flatten" => r#"(fn* (ls) (
                foldr (fn* (acc ls) (
                    concat acc (
                        if (sequential? ls)
                            ls
                            (list ls)
                    )
                )) () ls
            ))"#,
        "sign" => r#"(fn* (num) (
                if (< num 0) -1 1
            ))"#,
        "round" => r#"(fn* (num) (
                trunc (+
                    num
                    (* 0.5 (sign num))
                )
            ))"#,
        "floor" => r#"(fn* (num) (
                if (= num (trunc num))
                    num
                    (round (- num 0.5))
            ))"#,
        "ceil" => r#"(fn* (num) (
                if (= num (trunc num))
                    num
                    (round (+ num 0.5))
            ))"#,
        // shadowed by special form
        "do" => r#"(fn* (&args) (
                last args
            ))"#,
    }
});

macro_rules! make_lisp_funcs {
    ($($name:literal => $f:path,)*) => {
        hashmap! {
            $(hash($name) => LispValue::new(InnerValue::Object(
                    Arc::new(ObjectValue {
                        val: InnerObjectValue::BuiltinFunc(LispBuiltinFunc {
                            name: $name,
                            body: $f,
                        }),
                        meta: None,
                        quoted: false,
                    })
                )
            )),*
        }
    }
}

#[cfg(feature = "self-implemented")]
pub(crate) static BUILTINS_CORE: Lazy<HashMap<LispSymbol, LispValue>> = Lazy::new(|| {
    // fairly minimal set of functions needed to produce the others
    let mut funcs = make_lisp_funcs!(
        "+" => lisp_plus,
        "-" => lisp_minus,
        "*" => lisp_times,
        "/" => lisp_divide,
        "=" => lisp_equals,
        "<" => lisp_lt,
        "atom" => lisp_atom,
        "reset!" => lisp_reset,
        "read-string" => lisp_read_string,
        "str" => lisp_str,
        "typeof" => lisp_typeof,
        "first" => lisp_first,
        "rest" => lisp_rest,
        "inspect" => lisp_inspect,
        "throw" => lisp_throw,
        "symbol" => lisp_symbol,
        "vector" => lisp_vector,
        "keyword" => lisp_keyword,
        "assoc" => lisp_assoc,
        "dissoc" => lisp_dissoc,
        "get" => lisp_get,
        "contains?" => lisp_containsq,
        "meta" => lisp_meta,
        "with-meta" => lisp_with_meta,
        "dump-env" => lisp_dump_env,
        "pairs" => lisp_pairs,
        "trunc" => lisp_trunc,
    );

    funcs.insert(
        hash("*host-language*"),
        LispValue::string_for("Rust".to_owned()),
    );

    funcs
});

pub(crate) static BUILTINS_NO_IO: Lazy<HashMap<LispSymbol, LispValue>> = Lazy::new(|| {
    let mut funcs = make_lisp_funcs!(
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
        "dump-env" => lisp_dump_env,
        "pairs" => lisp_pairs,
        "join" => lisp_join,
        "and" => lisp_and,
        "or" => lisp_or,
        "bool" => lisp_bool,
        "reduce" => lisp_reduce,
        "last" => lisp_last,
        "foldr" => lisp_foldr,
        "rev" => lisp_rev,
        "flatten" => lisp_flatten,
        "sign" => lisp_sign,
        "trunc" => lisp_trunc,
        "round" => lisp_round,
        "floor" => lisp_floor,
        "ceil" => lisp_ceil,
    );

    funcs.insert(
        hash("*host-language*"),
        LispValue::string_for("Rust".to_owned()),
    );

    funcs
});

#[cfg(feature = "io-stdlib")]
pub(crate) static BUILTINS: Lazy<HashMap<LispSymbol, LispValue>> = Lazy::new(|| {
    let base = BUILTINS_NO_IO.clone();
    let mut ext = make_lisp_funcs!(
        "prn" => lisp_prn,
        "readline" => lisp_readline,
        "slurp" => lisp_slurp,
        "load-file" => lisp_load_file,
        "println" => lisp_println,
        "time-ms" => lisp_time_ms,
    );

    let args: Vec<String> = std::env::args().collect();
    let lisp_argv: LispValue = args
        .into_iter()
        // Lisp *ARGV* doesn't include the interpreter name
        .skip(1)
        .map(LispValue::string_for)
        .collect();
    ext.insert(hash("*ARGV*"), lisp_argv.quote());

    base.union(ext)
});
