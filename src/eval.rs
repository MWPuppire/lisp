use std::iter::zip;
use crate::{LispValue, LispError, Result, env::LispEnv};

fn lookup_variable(val: String, env: &LispEnv) -> Result<LispValue> {
    env.get(&val).ok_or(LispError::UndefinedVariable(val))
}

fn is_macro(val: &LispValue) -> bool {
    match val {
        LispValue::Func { is_macro, .. } => *is_macro,
        _ => false,
    }
}

fn is_macro_call(val: &LispValue, env: &mut LispEnv) -> Result<bool> {
    match val {
        LispValue::List(list) => {
            if list.len() == 0 {
                Ok(false)
            } else {
                if let Ok(sym) = list[0].expect_symbol() {
                    Ok(is_macro(&lookup_variable(sym.to_owned(), env)?))
                } else {
                    Ok(false)
                }
            }
        },
        _ => Ok(false),
    }
}

pub fn expand_macros(val: &LispValue, env: &mut LispEnv) -> Result<LispValue> {
    let mut out = val.clone();
    while is_macro_call(&out, env)? {
        let list = out.expect_list()?;
        let head = &list[0];
        out = eval_list(head, &list[1..], env)?;
    }
    Ok(out)
}

fn eval_list(head: &LispValue, rest: &[LispValue], env: &mut LispEnv) -> Result<LispValue> {
    match head {
        LispValue::Symbol(s) => {
            let val = lookup_variable(s.clone(), env)?;
            eval_list(&val, rest, env)
        },
        LispValue::List(l) => {
            if l.len() == 0 {
                Err(LispError::InvalidDataType("function", "list"))
            } else {
                let inner_head = &l[0];
                eval_list(&eval_list(inner_head, &l[1..], env)?, rest, env)
            }
        },
        LispValue::BuiltinFunc { f, .. } => f(rest, env),
        LispValue::String(_) => Err(LispError::InvalidDataType("function", "string")),
        LispValue::Number(_) => Err(LispError::InvalidDataType("function", "number")),
        LispValue::Bool(_) => Err(LispError::InvalidDataType("function", "bool")),
        LispValue::Nil => Err(LispError::InvalidDataType("function", "nil")),
        LispValue::Atom(_) => Err(LispError::InvalidDataType("function", "atom")),
        LispValue::Func { args, body, env: fn_env, .. } => {
            if rest.len() != args.len() {
                Err(LispError::IncorrectArguments(args.len(), rest.len()))
            } else {
                let mut fn_env = fn_env.clone();
                for (key, val) in zip(args, rest) {
                    let val = eval(val, env);
                    fn_env.set(key.to_owned(), val?.clone());
                }
                eval(body, &mut fn_env)
            }
        },
    }
}

pub fn eval(value: &LispValue, env: &mut LispEnv) -> Result<LispValue> {
    let value = expand_macros(value, env)?;
    match value {
        LispValue::Symbol(s) => lookup_variable(s.clone(), env),
        LispValue::List(l) => {
            if l.len() == 0 {
                Ok(LispValue::List(vec![]))
            } else {
                let head = &l[0];
                eval_list(head, &l[1..], env)
            }
        },
        _ => Ok(value.clone()),
    }
}
