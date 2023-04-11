use std::iter::zip;
use std::collections::VecDeque;
use crate::{LispValue, LispError, Result, env::LispEnv};

fn lookup_variable(val: String, env: &LispEnv) -> Result<LispValue> {
    env.get(&val).ok_or(LispError::UndefinedVariable(val))
}

fn is_macro(val: &LispValue) -> bool {
    match val {
        LispValue::Func(f) => f.is_macro,
        _ => false,
    }
}

fn is_macro_call(val: &LispValue, env: &LispEnv) -> Result<bool> {
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
        let mut list = out.into_list()?;
        // unreachable, since `is_macro_call` is true
        let Some(head) = list.pop_front() else { unreachable!() };
        out = eval_list(head, list, env)?;
    }
    Ok(out)
}

fn eval_list(head: LispValue, rest: VecDeque<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    match head {
        LispValue::Symbol(s) => {
            let val = lookup_variable(s, env)?;
            eval_list(val, rest, env)
        },
        LispValue::VariadicSymbol(s) => {
            let val = lookup_variable(s, env)?;
            eval_list(val, rest, env)
        },
        LispValue::List(mut l) => {
            if let Some(head) = l.pop_front() {
                eval_list(eval_list(head, l, env)?, rest, env)
            } else {
                Err(LispError::InvalidDataType("function", "list"))
            }
        },
        LispValue::BuiltinFunc { f, .. } => f.0(rest, env),
        LispValue::Func(f) => {
            if f.variadic && rest.len() >= (f.args.len() - 1) {
                let evaluator = if f.is_macro { expand_macros } else { eval };
                let mut vals = rest.iter().map(|x| evaluator(x, env)).collect::<Result<Vec<LispValue>>>()?;
                let last_vals = vals.split_off(f.args.len() - 1);
                let variadic_idx = f.args.len() - 1;
                let arg_names = f.args[0..variadic_idx].iter().map(|x| x.to_owned());
                let mut params: Vec<(String, LispValue)> = zip(arg_names, vals).collect();
                params.push((f.args[variadic_idx].to_owned(), LispValue::List(last_vals.into())));
                if let Some(name) = &f.name {
                    params.push((name.clone(), LispValue::Func(f.clone())));
                }
                let mut fn_env = f.closure.make_env(&params);
                eval(&f.body, &mut fn_env)
            } else if rest.len() != f.args.len() {
                Err(LispError::IncorrectArguments(f.args.len(), rest.len()))
            } else {
                let evaluator = if f.is_macro { expand_macros } else { eval };
                let vals = rest.iter().map(|x| evaluator(x, env)).collect::<Result<Vec<LispValue>>>()?;
                let arg_names = f.args.iter().map(|x| x.to_owned());
                let mut params: Vec<(String, LispValue)> = zip(arg_names, vals).collect();
                if let Some(name) = &f.name {
                    params.push((name.clone(), LispValue::Func(f.clone())));
                }
                let mut fn_env = f.closure.make_env(&params);
                eval(&f.body, &mut fn_env)
            }
        },
        _ => Err(LispError::InvalidDataType("function", head.type_of())),
    }
}

pub fn eval(value: &LispValue, env: &mut LispEnv) -> Result<LispValue> {
    let value = expand_macros(value, env)?;
    match value {
        LispValue::Symbol(s) => lookup_variable(s, env),
        LispValue::VariadicSymbol(s) => lookup_variable(s, env),
        LispValue::List(mut l) => {
            if let Some(head) = l.pop_front() {
                eval_list(head, l, env)
            } else {
                Ok(LispValue::List(l))
            }
        },
        LispValue::Vector(l) => {
            Ok(LispValue::Vector(l.iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?))
        },
        LispValue::Map(m) => {
            Ok(LispValue::Map(m.iter().map(|(key, val)| Ok((key.clone(), eval(val, env)?))).collect::<Result<Vec<(LispValue, LispValue)>>>()?.into()))
        },
        x => Ok(x),
    }
}
