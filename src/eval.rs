use std::iter::zip;
use std::sync::Arc;
use std::ops::Deref;
use im::{Vector, vector, HashMap};
use parking_lot::RwLock;
use crate::{LispValue, LispError, Result};
use crate::env::{LispEnv, LispSymbol};
use crate::util::{LispFunc, ObjectValue, LispSpecialForm, expect};
use crate::specials::special_form;

#[inline]
fn lookup_variable(val: LispSymbol, env: &LispEnv) -> Result<LispValue> {
    env.get(val).ok_or(LispError::UndefinedVariable(LispEnv::symbol_string(val).unwrap()))
}

fn is_macro_call(val: &LispValue, env: &LispEnv) -> bool {
    match val {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(l) => {
                if l.is_empty() {
                    false
                } else {
                    match &l[0] {
                        LispValue::Symbol(s) | LispValue::VariadicSymbol(s) => {
                            if let Ok(var) = lookup_variable(*s, env) {
                                match var {
                                    LispValue::Object(o) => matches!(
                                        o.deref(),
                                        ObjectValue::Macro(_),
                                    ),
                                    _ => false,
                                }
                            } else {
                                false
                            }
                        },
                        _ => false,
                    }
                }
            },
            _ => false,
        },
        _ => false,
    }
}

pub fn expand_macros(val: LispValue, env: &LispEnv) -> Result<(LispValue, LispEnv)> {
    if is_macro_call(&val, env) {
        let mut list = val.try_into_iter()?;
        let Some(head) = list.next() else { unreachable!() };
        let LispValue::Symbol(name) = head else { unreachable!() };
        let var = lookup_variable(name, env)?;
        let Ok(f) = var.expect_func() else { unreachable!() };
        // some code duplication from `expand_fn`, but I didn't want `is_macro`
        // as a parameter for `expand_fn` to be in public API
        if f.variadic && list.len() >= (f.args.len() - 1) {
            let mut args = list.into_iter().map(|x| {
                Ok(expand_macros(x, env)?.0)
            }).collect::<Result<Vector<LispValue>>>()?;
            let variadic_idx = f.args.len() - 1;
            let last_args = args.split_off(variadic_idx);
            let arg_names = f.args[0..variadic_idx].iter().copied();
            let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, args).collect();
            params.push((f.args[variadic_idx], LispValue::list_from(last_args)));
            let fn_env = f.closure.make_macro_env(&params, env);
            Ok((f.body.clone(), fn_env))
        } else if list.len() != f.args.len() {
            Err(LispError::IncorrectArguments(f.args.len(), list.len()))
        } else {
            let args = list.into_iter().map(|x| {
                Ok(expand_macros(x, env)?.0)
            }).collect::<Result<Vec<LispValue>>>()?;
            let params: Vec<(LispSymbol, LispValue)> = zip(f.args.iter().copied(), args).collect();
            let fn_env = f.closure.make_macro_env(&params, env);
            Ok((f.body.clone(), fn_env))
        }
    } else {
        // TODO cloning environments is cheap since it's just bumping an Arc,
        // but I should maybe still find a better solution to this
        Ok((val, env.clone()))
    }
}

// doesn't actually execute the function with its arguments (to allow TCO), just
// replaces the function with its body after creating the closure with all
// arguments properly applied
pub fn expand_fn(f: &LispFunc, args: Vector<LispValue>, env: &LispEnv) -> Result<(LispValue, LispEnv)> {
    if f.variadic && args.len() >= (f.args.len() - 1) {
        let mut args = args.into_iter().map(|x| eval(x, env)).collect::<Result<Vector<LispValue>>>()?;
        let variadic_idx = f.args.len() - 1;
        let last_args = args.split_off(variadic_idx);
        let arg_names = f.args[0..variadic_idx].iter().copied();
        let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, args).collect();
        params.push((f.args[variadic_idx], LispValue::list_from(last_args)));
        let fn_env = f.closure.make_env(&params);
        Ok((f.body.clone(), fn_env))
    } else if args.len() != f.args.len() {
        Err(LispError::IncorrectArguments(f.args.len(), args.len()))
    } else {
        let args = args.into_iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?;
        let params: Vec<(LispSymbol, LispValue)> = zip(f.args.iter().copied(), args).collect();
        let fn_env = f.closure.make_env(&params);
        Ok((f.body.clone(), fn_env))
    }
}

fn eval_ast(ast: LispValue, env: &LispEnv) -> Result<LispValue> {
    Ok(match ast {
        LispValue::Symbol(s) => lookup_variable(s, env)?,
        LispValue::VariadicSymbol(s) => lookup_variable(s, env)?,
        LispValue::Object(o) => LispValue::Object(match o.deref() {
            ObjectValue::Vector(l) => Arc::new(ObjectValue::Vector(
                l.iter().cloned()
                    .map(|x| eval(x, env))
                    .collect::<Result<Vec<LispValue>>>()?
            )),
            ObjectValue::Map(m) => Arc::new(ObjectValue::Map(
                m.iter()
                    .map(|(key, val)| Ok((key.clone(), eval(val.clone(), env)?)))
                    .collect::<Result<HashMap<LispValue, LispValue>>>()?
            )),
            _ => o.clone(),
        }),
        x => x
    })
}

pub fn eval(mut ast: LispValue, env: &LispEnv) -> Result<LispValue> {
    // similar to in `expand_macros`, I should maybe find something better than
    // this (really, the main problem is just that I clone in both places)
    let mut env = env.clone();
    loop {
        (ast, env) = expand_macros(ast, &env)?;
        if !matches!(ast, LispValue::Object(_)) {
            break eval_ast(ast, &env);
        }
        let LispValue::Object(ref arc) = ast else { unreachable!() };
        if !matches!(arc.deref(), ObjectValue::List(_)) {
            break eval_ast(ast, &env);
        }
        let Ok(mut list): Result<Vector<LispValue>> = ast.try_into() else { unreachable!() };
        let Some(head) = list.pop_front() else {
            // just return an empty list without evaluating anything
            break Ok(LispValue::list_from(list));
        };
        match eval(head, &env)? {
            LispValue::Special(form) => {
                ast = special_form!(form, list, eval, env);
            },
            LispValue::Object(o) => match o.deref() {
                ObjectValue::BuiltinFunc(f) => break (f.body)(list, &env),
                ObjectValue::Func(f) => {
                    let (new_ast, new_env) = expand_fn(f, list, &env)?;
                    ast = new_ast;
                    env = new_env;
                },
                x => break Err(
                    LispError::InvalidDataType("function", x.type_of())
                ),
            },
            x => break Err(
                LispError::InvalidDataType("function", x.type_of())
            ),
        }
    }
}
