use std::iter::zip;
use std::sync::Arc;
use std::ops::Deref;
use im::{Vector, vector, HashMap};
use crate::{LispValue, LispError, Result};
use crate::env::{LispEnv, LispSymbol};
use crate::util::{LispFunc, ObjectValue, LispSpecialForm, expect};
use crate::specials::special_form;

fn lookup_variable(val: LispSymbol, env: &LispEnv) -> Result<LispValue> {
    env.get(val).ok_or(LispError::UndefinedVariable(LispEnv::symbol_string(val).unwrap()))
}

fn is_macro_call(val: &LispValue, env: &LispEnv) -> bool {
    match val {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(l) => {
                if l.is_empty() {
                    false
                } else if let Ok(sym) = l[0].expect_symbol() {
                    if let Ok(var) = lookup_variable(sym, env) {
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
                } else {
                    false
                }
            },
            _ => false,
        },
        _ => false,
    }
}

pub fn expand_macros(val: LispValue, env: &LispEnv) -> Result<LispValue> {
    if is_macro_call(&val, env) {
        let mut list = val.into_list()?;
        let Some(head) = list.pop_front() else { unreachable!() };
        let LispValue::Symbol(name) = head else { unreachable!() };
        let var = lookup_variable(name, env)?;
        let Ok(f) = var.expect_func() else { unreachable!() };
        let (out, _) = apply(f, list, env, true)?;
        Ok(out)
    } else {
        Ok(val)
    }
}

fn apply(f: &LispFunc, args: Vector<LispValue>, env: &LispEnv, is_macro: bool) -> Result<(LispValue, LispEnv)> {
    let evaluator = if is_macro { expand_macros } else { eval };
    if f.variadic && args.len() >= (f.args.len() - 1) {
        let mut vals = args.into_iter().map(|x| evaluator(x, env)).collect::<Result<Vector<LispValue>>>()?;
        let last_vals = vals.split_off(f.args.len() - 1);
        let variadic_idx = f.args.len() - 1;
        let arg_names = f.args[0..variadic_idx].iter().copied();
        let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, vals).collect();
        params.push((f.args[variadic_idx], LispValue::list_from(last_vals)));
        let fn_env = if is_macro {
            f.closure.make_macro_env(&params, env)
        } else {
            f.closure.make_env(&params)
        };
        Ok((f.body.clone(), fn_env))
    } else if args.len() != f.args.len() {
        Err(LispError::IncorrectArguments(f.args.len(), args.len()))
    } else {
        let vals = args.into_iter().map(|x| evaluator(x, env)).collect::<Result<Vec<LispValue>>>()?;
        let params: Vec<(LispSymbol, LispValue)> = zip(f.args.iter().copied(), vals).collect();
        let fn_env = if is_macro {
            f.closure.make_macro_env(&params, env)
        } else {
            f.closure.make_env(&params)
        };
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
    // cloning an environment is cheap, just bumping an Arc
    let mut env = env.clone();
    loop {
        ast = expand_macros(ast, &env)?;
        if !matches!(ast, LispValue::Object(_)) {
            break eval_ast(ast, &env);
        }
        let LispValue::Object(ref arc) = ast else { unreachable!() };
        if !matches!(arc.deref(), ObjectValue::List(_)) {
            break eval_ast(ast, &env);
        }
        let Ok(mut list) = ast.into_list() else { unreachable!() };
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
                    let (new_ast, new_env) = apply(f, list, &env, false)?;
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
