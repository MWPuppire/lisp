use std::iter::zip;
use std::sync::Arc;
use std::ops::Deref;
use im::{Vector, HashMap};
use crate::{LispValue, LispError, Result};
use crate::env::{LispEnv, LispSymbol};
use crate::util::{LispFunc, LispBuiltinResult, ObjectValue};

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
                        if let Ok(f) = var.expect_func() {
                            f.is_macro
                        } else {
                            false
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

pub fn expand_macros(val: LispValue, env: &mut LispEnv) -> Result<LispValue> {
    if is_macro_call(&val, env) {
        let mut list = val.into_list()?;
        let Some(head) = list.pop_front() else { unreachable!() };
        let LispValue::Symbol(name) = head else { unreachable!() };
        let var = lookup_variable(name, env)?;
        let Ok(f) = var.expect_func() else { unreachable!() };
        let (out, _) = apply(f, list, env)?;
        Ok(out)
    } else {
        Ok(val)
    }
}

/*
fn wrap_macro(mut f: Box<LispFunc>, env: &mut LispEnv) -> Result<LispValue> {
    f.closure = Some(env.make_closure());
    f.args.clear();
    f.is_macro = false;
    f.variadic = false;
    f.body = expand_macros(f.body, env)?;
    Ok(LispValue::Func(f))
}
*/

fn apply(f: &LispFunc, args: Vector<LispValue>, env: &mut LispEnv) -> Result<(LispValue, LispEnv)> {
    let evaluator = if f.is_macro { expand_macros } else { eval };
    if f.variadic && args.len() >= (f.args.len() - 1) {
        let mut vals = args.into_iter().map(|x| evaluator(x, env)).collect::<Result<Vector<LispValue>>>()?;
        let last_vals = vals.split_off(f.args.len() - 1);
        let variadic_idx = f.args.len() - 1;
        let arg_names = f.args[0..variadic_idx].iter().copied();
        let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, vals).collect();
        params.push((f.args[variadic_idx], LispValue::list_from(last_vals)));
        let fn_env = if f.is_macro {
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
        let fn_env = if f.is_macro {
            f.closure.make_macro_env(&params, env)
        } else {
            f.closure.make_env(&params)
        };
        Ok((f.body.clone(), fn_env))
    }
}

/*
pub fn eval(value: LispValue, env: &mut LispEnv) -> Result<LispValue> {
    // TODO: current `queued` system is considered UB by Stacked Borrows
    // wrap in Cell or something?
    let mut queued = vec![(Some(value), None, env)];
    let mut envs_to_drop = vec![];
    while let Some((head, mut tail, mut env)) = queued.pop() {
        let Some(head) = head else { unreachable!() };
        let mut head = expand_macros(head, env)?;
        let new_head = loop {
            match head {
                LispValue::Symbol(s) => head = lookup_variable(s, env)?,
                LispValue::VariadicSymbol(s) => head = lookup_variable(s, env)?,
                LispValue::List(mut l) => {
                    if tail.is_some() {
                        if !l.is_empty() {
                            queued.push((None, tail.take(), unsafe {
                                NonNull::from(&*env).as_mut()
                            }));
                            head = LispValue::List(l);
                        } else {
                            break Err(LispError::InvalidDataType("function", "list"));
                        }
                    } else if let Some(inner_head) = l.pop_front() {
                        head = inner_head;
                        tail = Some(l);
                    } else {
                        break Ok(LispValue::List(l));
                    }
                },
                LispValue::BuiltinFunc { f, name } => {
                    if let Some(args) = tail.take() {
                        match f(args, env) {
                            LispBuiltinResult::Done(val) => {
                                break Ok(val);
                            },
                            LispBuiltinResult::Continue(expr) => {
                                head = expr;
                            },
                            LispBuiltinResult::ContinueIn(expr, new_env) => {
                                head = expr;
                                let boxed = Box::new(new_env);
                                unsafe {
                                    let ptr = Box::into_raw(boxed);
                                    env = NonNull::new_unchecked(ptr).as_mut();
                                    envs_to_drop.push(Box::from_raw(ptr));
                                }
                            },
                            LispBuiltinResult::Error(err) => {
                                break Err(err)
                            },
                        }
                    } else {
                        break Ok(LispValue::BuiltinFunc { f, name });
                    }
                },
                LispValue::Func(f) => {
                    if let Some(args) = tail.take() {
                        let (new_head, new_env) = apply(f, args, env)?;
                        queued.push((None, None, env));
                        head = new_head;
                        let boxed = Box::new(new_env);
                        unsafe {
                            let ptr = Box::into_raw(boxed);
                            env = NonNull::new_unchecked(ptr).as_mut();
                            envs_to_drop.push(Box::from_raw(ptr));
                        }
                    } else {
                        break Ok(LispValue::Func(f));
                    }
                },
                LispValue::Vector(l) => break if tail.is_some() {
                    Err(LispError::InvalidDataType("function", "vector"))
                } else {
                    let l = l.into_iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?;
                    Ok(LispValue::Vector(l))
                },
                LispValue::Map(m) => break if tail.is_some() {
                    Err(LispError::InvalidDataType("function", "map"))
                } else {
                    let m = m.into_iter().map(|(key, val)| {
                        Ok((key, eval(val, env)?))
                    }).collect::<Result<Vec<(LispValue, LispValue)>>>()?.into();
                    Ok(LispValue::Map(m))
                },
                x => break if tail.is_some() {
                    Err(LispError::InvalidDataType("function", x.type_of()))
                } else {
                    Ok(x)
                },
            }
        }?;
        if let Some(last) = queued.last_mut() {
            if last.0.is_none() {
                last.0 = Some(new_head);
            }
        } else {
            return Ok(new_head);
        }
    }
    Ok(LispValue::Nil)
}

pub fn eval_top(value: LispValue, env: &mut LispEnv) -> Result<LispValue> {
    match value {
        LispValue::Symbol(s) => lookup_variable(s, env),
        x => eval(x, env),
    }
}
*/

fn eval_ast(ast: LispValue, env: &mut LispEnv) -> Result<LispValue> {
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

pub fn eval(mut ast: LispValue, mut env: &mut LispEnv) -> Result<LispValue> {
    let mut envs_to_drop = vec![];
    loop {
        ast = expand_macros(ast, env)?;
        if !matches!(ast, LispValue::Object(_)) {
            break eval_ast(ast, env);
        }
        let LispValue::Object(ref arc) = ast else { unreachable!() };
        if !matches!(arc.deref(), ObjectValue::List(_)) {
            break eval_ast(ast, env);
        }
        let Ok(mut list) = ast.into_list() else { unreachable!() };
        let Some(head) = list.pop_front() else {
            // just return an empty list without evaluating anything
            break Ok(LispValue::list_from(list));
        };
        match eval(head, env)? {
            LispValue::Object(o) => match o.deref() {
                ObjectValue::BuiltinFunc { f, .. } => {
                    match f(list, env) {
                        LispBuiltinResult::Done(val) => {
                            break Ok(val);
                        },
                        LispBuiltinResult::Continue(expr) => {
                            ast = expr;
                        },
                        LispBuiltinResult::ContinueIn(expr, new_env) => {
                            ast = expr;
                            let idx = envs_to_drop.len();
                            envs_to_drop.push(new_env);
                            env = unsafe {
                                let ptr = envs_to_drop.as_mut_ptr();
                                ptr.add(idx).as_mut().unwrap()
                            };
                        },
                        LispBuiltinResult::Error(err) => {
                            break Err(err);
                        },
                    }
                },
                ObjectValue::Func(f) => {
                    let (new_ast, new_env) = apply(f, list, env)?;
                    ast = new_ast;
                    let idx = envs_to_drop.len();
                    envs_to_drop.push(new_env);
                    env = unsafe {
                        let ptr = envs_to_drop.as_mut_ptr();
                        ptr.add(idx).as_mut().unwrap()
                    };
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
