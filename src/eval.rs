use std::iter::zip;
use im::{Vector, vector};
use crate::{LispValue, LispError, Result};
use crate::env::{LispEnv, LispSymbol};
use crate::util::{LispFunc, LispBuiltinResult};

fn lookup_variable(val: LispSymbol, env: &LispEnv) -> Result<LispValue> {
    env.get(val).ok_or(LispError::UndefinedVariable(LispEnv::symbol_string(val).unwrap()))
}

fn is_macro_call(val: &LispValue, env: &LispEnv) -> bool {
    match val {
        LispValue::List(list) => {
            if list.is_empty() {
                false
            } else if let Ok(sym) = list[0].expect_symbol() {
                match lookup_variable(sym, env) {
                    Ok(LispValue::Func(f)) => f.is_macro,
                    _ => false,
                }
            } else {
                false
            }
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
        let LispValue::Func(f) = var else { unreachable!() };
        let (out, _) = apply(f, list, env)?;
        Ok(out)
    } else {
        Ok(val)
    }
}

fn wrap_macro(mut f: Box<LispFunc>, env: &mut LispEnv) -> Result<LispValue> {
    f.closure = Some(env.make_closure());
    f.args.clear();
    f.name.take();
    f.is_macro = false;
    f.variadic = false;
    f.body = expand_macros(f.body, env)?;
    Ok(LispValue::List(vector![LispValue::Func(f)]))
}

fn apply(f: Box<LispFunc>, args: Vector<LispValue>, env: &mut LispEnv) -> Result<(LispValue, LispEnv)> {
    let evaluator = if f.is_macro { expand_macros } else { eval };
    if f.variadic && args.len() >= (f.args.len() - 1) {
        let mut vals = args.into_iter().map(|x| evaluator(x, env)).collect::<Result<Vector<LispValue>>>()?;
        let mut last_vals = vals.split_off(f.args.len() - 1);
        let list_sym = LispEnv::symbol_for_static("list");
        last_vals.push_front(LispValue::Symbol(list_sym));
        let variadic_idx = f.args.len() - 1;
        let arg_names = f.args[0..variadic_idx].iter().map(|x| x.to_owned());
        let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, vals).collect();
        params.push((f.args[variadic_idx].to_owned(), LispValue::List(last_vals)));
        if let Some(name) = &f.name {
            params.push((*name, LispValue::Func(f.clone())));
        }
        if f.is_macro {
            let mut fn_env = if let Some(closure) = &f.closure {
                closure.make_macro_env(&params, env)
            } else {
                env.global()
            };
            Ok((wrap_macro(f, &mut fn_env)?, fn_env))
        } else {
            let fn_env = if let Some(closure) = &f.closure {
                closure.make_env(&params)
            } else {
                env.global()
            };
            Ok((f.body, fn_env))
        }
    } else if args.len() != f.args.len() {
        Err(LispError::IncorrectArguments(f.args.len(), args.len()))
    } else {
        let vals = args.into_iter().map(|x| evaluator(x, env)).collect::<Result<Vec<LispValue>>>()?;
        let arg_names = f.args.iter().map(|x| x.to_owned());
        let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, vals).collect();
        if let Some(name) = &f.name {
            params.push((*name, LispValue::Func(f.clone())));
        }
        if f.is_macro {
            let mut fn_env = if let Some(closure) = &f.closure {
                closure.make_macro_env(&params, env)
            } else {
                env.global()
            };
            Ok((wrap_macro(f, &mut fn_env)?, fn_env))
        } else {
            let fn_env = if let Some(closure) = &f.closure {
                closure.make_env(&params)
            } else {
                env.global()
            };
            Ok((f.body, fn_env))
        }
    }
}

pub fn eval(value: LispValue, env: &mut LispEnv) -> Result<LispValue> {
    let mut queued = vec![(Some(value), None, env.clone())];
    while let Some((head, mut tail, mut env)) = queued.pop() {
        let Some(head) = head else { unreachable!() };
        let mut head = expand_macros(head, &mut env)?;
        let new_head = loop {
            match head {
                LispValue::Symbol(s) => head = lookup_variable(s, &env)?,
                LispValue::VariadicSymbol(s) => head = lookup_variable(s, &env)?,
                LispValue::List(mut l) => {
                    if tail.is_some() {
                        if !l.is_empty() {
                            queued.push((None, tail.take(), env.clone()));
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
                        match f(args, &mut env) {
                            LispBuiltinResult::Done(val) => {
                                break Ok(val);
                            },
                            LispBuiltinResult::Continue(expr) => {
                                head = expr;
                            },
                            LispBuiltinResult::ContinueIn(expr, new_env) => {
                                head = expr;
                                env = new_env;
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
                        let (new_head, new_env) = apply(f, args, &mut env)?;
                        queued.push((None, None, env));
                        head = new_head;
                        env = new_env;
                    } else {
                        break Ok(LispValue::Func(f));
                    }
                },
                LispValue::Vector(l) => break if tail.is_some() {
                    Err(LispError::InvalidDataType("function", "vector"))
                } else {
                    let l = l.into_iter().map(|x| eval(x, &mut env)).collect::<Result<Vec<LispValue>>>()?;
                    Ok(LispValue::Vector(l))
                },
                LispValue::Map(m) => break if tail.is_some() {
                    Err(LispError::InvalidDataType("function", "map"))
                } else {
                    let m = m.into_iter().map(|(key, val)| {
                        Ok((key, eval(val, &mut env)?))
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
