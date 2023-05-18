use std::iter::zip;
use std::sync::Arc;
use std::ops::Deref;
use im::{Vector, vector, HashMap};
use crate::{LispValue, LispError, Result, expect};
use crate::env::{LispEnv, LispSymbol};
use crate::util::{LispFunc, ObjectValue, LispSpecialForm};

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

pub fn expand_macros(val: LispValue, env: &mut LispEnv) -> Result<LispValue> {
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

fn apply(f: &LispFunc, args: Vector<LispValue>, env: &mut LispEnv, is_macro: bool) -> Result<(LispValue, LispEnv)> {
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

fn inner_quasiquote(arg: LispValue, env: &mut LispEnv) -> Result<(LispValue, bool)> {
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(l) => {
                if l.is_empty() {
                    return Ok((LispValue::Object(o.clone()), false));
                }
                let ObjectValue::List(mut l) = Arc::unwrap_or_clone(o) else { unreachable!() };
                // established non-empty just prior
                if l[0] == LispValue::Special(LispSpecialForm::Unquote) {
                    expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                    Ok((eval(l.pop_back().unwrap(), env)?, false))
                } else if l[0] == LispValue::Special(LispSpecialForm::SpliceUnquote) {
                    expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                    Ok((eval(l.pop_back().unwrap(), env)?, true))
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
            LispValue::Special(form) => match form {
                LispSpecialForm::Def => {
                    expect!(list.len() == 2, LispError::IncorrectArguments(2, list.len()));
                    if let LispValue::Special(form) = &list[0] {
                        // provide a more helpful error, since `def! quote x`
                        // looks like it's definining a valid symbol
                        break Err(LispError::CannotRedefineSpecialForm(*form));
                    }
                    let name = list[0].expect_symbol()?;
                    let val = eval(list.pop_back().unwrap(), env)?;
                    env.set(name, val.clone());
                    break Ok(val);
                },
                LispSpecialForm::Defmacro => {
                    expect!(list.len() == 2, LispError::IncorrectArguments(2, list.len()));
                    if let LispValue::Special(form) = &list[0] {
                        // provide a more helpful error, since `def! quote x`
                        // looks like it's definining a valid symbol
                        break Err(LispError::CannotRedefineSpecialForm(*form));
                    }
                    let name = list[0].expect_symbol()?;
                    let val = eval(list.pop_back().unwrap(), env)?;
                    let val = match val {
                        LispValue::Object(o) => match o.deref() {
                            ObjectValue::Func(f) => {
                                LispValue::Object(Arc::new(
                                    ObjectValue::Macro(f.clone())
                                ))
                            },
                            x => break Err(LispError::InvalidDataType("function", x.type_of())),
                        }
                        x => break Err(LispError::InvalidDataType("function", x.type_of())),
                    };
                    env.set(name, val.clone());
                    break Ok(val);
                },
                LispSpecialForm::Let => {
                    expect!(list.len() == 2, LispError::IncorrectArguments(2, list.len()));
                    let mut new_env = env.new_nested();
                    let decls = list.pop_front().unwrap().into_list()?;
                    let mut decl_iter = decls.into_iter();
                    while let Some(name) = decl_iter.next() {
                        let name = name.expect_symbol()?;
                        let Some(val_expr) = decl_iter.next() else {
                            return Err(LispError::MissingBinding);
                        };
                        let val = eval(val_expr, &mut new_env)?;
                        new_env.set(name, val);
                    }
                    ast = list.pop_front().unwrap();
                    let idx = envs_to_drop.len();
                    envs_to_drop.push(new_env);
                    env = unsafe {
                        let ptr = envs_to_drop.as_mut_ptr();
                        ptr.add(idx).as_mut().unwrap()
                    };
                },
                LispSpecialForm::Quote => {
                    if let Some(quoted) = list.pop_front() {
                        break Ok(quoted);
                    } else {
                        break Err(LispError::IncorrectArguments(1, 0));
                    }
                },
                LispSpecialForm::Quasiquote => {
                    const UNQUOTE: LispValue = LispValue::Special(
                        LispSpecialForm::Unquote
                    );
                    const SPLICE_UNQUOTE: LispValue = LispValue::Special(
                        LispSpecialForm::SpliceUnquote
                    );
                    let Some(front) = list.pop_front() else {
                        break Err(LispError::IncorrectArguments(1, 0));
                    };
                    match front {
                        LispValue::Object(o) => match o.deref() {
                            ObjectValue::List(list) => {
                                if list.is_empty() {
                                    break Ok(LispValue::Object(o.clone()));
                                }
                                let ObjectValue::List(mut list) = Arc::unwrap_or_clone(o) else {
                                    unreachable!();
                                };
                                if list[0] == UNQUOTE || list[0] == SPLICE_UNQUOTE {
                                    expect!(list.len() == 2, LispError::IncorrectArguments(1, list.len() - 1));
                                    ast = list.pop_back().unwrap();
                                } else {
                                    let mut out = Vector::new();
                                    for val in list {
                                        let (new_val, inplace) = inner_quasiquote(val, env)?;
                                        if inplace {
                                            out.append(new_val.into_list()?);
                                        } else {
                                            out.push_back(new_val);
                                        }
                                    }
                                    break Ok(LispValue::list_from(out));
                                }
                            },
                            _ => break Ok(LispValue::Object(o.clone())),
                        },
                        x => break Ok(x),
                    }
                },
                // `unquote` and `splice-unquote` are handled internally in
                // `quasiquote`, so usage here must be outside quasiquote (which
                // is an error)
                LispSpecialForm::Unquote | LispSpecialForm::SpliceUnquote => {
                    break Err(LispError::OnlyInQuasiquote);
                },
                LispSpecialForm::Macroexpand => {
                    if let Some(quoted) = list.pop_front() {
                        break expand_macros(quoted, env);
                    } else {
                        break Err(LispError::IncorrectArguments(1, 0));
                    }
                },
                LispSpecialForm::Try => {
                    expect!(list.len() == 2, LispError::IncorrectArguments(2, list.len()));
                    let catch = list.pop_back().unwrap();
                    if let Ok(mut catch) = catch.into_list() {
                        expect!(catch.len() == 3, LispError::IncorrectArguments(2, catch.len() - 1));
                        let catch_sym = catch.pop_front().unwrap();
                        if catch_sym != LispValue::Special(LispSpecialForm::Catch) {
                            break Err(LispError::TryNoCatch);
                        }
                        let err_name = catch.pop_front().unwrap().expect_symbol()?;
                        let mut caught_env = env.new_nested();
                        match eval(list.pop_front().unwrap(), env) {
                            Ok(x) => break Ok(x),
                            Err(err) => {
                                if let LispError::UncaughtException(exc) = err {
                                    caught_env.set(err_name, exc);
                                } else {
                                    let s = err.to_string();
                                    caught_env.set(err_name, s.into());
                                }
                                ast = catch.pop_front().unwrap();
                                let idx = envs_to_drop.len();
                                envs_to_drop.push(caught_env);
                                env = unsafe {
                                    let ptr = envs_to_drop.as_mut_ptr();
                                    ptr.add(idx).as_mut().unwrap()
                                };
                            },
                        }
                    } else {
                        break Err(LispError::TryNoCatch);
                    }
                },
                // `catch*` is handled internally in `try*`, so being used here
                // automatically means it's outside a try block
                LispSpecialForm::Catch => {
                    break Err(LispError::OnlyInTry);
                },
                LispSpecialForm::Do => {
                    let Some(tail) = list.pop_back() else {
                        break Err(LispError::IncorrectArguments(1, 0));
                    };
                    for val in list.into_iter() {
                        eval(val, env)?;
                    }
                    ast = tail;
                },
                LispSpecialForm::If => {
                    expect!(list.len() > 1 && list.len() < 4, LispError::IncorrectArguments(2, list.len()));
                    let pred = list.pop_front().unwrap();
                    let pred = eval(pred, env)?.truthiness();
                    if pred {
                        ast = list.pop_front().unwrap();
                    } else if list.len() > 1 {
                        ast = list.pop_back().unwrap();
                    } else {
                        break Ok(LispValue::Nil);
                    }
                },
                LispSpecialForm::Fn => {
                    expect!(list.len() == 2, LispError::IncorrectArguments(2, list.len()));
                    let args_list = list.pop_front().unwrap().into_list()?;
                    let variadic = args_list.last().map(|last| {
                        matches!(last, LispValue::VariadicSymbol(_))
                    }).unwrap_or(false);
                    let args = args_list.into_iter()
                        .map(|x| x.expect_symbol())
                        .collect::<Result<Vec<LispSymbol>>>()?;
                    let body = list.pop_front().unwrap();
                    let closure = env.make_closure();
                    let inner = ObjectValue::Func(LispFunc {
                        args,
                        body,
                        closure,
                        variadic,
                    });
                    break Ok(LispValue::Object(Arc::new(inner)));
                },
                LispSpecialForm::Deref => {
                    if let Some(atom) = list.pop_front() {
                        let atom = eval(atom, env)?.into_atom()?;
                        let out = atom.read().unwrap().clone();
                        break Ok(out)
                    } else {
                        break Err(LispError::IncorrectArguments(1, 0));
                    }
                },
                LispSpecialForm::Eval => {
                    let Some(expr) = list.pop_front() else {
                        break Err(LispError::IncorrectArguments(1, 0));
                    };
                    ast = eval(expr, env)?;
                    let global = env.global();
                    let idx = envs_to_drop.len();
                    envs_to_drop.push(global);
                    env = unsafe {
                        let ptr = envs_to_drop.as_mut_ptr();
                        ptr.add(idx).as_mut().unwrap()
                    };
                },
                LispSpecialForm::Apply => {
                    expect!(list.len() > 1, LispError::IncorrectArguments(2, list.len()));
                    let f = eval(list.pop_front().unwrap(), env)?;
                    let args = eval(list.pop_back().unwrap(), env)?.into_list()?;
                    let mut full_list = vector![f];
                    full_list.append(list);
                    full_list.append(args);
                    ast = LispValue::list_from(full_list);
                },
                LispSpecialForm::Cond => {
                    expect!(list.len() & 1 == 0, LispError::OddCondArguments);
                    // return `nil` if none of the conditions ends up true
                    ast = LispValue::Nil;
                    while let Some(pred) = list.pop_front() {
                        let then = list.pop_front().unwrap();
                        if eval(pred, env)?.truthiness() {
                            ast = then;
                            break;
                        }
                    }
                },
            },
            LispValue::Object(o) => match o.deref() {
                ObjectValue::BuiltinFunc { f, .. } => break f(list, env),
                ObjectValue::Func(f) => {
                    let (new_ast, new_env) = apply(f, list, env, false)?;
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
