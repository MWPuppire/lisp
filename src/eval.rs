use std::iter::zip;
use im::Vector;
use crate::{LispValue, LispError, Result, env::LispEnv, util::LispFunc};

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
        let LispValue::Symbol(name) = head else { unreachable!() };
        let var = lookup_variable(name, env)?;
        let LispValue::Func(f) = var else { unreachable!() };
        out = apply(f, &list, env)?;
    }
    Ok(out)
}

fn apply(f: Box<LispFunc>, args: &Vector<LispValue>, env: &mut LispEnv) -> Result<LispValue> {
    let evaluator = if f.is_macro { expand_macros } else { eval };
    if f.variadic && args.len() >= (f.args.len() - 1) {
        let mut vals = args.iter().map(|x| evaluator(x, env)).collect::<Result<Vector<LispValue>>>()?;
        let last_vals = vals.split_off(f.args.len() - 1);
        let variadic_idx = f.args.len() - 1;
        let arg_names = f.args[0..variadic_idx].iter().map(|x| x.to_owned());
        let mut params: Vec<(String, LispValue)> = zip(arg_names, vals).collect();
        params.push((f.args[variadic_idx].to_owned(), LispValue::List(last_vals)));
        if let Some(name) = &f.name {
            params.push((name.clone(), LispValue::Func(f.clone())));
        }
        if env.is_from_closure(&f.closure) {
            env.mass_set(params);
            Ok(f.body.clone())
        } else {
            let mut fn_env = f.closure.make_env(&params);
            eval(&f.body, &mut fn_env)
        }
    } else if args.len() != f.args.len() {
        Err(LispError::IncorrectArguments(f.args.len(), args.len()))
    } else {
        let vals = args.iter().map(|x| evaluator(x, env)).collect::<Result<Vec<LispValue>>>()?;
        let arg_names = f.args.iter().map(|x| x.to_owned());
        let mut params: Vec<(String, LispValue)> = zip(arg_names, vals).collect();
        if let Some(name) = &f.name {
            params.push((name.clone(), LispValue::Func(f.clone())));
        }
        if env.is_from_closure(&f.closure) {
            env.mass_set(params);
            Ok(f.body.clone())
        } else {
            let mut fn_env = f.closure.make_env(&params);
            eval(&f.body, &mut fn_env)
        }
    }
}

pub fn eval(value: &LispValue, env: &mut LispEnv) -> Result<LispValue> {
    let mut head = expand_macros(value, env)?;
    let mut tail = None;
    loop {
        match head {
            LispValue::Symbol(s) => head = lookup_variable(s, env)?,
            LispValue::VariadicSymbol(s) => head = lookup_variable(s, env)?,
            LispValue::List(mut l) => {
                if tail.is_some() {
                    if l.len() > 0 {
                        head = eval(&LispValue::List(l), env)?;
                    } else {
                        break Err(LispError::InvalidDataType("function", "list"));
                    }
                } else {
                    if let Some(inner_head) = l.pop_front() {
                        head = inner_head;
                        tail = Some(l);
                    } else {
                        break Ok(LispValue::List(l));
                    }
                }
            },
            LispValue::BuiltinFunc { f, name } => {
                if let Some(args) = tail.take() {
                    let (new_head, cont) = f.0(args, env)?;
                    if cont {
                        head = new_head;
                    } else {
                        break Ok(new_head);
                    }
                } else {
                    break Ok(LispValue::BuiltinFunc { f, name });
                }
            },
            LispValue::Func(f) => {
                if let Some(args) = tail.take() {
                    head = apply(f, &args, env)?;
                } else {
                    break Ok(LispValue::Func(f));
                }
            },
            LispValue::Vector(l) => break if tail.is_some() {
                Err(LispError::InvalidDataType("function", "vector"))
            } else {
                let l = l.iter().map(|x| eval(x, env)).collect::<Result<Vec<LispValue>>>()?;
                Ok(LispValue::Vector(l))
            },
            LispValue::Map(m) => break if tail.is_some() {
                Err(LispError::InvalidDataType("function", "map"))
            } else {
                let m = m.iter().map(|(key, val)| {
                    Ok((key.clone(), eval(val, env)?))
                }).collect::<Result<Vec<(LispValue, LispValue)>>>()?.into();
                Ok(LispValue::Map(m))
            },
            x => break if tail.is_some() {
                Err(LispError::InvalidDataType("function", x.type_of()))
            } else {
                Ok(x)
            }
        }
    }
}
