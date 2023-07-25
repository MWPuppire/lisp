use crate::env::{LispEnv, LispSymbol};
use crate::specials::special_form;
use crate::util::{InnerObjectValue, LispFunc, ObjectValue};
use crate::{LispError, LispValue, Result};
use im::Vector;
use itertools::Itertools;
use std::iter::zip;
use std::sync::Arc;

#[inline]
fn lookup_variable(val: LispSymbol, env: &LispEnv) -> Result<LispValue> {
    env.get(val)
        .ok_or(LispError::UndefinedVariable(format!("\\{}", val)))
}

fn is_macro_call(val: &LispValue, env: &LispEnv) -> bool {
    if val.is_quoted() {
        return false;
    }
    match val {
        LispValue::Object(o) => match &o.val {
            InnerObjectValue::List(l) => {
                if l.is_empty() {
                    false
                } else {
                    match &l[0] {
                        LispValue::Symbol { sym, .. } => {
                            if let Ok(LispValue::Object(o)) = lookup_variable(*sym, env) {
                                matches!(o.val, InnerObjectValue::Macro(_),)
                            } else {
                                false
                            }
                        }
                        _ => false,
                    }
                }
            }
            _ => false,
        },
        _ => false,
    }
}

pub fn expand_macros(val: LispValue, env: &LispEnv) -> Result<LispValue> {
    if is_macro_call(&val, env) {
        let mut list = val.try_into_iter()?;
        let Some(head) = list.next() else { unreachable!() };
        let LispValue::Symbol { sym, .. } = head else { unreachable!() };
        let var = lookup_variable(sym, env)?;
        let LispValue::Object(obj) = var else { unreachable!() };
        let InnerObjectValue::Macro(f) = &obj.val else { unreachable!() };
        // some code duplication from `expand_fn`, but I didn't want `is_macro`
        // as a parameter for `expand_fn` to be in public API
        if f.variadic && list.len() >= (f.args.len() - 1) {
            let mut args: Vector<LispValue> = list.map(|x| expand_macros(x, env)).try_collect()?;
            let variadic_idx = f.args.len() - 1;
            let last_args = args.split_off(variadic_idx);
            let arg_names = f.args[0..variadic_idx].iter().copied();
            let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, args).collect();
            params.push((f.args[variadic_idx], last_args.into_iter().collect()));
            let fn_env = f.closure.make_macro_env(&params, env);
            eval(f.body.clone(), &fn_env)
        } else if list.len() != f.args.len() {
            Err(LispError::IncorrectArguments(f.args.len(), list.len()))
        } else {
            let args: Vec<LispValue> = list.map(|x| expand_macros(x, env)).try_collect()?;
            let params: Vec<(LispSymbol, LispValue)> = zip(f.args.iter().copied(), args).collect();
            let fn_env = f.closure.make_macro_env(&params, env);
            eval(f.body.clone(), &fn_env)
        }
    } else {
        Ok(val)
    }
}

// doesn't actually execute the function with its arguments (to allow TCO), just
// replaces the function with its body after creating the closure with all
// arguments properly applied
pub fn expand_fn(
    f: &LispFunc,
    args: Vector<LispValue>,
    env: &LispEnv,
) -> Result<(LispValue, Arc<LispEnv>)> {
    if f.variadic && args.len() >= (f.args.len() - 1) {
        let mut args: Vector<LispValue> = args.into_iter().map(|x| eval(x, env)).try_collect()?;
        let variadic_idx = f.args.len() - 1;
        let last_args = args.split_off(variadic_idx);
        let arg_names = f.args[0..variadic_idx].iter().copied();
        let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, args).collect();
        params.push((f.args[variadic_idx], last_args.into_iter().collect()));
        let fn_env = f.closure.make_env(&params);
        Ok((f.body.clone(), fn_env))
    } else if args.len() != f.args.len() {
        Err(LispError::IncorrectArguments(f.args.len(), args.len()))
    } else {
        let args: Vec<LispValue> = args.into_iter().map(|x| eval(x, env)).try_collect()?;
        let params: Vec<(LispSymbol, LispValue)> = zip(f.args.iter().copied(), args).collect();
        let fn_env = f.closure.make_env(&params);
        Ok((f.body.clone(), fn_env))
    }
}

fn eval_ast(ast: LispValue, env: &LispEnv) -> Result<LispValue> {
    Ok(match ast {
        LispValue::Symbol { sym, .. } => lookup_variable(sym, env)?,
        LispValue::Object(o) => LispValue::Object(Arc::new(ObjectValue {
            val: match &o.val {
                InnerObjectValue::Vector(l) => {
                    InnerObjectValue::Vector(l.iter().cloned().map(|x| eval(x, env)).try_collect()?)
                }
                InnerObjectValue::Map(m) => InnerObjectValue::Map(
                    m.iter()
                        .map(|(key, val)| {
                            Ok::<_, LispError>((key.clone(), eval(val.clone(), env)?))
                        })
                        .try_collect()?,
                ),
                _ => o.val.clone(),
            },
            meta: None,
            quoted: false,
        })),
        x => x,
    })
}

pub fn eval(mut ast: LispValue, env: &LispEnv) -> Result<LispValue> {
    let mut env = env.clone_arc();
    loop {
        // `is_macro_call` is used in `expand_macros`, so isn't needed here
        ast = expand_macros(ast, &env)?;
        if ast.is_quoted() {
            return Ok(ast.unquote());
        }
        if !matches!(ast, LispValue::Object(_)) {
            break eval_ast(ast, &env);
        }
        let LispValue::Object(ref arc) = ast else { unreachable!() };
        if !matches!(arc.val, InnerObjectValue::List(_)) {
            break eval_ast(ast, &env);
        }
        let Ok(mut list): Result<Vector<LispValue>> = ast.try_into() else { unreachable!() };
        let Some(head) = list.pop_front() else {
            // just return an empty list without evaluating anything
            break Ok(list.into_iter().collect());
        };
        match eval(head.unquote(), &env)? {
            LispValue::Special { form, .. } => {
                ast = special_form!(form, list, eval, env);
            }
            LispValue::Object(o) => match &o.val {
                InnerObjectValue::BuiltinFunc(f) => break (f.body)(list, &env),
                InnerObjectValue::Func(f) => {
                    let (new_ast, new_env) = expand_fn(f, list, &env)?;
                    ast = new_ast;
                    env = new_env;
                }
                x => break Err(LispError::InvalidDataType("function", x.type_of())),
            },
            x => break Err(LispError::InvalidDataType("function", x.type_of())),
        }
    }
}
