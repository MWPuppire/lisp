use crate::env::{LispEnv, LispSymbol};
use crate::specials::special_form;
use crate::util::{LispFunc, ObjectValue};
use crate::{LispError, LispValue, Result};
use im::Vector;
use itertools::Itertools;
use parking_lot::{RwLock, RwLockWriteGuard};
use std::iter::zip;
use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

#[inline]
fn lookup_variable(val: LispSymbol, env: &LispEnv) -> Result<LispValue> {
    env.get(val).ok_or(LispError::UndefinedVariable(
        LispEnv::symbol_string(val).unwrap(),
    ))
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
                            if let Ok(LispValue::Object(o)) = lookup_variable(*s, env) {
                                matches!(o.deref(), ObjectValue::Macro(_),)
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

pub fn expand_macros(
    val: LispValue,
    env: &mut LispEnv,
) -> Result<(LispValue, Arc<RwLock<LispEnv>>)> {
    if is_macro_call(&val, env) {
        let mut list = val.try_into_iter()?;
        let Some(head) = list.next() else { unreachable!() };
        let LispValue::Symbol(name) = head else { unreachable!() };
        let var = lookup_variable(name, env)?;
        let LispValue::Object(obj) = var else { unreachable!() };
        let ObjectValue::Macro(f) = obj.deref() else { unreachable!() };
        // some code duplication from `expand_fn`, but I didn't want `is_macro`
        // as a parameter for `expand_fn` to be in public API
        if f.variadic && list.len() >= (f.args.len() - 1) {
            let mut args: Vector<LispValue> = list
                .map(|x| Ok::<_, LispError>(expand_macros(x, env)?.0))
                .try_collect()?;
            let variadic_idx = f.args.len() - 1;
            let last_args = args.split_off(variadic_idx);
            let arg_names = f.args[0..variadic_idx].iter().copied();
            let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, args).collect();
            params.push((f.args[variadic_idx], last_args.into_iter().collect()));
            let fn_env = f.closure.make_macro_env(&params, env);
            Ok((f.body.clone(), fn_env))
        } else if list.len() != f.args.len() {
            Err(LispError::IncorrectArguments(f.args.len(), list.len()))
        } else {
            let args: Vec<LispValue> = list
                .map(|x| Ok::<_, LispError>(expand_macros(x, env)?.0))
                .try_collect()?;
            let params: Vec<(LispSymbol, LispValue)> = zip(f.args.iter().copied(), args).collect();
            let fn_env = f.closure.make_macro_env(&params, env);
            Ok((f.body.clone(), fn_env))
        }
    } else {
        // TODO cloning environments is cheap since it's just bumping an Arc,
        // but I should maybe still find a better solution to this
        Ok((val, env.clone_arc()))
    }
}

// doesn't actually execute the function with its arguments (to allow TCO), just
// replaces the function with its body after creating the closure with all
// arguments properly applied
pub fn expand_fn(
    f: &LispFunc,
    args: Vector<LispValue>,
    env: &mut LispEnv,
) -> Result<(LispValue, Arc<RwLock<LispEnv>>)> {
    if f.variadic && args.len() >= (f.args.len() - 1) {
        let mut args: Vector<LispValue> = args.into_iter().map(|x| eval(x, env)).try_collect()?;
        let variadic_idx = f.args.len() - 1;
        let last_args = args.split_off(variadic_idx);
        let arg_names = f.args[0..variadic_idx].iter().copied();
        let mut params: Vec<(LispSymbol, LispValue)> = zip(arg_names, args).collect();
        params.push((f.args[variadic_idx], last_args.into_iter().collect()));
        let fn_env = f.closure.make_env(&params, env);
        Ok((f.body.clone(), fn_env))
    } else if args.len() != f.args.len() {
        Err(LispError::IncorrectArguments(f.args.len(), args.len()))
    } else {
        let args: Vec<LispValue> = args.into_iter().map(|x| eval(x, env)).try_collect()?;
        let params: Vec<(LispSymbol, LispValue)> = zip(f.args.iter().copied(), args).collect();
        let fn_env = f.closure.make_env(&params, env);
        Ok((f.body.clone(), fn_env))
    }
}

fn eval_ast(ast: LispValue, env: &mut LispEnv) -> Result<LispValue> {
    Ok(match ast {
        LispValue::Symbol(s) => lookup_variable(s, env)?,
        LispValue::VariadicSymbol(s) => lookup_variable(s, env)?,
        LispValue::Object(o) => LispValue::Object(match o.deref() {
            ObjectValue::Vector(l) => Arc::new(ObjectValue::Vector(
                l.iter().cloned().map(|x| eval(x, env)).try_collect()?,
            )),
            ObjectValue::Map(m) => Arc::new(ObjectValue::Map(
                m.iter()
                    .map(|(key, val)| Ok::<_, LispError>((key.clone(), eval(val.clone(), env)?)))
                    .try_collect()?,
            )),
            _ => o.clone(),
        }),
        x => x,
    })
}

pub fn eval(mut ast: LispValue, mut env: &mut LispEnv) -> Result<LispValue> {
    let mut locks_to_drop: Vec<(
        *const RwLock<LispEnv>,
        ManuallyDrop<RwLockWriteGuard<'_, LispEnv>>,
    )> = vec![];
    let env_copy = env.clone_arc();
    // whether the environment was unlocked in `stash_env` and needs to be
    // re-locked before returning (bad design)
    let mut relock = false;
    // This whole function is an unsafe mess, and it almost certainly breaks
    // Stacked Borrows. (Unfortunately, `im` violating Stacked Borrows means
    // testing never gets far enough to see whether `eval` breaks it, too).
    // I'm not a huge fan, but I'm not sure how to do it better.
    let mut stash_env = |new_env: Arc<RwLock<LispEnv>>| unsafe {
        let idx = locks_to_drop.len();
        if idx > 0 {
            let last_ptr = std::ptr::addr_of_mut!(locks_to_drop[idx - 1].1);
            ManuallyDrop::drop(last_ptr.as_mut().unwrap());
        } else {
            let lock = env_copy.deref();
            if lock.is_locked_exclusive() {
                lock.force_unlock_write();
                relock = true;
            }
        }
        let ptr = Arc::into_raw(new_env);
        let lock = ManuallyDrop::new(ptr.as_ref().unwrap().write());
        locks_to_drop.push((ptr, lock));
        let lock_ptr = std::ptr::addr_of_mut!(locks_to_drop[idx].1);
        lock_ptr.as_mut().unwrap().deref_mut()
    };
    // Not a fan of this immediately invoked closure, but specific clean-up
    // needs to be performed at end-of-function (and the `?` syntax is too
    // convenient to give up), so this keeps the early-return from exiting the
    // whole function (and thereby skipping the clean-up, which needs to be
    // performed even in an error-case).
    let out = (|| loop {
        if is_macro_call(&ast, env) {
            let (new_ast, new_env) = expand_macros(ast, env)?;
            ast = new_ast;
            env = stash_env(new_env);
        }
        if !matches!(ast, LispValue::Object(_)) {
            break eval_ast(ast, env);
        }
        let LispValue::Object(ref arc) = ast else { unreachable!() };
        if !matches!(arc.deref(), ObjectValue::List(_)) {
            break eval_ast(ast, env);
        }
        let Ok(mut list): Result<Vector<LispValue>> = ast.try_into() else { unreachable!() };
        let Some(head) = list.pop_front() else {
            // just return an empty list without evaluating anything
            break Ok(list.into_iter().collect());
        };
        match eval(head, env)? {
            LispValue::Special(form) => {
                ast = special_form!(form, list, eval, env, stash_env);
            }
            LispValue::Object(o) => match o.deref() {
                ObjectValue::BuiltinFunc(f) => break (f.body)(list, env),
                ObjectValue::Func(f) => {
                    let (new_ast, new_env) = expand_fn(f, list, env)?;
                    ast = new_ast;
                    env = stash_env(new_env);
                }
                x => break Err(LispError::InvalidDataType("function", x.type_of())),
            },
            x => break Err(LispError::InvalidDataType("function", x.type_of())),
        }
    })();
    // Release those environments we've been holding on to, and release the lock
    // we've got, if any.
    if let Some((_, lock)) = locks_to_drop.last_mut() {
        unsafe { ManuallyDrop::drop(lock) };
    }
    locks_to_drop.into_iter().for_each(|(arc_ptr, _)| unsafe {
        drop(Arc::from_raw(arc_ptr));
    });
    // If `stash_env` unlocked an outer write-lock, a new lock needs to be
    // acquired before returning (otherwise, the write-lock that was NOT passed
    // to this function will stop working, causing UB [and making an error
    // that's a pain to track down with no compiler aid]).
    if relock {
        std::mem::forget(env_copy.deref().write());
    }
    out
}
