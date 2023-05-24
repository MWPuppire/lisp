use std::sync::Arc;
use std::ops::Deref;
use im::vector;
use crate::{LispValue, LispEnv, LispError, Result};
use crate::util::{LispSpecialForm, ObjectValue, expect};

pub(crate) const UNQUOTE: LispValue = LispValue::Special(LispSpecialForm::Unquote);
pub(crate) const SPLICE_UNQUOTE: LispValue = LispValue::Special(LispSpecialForm::SpliceUnquote);
pub(crate) const CATCH: LispValue = LispValue::Special(LispSpecialForm::Catch);

pub(crate) fn inner_quasiquote(arg: LispValue, eval: fn(LispValue, &mut LispEnv) -> Result<LispValue>, env: &mut LispEnv) -> Result<(LispValue, bool)> {
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(l) => {
                if l.is_empty() {
                    return Ok((LispValue::Object(o.clone()), false));
                }
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let ObjectValue::List(mut l) = cloned else { unreachable!() };
                // established non-empty just prior
                if l[0] == UNQUOTE {
                    expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                    Ok((eval(l.pop_back().unwrap(), env)?, false))
                } else if l[0] == SPLICE_UNQUOTE {
                    expect!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                    Ok((eval(l.pop_back().unwrap(), env)?, true))
                } else {
                    let mut out = vector![];
                    for val in l.into_iter() {
                        let (new_val, inplace) = inner_quasiquote(val, eval, env)?;
                        if inplace {
                            out.extend(new_val.try_into_iter()?);
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

macro_rules! special_form {
    ($form:expr, $list:expr, $eval:ident, $env:ident, $stash_env:expr, $outer:lifetime) => {
        match $form {
            LispSpecialForm::Def => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len(),
                ));
                if let LispValue::Special(form) = &$list[0] {
                    break Err(LispError::CannotRedefineSpecialForm(*form));
                }
                let name = $list.pop_front().unwrap().try_into()?;
                let val = $eval($list.pop_back().unwrap(), &mut $env)?;
                $env.set(name, val.clone());
                break Ok(val);
            },
            LispSpecialForm::Defmacro => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len(),
                ));
                if let LispValue::Special(form) = &$list[0] {
                    break Err(LispError::CannotRedefineSpecialForm(*form));
                }
                let name = $list.pop_front().unwrap().try_into()?;
                let val = $eval($list.pop_back().unwrap(), &mut $env)?;
                let val = match val {
                    LispValue::Object(o) => match o.deref() {
                        ObjectValue::Func(f) => LispValue::Object(Arc::new(
                            ObjectValue::Macro(f.clone())
                        )),
                        _ => LispValue::Object(o.clone()),
                    },
                    x => x,
                };
                $env.set(name, val.clone());
                break Ok(val);
            },
            LispSpecialForm::Let => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len(),
                ));
                let new_env = $env.new_nested();
                let mut decls = $list.pop_front().unwrap().try_into_iter()?;
                $env = $stash_env(new_env);
                while let Some(name) = decls.next() {
                    let name = name.try_into()?;
                    let Some(val_expr) = decls.next() else {
                        break $outer Err(LispError::MissingBinding)
                    };
                    let val = $eval(val_expr, $env)?;
                    $env.set(name, val);
                }
                // continue with the `let` body, but in the new environment
                /*
                let idx = $to_drop.len();
                $to_drop.push((new_env, lock));
                $env = $to_drop[idx].1.deref_mut();
                */
                $list.pop_front().unwrap()
            },
            LispSpecialForm::Quote => {
                if let Some(quoted) = $list.pop_front() {
                    break Ok(quoted);
                } else {
                    break Err(LispError::IncorrectArguments(1, 0));
                }
            },
            LispSpecialForm::Quasiquote => {
                let Some(front) = $list.pop_front() else {
                    break Err(LispError::IncorrectArguments(1, 0));
                };
                match front {
                    LispValue::Object(o) => match o.deref() {
                        ObjectValue::List(list) => {
                            if list.is_empty() {
                                break Ok(LispValue::Object(o.clone()));
                            }
                            let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                            let ObjectValue::List(mut list) = cloned else {
                                unreachable!()
                            };
                            if list[0] == $crate::specials::UNQUOTE || list[0] == $crate::specials::SPLICE_UNQUOTE {
                                expect!(
                                    list.len() == 2,
                                    LispError::IncorrectArguments(
                                        1,
                                        list.len() - 1,
                                    )
                                );
                                list.pop_back().unwrap()
                            } else {
                                let mut out = Vector::new();
                                for val in list {
                                    let (new_val, inplace) = $crate::specials::inner_quasiquote(
                                        val,
                                        $eval,
                                        &mut $env
                                    )?;
                                    if inplace {
                                        let mut iter = new_val.try_into_iter()?;
                                        out.extend(&mut iter);
                                    } else {
                                        out.push_back(new_val);
                                    }
                                }
                                break Ok(LispValue::list_from(out));
                            }
                        }
                        _ => break Ok(LispValue::Object(o.clone())),
                    },
                    x => break Ok(x),
                }
            },
            LispSpecialForm::Unquote | LispSpecialForm::SpliceUnquote => {
                break Err(LispError::OnlyInQuasiquote);
            },
            LispSpecialForm::Macroexpand => {
                if let Some(quoted) = $list.pop_front() {
                    break Ok(expand_macros(quoted, &mut $env)?.0);
                } else {
                    break Err(LispError::IncorrectArguments(1, 0));
                }
            },
            LispSpecialForm::Try => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len(),
                ));
                let catch = $list.pop_back().unwrap();
                let Ok(mut catch) = catch.try_into_iter() else {
                    break Err(LispError::TryNoCatch);
                };
                expect!(catch.len() == 3, LispError::IncorrectArguments(
                    2,
                    catch.len() - 1,
                ));
                if catch.next().unwrap() != $crate::specials::CATCH {
                    break Err(LispError::TryNoCatch);
                }
                match $eval($list.pop_front().unwrap(), &mut $env) {
                    Ok(x) => break Ok(x),
                    Err(err) => {
                        let err_name = catch.next().unwrap().try_into()?;
                        let caught_env = $env.new_nested();
                        let mut lock = caught_env.write();
                        if let LispError::UncaughtException(exc) = err {
                            lock.set(err_name, exc);
                        } else {
                            let s = err.to_string();
                            lock.set(err_name, s.into());
                        }
                        // run the `catch*` body in the new environment
                        /*
                        let idx = $to_drop.len();
                        $to_drop.push((caught_env, lock));
                        $env = $to_drop[idx].1.deref_mut();
                        */
                        drop(lock);
                        $env = $stash_env(caught_env);
                        catch.next().unwrap()
                    }
                }
            },
            LispSpecialForm::Catch => {
                break Err(LispError::OnlyInTry);
            },
            LispSpecialForm::Do => {
                let Some(tail) = $list.pop_back() else {
                    break Err(LispError::IncorrectArguments(1, 0));
                };
                for val in $list.into_iter() {
                    $eval(val, &mut $env)?;
                }
                tail
            },
            LispSpecialForm::If => {
                expect!(
                    $list.len() > 1 && $list.len() < 4,
                    LispError::IncorrectArguments(2, $list.len())
                );
                let pred = $list.pop_front().unwrap();
                let pred = $eval(pred, &mut $env)?.truthiness();
                if pred {
                    $list.pop_front().unwrap()
                } else if $list.len() > 1 {
                    $list.pop_back().unwrap()
                } else {
                    break Ok(LispValue::Nil);
                }
            },
            LispSpecialForm::Fn => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len()
                ));
                // TODO I don't especially like this, but I'm not sure what the
                // best approach would be, since `fn*` needs to support both
                // vector and list arguments, by Mal's spec
                let args_list: Vec<LispValue> = $list.pop_front().unwrap().try_into_iter()?.collect();
                let variadic = args_list.last().map(|last| {
                    matches!(last, LispValue::VariadicSymbol(_))
                }).unwrap_or(false);
                let args = args_list.into_iter()
                    .map(|x| x.try_into())
                    .collect::<Result<Vec<LispSymbol>>>()?;
                let body = $list.pop_front().unwrap();
                let closure = $env.make_closure();
                let inner = ObjectValue::Func(LispFunc {
                    args,
                    body,
                    closure,
                    variadic,
                });
                break Ok(LispValue::Object(Arc::new(inner)));
            },
            LispSpecialForm::Deref => {
                if let Some(atom) = $list.pop_front() {
                    let atom: Arc<RwLock<LispValue>> = $eval(atom, &mut $env)?.try_into()?;
                    let out = atom.read().clone();
                    break Ok(out);
                } else {
                    break Err(LispError::IncorrectArguments(1, 0));
                }
            },
            LispSpecialForm::Eval => {
                let Some(expr) = $list.pop_front() else {
                    break Err(LispError::IncorrectArguments(1, 0));
                };
                let global = $env.global();
                /*
                let lock = global.write();
                let idx = $to_drop.len();
                $to_drop.push((global, lock));
                $env = $to_drop[idx].1.deref_mut();
                */
                $env = $stash_env(global);
                $eval(expr, $env)?
            },
            LispSpecialForm::Apply => {
                expect!($list.len() > 1, LispError::IncorrectArguments(
                    2, $list.len()
                ));
                let f = $eval($list.pop_front().unwrap(), &mut $env)?;
                let mut args = $eval($list.pop_back().unwrap(), &mut $env)?.try_into_iter()?;
                let mut full_list = vector![f];
                full_list.append($list);
                full_list.extend(&mut args);
                LispValue::list_from(full_list)
            },
            LispSpecialForm::Cond => {
                expect!($list.len() & 1 == 0, LispError::OddCondArguments);
                // return `nil` if none of the conditions ends up true
                let mut out = LispValue::Nil;
                while let Some(pred) = $list.pop_front() {
                    let then = $list.pop_front().unwrap();
                    if $eval(pred, &mut $env)?.truthiness() {
                        out = then;
                        break;
                    }
                }
                out
            },
        }
    }
}

pub(crate) use special_form;
