use std::sync::Arc;
use std::ops::Deref;
use im::vector;
use crate::{LispValue, LispEnv, LispError, Result};
use crate::util::{LispSpecialForm, ObjectValue, expect};

pub(crate) const UNQUOTE: LispValue = LispValue::Special(LispSpecialForm::Unquote);
pub(crate) const SPLICE_UNQUOTE: LispValue = LispValue::Special(LispSpecialForm::SpliceUnquote);
pub(crate) const CATCH: LispValue = LispValue::Special(LispSpecialForm::Catch);

pub(crate) fn inner_quasiquote(arg: LispValue, eval: fn(LispValue, &LispEnv) -> Result<LispValue>, env: &LispEnv) -> Result<(LispValue, bool)> {
    match arg {
        LispValue::Object(o) => match o.deref() {
            ObjectValue::List(l) => {
                if l.is_empty() {
                    return Ok((LispValue::Object(o.clone()), false));
                }
                let ObjectValue::List(mut l) = Arc::unwrap_or_clone(o) else { unreachable!() };
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

macro_rules! special_form {
    ($form:expr, $list:expr, $eval:ident, $env:ident) => {
        match $form {
            LispSpecialForm::Def => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len(),
                ));
                if let LispValue::Special(form) = &$list[0] {
                    return Err(LispError::CannotRedefineSpecialForm(*form));
                }
                let name = $list[0].expect_symbol()?;
                let val = $eval($list.pop_back().unwrap(), &$env)?;
                $env.set(name, val.clone());
                return Ok(val);
            },
            LispSpecialForm::Defmacro => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len(),
                ));
                if let LispValue::Special(form) = &$list[0] {
                    return Err(LispError::CannotRedefineSpecialForm(*form));
                }
                let name = $list[0].expect_symbol()?;
                let val = $eval($list.pop_back().unwrap(), &$env)?;
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
                return Ok(val);
            },
            LispSpecialForm::Let => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len(),
                ));
                let new_env = $env.new_nested();
                let decls = $list.pop_front().unwrap().into_list()?;
                let mut decl_iter = decls.into_iter();
                while let Some(name) = decl_iter.next() {
                    let name = name.expect_symbol()?;
                    let Some(val_expr) = decl_iter.next() else {
                        return Err(LispError::MissingBinding);
                    };
                    let val = $eval(val_expr, &new_env)?;
                    new_env.set(name, val);
                }
                // continue with the `let` body, but in the new environment
                $env = new_env;
                $list.pop_front().unwrap()
            },
            LispSpecialForm::Quote => {
                if let Some(quoted) = $list.pop_front() {
                    return Ok(quoted);
                } else {
                    return Err(LispError::IncorrectArguments(1, 0));
                }
            },
            LispSpecialForm::Quasiquote => {
                let Some(front) = $list.pop_front() else {
                    return Err(LispError::IncorrectArguments(1, 0));
                };
                match front {
                    LispValue::Object(o) => match o.deref() {
                        ObjectValue::List(list) => {
                            if list.is_empty() {
                                return Ok(LispValue::Object(o.clone()));
                            }
                            let cloned = Arc::unwrap_or_clone(o);
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
                                        &$env
                                    )?;
                                    if inplace {
                                        out.append(new_val.into_list()?);
                                    } else {
                                        out.push_back(new_val);
                                    }
                                }
                                return Ok(LispValue::list_from(out));
                            }
                        }
                        _ => return Ok(LispValue::Object(o.clone())),
                    },
                    x => return Ok(x),
                }
            },
            LispSpecialForm::Unquote | LispSpecialForm::SpliceUnquote => {
                return Err(LispError::OnlyInQuasiquote);
            },
            LispSpecialForm::Macroexpand => {
                if let Some(quoted) = $list.pop_front() {
                    return expand_macros(quoted, &$env);
                } else {
                    return Err(LispError::IncorrectArguments(1, 0));
                }
            },
            LispSpecialForm::Try => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len(),
                ));
                let catch = $list.pop_back().unwrap();
                let Ok(mut catch) = catch.into_list() else {
                    return Err(LispError::TryNoCatch);
                };
                expect!(catch.len() == 3, LispError::IncorrectArguments(
                    2,
                    catch.len() - 1,
                ));
                if catch.pop_front().unwrap() != $crate::specials::CATCH {
                    return Err(LispError::TryNoCatch);
                }
                match $eval($list.pop_front().unwrap(), &$env) {
                    Ok(x) => return Ok(x),
                    Err(err) => {
                        let err_name = catch.pop_front().unwrap()
                            .expect_symbol()?;
                        let caught_env = $env.new_nested();
                        if let LispError::UncaughtException(exc) = err {
                            caught_env.set(err_name, exc);
                        } else {
                            let s = err.to_string();
                            caught_env.set(err_name, s.into());
                        }
                        // run the `catch*` body in the new environment
                        $env = caught_env;
                        catch.pop_front().unwrap()
                    }
                }
            },
            LispSpecialForm::Catch => {
                return Err(LispError::OnlyInTry);
            },
            LispSpecialForm::Do => {
                let Some(tail) = $list.pop_back() else {
                    return Err(LispError::IncorrectArguments(1, 0));
                };
                for val in $list.into_iter() {
                    $eval(val, &$env)?;
                }
                tail
            },
            LispSpecialForm::If => {
                expect!(
                    $list.len() > 1 && $list.len() < 4,
                    LispError::IncorrectArguments(2, $list.len())
                );
                let pred = $list.pop_front().unwrap();
                let pred = $eval(pred, &$env)?.truthiness();
                if pred {
                    $list.pop_front().unwrap()
                } else if $list.len() > 1 {
                    $list.pop_back().unwrap()
                } else {
                    return Ok(LispValue::Nil);
                }
            },
            LispSpecialForm::Fn => {
                expect!($list.len() == 2, LispError::IncorrectArguments(
                    2,
                    $list.len()
                ));
                let args_list = $list.pop_front().unwrap().into_list()?;
                let variadic = args_list.last().map(|last| {
                    matches!(last, LispValue::VariadicSymbol(_))
                }).unwrap_or(false);
                let args = args_list.into_iter()
                    .map(|x| x.expect_symbol())
                    .collect::<Result<Vec<LispSymbol>>>()?;
                let body = $list.pop_front().unwrap();
                let closure = $env.make_closure();
                let inner = ObjectValue::Func(LispFunc {
                    args,
                    body,
                    closure,
                    variadic,
                });
                return Ok(LispValue::Object(Arc::new(inner)));
            },
            LispSpecialForm::Deref => {
                if let Some(atom) = $list.pop_front() {
                    let atom = $eval(atom, &$env)?.into_atom()?;
                    let out = atom.read().clone();
                    return Ok(out);
                } else {
                    return Err(LispError::IncorrectArguments(1, 0));
                }
            },
            LispSpecialForm::Eval => {
                let Some(expr) = $list.pop_front() else {
                    break Err(LispError::IncorrectArguments(1, 0));
                };
                $env = $env.global();
                $eval(expr, &$env)?
            },
            LispSpecialForm::Apply => {
                expect!($list.len() > 1, LispError::IncorrectArguments(
                    2, $list.len()
                ));
                let f = $eval($list.pop_front().unwrap(), &$env)?;
                let args = $eval($list.pop_back().unwrap(), &$env)?.into_list()?;
                let mut full_list = vector![f];
                full_list.append($list);
                full_list.append(args);
                LispValue::list_from(full_list)
            },
            LispSpecialForm::Cond => {
                expect!($list.len() & 1 == 0, LispError::OddCondArguments);
                // return `nil` if none of the conditions ends up true
                let mut out = LispValue::Nil;
                while let Some(pred) = $list.pop_front() {
                    let then = $list.pop_front().unwrap();
                    if $eval(pred, &$env)?.truthiness() {
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
