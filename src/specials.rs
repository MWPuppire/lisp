use crate::util::assert_or_err;
use crate::value::{InnerObjectValue, InnerValue, LispSpecialForm};
use crate::{LispEnv, LispError, LispValue, Result};
use im::vector;
use std::sync::Arc;

pub(crate) const UNQUOTE: LispValue = LispValue::new(InnerValue::Special {
    form: LispSpecialForm::Unquote,
    quoted: false,
});
pub(crate) const SPLICE_UNQUOTE: LispValue = LispValue::new(InnerValue::Special {
    form: LispSpecialForm::SpliceUnquote,
    quoted: false,
});
pub(crate) const CATCH: LispValue = LispValue::new(InnerValue::Special {
    form: LispSpecialForm::Catch,
    quoted: false,
});

pub(crate) fn inner_quasiquote(
    arg: LispValue,
    eval: fn(LispValue, &LispEnv) -> Result<LispValue>,
    env: &LispEnv,
) -> Result<(LispValue, bool)> {
    match arg.val {
        InnerValue::Object(o) => match &o.val {
            InnerObjectValue::List(l) => {
                if l.is_empty() {
                    return Ok((LispValue::new(InnerValue::Object(o.clone())), false));
                }
                let cloned = Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                let InnerObjectValue::List(mut l) = cloned.val else { unreachable!() };
                // established non-empty just prior
                if l[0] == UNQUOTE {
                    assert_or_err!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
                    Ok((eval(l.pop_back().unwrap(), env)?, false))
                } else if l[0] == SPLICE_UNQUOTE {
                    assert_or_err!(l.len() == 2, LispError::IncorrectArguments(1, l.len() - 1));
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
                    Ok((out.into_iter().collect(), false))
                }
            }
            _ => Ok((LispValue::new(InnerValue::Object(o.clone())), false)),
        },
        x => Ok((LispValue::new(x), false)),
    }
}

macro_rules! special_form {
    ($form:expr, $list:expr, $eval:ident, $env:ident) => {
        match $form {
            $crate::value::LispSpecialForm::Def => {
                $crate::util::assert_or_err!(
                    $list.len() == 2,
                    $crate::LispError::IncorrectArguments(2, $list.len(),)
                );
                if let $crate::value::InnerValue::Special { form, .. } = &$list[0].val {
                    break Err($crate::LispError::CannotRedefineSpecialForm(*form));
                }
                let name = $list.pop_front().unwrap().try_into()?;
                let val = $eval($list.pop_back().unwrap(), &$env)?;
                $env.set(name, val.clone());
                break Ok(val);
            }
            $crate::value::LispSpecialForm::Defmacro => {
                $crate::util::assert_or_err!(
                    $list.len() == 2,
                    $crate::LispError::IncorrectArguments(2, $list.len(),)
                );
                if let $crate::value::InnerValue::Special { form, .. } = &$list[0].val {
                    break Err($crate::LispError::CannotRedefineSpecialForm(*form));
                }
                let name = $list.pop_front().unwrap().try_into()?;
                let val = $eval($list.pop_back().unwrap(), &$env)?;
                let val = match val.val {
                    $crate::value::InnerValue::Object(o) => match &o.val {
                        $crate::value::InnerObjectValue::Func(f) => {
                            $crate::LispValue::new($crate::value::InnerValue::Object(std::sync::Arc::new($crate::value::ObjectValue {
                                val: $crate::value::InnerObjectValue::Macro(f.clone()),
                                meta: None,
                                quoted: false,
                            })))
                        }
                        _ => $crate::LispValue::new($crate::value::InnerValue::Object(o.clone())),
                    },
                    x => $crate::LispValue::new(x),
                };
                $env.set(name, val.clone());
                break Ok(val);
            }
            $crate::value::LispSpecialForm::Let => {
                $crate::util::assert_or_err!(
                    $list.len() == 2,
                    $crate::LispError::IncorrectArguments(2, $list.len(),)
                );
                let new_env = $env.new_nested();
                let mut decls = $list.pop_front().unwrap().try_into_iter()?;
                $env = new_env;
                while let Some(name) = decls.next() {
                    let name = name.try_into()?;
                    let Some(val_expr) = decls.next() else {
                                                        return Err($crate::LispError::MissingBinding)
                                                    };
                    let val = $eval(val_expr, &$env)?;
                    $env.set(name, val);
                }
                // continue with the `let` body, but in the new environment
                $list.pop_front().unwrap()
            }
            $crate::value::LispSpecialForm::Quote => {
                if let Some(quoted) = $list.pop_front() {
                    break Ok(quoted.quote());
                } else {
                    break Err($crate::LispError::IncorrectArguments(1, 0));
                }
            }
            $crate::value::LispSpecialForm::Quasiquote => {
                let Some(front) = $list.pop_front() else {
                                                    break Err($crate::LispError::IncorrectArguments(1, 0));
                                                };
                match front.val {
                    $crate::value::InnerValue::Object(o) => match &o.val {
                        $crate::value::InnerObjectValue::List(list) => {
                            if list.is_empty() {
                                break Ok($crate::LispValue::new($crate::value::InnerValue::Object(o.clone())));
                            }
                            let cloned = std::sync::Arc::try_unwrap(o).unwrap_or_else(|arc| (*arc).clone());
                            let $crate::value::InnerObjectValue::List(mut list) = cloned.val else {
                                                                unreachable!()
                                                            };
                            if list[0] == $crate::specials::UNQUOTE
                                || list[0] == $crate::specials::SPLICE_UNQUOTE
                            {
                                $crate::util::assert_or_err!(
                                    list.len() == 2,
                                    $crate::LispError::IncorrectArguments(1, list.len() - 1,)
                                );
                                list.pop_back().unwrap()
                            } else {
                                let mut out = Vector::new();
                                for val in list {
                                    let (new_val, inplace) =
                                        $crate::specials::inner_quasiquote(val, $eval, &$env)?;
                                    if inplace {
                                        let mut iter = new_val.try_into_iter()?;
                                        out.extend(&mut iter);
                                    } else {
                                        out.push_back(new_val);
                                    }
                                }
                                break Ok(out.into_iter().collect());
                            }
                        }
                        _ => break Ok($crate::LispValue::new($crate::value::InnerValue::Object(o.clone()))),
                    },
                    x => break Ok($crate::LispValue::new(x)),
                }
            }
            // handled specially in `quasiquote`'s body
            $crate::value::LispSpecialForm::Unquote | $crate::value::LispSpecialForm::SpliceUnquote => {
                break Err($crate::LispError::OnlyInQuasiquote);
            }
            $crate::value::LispSpecialForm::Macroexpand => {
                if let Some(quoted) = $list.pop_front() {
                    break expand_macros(quoted, &$env);
                } else {
                    break Err($crate::LispError::IncorrectArguments(1, 0));
                }
            }
            $crate::value::LispSpecialForm::Try => {
                $crate::util::assert_or_err!(
                    $list.len() == 2,
                    $crate::LispError::IncorrectArguments(2, $list.len(),)
                );
                let catch = $list.pop_back().unwrap();
                let Ok(mut catch) = catch.try_into_iter() else {
                                                    break Err($crate::LispError::TryNoCatch);
                                                };
                $crate::util::assert_or_err!(
                    catch.len() == 3,
                    $crate::LispError::IncorrectArguments(2, catch.len() - 1,)
                );
                if catch.next().unwrap() != $crate::specials::CATCH {
                    break Err($crate::LispError::TryNoCatch);
                }
                match $eval($list.pop_front().unwrap(), &$env) {
                    Ok(x) => break Ok(x),
                    Err(err) => {
                        let err_name = catch.next().unwrap().try_into()?;
                        let caught_env = $env.new_nested();
                        if let $crate::LispError::UncaughtException(exc) = err {
                            caught_env.set(err_name, exc);
                        } else {
                            let s = err.to_string();
                            caught_env.set(err_name, s.into());
                        }
                        // run the `catch*` body in the new environment
                        $env = caught_env;
                        catch.next().unwrap()
                    }
                }
            }
            // handled specially in `try*`'s body
            $crate::value::LispSpecialForm::Catch => {
                break Err($crate::LispError::OnlyInTry);
            }
            $crate::value::LispSpecialForm::Do => {
                let Some(tail) = $list.pop_back() else {
                                                    break Err($crate::LispError::IncorrectArguments(1, 0));
                                                };
                for val in $list.into_iter() {
                    $eval(val, &$env)?;
                }
                tail
            }
            $crate::value::LispSpecialForm::If => {
                $crate::util::assert_or_err!(
                    $list.len() > 1 && $list.len() < 4,
                    $crate::LispError::IncorrectArguments(2, $list.len())
                );
                let pred = $list.pop_front().unwrap();
                let pred = $eval(pred, &$env)?.truthiness();
                if pred {
                    $list.pop_front().unwrap()
                } else if $list.len() > 1 {
                    $list.pop_back().unwrap()
                } else {
                    break Ok($crate::LispValue::nil());
                }
            }
            $crate::value::LispSpecialForm::Fn => {
                $crate::util::assert_or_err!(
                    $list.len() == 2,
                    $crate::LispError::IncorrectArguments(2, $list.len())
                );
                // TODO I don't especially like this, but I'm not sure what the
                // best approach would be, since `fn*` needs to support both
                // vector and list arguments, by Mal's spec
                let args_list: Vec<$crate::LispValue> =
                    $list.pop_front().unwrap().try_into_iter()?.collect();
                let variadic = args_list
                    .last()
                    .map(|last| matches!(last.val, InnerValue::Symbol { variadic: true, .. }))
                    .unwrap_or(false);
                let args = args_list.into_iter().map(|x| x.try_into()).try_collect()?;
                let body = $list.pop_front().unwrap();
                let closure = $env.make_closure();
                let inner = $crate::value::InnerObjectValue::Func($crate::value::LispFunc {
                    args,
                    body,
                    closure,
                    variadic,
                });
                break Ok($crate::LispValue::new($crate::value::InnerValue::Object(std::sync::Arc::new($crate::value::ObjectValue {
                    val: inner,
                    meta: None,
                    quoted: false,
                }))));
            }
            // copy-pasted from `Fn`, modified to return a `Macro`
            $crate::value::LispSpecialForm::FnMacro => {
                $crate::util::assert_or_err!(
                    $list.len() == 2,
                    $crate::LispError::IncorrectArguments(2, $list.len())
                );
                let args_list: Vec<$crate::LispValue> =
                    $list.pop_front().unwrap().try_into_iter()?.collect();
                let variadic = args_list
                    .last()
                    .map(|last| matches!(last.val, InnerValue::Symbol { variadic: true, .. }))
                    .unwrap_or(false);
                let args = args_list.into_iter().map(|x| x.try_into()).try_collect()?;
                let body = $list.pop_front().unwrap();
                let closure = $env.make_closure();
                let inner = $crate::value::InnerObjectValue::Macro($crate::value::LispFunc {
                    args,
                    body,
                    closure,
                    variadic,
                });
                break Ok($crate::LispValue::new($crate::value::InnerValue::Object(std::sync::Arc::new($crate::value::ObjectValue {
                    val: inner,
                    meta: None,
                    quoted: false,
                }))));
            }
            $crate::value::LispSpecialForm::Deref => {
                if let Some(atom) = $list.pop_front() {
                    let atom: std::sync::Arc<parking_lot::RwLock<$crate::LispValue>> = $eval(atom, &$env)?.try_into()?;
                    let out = atom.read().clone();
                    break Ok(out);
                } else {
                    break Err($crate::LispError::IncorrectArguments(1, 0));
                }
            }
            $crate::value::LispSpecialForm::Eval => {
                let Some(expr) = $list.pop_front() else {
                                                    break Err($crate::LispError::IncorrectArguments(1, 0));
                                                };
                let global = $env.global();
                let out = $eval(expr, &$env)?;
                $env = global;
                out
            }
            $crate::value::LispSpecialForm::Apply => {
                $crate::util::assert_or_err!(
                    $list.len() > 1,
                    LispError::IncorrectArguments(2, $list.len())
                );
                let f = $eval($list.pop_front().unwrap(), &$env)?;
                let mut args = $eval($list.pop_back().unwrap(), &$env)?.try_into_iter()?;
                let mut full_list = im::vector![f];
                full_list.append($list);
                full_list.extend(&mut args);
                full_list.into_iter().collect()
            }
            $crate::value::LispSpecialForm::Cond => {
                $crate::util::assert_or_err!($list.len() & 1 == 0, $crate::LispError::OddCondArguments);
                // return `nil` if none of the conditions ends up true
                let mut out = $crate::LispValue::nil();
                while let Some(pred) = $list.pop_front() {
                    let then = $list.pop_front().unwrap();
                    if $eval(pred, &$env)?.truthiness() {
                        out = then;
                        break;
                    }
                }
                out
            }
        }
    };
}

pub(crate) use special_form;
