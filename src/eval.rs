use std::iter::zip;
use crate::{LispValue, LispError, env::LispEnv};

fn lookup_variable<'a>(val: String, env: &'a LispEnv) -> Result<&'a LispValue, LispError> {
    env.get(&val).ok_or(LispError::UndefinedVariable(val))
}

fn eval_list(head: &LispValue, rest: &[LispValue], env: &mut LispEnv) -> Result<LispValue, LispError> {
    match head {
        LispValue::Symbol(s) => {
            let val = lookup_variable(s.clone(), env)?.clone();
            eval_list(&val, rest, env)
        },
        LispValue::List(l) => {
            if l.len() == 0 {
                Err(LispError::InvalidDataType("function", "list"))
            } else {
                let inner_head = &l[0];
                eval_list(&eval_list(inner_head, &l[1..], env)?, rest, env)
            }
        },
        LispValue::BuiltinFunc(f) => f(rest, env),
        LispValue::String(_) => Err(LispError::InvalidDataType("function", "string")),
        LispValue::Number(_) => Err(LispError::InvalidDataType("function", "number")),
        LispValue::Bool(_) => Err(LispError::InvalidDataType("function", "bool")),
        LispValue::Nil => Err(LispError::InvalidDataType("function", "nil")),
        LispValue::Func { args, body } => {
            if rest.len() != args.len() {
                Err(LispError::IncorrectArguments(args.len(), rest.len()))
            } else {
                let mut in_args = Vec::with_capacity(rest.len());
                for val in rest.iter() {
                    in_args.push(eval(val, env)?);
                }
                let mut fn_env = env.new_nested();
                for (key, val) in zip(args, in_args) {
                    fn_env.set(key.to_owned(), val.clone());
                }
                eval(body, &mut fn_env)
            }
        }
    }
}

pub fn eval_to_number(value: &LispValue, env: &mut LispEnv) -> Result<f64, LispError> {
    match value {
        LispValue::Number(f) => Ok(*f),
        LispValue::Bool(_) => Err(LispError::InvalidDataType("number", "bool")),
        LispValue::String(_) => Err(LispError::InvalidDataType("number", "string")),
        LispValue::Nil => Err(LispError::InvalidDataType("number", "nil")),
        _ => eval_to_number(&eval(value, env)?, env)
    }
}

pub fn eval_to_bool(value: &LispValue, env: &mut LispEnv) -> Result<bool, LispError> {
    match value {
        LispValue::Number(_) => Err(LispError::InvalidDataType("bool", "number")),
        LispValue::Bool(b) => Ok(*b),
        LispValue::String(_) => Err(LispError::InvalidDataType("bool", "string")),
        LispValue::Nil => Err(LispError::InvalidDataType("bool", "nil")),
        _ => eval_to_bool(&eval(value, env)?, env)
    }
}

pub fn eval(value: &LispValue, env: &mut LispEnv) -> Result<LispValue, LispError> {
    match value {
        LispValue::Symbol(s) => {
            let val = lookup_variable(s.clone(), env)?.clone();
            eval(&val, env)
        },
        LispValue::List(l) => {
            if l.len() == 0 {
                Ok(LispValue::List(vec![]))
            } else {
                let head = &l[0];
                eval_list(head, &l[1..], env)
            }
        },
        LispValue::BuiltinFunc(f) => f(&[], env),
        LispValue::Func { args, body } => {
            if args.len() != 0 {
                Err(LispError::IncorrectArguments(args.len(), 0))
            } else {
                eval(body, &mut env.new_nested())
            }
        },
        _ => Ok(value.clone()),
    }
}
