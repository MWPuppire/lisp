use std::collections::HashMap;
use std::collections::hash_map::Entry;
use lazy_static::lazy_static;
use crate::{LispValue, builtins::*};

lazy_static! {
    pub static ref BUILTIN_ENV: LispEnv<'static> = {
        let mut m = HashMap::new();
        m.insert("true".to_owned(), LispValue::Bool(true));
        m.insert("false".to_owned(), LispValue::Bool(false));
        m.insert("nil".to_owned(), LispValue::Nil);
        m.insert("+".to_owned(), LispValue::BuiltinFunc(lisp_plus));
        m.insert("-".to_owned(), LispValue::BuiltinFunc(lisp_minus));
        m.insert("*".to_owned(), LispValue::BuiltinFunc(lisp_times));
        m.insert("/".to_owned(), LispValue::BuiltinFunc(lisp_divide));
        m.insert("//".to_owned(), LispValue::BuiltinFunc(lisp_int_divide));
        m.insert("def!".to_owned(), LispValue::BuiltinFunc(lisp_def));
        m.insert("let*".to_owned(), LispValue::BuiltinFunc(lisp_let));
        m.insert("if".to_owned(), LispValue::BuiltinFunc(lisp_if));
        m.insert("do".to_owned(), LispValue::BuiltinFunc(lisp_do));
        m.insert("fn*".to_owned(), LispValue::BuiltinFunc(lisp_fn));
        LispEnv {
            data: m,
            outer: None,
        }
    };
}

#[derive(Clone)]
pub struct LispEnv<'a> {
    data: HashMap<String, LispValue>,
    outer: Option<&'a LispEnv<'a>>,
}
impl LispEnv<'_> {
    pub fn new_builtin() -> LispEnv<'static> {
        LispEnv {
            data: HashMap::new(),
            outer: Some(&BUILTIN_ENV),
        }
    }
    pub fn new_nested<'a>(&'a self) -> LispEnv<'a> {
        LispEnv {
            data: HashMap::new(),
            outer: Some(self),
        }
    }
}
impl<'a> LispEnv<'a> {
    pub fn get(&self, key: &str) -> Option<&LispValue> {
        self.find(key).map(|env| env.data.get(key)).flatten()
    }
    pub fn set(&mut self, key: String, val: LispValue) -> &LispValue {
        // can't assign to an outer value
        match self.data.entry(key) {
            Entry::Occupied(mut e) => {
                e.insert(val);
                &*e.into_mut()
            },
            Entry::Vacant(e) => {
                &*e.insert(val)
            }
        }
    }
    pub fn find(&self, key: &str) -> Option<&LispEnv> {
        if self.data.contains_key(key) {
            Some(self)
        } else if let Some(outer) = self.outer {
            outer.find(key)
        } else {
            None
        }
    }
}
