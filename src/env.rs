use std::fmt;
use im::HashMap;
use crate::{LispValue, Result, builtins::BUILTINS};

#[derive(Clone)]
pub struct LispEnv {
    data: HashMap<String, LispValue>,
    enclosing: HashMap<String, LispValue>,
}

impl LispEnv {
    pub fn new_empty() -> Self {
        LispEnv {
            data: HashMap::new(),
            enclosing: HashMap::new(),
        }
    }
    pub fn new_stdlib() -> Self {
        LispEnv {
            data: HashMap::new(),
            enclosing: BUILTINS.clone(),
        }
    }
    pub fn closure(&self) -> Self {
        LispEnv {
            data: HashMap::new(),
            enclosing: self.data.clone().union(self.enclosing.clone()),
        }
    }
    fn new_altered(&self, altered: HashMap<String, LispValue>) -> Self {
        LispEnv {
            data: altered,
            enclosing: self.enclosing.clone()
        }
    }

    pub fn get(&self, key: &str) -> Option<LispValue> {
        if let Some(val) = self.data.get(key) {
            Some(val.clone())
        } else {
            self.enclosing.get(key).map(|x| x.clone())
        }
    }
    pub fn insert(&mut self, key: String, val: LispValue) -> bool {
        if self.data.contains_key(&key) {
            false
        } else {
            self.set(key, val);
            true
        }
    }
    pub fn set(&mut self, key: String, val: LispValue) {
        // can't assign to an outer value
        self.data = self.data.alter(move |_| Some(val), key);
    }
    pub fn bind_func(&mut self, name: &'static str, f: fn(&[LispValue], &mut LispEnv) -> Result<LispValue>) {
        let val = LispValue::BuiltinFunc { name, f: crate::util::LispFunc(f) };
        self.data = self.data.alter(move |_| Some(val), name.to_owned());
    }
}

impl fmt::Debug for LispEnv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let map = &self.data;
        f.debug_map().entries(map.iter()).finish()
    }
}
