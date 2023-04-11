use im::HashMap;
use crate::{LispValue, Result, builtins::BUILTINS};

#[derive(Clone)]
pub struct LispEnv {
    data: HashMap<String, LispValue>,
    enclosing: HashMap<String, LispValue>,
}

#[derive(Clone, Debug, Hash)]
pub struct LispClosure {
    data: HashMap<String, LispValue>,
}
impl LispClosure {
    pub fn make_env(&self, args: &[(String, LispValue)]) -> LispEnv {
        LispEnv {
            data: args.into(),
            enclosing: self.data.clone(),
        }
    }
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
    pub fn new_nested(&self) -> Self {
        LispEnv {
            data: HashMap::new(),
            enclosing: self.data.clone().union(self.enclosing.clone()),
        }
    }

    pub fn make_closure(&self) -> LispClosure {
        LispClosure {
            data: self.data.clone().union(self.enclosing.clone()),
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
            self.data.insert(key, val);
            true
        }
    }
    pub fn set(&mut self, key: String, val: LispValue) -> bool {
        // can't assign to an outer value
        self.data.insert(key, val);
        true
    }
    pub fn bind_func(&mut self, name: &'static str, f: fn(&[LispValue], &mut LispEnv) -> Result<LispValue>) {
        let val = LispValue::BuiltinFunc { name, f: crate::util::ExternLispFunc(f) };
        self.data.insert(name.to_owned(), val);
    }
}
