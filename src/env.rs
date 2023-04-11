use std::collections::VecDeque;
use std::sync::{Arc, RwLock};
use std::hash;
use im::HashMap;
use lazy_static::lazy_static;
use crate::{LispValue, Result, builtins::BUILTINS};

#[derive(Clone, Debug)]
struct InnerEnv {
    data: HashMap<String, LispValue>,
    enclosing: Option<LispEnv>,
}

#[derive(Clone, Debug)]
pub struct LispClosure(LispEnv);
impl LispClosure {
    pub fn make_env(&self, args: &[(String, LispValue)]) -> LispEnv {
        let inner = InnerEnv {
            data: args.into(),
            enclosing: Some(self.0.clone()),
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
}
impl hash::Hash for LispClosure {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.map().hash(state)
    }
}

#[derive(Clone, Debug)]
pub struct LispEnv(Arc<RwLock<InnerEnv>>);

lazy_static! {
    static ref BUILTIN_ENV: LispEnv = {
        let inner = InnerEnv {
            data: BUILTINS.clone(),
            enclosing: None,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    };
}

impl LispEnv {
    pub fn new_empty() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: None,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
    pub fn new_stdlib() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: Some(BUILTIN_ENV.clone()),
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
    pub fn new_nested(&self) -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: Some(self.clone()),
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }

    fn map(&self) -> HashMap<String, LispValue> {
        let lock = self.0.read().unwrap();
        lock.data.clone()
    }

    pub fn make_closure(&self) -> LispClosure {
        LispClosure(self.clone())
    }
    pub fn get(&self, key: &str) -> Option<LispValue> {
        let mut env = Some(self.clone());
        while let Some(inner) = env {
            let lock = inner.0.read().unwrap();
            if let Some(val) = lock.data.get(key) {
                return Some(val.clone());
            } else {
                env = lock.enclosing.clone();
            }
        }
        None
    }
    pub fn insert(&mut self, key: String, val: LispValue) -> bool {
        let lock = self.0.read().unwrap();
        if lock.data.contains_key(&key) {
            false
        } else {
            drop(lock);
            let mut lock = self.0.write().unwrap();
            lock.data.insert(key, val);
            true
        }
    }
    pub fn set(&mut self, key: String, val: LispValue) -> bool {
        // can't assign to an outer value
        let mut lock = self.0.write().unwrap();
        lock.data.insert(key, val);
        true
    }
    pub fn bind_func(&mut self, name: &'static str, f: fn(VecDeque<LispValue>, &mut LispEnv) -> Result<LispValue>) {
        let mut lock = self.0.write().unwrap();
        let val = LispValue::BuiltinFunc { name, f: crate::util::ExternLispFunc(f) };
        lock.data.insert(name.to_owned(), val);
    }
}
