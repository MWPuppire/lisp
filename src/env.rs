use std::sync::{Arc, RwLock};
use std::hash;
use im::{HashMap, Vector};
use lazy_static::lazy_static;
use crate::{LispValue, Result, builtins::BUILTINS};

#[derive(Clone, Debug)]
struct InnerEnv {
    data: HashMap<String, LispValue>,
    global: Option<LispEnv>,
    enclosing: Option<LispEnv>,
}

#[derive(Clone, Debug)]
pub struct LispClosure(LispEnv);
impl LispClosure {
    pub fn make_env(&self, args: &[(String, LispValue)]) -> LispEnv {
        let enclosing = self.0.clone();
        let global = enclosing.global();
        let inner = InnerEnv {
            data: args.into(),
            enclosing: Some(enclosing),
            global: Some(global),
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
}
impl hash::Hash for LispClosure {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.0.map().hash(state)
    }
}
impl std::cmp::PartialEq for LispClosure {
    fn eq(&self, other: &LispClosure) -> bool {
        Arc::ptr_eq(&self.0.0, &other.0.0)
    }
}

#[derive(Clone, Debug)]
pub struct LispEnv(Arc<RwLock<InnerEnv>>);

lazy_static! {
    static ref BUILTIN_ENV: LispEnv = {
        let inner = InnerEnv {
            data: BUILTINS.clone(),
            enclosing: None,
            global: None,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    };
}

impl LispEnv {
    pub fn new_empty() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: None,
            global: None,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
    pub fn new_stdlib() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: Some(BUILTIN_ENV.clone()),
            global: None,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
    pub fn new_nested(&self) -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: Some(self.clone()),
            global: Some(self.global()),
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

    pub fn global(&self) -> LispEnv {
        let lock = self.0.read().unwrap();
        if let Some(env) = &lock.global {
            env.clone()
        } else {
            self.clone()
        }
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
    pub fn bind_func(&mut self, name: &'static str, f: fn(Vector<LispValue>, LispEnv) -> Result<(LispValue, LispEnv, bool)>) {
        let mut lock = self.0.write().unwrap();
        let val = LispValue::BuiltinFunc { name, f };
        lock.data.insert(name.to_owned(), val);
    }
}
