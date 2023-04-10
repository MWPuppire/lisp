use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::fmt;
use lazy_static::lazy_static;
use crate::{LispValue, Result, builtins::add_builtins};

struct InnerEnv {
    data: HashMap<String, LispValue>,
    enclosing: Option<LispEnv>,
}

#[derive(Clone)]
pub struct LispEnv {
    inner: Arc<RwLock<InnerEnv>>,
}

lazy_static! {
    static ref BUILTINS: LispEnv = {
        let mut m = LispEnv::new_empty();
        add_builtins(&mut m);
        m
    };
}

impl LispEnv {
    pub fn new_empty() -> LispEnv {
        LispEnv {
            inner: Arc::new(RwLock::new(InnerEnv {
                data: HashMap::new(),
                enclosing: None,
            })),
        }
    }
    pub fn new_stdlib() -> LispEnv {
        LispEnv {
            inner: Arc::new(RwLock::new(InnerEnv {
                data: HashMap::new(),
                enclosing: Some(BUILTINS.clone()),
            })),
        }
    }
    pub fn closure(&self) -> LispEnv {
        LispEnv {
            inner: Arc::new(RwLock::new(InnerEnv {
                data: HashMap::new(),
                enclosing: Some(self.clone()),
            })),
        }
    }
    pub fn get(&self, key: &str) -> Option<LispValue> {
        // since `find()` succeeded, the `get()` must resolve successfully,
        // so `unwrap_unchecked` is safe
        self.find(key).map(|env| unsafe { env.read().unwrap().data.get(key).unwrap_unchecked().clone() })
    }
    pub fn set(&mut self, key: String, val: LispValue) {
        // can't assign to an outer value
        self.inner.write().unwrap().data.insert(key, val);
    }
    pub fn bind_func(&mut self, name: &'static str, f: fn(&[LispValue], &mut LispEnv) -> Result<LispValue>) {
        let val = LispValue::BuiltinFunc { name, f: crate::util::LispFunc(f) };
        self.inner.write().unwrap().data.insert(name.to_owned(), val);
    }
    fn find(&self, key: &str) -> Option<Arc<RwLock<InnerEnv>>> {
        let mut env = Some(self.inner.clone());
        while let Some(inner) = env {
            if inner.read().unwrap().data.contains_key(key) {
                return Some(inner);
            } else {
                env = inner.read().unwrap().enclosing.clone().map(|x| x.inner);
            }
        }
        None
    }
}

impl fmt::Debug for LispEnv {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let map = &self.inner.read().unwrap().data;
        f.debug_map().entries(map.iter()).finish()
    }
}
