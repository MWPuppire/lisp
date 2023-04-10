use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use crate::{LispValue, Result, builtins::create_builtins};

struct InnerEnv {
    data: HashMap<String, LispValue>,
    enclosing: Option<LispEnv>,
}

#[derive(Clone)]
pub struct LispEnv {
    inner: Rc<RefCell<InnerEnv>>,
}

impl LispEnv {
    pub fn new_empty() -> LispEnv {
        LispEnv {
            inner: Rc::new(RefCell::new(InnerEnv {
                data: HashMap::new(),
                enclosing: None,
            })),
        }
    }
    pub fn new_stdlib() -> LispEnv {
        let builtins = LispEnv {
            inner: Rc::new(RefCell::new(InnerEnv {
                data: create_builtins(),
                enclosing: None,
            })),
        };
        LispEnv {
            inner: Rc::new(RefCell::new(InnerEnv {
                data: HashMap::new(),
                enclosing: Some(builtins),
            })),
        }
    }
    pub fn closure(&self) -> LispEnv {
        LispEnv {
            inner: Rc::new(RefCell::new(InnerEnv {
                data: HashMap::new(),
                enclosing: Some(self.clone()),
            })),
        }
    }
    pub fn get(&self, key: &str) -> Option<LispValue> {
        // since `find()` succeeded, the `get()` must resolve successfully,
        // so `unwrap_unchecked` is safe
        self.find(key).map(|env| unsafe { env.borrow().data.get(key).unwrap_unchecked().clone() })
    }
    pub fn set(&mut self, key: String, val: LispValue) {
        // can't assign to an outer value
        self.inner.borrow_mut().data.insert(key, val);
    }
    pub fn bind_func(&mut self, name: &str, f: fn(&[LispValue], &mut LispEnv) -> Result<LispValue>) {
        let val = LispValue::BuiltinFunc(f);
        self.inner.borrow_mut().data.insert(name.to_owned(), val);
    }
    fn find(&self, key: &str) -> Option<Rc<RefCell<InnerEnv>>> {
        let mut env = Some(self.inner.clone());
        while let Some(inner) = env {
            if inner.borrow().data.contains_key(key) {
                return Some(inner);
            } else {
                env = inner.borrow().enclosing.clone().map(|x| x.inner);
            }
        }
        None
    }
}
