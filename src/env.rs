use std::sync::{Arc, RwLock};
use std::hash;
use std::ops::{Deref, DerefMut};
use im::HashMap;
use lazy_static::lazy_static;
use string_interner::{StringInterner, DefaultSymbol};
use owning_ref::OwningRef;
use crate::LispValue;
use crate::builtins;
use crate::util::LispBuiltinFunc;

pub type LispSymbol = DefaultSymbol;

#[derive(Clone, Debug)]
struct InnerEnv {
    data: HashMap<LispSymbol, LispValue>,
    global: Option<LispEnv>,
    enclosing: Option<LispEnv>,
    constant: bool,
}

#[derive(Clone, Debug)]
pub struct LispClosure(LispEnv);
impl LispClosure {
    pub fn make_env(&self, args: &[(LispSymbol, LispValue)]) -> LispEnv {
        let enclosing = self.0.clone();
        let global = enclosing.global();
        let inner = InnerEnv {
            data: args.into(),
            enclosing: Some(enclosing),
            global: Some(global),
            constant: false,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
    pub fn make_macro_env(&self, args: &[(LispSymbol, LispValue)], surrounding: &LispEnv) -> LispEnv {
        let enclosing = self.0.union(surrounding);
        let global = enclosing.global();
        let inner = InnerEnv {
            data: args.into(),
            enclosing: Some(enclosing.union(surrounding)),
            global: Some(global),
            constant: false,
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
    static ref INTERNER: RwLock<StringInterner> = {
        RwLock::new(StringInterner::new())
    };

    static ref BUILTIN_NO_IO_ENV: LispEnv = {
        let inner = InnerEnv {
            data: builtins::BUILTINS_NO_IO.clone(),
            enclosing: None,
            global: None,
            constant: true,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    };
}

#[cfg(feature = "io-stdlib")]
lazy_static! {
    static ref BUILTIN_ENV: LispEnv = {
        let inner = InnerEnv {
            data: builtins::BUILTINS.clone(),
            enclosing: None,
            global: None,
            constant: true,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    };
}

impl LispEnv {
    #[cfg(feature = "io-stdlib")]
    pub fn new_stdlib() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: Some(BUILTIN_ENV.clone()),
            global: None,
            constant: false,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
    pub fn new_stdlib_protected() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: Some(BUILTIN_NO_IO_ENV.clone()),
            global: None,
            constant: false,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
    pub fn new_nested(&self) -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: Some(self.clone()),
            global: Some(self.global()),
            constant: false,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }

    fn map(&self) -> impl Deref<Target = HashMap<LispSymbol, LispValue>> + '_ {
        let lock = self.0.read().unwrap();
        let or = OwningRef::new(lock);
        or.map(|x| &x.data)
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

    pub(crate) fn interner_mut() -> impl DerefMut<Target = StringInterner> {
        INTERNER.write().unwrap()
    }
    pub fn symbol_for(s: &str) -> LispSymbol {
        let mut interner = INTERNER.write().unwrap();
        interner.get_or_intern(s)
    }
    pub fn symbol_for_static(s: &'static str) -> LispSymbol {
        let mut interner = INTERNER.write().unwrap();
        interner.get_or_intern_static(s)
    }
    pub fn symbol_string(sym: LispSymbol) -> Option<&'static str> {
        let interner = INTERNER.read().unwrap();
        unsafe { std::mem::transmute(interner.resolve(sym)) }
    }

    pub fn get(&self, sym: LispSymbol) -> Option<LispValue> {
        let mut env = Some(self.clone());
        while let Some(inner) = env {
            let lock = inner.0.read().unwrap();
            if let Some(val) = lock.data.get(&sym) {
                return Some(val.clone());
            } else {
                env = lock.enclosing.clone();
            }
        }
        None
    }
    pub fn set(&mut self, sym: LispSymbol, val: LispValue) -> bool {
        let lock = self.0.read().unwrap();
        if lock.constant && lock.data.contains_key(&sym) {
            false
        } else {
            drop(lock);
            let mut lock = self.0.write().unwrap();
            lock.data.insert(sym, val);
            true
        }
    }
    pub fn get_by_str(&self, key: &str) -> Option<LispValue> {
        let sym = Self::symbol_for(key);
        self.get(sym)
    }
    pub fn set_by_str(&mut self, key: &str, val: LispValue) -> bool {
        let sym = Self::symbol_for(key);
        self.set(sym, val)
    }
    pub fn bind_func(&mut self, name: &'static str, f: LispBuiltinFunc) {
        let sym = Self::symbol_for_static(name);
        let mut lock = self.0.write().unwrap();
        let val = LispValue::BuiltinFunc { name, f };
        lock.data.insert(sym, val);
    }
    pub fn union(&self, other: &LispEnv) -> LispEnv {
        let lock = self.0.read().unwrap();
        let reader = other.0.read().unwrap();
        let inner = InnerEnv {
            data: lock.data.clone().union(reader.data.clone()),
            enclosing: lock.enclosing.clone(),
            global: lock.global.clone(),
            constant: lock.constant,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
}

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        impl Default for LispEnv {
            fn default() -> Self {
                Self::new_stdlib()
            }
        }
    } else {
        impl Default for LispEnv {
            fn default() -> Self {
                Self::new_stdlib_protected()
            }
        }
    }
}
