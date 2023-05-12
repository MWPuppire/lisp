use std::sync::{Arc, RwLock, Weak};
use std::hash;
use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
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
    global: Weak<RwLock<InnerEnv>>,
    enclosing: Option<LispEnv>,
    stdlib: &'static HashMap<LispSymbol, LispValue>,
}

#[derive(Clone, Debug)]
pub struct LispClosure(LispEnv);
impl LispClosure {
    pub fn make_env(&self, args: &[(LispSymbol, LispValue)]) -> LispEnv {
        let enclosing = self.0.clone();
        let lock = self.0.0.read().unwrap();
        let inner = InnerEnv {
            data: args.into(),
            enclosing: Some(enclosing),
            global: lock.global.clone(),
            stdlib: lock.stdlib,
        };
        LispEnv(Arc::new(RwLock::new(inner)))
    }
    pub fn make_macro_env(&self, args: &[(LispSymbol, LispValue)], surrounding: &LispEnv) -> LispEnv {
        let enclosing = self.0.clone();
        let lock = self.0.0.read().unwrap();
        let inner = InnerEnv {
            data: args.into(),
            enclosing: Some(enclosing.union(surrounding)),
            global: lock.global.clone(),
            stdlib: lock.stdlib,
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
}

// creates a `LispEnv` where `global` refers to `self` from an `InnerEnv`
unsafe fn assign_global_self(inner: InnerEnv) -> LispEnv {
    let mut boxed = Arc::new(RwLock::new(inner));
    let mut inner = NonNull::from(Arc::get_mut(&mut boxed).unwrap().get_mut().unwrap());
    let weak = Arc::downgrade(&boxed);
    inner.as_mut().global = weak;
    LispEnv(boxed)
}

impl LispEnv {
    #[cfg(feature = "io-stdlib")]
    pub fn new_stdlib() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: None,
            global: Weak::new(),
            stdlib: &builtins::BUILTINS,
        };
        unsafe { assign_global_self(inner) }
    }
    pub fn new_stdlib_protected() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: None,
            global: Weak::new(),
            stdlib: &builtins::BUILTINS_NO_IO,
        };
        unsafe { assign_global_self(inner) }
    }
    pub fn new_nested(&self) -> Self {
        let copy = self.clone();
        let lock = self.0.read().unwrap();
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: Some(copy),
            global: lock.global.clone(),
            stdlib: lock.stdlib,
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
        // if `self` still exists, the Arc chain must make `global` exist,
        // since `global` has to be connected by `enclosing` (or be the same
        // value as `self` in the case of the global environment)
        LispEnv(lock.global.upgrade().unwrap())
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
        let this = self.0.read().unwrap();
        let mut env = Some(self.clone());
        while let Some(inner) = env {
            let lock = inner.0.read().unwrap();
            if let Some(val) = lock.data.get(&sym) {
                return Some(val.clone());
            } else {
                env = lock.enclosing.clone();
            }
        }
        this.stdlib.get(&sym).map(LispValue::clone)
    }
    pub fn set(&mut self, sym: LispSymbol, val: LispValue) {
        let mut lock = self.0.write().unwrap();
        lock.data.insert(sym, val);
    }
    pub fn get_by_str(&self, key: &str) -> Option<LispValue> {
        let sym = Self::symbol_for(key);
        self.get(sym)
    }
    pub fn set_by_str(&mut self, key: &str, val: LispValue) {
        let sym = Self::symbol_for(key);
        self.set(sym, val);
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
            stdlib: lock.stdlib,
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
