use std::sync::{Arc, RwLock, Weak};
use std::ops::DerefMut;
use im::HashMap;
use lazy_static::lazy_static;
use string_interner::{StringInterner, DefaultSymbol};
use by_address::ByAddress;
use crate::LispValue;
use crate::builtins;
use crate::util::{LispBuiltinFunc, ObjectValue};

pub type LispSymbol = DefaultSymbol;

#[derive(Clone, Debug)]
struct InnerEnv {
    data: HashMap<LispSymbol, LispValue>,
    global: Weak<RwLock<InnerEnv>>,
    enclosing: Option<LispEnv>,
    stdlib: &'static HashMap<LispSymbol, LispValue>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LispClosure(ByAddress<Arc<RwLock<InnerEnv>>>);
impl LispClosure {
    pub fn make_env(&self, args: &[(LispSymbol, LispValue)]) -> LispEnv {
        let enclosing = LispEnv(self.0.0.clone());
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
        let enclosing = LispEnv(self.0.0.clone());
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

#[derive(Clone, Debug)]
pub struct LispEnv(Arc<RwLock<InnerEnv>>);

lazy_static! {
    static ref INTERNER: RwLock<StringInterner> = {
        RwLock::new(StringInterner::new())
    };
}

// creates a `LispEnv` where `global` refers to `self` from an `InnerEnv`
fn assign_global_self(inner: InnerEnv) -> LispEnv {
    let mut boxed = Arc::new(RwLock::new(inner));
    let weak = Arc::downgrade(&boxed);
    unsafe {
        Arc::get_mut_unchecked(&mut boxed).get_mut().unwrap().global = weak;
    };
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
        assign_global_self(inner)
    }
    pub fn new_stdlib_protected() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            enclosing: None,
            global: Weak::new(),
            stdlib: &builtins::BUILTINS_NO_IO,
        };
        assign_global_self(inner)
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

    pub fn make_closure(&self) -> LispClosure {
        LispClosure(ByAddress(self.0.clone()))
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
    pub(crate) fn symbol_for(s: &str) -> LispSymbol {
        let mut interner = INTERNER.write().unwrap();
        interner.get_or_intern(s)
    }
    pub(crate) fn symbol_for_static(s: &'static str) -> LispSymbol {
        let mut interner = INTERNER.write().unwrap();
        interner.get_or_intern_static(s)
    }
    // TODO: I think this is actually an unsound transmute,
    // and I never did like having a single static `StringInterner`
    // not a member of any type
    pub(crate) fn symbol_string(sym: LispSymbol) -> Option<&'static str> {
        let interner = INTERNER.read().unwrap();
        unsafe { std::mem::transmute(interner.resolve(sym)) }
    }

    pub fn get(&self, sym: LispSymbol) -> Option<LispValue> {
        for env in self.nested_envs() {
            let lock = env.0.read().unwrap();
            if let Some(val) = lock.data.get(&sym) {
                return Some(val.clone());
            }
        }
        let lock = self.0.read().unwrap();
        lock.stdlib.get(&sym).map(LispValue::clone)
    }
    pub fn set(&self, sym: LispSymbol, val: LispValue) {
        let mut lock = self.0.write().unwrap();
        lock.data.insert(sym, val);
    }
    pub fn get_by_str(&self, key: &str) -> Option<LispValue> {
        let sym = Self::symbol_for(key);
        self.get(sym)
    }
    pub fn set_by_str(&self, key: &str, val: LispValue) {
        let sym = Self::symbol_for(key);
        self.set(sym, val);
    }
    pub fn bind_func(&self, name: &'static str, f: LispBuiltinFunc) {
        let f = LispValue::Object(
            Arc::new(ObjectValue::BuiltinFunc {
                name,
                f,
            })
        );
        let sym = Self::symbol_for_static(name);
        self.set(sym, f);
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

    pub fn keys(&self) -> impl Iterator<Item = LispSymbol> {
        let lock = self.0.read().unwrap();
        lock.stdlib.keys().copied().chain(self.nested_envs().map(|env| {
            let lock = env.0.read().unwrap();
            // `collect()` and `into_iter()` because otherwise it's borrowing
            // `lock`, which goes out of scope right away
            lock.data.keys().copied().collect::<Vec<_>>().into_iter()
        }).flatten())
    }

    pub fn nested_envs(&self) -> impl Iterator<Item = LispEnv> {
        NestedEnvIter {
            current: Some(self.clone())
        }
    }
}

pub struct NestedEnvIter {
    current: Option<LispEnv>,
}
impl Iterator for NestedEnvIter {
    type Item = LispEnv;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current.as_ref() {
            let out = current.clone();
            let lock = current.0.read().unwrap();
            let enclosing = lock.enclosing.clone();
            drop(lock);
            self.current = enclosing;
            Some(out)
        } else {
            None
        }
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
