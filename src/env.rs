use std::sync::{Arc, Weak};
use std::ops::DerefMut;
use im::{HashMap, Vector};
use lazy_static::lazy_static;
use string_interner::{StringInterner, DefaultSymbol};
use by_address::ByAddress;
use parking_lot::RwLock;
use crate::LispValue;
use crate::builtins;
use crate::util::{LispBuiltinFunc, ObjectValue, Result};

pub type LispSymbol = DefaultSymbol;

#[derive(Clone, Debug)]
struct InnerEnv {
    data: HashMap<LispSymbol, LispValue>,
    this: Weak<RwLock<InnerEnv>>,
    enclosing: Option<LispEnv>,
    stdlib: &'static HashMap<LispSymbol, LispValue>,
}

// creates a `LispEnv` where `global` refers to `self` from an `InnerEnv`
#[inline]
fn assign_this_self(inner: InnerEnv) -> LispEnv {
    let mut boxed = RwLock::new(inner);
    let arc = Arc::new_cyclic(|weak| {
        boxed.get_mut().this = weak.clone();
        boxed
    });
    LispEnv(arc)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LispClosure(ByAddress<Arc<RwLock<InnerEnv>>>);
impl LispClosure {
    pub fn make_env(&self, args: &[(LispSymbol, LispValue)]) -> LispEnv {
        let enclosing = LispEnv(self.0.0.clone());
        let lock = self.0.0.read();
        let inner = InnerEnv {
            data: args.into(),
            this: Weak::new(),
            enclosing: Some(enclosing),
            stdlib: lock.stdlib,
        };
        assign_this_self(inner)
    }
    pub fn make_macro_env(&self, args: &[(LispSymbol, LispValue)], surrounding: &LispEnv) -> LispEnv {
        let enclosing = LispEnv(self.0.0.clone());
        let lock = self.0.0.read();
        let inner = InnerEnv {
            data: args.into(),
            this: Weak::new(),
            enclosing: Some(enclosing.union(surrounding)),
            stdlib: lock.stdlib,
        };
        assign_this_self(inner)
    }
}

#[derive(Clone, Debug)]
pub struct LispEnv(Arc<RwLock<InnerEnv>>);

// TODO I'd probably rather not have a single static interner
lazy_static! {
    static ref INTERNER: RwLock<StringInterner> = {
        RwLock::new(StringInterner::new())
    };
}

impl LispEnv {
    #[cfg(feature = "io-stdlib")]
    pub fn new_stdlib() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            this: Weak::new(),
            enclosing: None,
            stdlib: &builtins::BUILTINS,
        };
        assign_this_self(inner)
    }
    pub fn new_stdlib_protected() -> Self {
        let inner = InnerEnv {
            data: HashMap::new(),
            this: Weak::new(),
            enclosing: None,
            stdlib: &builtins::BUILTINS_NO_IO,
        };
        assign_this_self(inner)
    }
    pub fn new_nested(&self) -> Self {
        let copy = self.clone();
        let lock = self.0.read();
        let inner = InnerEnv {
            data: HashMap::new(),
            this: Weak::new(),
            enclosing: Some(copy),
            stdlib: lock.stdlib,
        };
        assign_this_self(inner)
    }

    #[inline]
    pub fn make_closure(&self) -> LispClosure {
        LispClosure(ByAddress(self.0.clone()))
    }

    pub fn global(&self) -> LispEnv {
        let mut peekable = self.nested_envs().peekable();
        while let Some(env) = peekable.next() {
            if peekable.peek().is_none() {
                let lock = env.0.read();
                return LispEnv(lock.this.upgrade().unwrap());
            }
        }
        unreachable!()
    }

    #[inline]
    pub(crate) fn interner_mut() -> impl DerefMut<Target = StringInterner> {
        INTERNER.write()
    }
    #[inline]
    pub(crate) fn symbol_for(s: &str) -> LispSymbol {
        let mut interner = INTERNER.write();
        interner.get_or_intern(s)
    }
    #[inline]
    pub(crate) fn symbol_for_static(s: &'static str) -> LispSymbol {
        let mut interner = INTERNER.write();
        interner.get_or_intern_static(s)
    }
    #[inline]
    pub(crate) fn symbol_string(sym: LispSymbol) -> Option<String> {
        let interner = INTERNER.read();
        interner.resolve(sym).map(ToString::to_string)
    }

    pub fn get(&self, sym: LispSymbol) -> Option<LispValue> {
        for env in self.nested_envs() {
            let lock = env.0.read();
            if let Some(val) = lock.data.get(&sym) {
                return Some(val.clone());
            }
        }
        let lock = self.0.read();
        lock.stdlib.get(&sym).map(LispValue::clone)
    }
    pub fn set(&self, sym: LispSymbol, val: LispValue) {
        let mut lock = self.0.write();
        lock.data.insert(sym, val);
    }
    #[inline]
    pub fn get_by_str(&self, key: &str) -> Option<LispValue> {
        let sym = Self::symbol_for(key);
        self.get(sym)
    }
    #[inline]
    pub fn set_by_str(&self, key: &str, val: LispValue) {
        let sym = Self::symbol_for(key);
        self.set(sym, val);
    }
    #[inline]
    pub fn bind_func(&self, name: &'static str, body: fn(Vector<LispValue>, &LispEnv) -> Result<LispValue>) {
        let f = LispValue::Object(
            Arc::new(ObjectValue::BuiltinFunc(LispBuiltinFunc {
                name,
                body,
            }))
        );
        let sym = Self::symbol_for_static(name);
        self.set(sym, f);
    }

    pub fn union(&self, other: &LispEnv) -> LispEnv {
        let lock = self.0.read();
        let reader = other.0.read();
        let inner = InnerEnv {
            data: lock.data.clone().union(reader.data.clone()),
            this: Weak::new(),
            enclosing: lock.enclosing.clone(),
            stdlib: lock.stdlib,
        };
        assign_this_self(inner)
    }

    pub fn keys(&self) -> impl Iterator<Item = LispSymbol> {
        let lock = self.0.read();
        lock.stdlib.keys().copied().chain(self.nested_envs().map(|env| {
            let lock = env.0.read();
            // `collect()` and `into_iter()` because otherwise it's borrowing
            // `lock`, which goes out of scope right away
            lock.data.keys().copied().collect::<Vec<_>>().into_iter()
        }).flatten())
    }

    #[inline]
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
            let lock = current.0.read();
            let enclosing = lock.enclosing.clone();
            drop(lock);
            self.current = enclosing;
            Some(out)
        } else {
            None
        }
    }
}
impl std::iter::FusedIterator for NestedEnvIter { }

cfg_if::cfg_if! {
    if #[cfg(feature = "io-stdlib")] {
        impl Default for LispEnv {
            #[inline]
            fn default() -> Self {
                Self::new_stdlib()
            }
        }
    } else {
        impl Default for LispEnv {
            #[inline]
            fn default() -> Self {
                Self::new_stdlib_protected()
            }
        }
    }
}
