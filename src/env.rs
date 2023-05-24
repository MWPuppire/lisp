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
pub struct LispEnv {
    data: HashMap<LispSymbol, LispValue>,
    this: Weak<RwLock<LispEnv>>,
    enclosing: Option<Arc<RwLock<LispEnv>>>,
    stdlib: &'static HashMap<LispSymbol, LispValue>,
}

#[inline]
fn assign_this_self(inner: LispEnv) -> Arc<RwLock<LispEnv>> {
    let mut boxed = RwLock::new(inner);
    let arc = Arc::new_cyclic(|weak| {
        boxed.get_mut().this = weak.clone();
        boxed
    });
    arc
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LispClosure(ByAddress<Arc<RwLock<LispEnv>>>);
impl LispClosure {
    pub fn make_env(&self, args: &[(LispSymbol, LispValue)]) -> Arc<RwLock<LispEnv>> {
        let enclosing = self.0.0.clone();
        let lock = self.0.0.read();
        let inner = LispEnv {
            data: args.into(),
            this: Weak::new(),
            enclosing: Some(enclosing),
            stdlib: lock.stdlib,
        };
        assign_this_self(inner)
    }
    pub fn make_macro_env(&self, args: &[(LispSymbol, LispValue)], surrounding: &LispEnv) -> Arc<RwLock<LispEnv>> {
        let lock = self.0.0.read();
        let inner = LispEnv {
            data: args.into(),
            this: Weak::new(),
            enclosing: Some(lock.union(surrounding)),
            stdlib: lock.stdlib,
        };
        assign_this_self(inner)
    }
}

// TODO I'd probably rather not have a single static interner
lazy_static! {
    static ref INTERNER: RwLock<StringInterner> = {
        RwLock::new(StringInterner::new())
    };
}

impl LispEnv {
    #[cfg(feature = "io-stdlib")]
    pub fn new_stdlib() -> Arc<RwLock<Self>> {
        let inner = LispEnv {
            data: HashMap::new(),
            this: Weak::new(),
            enclosing: None,
            stdlib: &builtins::BUILTINS,
        };
        assign_this_self(inner)
    }
    pub fn new_stdlib_protected() -> Arc<RwLock<Self>> {
        let inner = LispEnv {
            data: HashMap::new(),
            this: Weak::new(),
            enclosing: None,
            stdlib: &builtins::BUILTINS_NO_IO,
        };
        assign_this_self(inner)
    }
    pub fn new_nested(&self) -> Arc<RwLock<Self>> {
        let inner = LispEnv {
            data: HashMap::new(),
            this: Weak::new(),
            enclosing: Some(self.this.upgrade().unwrap()),
            stdlib: self.stdlib,
        };
        assign_this_self(inner)
    }

    #[inline]
    pub fn make_closure(&self) -> LispClosure {
        LispClosure(ByAddress(self.this.upgrade().unwrap()))
    }

    pub fn global(&self) -> Arc<RwLock<Self>> {
        let mut peekable = self.nested_envs().peekable();
        while let Some(env) = peekable.next() {
            if peekable.peek().is_none() {
                return env;
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

    #[inline]
    pub(crate) fn clone_arc(&self) -> Arc<RwLock<Self>> {
        self.this.upgrade().unwrap()
    }

    // dead-locks when ran in `eval` if the current environment is a nested
    // environment from the one running in `eval` (since a write-lock holds the
    // environment outside the scope of `eval` but a read-lock is attempted
    // here); not sure the best way to solve this
    pub fn get(&self, sym: LispSymbol) -> Option<LispValue> {
        if let Some(val) = self.data.get(&sym) {
            return Some(val.clone());
        }
        // skip(1) in case `self` has a write-lock holding it
        for env in self.nested_envs().skip(1) {
            let lock = env.read();
            if let Some(val) = lock.data.get(&sym) {
                return Some(val.clone());
            }
        }
        self.stdlib.get(&sym).map(LispValue::clone)
    }
    #[inline]
    pub fn set(&mut self, sym: LispSymbol, val: LispValue) {
        self.data.insert(sym, val);
    }
    #[inline]
    pub fn get_by_str(&self, key: &str) -> Option<LispValue> {
        let sym = Self::symbol_for(key);
        self.get(sym)
    }
    #[inline]
    pub fn set_by_str(&mut self, key: &str, val: LispValue) {
        let sym = Self::symbol_for(key);
        self.set(sym, val);
    }
    #[inline]
    pub fn bind_func(&mut self, name: &'static str, body: fn(Vector<LispValue>, &mut Self) -> Result<LispValue>) {
        let f = LispValue::Object(
            Arc::new(ObjectValue::BuiltinFunc(LispBuiltinFunc {
                name,
                body,
            }))
        );
        let sym = Self::symbol_for_static(name);
        self.set(sym, f);
    }

    pub fn union(&self, other: &Self) -> Arc<RwLock<Self>> {
        let inner = LispEnv {
            data: self.data.clone().union(other.data.clone()),
            this: Weak::new(),
            enclosing: self.enclosing.clone(),
            stdlib: self.stdlib,
        };
        assign_this_self(inner)
    }

    pub fn keys(&self) -> impl Iterator<Item = LispSymbol> + '_ {
        self.data.keys().copied().chain(self.stdlib.keys().copied()).chain(
            // like in `get`, skip(1) to avoid potential dead-lock
            self.nested_envs().skip(1).map(|env| {
                let lock = env.read();
                // `collect()` + `into_iter()` to avoid borrowing `lock`, which
                // goes out of scope right away
                lock.data.keys().copied().collect::<Vec<_>>().into_iter()
            }).flatten()
        )
    }

    #[inline]
    pub fn nested_envs(&self) -> impl Iterator<Item = Arc<RwLock<Self>>> + std::iter::FusedIterator {
        // as with a couple other places, taking care to avoid dead-lock if the
        // owner of `self` has a write-lock
        std::iter::once(self.this.upgrade().unwrap())
            .chain(NestedEnvIter {
                current: self.enclosing.clone(),
            })
    }
}

pub struct NestedEnvIter {
    current: Option<Arc<RwLock<LispEnv>>>,
}
impl Iterator for NestedEnvIter {
    type Item = Arc<RwLock<LispEnv>>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current.take() {
            let lock = current.read();
            self.current = lock.enclosing.clone();
            drop(lock);
            Some(current)
        } else {
            None
        }
    }
}
impl std::iter::FusedIterator for NestedEnvIter { }
