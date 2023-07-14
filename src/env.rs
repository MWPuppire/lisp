use crate::builtins;
use crate::util::{LispBuiltinFunc, ObjectValue, Result};
use crate::LispValue;
use by_address::ByAddress;
use im::{HashMap, Vector};
use parking_lot::RwLock;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Weak};

pub type LispSymbol = u32;
#[inline]
pub(crate) fn hash(s: &str) -> LispSymbol {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish() as LispSymbol
}

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
    Arc::new_cyclic(|weak| {
        boxed.get_mut().this = weak.clone();
        boxed
    })
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LispClosure(ByAddress<Arc<RwLock<LispEnv>>>);
impl LispClosure {
    pub fn make_env(
        &self,
        args: &[(LispSymbol, LispValue)],
        surrounding: &LispEnv,
    ) -> Arc<RwLock<LispEnv>> {
        let enclosing = self.0 .0.clone();
        let surrounding_arc = surrounding.clone_arc();
        let stdlib = if Arc::ptr_eq(&enclosing, &surrounding_arc) {
            surrounding.stdlib
        } else {
            let lock = self.0 .0.read();
            lock.stdlib
        };
        let inner = LispEnv {
            data: args.into(),
            this: Weak::new(),
            enclosing: Some(enclosing),
            stdlib,
        };
        assign_this_self(inner)
    }
    pub fn make_macro_env(
        &self,
        args: &[(LispSymbol, LispValue)],
        surrounding: &LispEnv,
    ) -> Arc<RwLock<LispEnv>> {
        let surrounding_arc = surrounding.clone_arc();
        let (enclosing, stdlib) = if Arc::ptr_eq(&self.0 .0, &surrounding_arc) {
            (Some(surrounding.clone_arc()), surrounding.stdlib)
        } else {
            let lock = self.0 .0.read();
            let enclosing = lock.union(surrounding);
            (Some(enclosing), lock.stdlib)
        };
        let inner = LispEnv {
            data: args.into(),
            this: Weak::new(),
            enclosing,
            stdlib,
        };
        assign_this_self(inner)
    }
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
        let mut peekable = self.nested_envs().skip(1).peekable();
        while let Some(env) = peekable.next() {
            if peekable.peek().is_none() {
                return env;
            }
        }
        self.clone_arc()
    }

    #[inline]
    pub fn clone_arc(&self) -> Arc<RwLock<Self>> {
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
        let sym = hash(key);
        self.get(sym)
    }
    #[inline]
    pub fn set_by_str(&mut self, key: &str, val: LispValue) {
        let sym = hash(key);
        self.set(sym, val);
    }
    #[inline]
    pub fn bind_func(
        &mut self,
        name: &'static str,
        body: fn(Vector<LispValue>, &mut Self) -> Result<LispValue>,
    ) {
        let f = LispValue::Object(Arc::new(ObjectValue::BuiltinFunc(LispBuiltinFunc {
            name,
            body,
        })));
        let sym = hash(name);
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
        self.data
            .keys()
            .copied()
            .chain(self.stdlib.keys().copied())
            .chain(
                // like in `get`, skip(1) to avoid potential dead-lock
                self.nested_envs().skip(1).flat_map(|env| {
                    let lock = env.read();
                    // `collect()` + `into_iter()` to avoid borrowing `lock`, which
                    // goes out of scope right away
                    lock.data.keys().copied().collect::<Vec<_>>().into_iter()
                }),
            )
    }

    #[inline]
    pub fn nested_envs(
        &self,
    ) -> impl Iterator<Item = Arc<RwLock<Self>>> + std::iter::FusedIterator {
        // as with a couple other places, taking care to avoid dead-lock if the
        // owner of `self` has a write-lock
        std::iter::once(self.this.upgrade().unwrap()).chain(NestedEnvIter {
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
impl std::iter::FusedIterator for NestedEnvIter {}
