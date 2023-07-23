use crate::builtins;
use crate::util::{InnerObjectValue, InnerValue, LispBuiltinFunc, ObjectValue, Result};
use crate::LispValue;
use by_address::ByAddress;
use dashmap::DashMap;
use im::{HashMap, Vector};
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
    data: DashMap<LispSymbol, LispValue>,
    this: Weak<LispEnv>,
    enclosing: Option<Arc<LispEnv>>,
    stdlib: &'static HashMap<LispSymbol, LispValue>,
}

#[inline]
fn assign_this_self(inner: LispEnv) -> Arc<LispEnv> {
    let mut boxed = inner;
    Arc::new_cyclic(|weak| {
        boxed.this = weak.clone();
        boxed
    })
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LispClosure(ByAddress<Arc<LispEnv>>);
impl LispClosure {
    pub fn make_env(&self, args: &[(LispSymbol, LispValue)]) -> Arc<LispEnv> {
        let enclosing = self.0 .0.clone();
        let stdlib = self.0 .0.stdlib;
        let inner = LispEnv {
            data: args.iter().cloned().collect(),
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
    ) -> Arc<LispEnv> {
        let enclosing = self.0 .0.union(surrounding);
        let stdlib = self.0 .0.stdlib;
        let inner = LispEnv {
            data: args.iter().cloned().collect(),
            this: Weak::new(),
            enclosing: Some(enclosing),
            stdlib,
        };
        assign_this_self(inner)
    }
}

impl LispEnv {
    #[cfg(feature = "io-stdlib")]
    pub fn new_stdlib() -> Arc<Self> {
        let inner = LispEnv {
            data: DashMap::new(),
            this: Weak::new(),
            enclosing: None,
            stdlib: &builtins::BUILTINS,
        };
        assign_this_self(inner)
    }
    pub fn new_stdlib_protected() -> Arc<Self> {
        let inner = LispEnv {
            data: DashMap::new(),
            this: Weak::new(),
            enclosing: None,
            stdlib: &builtins::BUILTINS_NO_IO,
        };
        assign_this_self(inner)
    }
    pub fn new_nested(&self) -> Arc<Self> {
        let inner = LispEnv {
            data: DashMap::new(),
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

    pub fn global(&self) -> Arc<Self> {
        let mut peekable = self.nested_envs().skip(1).peekable();
        while let Some(env) = peekable.next() {
            if peekable.peek().is_none() {
                return env;
            }
        }
        self.clone_arc()
    }

    #[inline]
    pub fn clone_arc(&self) -> Arc<Self> {
        self.this.upgrade().unwrap()
    }

    pub fn get(&self, sym: LispSymbol) -> Option<LispValue> {
        if let Some(val) = self.data.get(&sym) {
            return Some(val.clone());
        }
        for env in self.nested_envs().skip(1) {
            if let Some(val) = env.data.get(&sym) {
                return Some(val.clone());
            }
        }
        self.stdlib.get(&sym).map(LispValue::clone)
    }
    #[inline]
    pub fn set(&self, sym: LispSymbol, val: LispValue) {
        self.data.insert(sym, val);
    }
    #[inline]
    pub fn get_by_str(&self, key: &str) -> Option<LispValue> {
        let sym = hash(key);
        self.get(sym)
    }
    #[inline]
    pub fn set_by_str(&self, key: &str, val: LispValue) {
        let sym = hash(key);
        self.set(sym, val);
    }
    #[inline]
    pub fn bind_func(
        &self,
        name: &'static str,
        body: fn(Vector<LispValue>, &Self) -> Result<LispValue>,
    ) {
        let f = LispValue::new(InnerValue::Object(Arc::new(ObjectValue {
            val: InnerObjectValue::BuiltinFunc(LispBuiltinFunc { name, body }),
            meta: None,
            quoted: false,
        })));
        let sym = hash(name);
        self.set(sym, f);
    }

    pub fn union(&self, other: &Self) -> Arc<Self> {
        let mut data = other.data.clone();
        data.extend(self.data.iter().map(|x| {
            let (key, val) = x.pair();
            (*key, val.clone())
        }));
        let inner = LispEnv {
            data,
            this: Weak::new(),
            enclosing: self.enclosing.clone(),
            stdlib: self.stdlib,
        };
        assign_this_self(inner)
    }

    pub fn keys(&self) -> impl Iterator<Item = LispSymbol> + '_ {
        self.nested_envs()
            .flat_map(|env| {
                // `collect()` + `into_iter()` to avoid borrowing `env`, which
                // goes out of scope right away
                env.data
                    .iter()
                    .map(|x| *x.key())
                    .collect::<Vec<_>>()
                    .into_iter()
            })
            .chain(self.stdlib.keys().copied())
    }

    #[inline]
    pub fn nested_envs(&self) -> impl Iterator<Item = Arc<Self>> + std::iter::FusedIterator {
        std::iter::once(self.this.upgrade().unwrap()).chain(NestedEnvIter {
            current: self.enclosing.clone(),
        })
    }
}

pub struct NestedEnvIter {
    current: Option<Arc<LispEnv>>,
}
impl Iterator for NestedEnvIter {
    type Item = Arc<LispEnv>;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current) = self.current.take() {
            self.current = current.enclosing.clone();
            Some(current)
        } else {
            None
        }
    }
}
impl std::iter::FusedIterator for NestedEnvIter {}
