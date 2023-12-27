use crate::builtins;
use crate::util::Result;
use crate::value::{InnerObjectValue, InnerValue, LispBuiltinFunc, ObjectValue};
use crate::LispValue;
use by_address::ByAddress;
use dashmap::DashMap;
use im::{HashMap, Vector};
use itertools::Itertools;
use std::sync::{Arc, Weak};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LispSymbol(String);
#[inline]
pub(crate) fn hash(s: impl Into<String>) -> LispSymbol {
    LispSymbol(s.into())
}
impl std::fmt::Display for LispSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl From<LispSymbol> for String {
    fn from(item: LispSymbol) -> String {
        item.0
    }
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

// used with dumping environments
type AtomMap = HashMap<ByAddress<Arc<parking_lot::RwLock<LispValue>>>, LispSymbol>;

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

    #[cfg(feature = "self-implemented")]
    pub fn new_self_implemented() -> Arc<Self> {
        let inner = LispEnv {
            data: DashMap::new(),
            this: Weak::new(),
            enclosing: None,
            stdlib: &builtins::BUILTINS_CORE,
        };
        let arced = assign_this_self(inner);
        let mut parser = crate::LispParser::new();
        parser
            .add_tokenize(builtins::BUILTINS_SELF_IMPLEMENTED_SRC)
            .expect("Builtins should be syntactically correct");
        for val in parser {
            let val = val.expect("Builtins should be syntactically correct");
            crate::eval(val, &arced).expect("Builtins should pass evaluation");
        }
        arced
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

    pub fn get(&self, sym: &LispSymbol) -> Option<LispValue> {
        if let Some(val) = self.data.get(sym) {
            return Some(val.clone());
        }
        for env in self.nested_envs().skip(1) {
            if let Some(val) = env.data.get(sym) {
                return Some(val.clone());
            }
        }
        self.stdlib.get(sym).map(LispValue::clone)
    }
    #[inline]
    pub fn set(&self, sym: LispSymbol, val: LispValue) {
        self.data.insert(sym, val);
    }
    #[inline]
    pub fn get_by_str(&self, key: &str) -> Option<LispValue> {
        let sym = hash(key);
        self.get(&sym)
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
        data.extend(self.data.iter().map(|item| {
            let (key, val) = item.pair();
            (key.clone(), val.clone())
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
                    .map(|x| x.key().clone())
                    .collect::<Vec<_>>()
                    .into_iter()
            })
            .chain(self.stdlib.keys().cloned())
    }

    #[inline]
    pub fn nested_envs(&self) -> impl Iterator<Item = Arc<Self>> + std::iter::FusedIterator {
        std::iter::once(self.this.upgrade().unwrap()).chain(NestedEnvIter {
            current: self.enclosing.clone(),
        })
    }

    fn dump_inner(&self, def: &str, defclose: &str, atoms: &mut AtomMap) -> String {
        self.data
            .iter()
            .map(|item| {
                let (key, val) = item.pair();
                match &val.val {
                    InnerValue::Atom(x) => {
                        if let Some(sym) = atoms.get(x) {
                            format!("{}{} {}{}", def, key, sym, defclose)
                        } else {
                            atoms.insert(x.clone(), key.clone());
                            format!("{}{} {}{}", def, key, val, defclose)
                        }
                    }
                    InnerValue::Object(o) => match &o.val {
                        InnerObjectValue::Func(f) | InnerObjectValue::Macro(f) => {
                            if Weak::ptr_eq(&Arc::downgrade(&f.closure.0 .0), &self.this) {
                                format!("{}{} {}{}", def, key, o, defclose)
                            } else {
                                format!(
                                    "{}{} (let* ({}) {:#}){}",
                                    def,
                                    key,
                                    f.closure.0.dump_inner("", "", atoms),
                                    o,
                                    defclose
                                )
                            }
                        }
                        _ => format!("{}{} {:#}{}", def, key, o, defclose),
                    },
                    _ => format!("{}{} {:#}{}", def, key, val, defclose),
                }
            })
            .join(" ")
    }

    #[inline]
    pub fn dump(&self) -> String {
        // Special case atoms, since they aren't referentially transparent.
        // Not doing similar for functions/macros will only affect comparison
        // operators, since functions/macros can't be modified after creation.
        // Most other values (should) have referential transparency.
        let mut atoms: AtomMap = HashMap::new();
        self.dump_inner("(def! ", ")", &mut atoms)
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
