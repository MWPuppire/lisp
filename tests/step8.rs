mod common;
use common::*;

fn basic_macros() -> Arc<RwLock<LispEnv>> {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(defmacro! one (fn* () 1))", lock.deref_mut());
    eval!("(defmacro! two (fn* () 2))", lock.deref_mut());
    eval!(
        "(defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a)))",
        lock.deref_mut()
    );
    eval!(
        "(defmacro! unless2 (fn* (pred a b) (list 'if (list 'not pred) a b)))",
        lock.deref_mut()
    );
    eval!("(defmacro! identity (fn* (x) x))", lock.deref_mut());
    drop(lock);
    env
}

#[test]
fn macros() {
    let env = basic_macros();
    let mut lock = env.write();
    assert_eq!(eval!("(one)", lock.deref_mut()), 1.0.into());
    assert_eq!(eval!("(two)", lock.deref_mut()), 2.0.into());
    assert_eq!(eval!("(unless false 7 8)", lock.deref_mut()), 7.0.into());
    assert_eq!(eval!("(unless true 7 8)", lock.deref_mut()), 8.0.into());
    assert_eq!(eval!("(unless2 false 7 8)", lock.deref_mut()), 7.0.into());
    assert_eq!(eval!("(unless2 true 7 8)", lock.deref_mut()), 8.0.into());
    assert_eq!(
        eval!("(let* (a 123) (identity a))", lock.deref_mut()),
        123.0.into()
    );
}

#[test]
fn macroexpand() {
    let env = basic_macros();
    let mut lock = env.write();
    assert_eq!(eval!("(macroexpand (one))", lock.deref_mut()), 1.0.into());
    assert_eq!(
        eval!("(macroexpand (unless PRED A B))", lock.deref_mut()),
        vector![
            LispValue::symbol_for_static("if"),
            LispValue::symbol_for_static("PRED"),
            LispValue::symbol_for_static("B"),
            LispValue::symbol_for_static("A"),
        ]
        .into()
    );
    assert_eq!(
        eval!("(macroexpand (unless2 PRED A B))", lock.deref_mut()),
        vector![
            LispValue::symbol_for_static("if"),
            vector![
                LispValue::symbol_for_static("not"),
                LispValue::symbol_for_static("PRED"),
            ]
            .into(),
            LispValue::symbol_for_static("A"),
            LispValue::symbol_for_static("B"),
        ]
        .into()
    );
    assert_eq!(
        eval!("(macroexpand (unless2 2 3 4))", lock.deref_mut()),
        vector![
            LispValue::symbol_for_static("if"),
            vector![LispValue::symbol_for_static("not"), 2.0.into(),].into(),
            3.0.into(),
            4.0.into(),
        ]
        .into()
    );
    assert_eq!(
        eval!(
            "(let* (a 123) (macroexpand (identity a)))",
            lock.deref_mut()
        ),
        LispValue::symbol_for_static("a"),
    );
}

#[test]
fn list_functions() {
    assert_eq!(eval!("(nth (list 1) 0)"), 1.0.into());
    assert_eq!(eval!("(nth (list 1 2) 1)"), 2.0.into());
    assert_eq!(eval!("(nth (list 1 2 nil) 2)"), LispValue::Nil);
    assert_eq!(eval!("(first (list))"), LispValue::Nil);
    assert_eq!(eval!("(first (list 6))"), 6.0.into());
    assert_eq!(eval!("(first (list 7 8 9))"), 7.0.into());
    assert_eq!(eval!("(rest (list))"), vector![].into());
    assert_eq!(eval!("(rest (list 6))"), vector![].into());
    assert_eq!(
        eval!("(rest (list 7 8 9))"),
        vector![8.0.into(), 9.0.into(),].into()
    );
    assert_eq!(eval!("(first [10])"), 10.0.into());
    assert_eq!(
        eval!("(rest [10 11 12])"),
        vector![11.0.into(), 12.0.into(),].into()
    );
}

#[test]
#[should_panic]
fn out_of_bounds_access() {
    eval!("(nth (list 1 2) 2)");
}

#[test]
fn cond() {
    assert_eq!(eval!("(cond)"), LispValue::Nil);
    assert_eq!(eval!("(cond true 7)"), 7.0.into());
    assert_eq!(eval!("(cond false 7)"), LispValue::Nil);
    assert_eq!(eval!("(cond true 7 true 8)"), 7.0.into());
    assert_eq!(eval!("(cond false 7 true 8)"), 8.0.into());
    assert_eq!(eval!("(cond false 7 false 8 \"else\" 9)"), 9.0.into());
    assert_eq!(eval!("(cond false 7 false 8 false 9)"), LispValue::Nil);
    assert_eq!(
        eval!("(let* (x (cond false \"no\" true \"yes\")) x)"),
        "yes".to_owned().into()
    );
}

#[test]
#[should_panic]
fn cond_uneven() {
    eval!("(cond true)");
}

#[test]
fn macro_closure() {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(def! x 2)", lock.deref_mut());
    eval!("(defmacro! a (fn* [] x))", lock.deref_mut());
    assert_eq!(eval!("(a)", lock.deref_mut()), 2.0.into());
    assert_eq!(eval!("(let* (x 3) (a))", lock.deref_mut()), 2.0.into());
}
