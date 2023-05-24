mod common;
use common::*;

#[test]
fn def() {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(def! x 3)", lock.deref_mut());
    assert_eq!(eval!("x", lock.deref_mut()), 3.0.into());
    eval!("(def! x 4)", lock.deref_mut());
    assert_eq!(eval!("x", lock.deref_mut()), 4.0.into());
}

#[test]
fn def_immediate_evaluation() {
    let env = testing_env();
    let mut lock = env.write();
    assert_eq!(eval!("(def! x (+ 1 7))", lock.deref_mut()), 8.0.into());
    assert_eq!(eval!("x", lock.deref_mut()), 8.0.into());
}

#[test]
fn case_sensitive_symbols() {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(def! mynum 111)", lock.deref_mut());
    eval!("(def! MYNUM 222)", lock.deref_mut());
    assert_eq!(eval!("mynum", lock.deref_mut()), 111.0.into());
    assert_eq!(eval!("MYNUM", lock.deref_mut()), 222.0.into());
}

#[test]
fn cancel_def_on_error() {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(def! w 123)", lock.deref_mut());
    assert!(eval_str_in_env("(def! w (abc))", lock.deref_mut()).is_err());
    assert_eq!(eval!("w", lock.deref_mut()), 123.0.into());
}

#[test]
fn let_statement() {
    assert_eq!(eval!("(let* (x 9) x)"), 9.0.into());
    assert_eq!(eval!("(let* (z (+ 2 3)) (+ 1 z))"), 6.0.into());
    assert_eq!(eval!("(let* (p (+ 2 3) q (+ 2 p)) (+ p q))"), 12.0.into());
}

#[test]
fn let_scopes() {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(def! x 4)", lock.deref_mut());
    assert_eq!(eval!("(let* (x 9) x)", lock.deref_mut()), 9.0.into());
    assert_eq!(eval!("x", lock.deref_mut()), 4.0.into());
    assert_eq!(eval!("(let* (q 9) x)", lock.deref_mut()), 4.0.into());
}

#[test]
fn vector_let() {
    assert_eq!(eval!("(let* [z 9] z)"), 9.0.into());
    assert_eq!(eval!("(let* [p (+ 2 3) q (+ 2 p)] (+ p q))"), 12.0.into());
}

#[test]
fn last_binding_priority() {
    assert_eq!(eval!("(let* (x 2 x 3) x)"), 3.0.into());
}
