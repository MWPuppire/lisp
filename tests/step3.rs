mod common;
use common::*;

#[test]
fn def() {
    let mut env = testing_env();
    eval!("(def! x 3)", &mut env);
    assert_eq!(eval!("x", &mut env), 3.0.into());
    eval!("(def! x 4)", &mut env);
    assert_eq!(eval!("x", &mut env), 4.0.into());
}

#[test]
fn def_immediate_evaluation() {
    let mut env = testing_env();
    assert_eq!(eval!("(def! x (+ 1 7))", &mut env), 8.0.into());
    assert_eq!(eval!("x", &mut env), 8.0.into());
}

#[test]
fn case_sensitive_symbols() {
    let mut env = testing_env();
    eval!("(def! mynum 111)", &mut env);
    eval!("(def! MYNUM 222)", &mut env);
    assert_eq!(eval!("mynum", &mut env), 111.0.into());
    assert_eq!(eval!("MYNUM", &mut env), 222.0.into());
}

#[test]
fn cancel_def_on_error() {
    let mut env = testing_env();
    eval!("(def! w 123)", &mut env);
    assert!(eval_str_in_env("(def! w (abc))", &mut env).is_err());
    assert_eq!(eval!("w", &mut env), 123.0.into());
}

#[test]
fn let_statement() {
    assert_eq!(eval!("(let* (x 9) x)"), 9.0.into());
    assert_eq!(eval!("(let* (z (+ 2 3)) (+ 1 z))"), 6.0.into());
    assert_eq!(eval!("(let* (p (+ 2 3) q (+ 2 p)) (+ p q))"), 12.0.into());
}

#[test]
fn let_scopes() {
    let mut env = testing_env();
    eval!("(def! x 4)", &mut env);
    assert_eq!(eval!("(let* (x 9) x)", &mut env), 9.0.into());
    assert_eq!(eval!("x", &mut env), 4.0.into());
    assert_eq!(eval!("(let* (q 9) x)", &mut env), 4.0.into());
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
