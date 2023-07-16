mod common;
use common::*;

#[test]
fn def() {
    let env = testing_env();
    eval!("(def! x 3)", &env);
    assert_eq!(eval!("x", &env), 3.0.into());
    eval!("(def! x 4)", &env);
    assert_eq!(eval!("x", &env), 4.0.into());
}

#[test]
fn def_immediate_evaluation() {
    let env = testing_env();
    assert_eq!(eval!("(def! x (+ 1 7))", &env), 8.0.into());
    assert_eq!(eval!("x", &env), 8.0.into());
}

#[test]
fn case_sensitive_symbols() {
    let env = testing_env();
    eval!("(def! mynum 111)", &env);
    eval!("(def! MYNUM 222)", &env);
    assert_eq!(eval!("mynum", &env), 111.0.into());
    assert_eq!(eval!("MYNUM", &env), 222.0.into());
}

#[test]
fn cancel_def_on_error() {
    let env = testing_env();
    eval!("(def! w 123)", &env);
    assert!(eval_str_in_env("(def! w (abc))", &env).is_err());
    assert_eq!(eval!("w", &env), 123.0.into());
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
    eval!("(def! x 4)", &env);
    assert_eq!(eval!("(let* (x 9) x)", &env), 9.0.into());
    assert_eq!(eval!("x", &env), 4.0.into());
    assert_eq!(eval!("(let* (q 9) x)", &env), 4.0.into());
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
