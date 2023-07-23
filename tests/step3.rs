mod common;
use common::*;

#[test]
fn def() {
    let env = testing_env();
    eval!("(def! x 3)", &env);
    eval_eq!("x", &env, 3.0);
    eval!("(def! x 4)", &env);
    eval_eq!("x", &env, 4.0);
}

#[test]
fn def_immediate_evaluation() {
    let env = testing_env();
    eval_eq!("(def! x (+ 1 7))", &env, 8.0);
    eval_eq!("x", &env, 8.0);
}

#[test]
fn case_sensitive_symbols() {
    let env = testing_env();
    eval!("(def! mynum 111)", &env);
    eval!("(def! MYNUM 222)", &env);
    eval_eq!("mynum", &env, 111.0);
    eval_eq!("MYNUM", &env, 222.0);
}

#[test]
fn cancel_def_on_error() {
    let env = testing_env();
    eval!("(def! w 123)", &env);
    assert!(eval_str_in_env("(def! w (abc))", &env).is_err());
    eval_eq!("w", &env, 123.0);
}

#[test]
fn let_statement() {
    eval_eq!("(let* (x 9) x)", 9.0);
    eval_eq!("(let* (z (+ 2 3)) (+ 1 z))", 6.0);
    eval_eq!("(let* (p (+ 2 3) q (+ 2 p)) (+ p q))", 12.0);
}

#[test]
fn let_scopes() {
    let env = testing_env();
    eval!("(def! x 4)", &env);
    eval_eq!("(let* (x 9) x)", &env, 9.0);
    eval_eq!("x", &env, 4.0);
    eval_eq!("(let* (q 9) x)", &env, 4.0);
}

#[test]
fn vector_let() {
    eval_eq!("(let* [z 9] z)", 9.0);
    eval_eq!("(let* [p (+ 2 3) q (+ 2 p)] (+ p q))", 12.0);
}

#[test]
fn last_binding_priority() {
    eval_eq!("(let* (x 2 x 3) x)", 3.0);
}
