mod common;
use common::*;

#[test]
fn def() {
    let mut env = testing_env();
    eval!("(def! x 3)", &mut env);
    assert_eq!(eval!("x", &mut env), LispValue::Number(3.0));
    eval!("(def! x 4)", &mut env);
    assert_eq!(eval!("x", &mut env), LispValue::Number(4.0));
}

#[test]
fn def_immediate_evaluation() {
    let mut env = testing_env();
    assert_eq!(eval!("(def! x (+ 1 7))", &mut env), LispValue::Number(8.0));
    assert_eq!(eval!("x", &mut env), LispValue::Number(8.0));
}

#[test]
fn case_sensitive_symbols() {
    let mut env = testing_env();
    eval!("(def! mynum 111)", &mut env);
    eval!("(def! MYNUM 222)", &mut env);
    assert_eq!(eval!("mynum", &mut env), LispValue::Number(111.0));
    assert_eq!(eval!("MYNUM", &mut env), LispValue::Number(222.0));
}

#[test]
fn cancel_def_on_error() {
    let mut env = testing_env();
    eval!("(def! w 123)", &mut env);
    assert!(eval_str_in_env("(def! w (abc))", &mut env).is_err());
    assert_eq!(eval!("w", &mut env), LispValue::Number(123.0));
}

#[test]
fn let_statement() {
    assert_eq!(eval!("(let* (x 9) x)"), LispValue::Number(9.0));
    assert_eq!(eval!("(let* (z (+ 2 3)) (+ 1 z))"), LispValue::Number(6.0));
    assert_eq!(eval!("(let* (p (+ 2 3) q (+ 2 p)) (+ p q))"), LispValue::Number(12.0));
}

#[test]
fn let_scopes() {
    let mut env = testing_env();
    eval!("(def! x 4)", &mut env);
    assert_eq!(eval!("(let* (x 9) x)", &mut env), LispValue::Number(9.0));
    assert_eq!(eval!("x", &mut env), LispValue::Number(4.0));
    assert_eq!(eval!("(let* (q 9) x)", &mut env), LispValue::Number(4.0));
}

#[test]
fn vector_let() {
    assert_eq!(eval!("(let* [z 9] z)"), LispValue::Number(9.0));
    assert_eq!(eval!("(let* [p (+ 2 3) q (+ 2 p)] (+ p q))"), LispValue::Number(12.0));
}

#[test]
fn last_binding_priority() {
    assert_eq!(eval!("(let* (x 2 x 3) x)"), LispValue::Number(3.0));
}
