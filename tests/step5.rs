mod common;
use common::*;

#[test]
fn recursive_tail_call() {
    let mut env = testing_env();
    eval!("(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))", &mut env);
    assert_eq!(eval!("(sum2 10 0)", &mut env), LispValue::Number(55.0));
    assert_eq!(eval!("(sum2 10000 0)", &mut env), LispValue::Number(50005000.0));
}

#[test]
fn mutually_recursive_tail_call() {
    let mut env = testing_env();
    eval!("(def! foo (fn* (n) (if (= n 0) 0 (bar (- n 1)))))", &mut env);
    eval!("(def! bar (fn* (n) (if (= n 0) 0 (foo (- n 1)))))", &mut env);
    assert_eq!(eval!("(foo 10000)", &mut env), LispValue::Number(0.0));
}

#[test]
fn do_do() {
    assert_eq!(eval!("(do (do 1 2))"), LispValue::Number(2.0));
}
