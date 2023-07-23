mod common;
use common::*;

#[test]
fn recursive_tail_call() {
    let env = testing_env();
    eval!(
        "(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))",
        &env
    );
    eval_eq!("(sum2 10 0)", &env, 55.0);
    eval_eq!("(sum2 10000 0)", &env, 50005000.0);
}

#[test]
fn mutually_recursive_tail_call() {
    let env = testing_env();
    eval!("(def! foo (fn* (n) (if (= n 0) 0 (bar (- n 1)))))", &env);
    eval!("(def! bar (fn* (n) (if (= n 0) 0 (foo (- n 1)))))", &env);
    eval_eq!("(foo 10000)", &env, 0.0);
}

#[test]
fn do_do() {
    eval_eq!("(do (do 1 2))", 2.0);
}
