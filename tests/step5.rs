mod common;
use common::*;

#[test]
fn recursive_tail_call() {
    let env = testing_env();
    let mut lock = env.write();
    eval!(
        "(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))",
        lock.deref_mut()
    );
    assert_eq!(eval!("(sum2 10 0)", lock.deref_mut()), 55.0.into());
    assert_eq!(eval!("(sum2 10000 0)", lock.deref_mut()), 50005000.0.into());
}

#[test]
fn mutually_recursive_tail_call() {
    let env = testing_env();
    let mut lock = env.write();
    eval!(
        "(def! foo (fn* (n) (if (= n 0) 0 (bar (- n 1)))))",
        lock.deref_mut()
    );
    eval!(
        "(def! bar (fn* (n) (if (= n 0) 0 (foo (- n 1)))))",
        lock.deref_mut()
    );
    assert_eq!(eval!("(foo 10000)", lock.deref_mut()), 0.0.into());
}

#[test]
fn do_do() {
    assert_eq!(eval!("(do (do 1 2))"), 2.0.into());
}
