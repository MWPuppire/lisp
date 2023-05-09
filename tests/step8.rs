mod common;
use common::*;

fn basic_macros() -> LispEnv {
    let mut env = testing_env();
    eval!("(defmacro! one (fn* () 1))", &mut env);
    eval!("(defmacro! two (fn* () 2))", &mut env);
    eval!("(defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a)))", &mut env);
    eval!("(defmacro! unless2 (fn* (pred a b) (list 'if (list 'not pred) a b)))", &mut env);
    eval!("(defmacro! identity (fn* (x) x))", &mut env);
    env
}

#[test]
fn macros() {
    let mut env = basic_macros();
    assert_eq!(eval!("(one)", &mut env), LispValue::Number(1.0));
    assert_eq!(eval!("(two)", &mut env), LispValue::Number(2.0));
    assert_eq!(eval!("(unless false 7 8)", &mut env), LispValue::Number(7.0));
    assert_eq!(eval!("(unless true 7 8)", &mut env), LispValue::Number(8.0));
    assert_eq!(eval!("(unless2 false 7 8)", &mut env), LispValue::Number(7.0));
    assert_eq!(eval!("(unless2 true 7 8)", &mut env), LispValue::Number(8.0));
    assert_eq!(eval!("(let* (a 123) (identity a))", &mut env), LispValue::Number(123.0));
}

// always blows the stack for some reason, so `ignore`
// TODO figure out why
#[test]
#[ignore]
fn macroexpand() {
    let mut env = basic_macros();
    assert_eq!(eval!("(macroexpand (one))", &mut env), LispValue::Number(1.0));
    assert_eq!(eval!("(macroexpand (unless PRED A B))", &mut env), LispValue::List(vector![
        LispValue::symbol_for_static("if"),
        LispValue::symbol_for_static("PRED"),
        LispValue::symbol_for_static("B"),
        LispValue::symbol_for_static("A"),
    ]));
    assert_eq!(eval!("(macroexpand (unless2 PRED A B))", &mut env), LispValue::List(vector![
        LispValue::symbol_for_static("if"),
        LispValue::List(vector![
            LispValue::symbol_for_static("not"),
            LispValue::symbol_for_static("PRED"),
        ]),
        LispValue::symbol_for_static("A"),
        LispValue::symbol_for_static("B"),
    ]));
    assert_eq!(eval!("(macroexpand (unless2 2 3 4))", &mut env), LispValue::List(vector![
        LispValue::symbol_for_static("if"),
        LispValue::List(vector![
            LispValue::symbol_for_static("not"),
            LispValue::Number(2.0),
        ]),
        LispValue::Number(3.0),
        LispValue::Number(4.0),
    ]));
    assert_eq!(eval!("(let* (a 123) (macroexpand (identity a)))", &mut env),
        LispValue::symbol_for_static("a"),
    );
}

#[test]
fn list_functions() {
    assert_eq!(eval!("(nth (list 1) 0)"), LispValue::Number(1.0));
    assert_eq!(eval!("(nth (list 1 2) 1)"), LispValue::Number(2.0));
    assert_eq!(eval!("(nth (list 1 2 nil) 2)"), LispValue::Nil);
    assert_eq!(eval!("(first (list))"), LispValue::Nil);
    assert_eq!(eval!("(first (list 6))"), LispValue::Number(6.0));
    assert_eq!(eval!("(first (list 7 8 9))"), LispValue::Number(7.0));
    assert_eq!(eval!("(rest (list))"), LispValue::List(vector![]));
    assert_eq!(eval!("(rest (list 6))"), LispValue::List(vector![]));
    assert_eq!(eval!("(rest (list 7 8 9))"), LispValue::List(vector![
        LispValue::Number(8.0),
        LispValue::Number(9.0),
    ]));
    assert_eq!(eval!("(first [10])"), LispValue::Number(10.0));
    assert_eq!(eval!("(rest [10 11 12])"), LispValue::List(vector![
        LispValue::Number(11.0),
        LispValue::Number(12.0),
    ]));
}

#[test]
#[should_panic]
fn out_of_bounds_access() {
    eval!("(nth (list 1 2) 2)");
}

#[test]
fn cond() {
    assert_eq!(eval!("(cond)"), LispValue::Nil);
    assert_eq!(eval!("(cond true 7)"), LispValue::Number(7.0));
    assert_eq!(eval!("(cond false 7)"), LispValue::Nil);
    assert_eq!(eval!("(cond true 7 true 8)"), LispValue::Number(7.0));
    assert_eq!(eval!("(cond false 7 true 8)"), LispValue::Number(8.0));
    assert_eq!(eval!("(cond false 7 false 8 \"else\" 9)"), LispValue::Number(9.0));
    assert_eq!(eval!("(cond false 7 false 8 false 9)"), LispValue::Nil);
    assert_eq!(eval!("(let* (x (cond false \"no\" true \"yes\")) x)"), LispValue::String("yes".to_string()));
}

#[test]
#[should_panic]
fn cond_uneven() {
    eval!("(cond true)");
}

#[test]
fn macro_closure() {
    let mut env = testing_env();
    eval!("(def! x 2)", &mut env);
    eval!("(defmacro! a (fn* [] x))", &mut env);
    assert_eq!(eval!("(a)", &mut env), LispValue::Number(2.0));
    assert_eq!(eval!("(let* (x 3) (a))", &mut env), LispValue::Number(2.0));
}
