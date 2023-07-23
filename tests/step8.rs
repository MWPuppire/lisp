mod common;
use common::*;

fn basic_macros() -> Arc<LispEnv> {
    let env = testing_env();
    eval!("(defmacro! one (fn* () 1))", &env);
    eval!("(defmacro! two (fn* () 2))", &env);
    eval!(
        "(defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a)))",
        &env
    );
    eval!(
        "(defmacro! unless2 (fn* (pred a b) (list 'if (list 'not pred) a b)))",
        &env
    );
    eval!("(defmacro! identity (fn* (x) x))", &env);
    env
}

#[test]
fn macros() {
    let env = basic_macros();
    assert_eq!(eval!("(one)", &env), 1.0.into());
    assert_eq!(eval!("(two)", &env), 2.0.into());
    assert_eq!(eval!("(unless false 7 8)", &env), 7.0.into());
    assert_eq!(eval!("(unless true 7 8)", &env), 8.0.into());
    assert_eq!(eval!("(unless2 false 7 8)", &env), 7.0.into());
    assert_eq!(eval!("(unless2 true 7 8)", &env), 8.0.into());
    assert_eq!(eval!("(let* (a 123) (identity a))", &env), 123.0.into());
}

#[test]
fn macroexpand() {
    let env = basic_macros();
    assert_eq!(eval!("(macroexpand (one))", &env), 1.0.into());
    assert_eq!(
        eval!("(macroexpand (unless PRED A B))", &env),
        vector![
            LispValue::symbol_for("if"),
            LispValue::symbol_for("PRED"),
            LispValue::symbol_for("B"),
            LispValue::symbol_for("A"),
        ]
        .into()
    );
    assert_eq!(
        eval!("(macroexpand (unless2 PRED A B))", &env),
        vector![
            LispValue::symbol_for("if"),
            vector![LispValue::symbol_for("not"), LispValue::symbol_for("PRED"),].into(),
            LispValue::symbol_for("A"),
            LispValue::symbol_for("B"),
        ]
        .into()
    );
    assert_eq!(
        eval!("(macroexpand (unless2 2 3 4))", &env),
        vector![
            LispValue::symbol_for("if"),
            vector![LispValue::symbol_for("not"), 2.0.into(),].into(),
            3.0.into(),
            4.0.into(),
        ]
        .into()
    );
    assert_eq!(
        eval!("(let* (a 123) (macroexpand (identity a)))", &env),
        LispValue::symbol_for("a"),
    );
}

#[test]
fn list_functions() {
    assert_eq!(eval!("(nth (list 1) 0)"), 1.0.into());
    assert_eq!(eval!("(nth (list 1 2) 1)"), 2.0.into());
    assert_eq!(eval!("(nth (list 1 2 nil) 2)"), LispValue::nil());
    assert_eq!(eval!("(first (list))"), LispValue::nil());
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
    assert_eq!(eval!("(cond)"), LispValue::nil());
    assert_eq!(eval!("(cond true 7)"), 7.0.into());
    assert_eq!(eval!("(cond false 7)"), LispValue::nil());
    assert_eq!(eval!("(cond true 7 true 8)"), 7.0.into());
    assert_eq!(eval!("(cond false 7 true 8)"), 8.0.into());
    assert_eq!(eval!("(cond false 7 false 8 \"else\" 9)"), 9.0.into());
    assert_eq!(eval!("(cond false 7 false 8 false 9)"), LispValue::nil());
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
    eval!("(def! x 2)", &env);
    eval!("(defmacro! a (fn* [] x))", &env);
    assert_eq!(eval!("(a)", &env), 2.0.into());
    assert_eq!(eval!("(let* (x 3) (a))", &env), 2.0.into());
}
