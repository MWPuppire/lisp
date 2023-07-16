mod common;
use common::*;

#[test]
fn list_functions() {
    assert_eq!(eval!("(cons 1 (list))"), vector![1.0.into(),].into());
    assert_eq!(
        eval!("(cons 1 (list 2))"),
        vector![1.0.into(), 2.0.into(),].into()
    );
    assert_eq!(
        eval!("(cons 1 (list 2 3))"),
        vector![1.0.into(), 2.0.into(), 3.0.into(),].into()
    );
    assert_eq!(
        eval!("(cons (list 1) (list 2 3))"),
        vector![vector![1.0.into()].into(), 2.0.into(), 3.0.into(),].into()
    );
    assert_eq!(eval!("(concat)"), vector![].into());
    assert_eq!(
        eval!("(concat (list 1 2))"),
        vector![1.0.into(), 2.0.into(),].into()
    );
    assert_eq!(
        eval!("(concat (list 1 2) (list 3 4))"),
        vector![1.0.into(), 2.0.into(), 3.0.into(), 4.0.into(),].into()
    );
    assert_eq!(
        eval!("(concat (list 1 2) (list 3 4) (list 5 6))"),
        vector![
            1.0.into(),
            2.0.into(),
            3.0.into(),
            4.0.into(),
            5.0.into(),
            6.0.into(),
        ]
        .into()
    );
    assert_eq!(eval!("(concat (list) (list))"), vector![].into());
    assert_eq!(eval!("(cons 1 [])"), vector![1.0.into(),].into());
    assert_eq!(
        eval!("(cons [1] [2 3])"),
        vector![
            LispValue::vector_from(vec![1.0.into(),]),
            2.0.into(),
            3.0.into(),
        ]
        .into()
    );
    assert_eq!(
        eval!("(concat [1 2] (list 3 4) [5 6])"),
        vector![
            1.0.into(),
            2.0.into(),
            3.0.into(),
            4.0.into(),
            5.0.into(),
            6.0.into(),
        ]
        .into()
    );
}

#[test]
fn quote() {
    assert_eq!(eval!("(quote 7)"), 7.0.into());
    assert_eq!(
        eval!("(quote (1 2 3))"),
        vector![1.0.into(), 2.0.into(), 3.0.into(),].into()
    );
    assert_eq!(
        eval!("(quote (1 2 (3 4)))"),
        vector![
            1.0.into(),
            2.0.into(),
            vector![3.0.into(), 4.0.into(),].into(),
        ]
        .into()
    );
    assert_eq!(eval!("(quote a)"), LispValue::symbol_for("a"));
    assert_eq!(eval!("'7"), 7.0.into());
    assert_eq!(
        eval!("'(1 2 3)"),
        vector![1.0.into(), 2.0.into(), 3.0.into(),].into()
    );
    assert_eq!(
        eval!("'(1 2 (3 4))"),
        vector![
            1.0.into(),
            2.0.into(),
            vector![3.0.into(), 4.0.into(),].into(),
        ]
        .into()
    );
}

#[test]
fn quasiquote() {
    assert_eq!(eval!("(quasiquote nil)"), LispValue::Nil);
    assert_eq!(eval!("(quasiquote 7)"), 7.0.into());
    assert_eq!(eval!("(quasiquote ())"), vector![].into());
    assert_eq!(
        eval!("(quasiquote (1 2))"),
        vector![1.0.into(), 2.0.into(),].into()
    );
    assert_eq!(
        eval!("(quasiquote (1 () 2))"),
        vector![1.0.into(), vector![].into(), 2.0.into(),].into()
    );
    assert_eq!(
        eval!("(quasiquote (()))"),
        vector![vector![].into(),].into()
    );
    assert_eq!(eval!("`7"), 7.0.into());
    assert_eq!(
        eval!("`(1 2 3)"),
        vector![1.0.into(), 2.0.into(), 3.0.into(),].into()
    );
}

#[test]
fn unquote() {
    let env = testing_env();
    eval!("(def! a 8)", &env);
    assert_eq!(eval!("(quasiquote a)", &env), LispValue::symbol_for("a"));
    assert_eq!(eval!("(quasiquote (unquote a))", &env), 8.0.into());
    assert_eq!(
        eval!("(quasiquote (1 a 3))", &env),
        vector![1.0.into(), LispValue::symbol_for("a"), 3.0.into(),].into()
    );
    assert_eq!(
        eval!("(quasiquote (1 (unquote a) 3))", &env),
        vector![1.0.into(), 8.0.into(), 3.0.into(),].into()
    );
    eval!("(def! b (quote (1 \"b\" \"d\")))", &env);
    assert_eq!(
        eval!("(quasiquote (1 b 3))", &env),
        vector![1.0.into(), LispValue::symbol_for("b"), 3.0.into(),].into()
    );
    assert_eq!(
        eval!("(quasiquote (1 (unquote b) 3))", &env),
        vector![
            1.0.into(),
            vector![1.0.into(), "b".to_owned().into(), "d".to_owned().into(),].into(),
            3.0.into(),
        ]
        .into()
    );
    assert_eq!(
        eval!("(quasiquote ((unquote 1) (unquote 2)))", &env),
        vector![1.0.into(), 2.0.into(),].into()
    );
    assert_eq!(
        eval!("(let* (x 0) (quasiquote (unquote x)))", &env),
        0.0.into()
    );
    assert_eq!(eval!("`~7", &env), 7.0.into());
    assert_eq!(
        eval!("`(1 ~a 3)", &env),
        vector![1.0.into(), 8.0.into(), 3.0.into(),].into()
    );
    assert_eq!(
        eval!("`(1 ~b 3)", &env),
        vector![
            1.0.into(),
            vector![1.0.into(), "b".to_owned().into(), "d".to_owned().into(),].into(),
            3.0.into(),
        ]
        .into()
    );
}

#[test]
fn splice_unquote() {
    let env = testing_env();
    eval!("(def! c (quote (1 \"b\" \"d\")))", &env);
    assert_eq!(
        eval!("(quasiquote (1 c 3))", &env),
        vector![1.0.into(), LispValue::symbol_for("c"), 3.0.into(),].into()
    );
    assert_eq!(
        eval!("(quasiquote (1 (splice-unquote c) 3))", &env),
        vector![
            1.0.into(),
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
            3.0.into(),
        ]
        .into()
    );
    assert_eq!(
        eval!("(quasiquote ((splice-unquote c) (splice-unquote c)))", &env),
        vector![
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
        ]
        .into()
    );
    assert_eq!(
        eval!("`(1 ~@c 3)", &env),
        vector![
            1.0.into(),
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
            3.0.into(),
        ]
        .into()
    );
}

#[test]
fn vec() {
    assert_eq!(eval!("(vec (list))"), LispValue::vector_from(vec![]));
    assert_eq!(
        eval!("(vec (list 1 2))"),
        LispValue::vector_from(vec![1.0.into(), 2.0.into(),])
    );
    assert_eq!(eval!("(vec [])"), LispValue::vector_from(vec![]));
}

#[test]
fn quine() {
    let expr = LispParser::parse(
        r#"
    ((fn* (q)
        (quasiquote ((unquote q) (quote (unquote q))))
    ) (quote (fn* (q)
        (quasiquote ((unquote q) (quote (unquote q))))
    )))
    "#,
    )
    .unwrap()
    .unwrap();
    let env = testing_env();
    assert_eq!(&eval(expr.clone(), &env).unwrap(), &expr);
}
