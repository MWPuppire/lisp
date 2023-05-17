mod common;
use common::*;

#[test]
fn list_functions() {
    assert_eq!(eval!("(cons 1 (list))"), LispValue::list_from(vector![
        1.0.into(),
    ]));
    assert_eq!(eval!("(cons 1 (list 2))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
    ]));
    assert_eq!(eval!("(cons 1 (list 2 3))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        3.0.into(),
    ]));
    assert_eq!(eval!("(cons (list 1) (list 2 3))"), LispValue::list_from(vector![
        LispValue::list_from(vector![1.0.into()]),
        2.0.into(),
        3.0.into(),
    ]));
    assert_eq!(eval!("(concat)"), LispValue::list_from(vector![]));
    assert_eq!(eval!("(concat (list 1 2))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
    ]));
    assert_eq!(eval!("(concat (list 1 2) (list 3 4))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        3.0.into(),
        4.0.into(),
    ]));
    assert_eq!(eval!("(concat (list 1 2) (list 3 4) (list 5 6))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        3.0.into(),
        4.0.into(),
        5.0.into(),
        6.0.into(),
    ]));
    assert_eq!(eval!("(concat (list) (list))"), LispValue::list_from(vector![]));
    assert_eq!(eval!("(cons 1 [])"), LispValue::list_from(vector![
        1.0.into(),
    ]));
    assert_eq!(eval!("(cons [1] [2 3])"), LispValue::list_from(vector![
        LispValue::vector_from(vec![
            1.0.into(),
        ]),
        2.0.into(),
        3.0.into(),
    ]));
    assert_eq!(eval!("(concat [1 2] (list 3 4) [5 6])"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        3.0.into(),
        4.0.into(),
        5.0.into(),
        6.0.into(),
    ]));
}

#[test]
fn quote() {
    assert_eq!(eval!("(quote 7)"), 7.0.into());
    assert_eq!(eval!("(quote (1 2 3))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        3.0.into(),
    ]));
    assert_eq!(eval!("(quote (1 2 (3 4)))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        LispValue::list_from(vector![
            3.0.into(),
            4.0.into(),
        ]),
    ]));
    assert_eq!(eval!("(quote a)"), LispValue::symbol_for_static("a"));
    assert_eq!(eval!("'7"), 7.0.into());
    assert_eq!(eval!("'(1 2 3)"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        3.0.into(),
    ]));
    assert_eq!(eval!("'(1 2 (3 4))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        LispValue::list_from(vector![
            3.0.into(),
            4.0.into(),
        ]),
    ]));
}

#[test]
fn quasiquote() {
    assert_eq!(eval!("(quasiquote nil)"), LispValue::Nil);
    assert_eq!(eval!("(quasiquote 7)"), 7.0.into());
    assert_eq!(eval!("(quasiquote ())"), LispValue::list_from(vector![]));
    assert_eq!(eval!("(quasiquote (1 2))"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
    ]));
    assert_eq!(eval!("(quasiquote (1 () 2))"), LispValue::list_from(vector![
        1.0.into(),
        LispValue::list_from(vector![]),
        2.0.into(),
    ]));
    assert_eq!(eval!("(quasiquote (()))"), LispValue::list_from(vector![
        LispValue::list_from(vector![]),
    ]));
    assert_eq!(eval!("`7"), 7.0.into());
    assert_eq!(eval!("`(1 2 3)"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        3.0.into(),
    ]));
}

#[test]
fn unquote() {
    let mut env = testing_env();
    eval!("(def! a 8)", &mut env);
    assert_eq!(eval!("(quasiquote a)", &mut env), LispValue::symbol_for_static("a"));
    assert_eq!(eval!("(quasiquote (unquote a))", &mut env), 8.0.into());
    assert_eq!(eval!("(quasiquote (1 a 3))", &mut env), LispValue::list_from(vector![
        1.0.into(),
        LispValue::symbol_for_static("a"),
        3.0.into(),
    ]));
    assert_eq!(eval!("(quasiquote (1 (unquote a) 3))", &mut env), LispValue::list_from(vector![
        1.0.into(),
        8.0.into(),
        3.0.into(),
    ]));
    eval!("(def! b (quote (1 \"b\" \"d\")))", &mut env);
    assert_eq!(eval!("(quasiquote (1 b 3))", &mut env), LispValue::list_from(vector![
        1.0.into(),
        LispValue::symbol_for_static("b"),
        3.0.into(),
    ]));
    assert_eq!(eval!("(quasiquote (1 (unquote b) 3))", &mut env), LispValue::list_from(vector![
        1.0.into(),
        LispValue::list_from(vector![
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
        ]),
        3.0.into(),
    ]));
    assert_eq!(eval!("(quasiquote ((unquote 1) (unquote 2)))", &mut env), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
    ]));
    assert_eq!(eval!("(let* (x 0) (quasiquote (unquote x)))", &mut env), 0.0.into());
    assert_eq!(eval!("`~7", &mut env), 7.0.into());
    assert_eq!(eval!("`(1 ~a 3)", &mut env), LispValue::list_from(vector![
        1.0.into(),
        8.0.into(),
        3.0.into(),
    ]));
    assert_eq!(eval!("`(1 ~b 3)", &mut env), LispValue::list_from(vector![
        1.0.into(),
        LispValue::list_from(vector![
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
        ]),
        3.0.into(),
    ]));
}

#[test]
fn splice_unquote() {
    let mut env = testing_env();
    eval!("(def! c (quote (1 \"b\" \"d\")))", &mut env);
    assert_eq!(eval!("(quasiquote (1 c 3))", &mut env), LispValue::list_from(vector![
        1.0.into(),
        LispValue::symbol_for_static("c"),
        3.0.into(),
    ]));
    assert_eq!(eval!("(quasiquote (1 (splice-unquote c) 3))", &mut env), LispValue::list_from(vector![
        1.0.into(),
        1.0.into(),
        "b".to_owned().into(),
        "d".to_owned().into(),
        3.0.into(),
    ]));
    assert_eq!(eval!("(quasiquote ((splice-unquote c) (splice-unquote c)))", &mut env), LispValue::list_from(vector![
        1.0.into(),
        "b".to_owned().into(),
        "d".to_owned().into(),
        1.0.into(),
        "b".to_owned().into(),
        "d".to_owned().into(),
    ]));
    assert_eq!(eval!("`(1 ~@c 3)", &mut env), LispValue::list_from(vector![
        1.0.into(),
        1.0.into(),
        "b".to_owned().into(),
        "d".to_owned().into(),
        3.0.into(),
    ]));
}

#[test]
fn vec() {
    assert_eq!(eval!("(vec (list))"), LispValue::vector_from(vec![]));
    assert_eq!(eval!("(vec (list 1 2))"), LispValue::vector_from(vec![
        1.0.into(),
        2.0.into(),
    ]));
    assert_eq!(eval!("(vec [])"), LispValue::vector_from(vec![]));
}

#[test]
fn quine() {
    let expr = LispParser::parse(r#"
    ((fn* (q)
        (quasiquote ((unquote q) (quote (unquote q))))
    ) (quote (fn* (q)
        (quasiquote ((unquote q) (quote (unquote q))))
    )))
    "#).unwrap().unwrap();
    let mut env = testing_env();
    assert_eq!(&eval(expr.clone(), &mut env).unwrap(), &expr);
}
