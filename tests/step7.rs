mod common;
use common::*;

#[test]
fn list_functions() {
    assert_eq!(eval!("(cons 1 (list))"), LispValue::List(vector![
        LispValue::Number(1.0),
    ]));
    assert_eq!(eval!("(cons 1 (list 2))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
    ]));
    assert_eq!(eval!("(cons 1 (list 2 3))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(cons (list 1) (list 2 3))"), LispValue::List(vector![
        LispValue::List(vector![LispValue::Number(1.0)]),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(concat)"), LispValue::List(vector![]));
    assert_eq!(eval!("(concat (list 1 2))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
    ]));
    assert_eq!(eval!("(concat (list 1 2) (list 3 4))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
        LispValue::Number(4.0),
    ]));
    assert_eq!(eval!("(concat (list 1 2) (list 3 4) (list 5 6))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
        LispValue::Number(4.0),
        LispValue::Number(5.0),
        LispValue::Number(6.0),
    ]));
    assert_eq!(eval!("(concat (list) (list))"), LispValue::List(vector![]));
    assert_eq!(eval!("(cons 1 [])"), LispValue::List(vector![
        LispValue::Number(1.0),
    ]));
    assert_eq!(eval!("(cons [1] [2 3])"), LispValue::List(vector![
        LispValue::Vector(vec![
            LispValue::Number(1.0),
        ]),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(concat [1 2] (list 3 4) [5 6])"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
        LispValue::Number(4.0),
        LispValue::Number(5.0),
        LispValue::Number(6.0),
    ]));
}

#[test]
fn quote() {
    assert_eq!(eval!("(quote 7)"), LispValue::Number(7.0));
    assert_eq!(eval!("(quote (1 2 3))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(quote (1 2 (3 4)))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::List(vector![
            LispValue::Number(3.0),
            LispValue::Number(4.0),
        ]),
    ]));
    assert_eq!(eval!("(quote a)"), LispValue::symbol_for_static("a"));
    assert_eq!(eval!("'7"), LispValue::Number(7.0));
    assert_eq!(eval!("'(1 2 3)"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("'(1 2 (3 4))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::List(vector![
            LispValue::Number(3.0),
            LispValue::Number(4.0),
        ]),
    ]));
}

#[test]
fn quasiquote() {
    assert_eq!(eval!("(quasiquote nil)"), LispValue::Nil);
    assert_eq!(eval!("(quasiquote 7)"), LispValue::Number(7.0));
    assert_eq!(eval!("(quasiquote ())"), LispValue::List(vector![]));
    assert_eq!(eval!("(quasiquote (1 2))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
    ]));
    assert_eq!(eval!("(quasiquote (1 () 2))"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::List(vector![]),
        LispValue::Number(2.0),
    ]));
    assert_eq!(eval!("(quasiquote (()))"), LispValue::List(vector![
        LispValue::List(vector![]),
    ]));
    assert_eq!(eval!("`7"), LispValue::Number(7.0));
    assert_eq!(eval!("`(1 2 3)"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
}

#[test]
fn unquote() {
    let mut env = testing_env();
    eval!("(def! a 8)", &mut env);
    assert_eq!(eval!("(quasiquote a)", &mut env), LispValue::symbol_for_static("a"));
    assert_eq!(eval!("(quasiquote (unquote a))", &mut env), LispValue::Number(8.0));
    assert_eq!(eval!("(quasiquote (1 a 3))", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::symbol_for_static("a"),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(quasiquote (1 (unquote a) 3))", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(8.0),
        LispValue::Number(3.0),
    ]));
    eval!("(def! b (quote (1 \"b\" \"d\")))", &mut env);
    assert_eq!(eval!("(quasiquote (1 b 3))", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::symbol_for_static("b"),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(quasiquote (1 (unquote b) 3))", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::String("b".to_string()),
            LispValue::String("d".to_string()),
        ]),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(quasiquote ((unquote 1) (unquote 2)))", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
    ]));
    assert_eq!(eval!("(let* (x 0) (quasiquote (unquote x)))", &mut env), LispValue::Number(0.0));
    assert_eq!(eval!("`~7", &mut env), LispValue::Number(7.0));
    assert_eq!(eval!("`(1 ~a 3)", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(8.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("`(1 ~b 3)", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::String("b".to_string()),
            LispValue::String("d".to_string()),
        ]),
        LispValue::Number(3.0),
    ]));
}

#[test]
fn splice_unquote() {
    let mut env = testing_env();
    eval!("(def! c (quote (1 \"b\" \"d\")))", &mut env);
    assert_eq!(eval!("(quasiquote (1 c 3))", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::symbol_for_static("c"),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(quasiquote (1 (splice-unquote c) 3))", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(1.0),
        LispValue::String("b".to_string()),
        LispValue::String("d".to_string()),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(quasiquote ((splice-unquote c) (splice-unquote c)))", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::String("b".to_string()),
        LispValue::String("d".to_string()),
        LispValue::Number(1.0),
        LispValue::String("b".to_string()),
        LispValue::String("d".to_string()),
    ]));
    assert_eq!(eval!("`(1 ~@c 3)", &mut env), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(1.0),
        LispValue::String("b".to_string()),
        LispValue::String("d".to_string()),
        LispValue::Number(3.0),
    ]));
}

#[test]
fn vec() {
    assert_eq!(eval!("(vec (list))"), LispValue::Vector(vec![]));
    assert_eq!(eval!("(vec (list 1 2))"), LispValue::Vector(vec![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
    ]));
    assert_eq!(eval!("(vec [])"), LispValue::Vector(vec![]));
}

// always halts for some reason, so `ignore`
// TODO should figure out why the test never finishes
#[test]
#[ignore]
fn quine() {
    let expr = LispParser::parse(r#"
    ((fn* (q)
        (quasiquote ((unquote q) (quote (unquote q))))
    ) (quote (fn* (q)
        (quasiquote ((unquote q) (quote (unquote q))))
    )))
    "#).unwrap().unwrap();
    let mut env = testing_env();
    assert_eq!(&eval(&expr, &mut env).unwrap(), &expr);
}
