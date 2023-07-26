mod common;
use common::*;

#[test]
fn list_functions() {
    eval_eq!("(cons 1 (list))", vector![1.0.into(),]);
    eval_eq!("(cons 1 (list 2))", vector![1.0.into(), 2.0.into(),]);
    eval_eq!(
        "(cons 1 (list 2 3))",
        vector![1.0.into(), 2.0.into(), 3.0.into(),]
    );
    eval_eq!(
        "(cons (list 1) (list 2 3))",
        vector![vector![1.0.into()].into(), 2.0.into(), 3.0.into(),]
    );
    eval_eq!("(concat)", vector![]);
    eval_eq!("(concat (list 1 2))", vector![1.0.into(), 2.0.into(),]);
    eval_eq!(
        "(concat (list 1 2) (list 3 4))",
        vector![1.0.into(), 2.0.into(), 3.0.into(), 4.0.into(),]
    );
    eval_eq!(
        "(concat (list 1 2) (list 3 4) (list 5 6))",
        vector![
            1.0.into(),
            2.0.into(),
            3.0.into(),
            4.0.into(),
            5.0.into(),
            6.0.into(),
        ]
    );
    eval_eq!("(concat (list) (list))", vector![]);
    eval_eq!("(cons 1 [])", vector![1.0.into(),]);
    eval_eq!(
        "(cons [1] [2 3])",
        vector![
            LispValue::vector_from(vec![1.0.into(),]),
            2.0.into(),
            3.0.into(),
        ]
    );
    eval_eq!(
        "(concat [1 2] (list 3 4) [5 6])",
        vector![
            1.0.into(),
            2.0.into(),
            3.0.into(),
            4.0.into(),
            5.0.into(),
            6.0.into(),
        ]
    );
}

#[test]
fn quote() {
    eval_eq!("(quote 7)", 7.0);
    eval_eq!(
        "(quote (1 2 3))",
        vector![1.0.into(), 2.0.into(), 3.0.into(),]
    );
    eval_eq!(
        "(quote (1 2 (3 4)))",
        vector![
            1.0.into(),
            2.0.into(),
            vector![3.0.into(), 4.0.into(),].into(),
        ]
    );
    eval_eq!("(quote a)", LispValue::symbol_for("a"));
    eval_eq!("'7", 7.0);
    eval_eq!("'(1 2 3)", vector![1.0.into(), 2.0.into(), 3.0.into(),]);
    eval_eq!(
        "'(1 2 (3 4))",
        vector![
            1.0.into(),
            2.0.into(),
            vector![3.0.into(), 4.0.into(),].into(),
        ]
    );
}

#[test]
fn quasiquote() {
    eval_eq!("(quasiquote nil)", LispValue::nil());
    eval_eq!("(quasiquote 7)", 7.0);
    eval_eq!("(quasiquote ())", vector![]);
    eval_eq!("(quasiquote (1 2))", vector![1.0.into(), 2.0.into(),]);
    eval_eq!(
        "(quasiquote (1 () 2))",
        vector![1.0.into(), vector![].into(), 2.0.into(),]
    );
    eval_eq!("(quasiquote (()))", vector![vector![].into(),]);
    eval_eq!("`7", 7.0);
    eval_eq!("`(1 2 3)", vector![1.0.into(), 2.0.into(), 3.0.into(),]);
}

#[test]
fn unquote() {
    let env = testing_env();
    eval!("(def! a 8)", &env);
    eval_eq!("(quasiquote a)", &env, LispValue::symbol_for("a"));
    eval_eq!("(quasiquote (unquote a))", &env, 8.0);
    eval_eq!(
        "(quasiquote (1 a 3))",
        &env,
        vector![1.0.into(), LispValue::symbol_for("a"), 3.0.into(),]
    );
    eval_eq!(
        "(quasiquote (1 (unquote a) 3))",
        &env,
        vector![1.0.into(), 8.0.into(), 3.0.into(),]
    );
    eval!("(def! b (quote (1 \"b\" \"d\")))", &env);
    eval_eq!(
        "(quasiquote (1 b 3))",
        &env,
        vector![1.0.into(), LispValue::symbol_for("b"), 3.0.into(),]
    );
    eval_eq!(
        "(quasiquote (1 (unquote b) 3))",
        &env,
        vector![
            1.0.into(),
            vector![1.0.into(), "b".to_owned().into(), "d".to_owned().into(),].into(),
            3.0.into(),
        ]
    );
    eval_eq!(
        "(quasiquote ((unquote 1) (unquote 2)))",
        &env,
        vector![1.0.into(), 2.0.into(),]
    );
    eval_eq!("(let* (x 0) (quasiquote (unquote x)))", &env, 0.0);
    eval_eq!("`~7", &env, 7.0);
    eval_eq!(
        "`(1 ~a 3)",
        &env,
        vector![1.0.into(), 8.0.into(), 3.0.into(),]
    );
    eval_eq!(
        "`(1 ~b 3)",
        &env,
        vector![
            1.0.into(),
            vector![1.0.into(), "b".to_owned().into(), "d".to_owned().into(),].into(),
            3.0.into(),
        ]
    );
}

#[test]
fn splice_unquote() {
    let env = testing_env();
    eval!("(def! c (quote (1 \"b\" \"d\")))", &env);
    eval_eq!(
        "(quasiquote (1 c 3))",
        &env,
        vector![1.0.into(), LispValue::symbol_for("c"), 3.0.into(),]
    );
    eval_eq!(
        "(quasiquote (1 (splice-unquote c) 3))",
        &env,
        vector![
            1.0.into(),
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
            3.0.into(),
        ]
    );
    eval_eq!(
        "(quasiquote ((splice-unquote c) (splice-unquote c)))",
        &env,
        vector![
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
        ]
    );
    eval_eq!(
        "`(1 ~@c 3)",
        &env,
        vector![
            1.0.into(),
            1.0.into(),
            "b".to_owned().into(),
            "d".to_owned().into(),
            3.0.into(),
        ]
    );
}

#[test]
fn vec() {
    eval_eq!("(vec (list))", LispValue::vector_from(vec![]));
    eval_eq!(
        "(vec (list 1 2))",
        LispValue::vector_from(vec![1.0.into(), 2.0.into(),])
    );
    eval_eq!("(vec [])", LispValue::vector_from(vec![]));
}

#[test]
fn quine() {
    let expr = parse(
        r#"
    ((fn* (q)
        (quasiquote ((unquote q) (quote (unquote q))))
    ) (quote (fn* (q)
        (quasiquote ((unquote q) (quote (unquote q))))
    )))
    "#,
    );
    let env = testing_env();
    assert_eq!(eval(expr.clone(), &env).unwrap(), expr);
}
