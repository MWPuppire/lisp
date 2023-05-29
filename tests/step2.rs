mod common;
use common::*;

#[test]
fn arithmetic() {
    assert_eq!(eval!("(+ 1 2)"), 3.0.into());
    assert_eq!(eval!("(+ 5 (* 2 3))"), 11.0.into());
    assert_eq!(eval!("(- (+ 5 (* 2 3)) 3)"), 8.0.into());
    assert_eq!(eval!("(/ (- (+ 5 (* 2 3)) 3) 4)"), 2.0.into());
    assert_eq!(eval!("(/ (- (+ 515 (* 87 311)) 302) 27)"), 1010.0.into());
    assert_eq!(eval!("(* -3 6)"), (-18.0).into());
    assert_eq!(eval!("(/ (- (+ 515 (* -87 311)) 296) 27)"), (-994.0).into());
}

#[test]
#[should_panic]
fn fail_undefined_func() {
    eval!("(abc 1 2 3)");
}

#[test]
fn empty_collection_nop() {
    assert_eq!(eval!("()"), vector![].into());
    assert_eq!(eval!("[]"), LispValue::vector_from(vec![]));
    assert_eq!(eval!("{}"), hashmap! {}.into());
}

#[test]
fn evaluate_in_collections() {
    assert_eq!(
        eval!("[1 2 (+ 1 2)]"),
        LispValue::vector_from(vec![1.0.into(), 2.0.into(), 3.0.into(),])
    );
    assert_eq!(
        eval!("{\"a\" (+ 7 8)}"),
        hashmap! {
            "a".to_owned().into() => 15.0.into(),
        }
        .into()
    );
    assert_eq!(
        eval!("{:a (+ 7 8)}"),
        hashmap! {
            LispValue::keyword_for("a".to_owned()) => 15.0.into(),
        }
        .into()
    );
}
