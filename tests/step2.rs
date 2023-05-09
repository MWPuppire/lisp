mod common;
use common::*;

#[test]
fn arithmetic() {
    assert_eq!(eval!("(+ 1 2)"), LispValue::Number(3.0));
    assert_eq!(eval!("(+ 5 (* 2 3))"), LispValue::Number(11.0));
    assert_eq!(eval!("(- (+ 5 (* 2 3)) 3)"), LispValue::Number(8.0));
    assert_eq!(eval!("(/ (- (+ 5 (* 2 3)) 3) 4)"), LispValue::Number(2.0));
    assert_eq!(eval!("(/ (- (+ 515 (* 87 311)) 302) 27)"), LispValue::Number(1010.0));
    assert_eq!(eval!("(* -3 6)"), LispValue::Number(-18.0));
    assert_eq!(eval!("(/ (- (+ 515 (* -87 311)) 296) 27)"), LispValue::Number(-994.0));
}

#[test]
#[should_panic]
fn fail_undefined_func() {
    eval!("(abc 1 2 3)");
}

#[test]
fn empty_collection_nop() {
    assert_eq!(eval!("()"), LispValue::List(vector![]));
    assert_eq!(eval!("[]"), LispValue::Vector(vec![]));
    assert_eq!(eval!("{}"), LispValue::Map(HashMap::new()));
}

#[test]
fn evaluate_in_collections() {
    assert_eq!(eval!("[1 2 (+ 1 2)]"), LispValue::Vector(vec![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("{\"a\" (+ 7 8)}"), LispValue::Map(
        HashMap::unit(
            LispValue::String("a".to_string()),
            LispValue::Number(15.0),
        )
    ));
    assert_eq!(eval!("{:a (+ 7 8)}"), LispValue::Map(
        HashMap::unit(
            LispValue::Keyword("a".to_string()),
            LispValue::Number(15.0),
        )
    ));
}
