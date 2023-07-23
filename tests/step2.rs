mod common;
use common::*;

#[test]
fn arithmetic() {
    eval_eq!("(+ 1 2)", 3.0);
    eval_eq!("(+ 5 (* 2 3))", 11.0);
    eval_eq!("(- (+ 5 (* 2 3)) 3)", 8.0);
    eval_eq!("(/ (- (+ 5 (* 2 3)) 3) 4)", 2.0);
    eval_eq!("(/ (- (+ 515 (* 87 311)) 302) 27)", 1010.0);
    eval_eq!("(* -3 6)", (-18.0));
    eval_eq!("(/ (- (+ 515 (* -87 311)) 296) 27)", (-994.0));
}

#[test]
#[should_panic]
fn fail_undefined_func() {
    eval!("(abc 1 2 3)");
}

#[test]
fn empty_collection_nop() {
    eval_eq!("()", vector![]);
    eval_eq!("[]", LispValue::vector_from(vec![]));
    eval_eq!("{}", hashmap! {});
}

#[test]
fn evaluate_in_collections() {
    eval_eq!(
        "[1 2 (+ 1 2)]",
        LispValue::vector_from(vec![1.0.into(), 2.0.into(), 3.0.into(),])
    );
    eval_eq!(
        "{\"a\" (+ 7 8)}",
        hashmap! {
            "a".to_owned().into() => 15.0.into(),
        }
    );
    eval_eq!(
        "{:a (+ 7 8)}",
        hashmap! {
            LispValue::keyword_for("a".to_owned()) => 15.0.into(),
        }
    );
}
