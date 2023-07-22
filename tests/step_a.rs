mod common;
use common::*;

#[test]
fn hashmap_atom() {
    let env = testing_env();
    eval!("(def! e (atom {\"+\" +}))", &env);
    eval!("(swap! e assoc \"-\" -)", &env);
    assert_eq!(eval!("((get @e \"+\") 7 8)", &env), 15.0.into());
    assert_eq!(eval!("((get @e \"-\") 11 8)", &env), 3.0.into());
    eval!("(swap! e assoc \"foo\" (list))", &env);
    assert_eq!(eval!("(get @e \"foo\")", &env), vector![].into());
    eval!("(swap! e assoc \"bar\" '(1 2 3))", &env);
    assert_eq!(
        eval!("(get @e \"bar\")", &env),
        vector![1.0.into(), 2.0.into(), 3.0.into()].into()
    );
}

#[test]
fn metadata() {
    let env = testing_env();
    assert_eq!(eval!("(meta (fn* (a) a))"), LispValue::Nil);
    assert_eq!(
        eval!("(meta (with-meta (fn* (a) a) {\"b\" 1}))"),
        hashmap! {
            "b".to_owned().into() => 1.0.into(),
        }
        .into()
    );
    assert_eq!(
        eval!("(meta (with-meta (fn* (a) a) \"abc\"))"),
        "abc".to_owned().into()
    );
    eval!("(def! l-wm (with-meta (fn* (a) a) {\"b\" 2}))", &env);
    assert_eq!(
        eval!("(meta l-wm)", &env),
        hashmap! {
            "b".to_owned().into() => 2.0.into(),
        }
        .into()
    );
    assert_eq!(
        eval!("(meta (with-meta l-wm {\"new_meta\" 123}))", &env),
        hashmap! {
            "new_meta".to_owned().into() => 123.0.into(),
        }
        .into()
    );
    assert_eq!(
        eval!("(meta l-wm)", &env),
        hashmap! {
            "b".to_owned().into() => 2.0.into(),
        }
        .into()
    );
    assert_eq!(eval!("(meta +)"), LispValue::Nil);
}

#[test]
fn seq() {
    assert_eq!(
        eval!("(seq \"abc\")"),
        vector![
            "a".to_owned().into(),
            "b".to_owned().into(),
            "c".to_owned().into()
        ]
        .into()
    );
    assert_eq!(
        eval!("(apply str (seq \"this is a test\"))"),
        "this is a test".to_owned().into()
    );
    assert_eq!(
        eval!("(seq '(2 3 4))"),
        vector![2.0.into(), 3.0.into(), 4.0.into()].into()
    );
}
