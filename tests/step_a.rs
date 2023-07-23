mod common;
use common::*;

#[test]
fn hashmap_atom() {
    let env = testing_env();
    eval!("(def! e (atom {\"+\" +}))", &env);
    eval!("(swap! e assoc \"-\" -)", &env);
    eval_eq!("((get @e \"+\") 7 8)", &env, 15.0);
    eval_eq!("((get @e \"-\") 11 8)", &env, 3.0);
    eval!("(swap! e assoc \"foo\" (list))", &env);
    eval_eq!("(get @e \"foo\")", &env, vector![]);
    eval!("(swap! e assoc \"bar\" '(1 2 3))", &env);
    eval_eq!(
        "(get @e \"bar\")",
        &env,
        vector![1.0.into(), 2.0.into(), 3.0.into()]
    );
}

#[test]
fn metadata() {
    let env = testing_env();
    eval_eq!("(meta (fn* (a) a))", LispValue::Nil);
    eval_eq!(
        "(meta (with-meta (fn* (a) a) {\"b\" 1}))",
        hashmap! {
            "b".to_owned().into() => 1.0.into(),
        }
    );
    eval_eq!("(meta (with-meta (fn* (a) a) \"abc\"))", "abc".to_owned());
    eval!("(def! l-wm (with-meta (fn* (a) a) {\"b\" 2}))", &env);
    eval_eq!(
        "(meta l-wm)",
        &env,
        hashmap! {
            "b".to_owned().into() => 2.0.into(),
        }
    );
    eval_eq!(
        "(meta (with-meta l-wm {\"new_meta\" 123}))",
        &env,
        hashmap! {
            "new_meta".to_owned().into() => 123.0.into(),
        }
    );
    eval_eq!(
        "(meta l-wm)",
        &env,
        hashmap! {
            "b".to_owned().into() => 2.0.into(),
        }
    );
    eval_eq!("(meta +)", LispValue::Nil);
}

#[test]
fn seq() {
    eval_eq!(
        "(seq \"abc\")",
        vector![
            "a".to_owned().into(),
            "b".to_owned().into(),
            "c".to_owned().into()
        ]
    );
    eval_eq!(
        "(apply str (seq \"this is a test\"))",
        "this is a test".to_owned()
    );
    eval_eq!(
        "(seq '(2 3 4))",
        vector![2.0.into(), 3.0.into(), 4.0.into()]
    );
}
