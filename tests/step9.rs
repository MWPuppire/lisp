mod common;
use common::*;

#[test]
#[should_panic]
fn test_throw() {
    eval!("(throw \"err1\")");
}

#[test]
fn try_catch() {
    assert_eq!(eval!("(try* 123 (catch* e 456))"), 123.0.into());
    assert_eq!(eval!("(try* abc (catch* exc (str \"error: \" exc)))"),
        "error: undefined variable `abc`".to_owned().into()
    );
    assert_eq!(eval!("(try* (nth () 1) (catch* exc (str \"error: \" exc)))"),
        "error: index 1 out of range".to_owned().into()
    );
    assert_eq!(eval!("(try* (throw \"my exception\") (catch* exc (str \"error: \" exc)))"),
        "error: my exception".to_owned().into()
    );
    assert_eq!(eval!("(try* (do (try* \"t1\" (catch* e \"c1\")) (throw \"e1\")) (catch* e \"c2\"))"),
        "c2".to_owned().into()
    );
    assert_eq!(eval!("(try* (map throw (list \"my err\")) (catch* exc exc))"),
        "my err".to_owned().into()
    );
}

#[test]
fn query_functions() {
    assert_eq!(eval!("(symbol? 'abc)"), true.into());
    assert_eq!(eval!("(symbol? \"abc\")"), false.into());
    assert_eq!(eval!("(nil? nil)"), true.into());
    assert_eq!(eval!("(nil? true)"), false.into());
    assert_eq!(eval!("(true? true)"), true.into());
    assert_eq!(eval!("(true? false)"), false.into());
    assert_eq!(eval!("(false? false)"), true.into());
    assert_eq!(eval!("(false? true)"), false.into());
}

#[test]
fn apply() {
    assert_eq!(eval!("(apply + (list 2 3))"), 5.0.into());
    assert_eq!(eval!("(apply + 4 (list 2 3))"), 9.0.into());
    assert_eq!(eval!("(apply list (list))"), LispValue::list_from(vector![]));
    assert_eq!(eval!("(apply symbol? (list 'two))"), true.into());
    assert_eq!(eval!("(apply (fn* (a b) (+ a b)) (list 2 3))"), 5.0.into());
}

#[test]
fn map() {
    let mut env = testing_env();
    eval!("(def! nums (list 1 2 3))", &mut env);
    eval!("(def! double (fn* (a) (* 2 a)))", &mut env);
    assert_eq!(eval!("(map double nums)", &mut env), LispValue::list_from(vector![
        2.0.into(),
        4.0.into(),
        6.0.into(),
    ]));
    assert_eq!(eval!("(map (fn* (x) (symbol? x)) (list 1 (quote two) \"three\"))"), LispValue::list_from(vector![
        false.into(),
        true.into(),
        false.into(),
    ]));
    assert_eq!(eval!("(= () (map str ()))"), true.into());
}

#[test]
fn hashmap_functions() {
    assert_eq!(eval!(r#"{"a" 1}"#), LispValue::map_from(hashmap!{
        "a".to_owned().into() => 1.0.into(),
    }));
    assert_eq!(eval!(r#"(assoc {} "a" 1)"#), LispValue::map_from(hashmap!{
        "a".to_owned().into() => 1.0.into(),
    }));
    assert_eq!(
        eval!(r#"(get (assoc (assoc {"a" 1} "b" 2) "c" 3) "a")"#),
        1.0.into(),
    );
    assert_eq!(
        eval!(r#"(get (assoc (assoc {"a" 1} "b" 2) "a" 3) "a")"#),
        3.0.into(),
    );
    assert_eq!(eval!("(keys {})"), LispValue::list_from(vector![]));

    let tested = eval!(r#"(keys {"1" 1 "2" 2})"#);
    // order of hash-map keys isn't guaranteed, so either is acceptable
    assert!(
        tested == LispValue::list_from(vector![
            "1".to_owned().into(),
            "2".to_owned().into(),
        ]) || tested == LispValue::list_from(vector![
            "2".to_owned().into(),
            "1".to_owned().into(),
        ])
    );
}
