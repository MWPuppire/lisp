mod common;
use common::*;

#[test]
#[should_panic]
fn test_throw() {
    eval!("(throw \"err1\")");
}

#[test]
fn try_catch() {
    assert_eq!(eval!("(try* 123 (catch* e 456))"), LispValue::Number(123.0));
    assert_eq!(eval!("(try* abc (catch* exc (str \"error: \" exc)))"),
        LispValue::String("error: undefined variable `abc`".to_string())
    );
    assert_eq!(eval!("(try* (nth () 1) (catch* exc (str \"error: \" exc)))"),
        LispValue::String("error: index 1 out of range".to_string())
    );
    assert_eq!(eval!("(try* (throw \"my exception\") (catch* exc (str \"error: \" exc)))"),
        LispValue::String("error: my exception".to_string())
    );
    assert_eq!(eval!("(try* (do (try* \"t1\" (catch* e \"c1\")) (throw \"e1\")) (catch* e \"c2\"))"),
        LispValue::String("c2".to_string())
    );
    assert_eq!(eval!("(try* (map throw (list \"my err\")) (catch* exc exc))"),
        LispValue::String("my err".to_string())
    );
}

#[test]
fn query_functions() {
    assert_eq!(eval!("(symbol? 'abc)"), LispValue::Bool(true));
    assert_eq!(eval!("(symbol? \"abc\")"), LispValue::Bool(false));
    assert_eq!(eval!("(nil? nil)"), LispValue::Bool(true));
    assert_eq!(eval!("(nil? true)"), LispValue::Bool(false));
    assert_eq!(eval!("(true? true)"), LispValue::Bool(true));
    assert_eq!(eval!("(true? false)"), LispValue::Bool(false));
    assert_eq!(eval!("(false? false)"), LispValue::Bool(true));
    assert_eq!(eval!("(false? true)"), LispValue::Bool(false));
}

#[test]
fn apply() {
    assert_eq!(eval!("(apply + (list 2 3))"), LispValue::Number(5.0));
    assert_eq!(eval!("(apply + 4 (list 2 3))"), LispValue::Number(9.0));
    assert_eq!(eval!("(apply list (list))"), LispValue::List(vector![]));
    assert_eq!(eval!("(apply symbol? (list 'two))"), LispValue::Bool(true));
    assert_eq!(eval!("(apply (fn* (a b) (+ a b)) (list 2 3))"), LispValue::Number(5.0));
}

#[test]
fn map() {
    let mut env = testing_env();
    eval!("(def! nums (list 1 2 3))", &mut env);
    eval!("(def! double (fn* (a) (* 2 a)))", &mut env);
    assert_eq!(eval!("(map double nums)", &mut env), LispValue::List(vector![
        LispValue::Number(2.0),
        LispValue::Number(4.0),
        LispValue::Number(6.0),
    ]));
    assert_eq!(eval!("(map (fn* (x) (symbol? x)) (list 1 (quote two) \"three\"))"), LispValue::List(vector![
        LispValue::Bool(false),
        LispValue::Bool(true),
        LispValue::Bool(false),
    ]));
    assert_eq!(eval!("(= () (map str ()))"), LispValue::Bool(true));
}

#[test]
fn hashmap_functions() {
    assert_eq!(eval!(r#"{"a" 1}"#), LispValue::Map(hashmap!{
        LispValue::String("a".to_owned()) => LispValue::Number(1.0),
    }));
    assert_eq!(eval!(r#"(assoc {} "a" 1)"#), LispValue::Map(hashmap!{
        LispValue::String("a".to_owned()) => LispValue::Number(1.0),
    }));
    assert_eq!(
        eval!(r#"(get (assoc (assoc {"a" 1} "b" 2) "c" 3) "a")"#),
        LispValue::Number(1.0),
    );
    assert_eq!(
        eval!(r#"(get (assoc (assoc {"a" 1} "b" 2) "a" 3) "a")"#),
        LispValue::Number(3.0),
    );
    assert_eq!(eval!("(keys {})"), LispValue::List(vector![]));

    let tested = eval!(r#"(keys {"1" 1 "2" 2})"#);
    // order of hash-map keys isn't guaranteed, so either is acceptable
    assert!(
        tested == LispValue::List(vector![
            LispValue::String("1".to_owned()),
            LispValue::String("2".to_owned()),
        ]) || tested == LispValue::List(vector![
            LispValue::String("2".to_owned()),
            LispValue::String("1".to_owned()),
        ])
    );
}
