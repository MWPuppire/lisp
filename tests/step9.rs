mod common;
use common::*;

#[test]
#[should_panic]
fn test_throw() {
    eval!("(throw \"err1\")");
}

#[test]
fn try_catch() {
    eval_eq!("(try* 123 (catch* e 456))", 123.0);
    let hash_inspect = eval!("(inspect 'abc)");
    let hash_str = hash_inspect.expect_string().unwrap();
    eval_eq!(
        "(try* abc (catch* exc (str \"error: \" exc)))",
        // skip the apostrophe at the beginning of the string
        format!("error: undefined variable `{}`", &hash_str[1..])
    );
    eval_eq!(
        "(try* (nth () 1) (catch* exc (str \"error: \" exc)))",
        "error: index 1 out of range".to_owned()
    );
    eval_eq!(
        "(try* (throw \"my exception\") (catch* exc (str \"error: \" exc)))",
        "error: my exception".to_owned()
    );
    eval_eq!(
        "(try* (do (try* \"t1\" (catch* e \"c1\")) (throw \"e1\")) (catch* e \"c2\"))",
        "c2".to_owned()
    );
    eval_eq!(
        "(try* (map throw (list \"my err\")) (catch* exc exc))",
        "my err".to_owned()
    );
}

#[test]
fn query_functions() {
    eval_eq!("(symbol? 'abc)", true);
    eval_eq!("(symbol? \"abc\")", false);
    eval_eq!("(nil? nil)", true);
    eval_eq!("(nil? true)", false);
    eval_eq!("(true? true)", true);
    eval_eq!("(true? false)", false);
    eval_eq!("(false? false)", true);
    eval_eq!("(false? true)", false);
}

#[test]
fn apply() {
    eval_eq!("(apply + (list 2 3))", 5.0);
    eval_eq!("(apply + 4 (list 2 3))", 9.0);
    eval_eq!("(apply list (list))", vector![]);
    eval_eq!("(apply symbol? (list (quote two)))", true);
    eval_eq!("(apply (fn* (a b) (+ a b)) (list 2 3))", 5.0);
}

#[test]
fn map() {
    let env = testing_env();
    eval!("(def! nums (list 1 2 3))", &env);
    eval!("(def! double (fn* (a) (* 2 a)))", &env);
    eval_eq!(
        "(map double nums)",
        &env,
        vector![2.0.into(), 4.0.into(), 6.0.into(),]
    );
    eval_eq!(
        "(map (fn* (x) (symbol? x)) (list 1 (quote two) \"three\"))",
        vector![false.into(), true.into(), false.into(),]
    );
    eval_eq!("(= () (map str ()))", true);
}

#[test]
fn hashmap_functions() {
    eval_eq!(
        r#"{"a" 1}"#,
        hashmap! {
            "a".to_owned().into() => 1.0.into(),
        }
    );
    eval_eq!(
        r#"(assoc {} "a" 1)"#,
        hashmap! {
            "a".to_owned().into() => 1.0.into(),
        }
    );
    eval_eq!(r#"(get (assoc (assoc {"a" 1} "b" 2) "c" 3) "a")"#, 1.0);
    eval_eq!(r#"(get (assoc (assoc {"a" 1} "b" 2) "a" 3) "a")"#, 3.0);
    eval_eq!("(keys {})", vector![]);

    let tested = eval!(r#"(keys {"1" 1 "2" 2})"#);
    // order of hash-map keys isn't specified, so either is acceptable
    assert!(
        tested == vector!["1".to_owned().into(), "2".to_owned().into(),].into()
            || tested == vector!["2".to_owned().into(), "1".to_owned().into(),].into()
    );

    let tested = eval!(r#"(pairs {"1" 1 "2" 2})"#);
    // as above, order isn't specified
    assert!(
        tested
            == vector![
                vector!["1".to_owned().into(), 1.0.into()].into(),
                vector!["2".to_owned().into(), 2.0.into()].into()
            ]
            .into()
            || tested
                == vector![
                    vector!["2".to_owned().into(), 2.0.into()].into(),
                    vector!["1".to_owned().into(), 1.0.into()].into()
                ]
                .into()
    );
}
