mod common;
use common::*;

#[inline]
fn test_inspect(expr: &str) -> bool {
    let formatted = format!("(eval (read-string (inspect {})))", expr);
    eval!(expr) == eval!(&formatted)
}

#[test]
fn inspect_outputs_code() {
    assert!(test_inspect("7"));
    assert!(test_inspect("'a"));
    assert!(test_inspect("[1 2 3 4]"));
    assert!(test_inspect("['a 'b]"));
    assert!(test_inspect("(list 1 2 3)"));
    assert!(test_inspect("{:a 9 :b (+ 2 3)}"));
    assert!(test_inspect("'(+ 2 3)"));
}

#[test]
fn read_string() {
    assert_eq!(
        eval!("(read-string \"(1 2 (3 4) nil)\")"),
        vector![
            1.0.into(),
            2.0.into(),
            vector![3.0.into(), 4.0.into(),].into(),
            LispValue::Nil,
        ]
        .into()
    );
    assert_eq!(eval!("(read-string \"nil\")"), LispValue::Nil);
    assert_eq!(
        eval!("(read-string \"(+ 2 3)\")"),
        vector![LispValue::symbol_for("+"), 2.0.into(), 3.0.into(),].into()
    );
    assert_eq!(eval!("(read-string \"7 ;; comment\")"), 7.0.into());
    assert_eq!(eval!("(read-string \";; comment\")"), LispValue::Nil);
}

#[test]
fn test_eval() {
    assert_eq!(eval!("(eval (list + 2 3))"), 5.0.into());
    assert_eq!(eval!("(eval (read-string \"(+ 2 3)\"))"), 5.0.into());
}

#[test]
fn test_eval_global_scope() {
    assert_eq!(
        eval!("(let* (b 12) (do (eval (read-string \"(def! aa 7)\")) aa))"),
        7.0.into()
    );
    let env = testing_env();
    eval!("(def! a 1)", &env);
    assert_eq!(
        eval!("(let* (a 2) (eval (read-string \"a\")))", &env),
        1.0.into()
    );
}

#[test]
fn test_slurp() {
    assert_eq!(
        eval!("(slurp \"test.txt\")"),
        "A line of text".to_owned().into()
    );
}

#[test]
fn test_load_file() {
    let env = testing_env();
    eval!("(load-file \"inc.mal\")", &env);
    assert_eq!(eval!("(inc1 7)", &env), 8.0.into());
    assert_eq!(eval!("(inc2 7)", &env), 9.0.into());
    assert_eq!(eval!("(inc3 9)", &env), 12.0.into());
    eval!("(load-file \"computations.mal\")", &env);
    assert_eq!(eval!("(sumdown 2)", &env), 3.0.into());
    assert_eq!(eval!("(fib 2)", &env), 1.0.into());
    eval!("(load-file \"incB.mal\")", &env);
    assert_eq!(eval!("(inc4 7)", &env), 11.0.into());
    assert_eq!(eval!("(inc5 7)", &env), 12.0.into());
    eval!("(load-file \"incC.mal\")", &env);
    assert_eq!(
        eval!("mymap", &env),
        hashmap! {
            "a".to_owned().into() => 1.0.into(),
        }
        .into()
    );
}

#[test]
fn test_atom() {
    let env = testing_env();
    eval!("(def! inc3 (fn* (a) (+ 3 a)))", &env);
    eval!("(def! a (atom 2))", &env);
    assert_eq!(eval!("(atom? a)", &env), true.into());
    assert_eq!(eval!("(atom? 1)", &env), false.into());
    assert_eq!(eval!("(deref a)", &env), 2.0.into());
    eval!("(reset! a 3)", &env);
    assert_eq!(eval!("@a", &env), 3.0.into());
    eval!("(swap! a inc3)", &env);
    assert_eq!(eval!("@a", &env), 6.0.into());
    eval!("(swap! a (fn* (a) (* 2 a)))", &env);
    assert_eq!(eval!("@a", &env), 12.0.into());
    eval!("(swap! a (fn* (a b) (* a b)) 10)", &env);
    assert_eq!(eval!("@a", &env), 120.0.into());
    eval!("(swap! a + 3)", &env);
    assert_eq!(eval!("@a", &env), 123.0.into());
}

#[test]
fn atom_closure() {
    let env = testing_env();
    eval!("(def! inc-it (fn* (a) (+ 1 a)))", &env);
    eval!("(def! atm (atom 7))", &env);
    eval!("(def! f (fn* () (swap! atm inc-it)))", &env);
    assert_eq!(eval!("(f)", &env), 8.0.into());
    assert_eq!(eval!("(f)", &env), 9.0.into());
    eval!("(def! g (let* (atm (atom 0)) (fn* () @atm)))", &env);
    assert_eq!(eval!("(f)", &env), 10.0.into());
    assert_eq!(eval!("(g)", &env), 0.0.into());
}
