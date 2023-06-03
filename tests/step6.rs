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
        vector![LispValue::symbol_for_static("+"), 2.0.into(), 3.0.into(),].into()
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
    let mut lock = env.write();
    eval!("(def! a 1)", &mut lock);
    assert_eq!(
        eval!("(let* (a 2) (eval (read-string \"a\")))", &mut lock),
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
    let mut lock = env.write();
    eval!("(load-file \"inc.mal\")", &mut lock);
    assert_eq!(eval!("(inc1 7)", &mut lock), 8.0.into());
    assert_eq!(eval!("(inc2 7)", &mut lock), 9.0.into());
    assert_eq!(eval!("(inc3 9)", &mut lock), 12.0.into());
    eval!("(load-file \"computations.mal\")", &mut lock);
    assert_eq!(eval!("(sumdown 2)", &mut lock), 3.0.into());
    assert_eq!(eval!("(fib 2)", &mut lock), 1.0.into());
    eval!("(load-file \"incB.mal\")", &mut lock);
    assert_eq!(eval!("(inc4 7)", &mut lock), 11.0.into());
    assert_eq!(eval!("(inc5 7)", &mut lock), 12.0.into());
    eval!("(load-file \"incC.mal\")", &mut lock);
    assert_eq!(
        eval!("mymap", &mut lock),
        hashmap! {
            "a".to_owned().into() => 1.0.into(),
        }
        .into()
    );
}

#[test]
fn test_atom() {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(def! inc3 (fn* (a) (+ 3 a)))", &mut lock);
    eval!("(def! a (atom 2))", &mut lock);
    assert_eq!(eval!("(atom? a)", &mut lock), true.into());
    assert_eq!(eval!("(atom? 1)", &mut lock), false.into());
    assert_eq!(eval!("(deref a)", &mut lock), 2.0.into());
    eval!("(reset! a 3)", &mut lock);
    assert_eq!(eval!("@a", &mut lock), 3.0.into());
    eval!("(swap! a inc3)", &mut lock);
    assert_eq!(eval!("@a", &mut lock), 6.0.into());
    eval!("(swap! a (fn* (a) (* 2 a)))", &mut lock);
    assert_eq!(eval!("@a", &mut lock), 12.0.into());
    eval!("(swap! a (fn* (a b) (* a b)) 10)", &mut lock);
    assert_eq!(eval!("@a", &mut lock), 120.0.into());
    eval!("(swap! a + 3)", &mut lock);
    assert_eq!(eval!("@a", &mut lock), 123.0.into());
}

#[test]
fn atom_closure() {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(def! inc-it (fn* (a) (+ 1 a)))", &mut lock);
    eval!("(def! atm (atom 7))", &mut lock);
    eval!("(def! f (fn* () (swap! atm inc-it)))", &mut lock);
    assert_eq!(eval!("(f)", &mut lock), 8.0.into());
    assert_eq!(eval!("(f)", &mut lock), 9.0.into());
    eval!("(def! g (let* (atm (atom 0)) (fn* () @atm)))", &mut lock);
    assert_eq!(eval!("(f)", &mut lock), 10.0.into());
    assert_eq!(eval!("(g)", &mut lock), 0.0.into());
}
