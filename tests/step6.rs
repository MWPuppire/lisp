mod common;
use common::*;

#[inline]
fn test_inspect(expr: &str) {
    let formatted = format!("(eval (read-string (inspect {})))", expr);
    assert_eq!(eval!(expr), eval!(&formatted))
}

#[test]
fn inspect_outputs_code() {
    test_inspect("7");
    test_inspect("'a");
    test_inspect("[1 2 3 4]");
    test_inspect("['a 'b]");
    test_inspect("(list 1 2 3)");
    test_inspect("{:a 9 :b (+ 2 3)}");
    test_inspect("'(+ 2 3)");
}

#[test]
fn read_string() {
    eval_eq!(
        "(read-string \"(1 2 (3 4) nil)\")",
        vector![
            1.0.into(),
            2.0.into(),
            vector![3.0.into(), 4.0.into(),].into(),
            LispValue::nil(),
        ]
    );
    eval_eq!("(read-string \"nil\")", LispValue::nil());
    eval_eq!(
        "(read-string \"(+ 2 3)\")",
        vector![LispValue::symbol_for("+"), 2.0.into(), 3.0.into(),]
    );
    eval_eq!("(read-string \"7 ;; comment\")", 7.0);
    eval_eq!("(read-string \";; comment\")", LispValue::nil());
}

#[test]
fn test_eval() {
    eval_eq!("(eval (list + 2 3))", 5.0);
    eval_eq!("(eval (read-string \"(+ 2 3)\"))", 5.0);
}

#[test]
fn test_slurp() {
    eval_eq!("(slurp \"test.txt\")", "A line of text".to_owned());
}

#[test]
fn test_load_file() {
    let env = testing_env();
    eval!("(load-file \"inc.mal\")", &env);
    eval_eq!("(inc1 7)", &env, 8.0);
    eval_eq!("(inc2 7)", &env, 9.0);
    eval_eq!("(inc3 9)", &env, 12.0);
    eval!("(load-file \"computations.mal\")", &env);
    eval_eq!("(sumdown 2)", &env, 3.0);
    eval_eq!("(fib 2)", &env, 1.0);
    eval!("(load-file \"incB.mal\")", &env);
    eval_eq!("(inc4 7)", &env, 11.0);
    eval_eq!("(inc5 7)", &env, 12.0);
    eval!("(load-file \"incC.mal\")", &env);
    eval_eq!(
        "mymap",
        &env,
        hashmap! {
            "a".to_owned().into() => 1.0.into(),
        }
    );
}

#[test]
fn test_atom() {
    let env = testing_env();
    eval!("(def! inc3 (fn* (a) (+ 3 a)))", &env);
    eval!("(def! a (atom 2))", &env);
    eval_eq!("(atom? a)", &env, true);
    eval_eq!("(atom? 1)", &env, false);
    eval_eq!("(deref a)", &env, 2.0);
    eval!("(reset! a 3)", &env);
    eval_eq!("@a", &env, 3.0);
    eval!("(swap! a inc3)", &env);
    eval_eq!("@a", &env, 6.0);
    eval!("(swap! a (fn* (a) (* 2 a)))", &env);
    eval_eq!("@a", &env, 12.0);
    eval!("(swap! a (fn* (a b) (* a b)) 10)", &env);
    eval_eq!("@a", &env, 120.0);
    eval!("(swap! a + 3)", &env);
    eval_eq!("@a", &env, 123.0);
}

#[test]
fn atom_closure() {
    let env = testing_env();
    eval!("(def! inc-it (fn* (a) (+ 1 a)))", &env);
    eval!("(def! atm (atom 7))", &env);
    eval!("(def! f (fn* () (swap! atm inc-it)))", &env);
    eval_eq!("(f)", &env, 8.0);
    eval_eq!("(f)", &env, 9.0);
    eval!("(def! g (let* (atm (atom 0)) (fn* () @atm)))", &env);
    eval_eq!("(f)", &env, 10.0);
    eval_eq!("(g)", &env, 0.0);
}
