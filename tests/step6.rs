mod common;
use common::*;

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
    assert!(test_inspect("'(+ 2 3)"))
}

#[test]
fn read_string() {
    assert_eq!(eval!("(read-string \"(1 2 (3 4) nil)\")"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::List(vector![
            LispValue::Number(3.0),
            LispValue::Number(4.0),
        ]),
        LispValue::Nil,
    ]));
    assert_eq!(eval!("(read-string \"nil\")"), LispValue::Nil);
    assert_eq!(eval!("(read-string \"(+ 2 3)\")"), LispValue::List(vector![
        LispValue::symbol_for_static("+"),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(read-string \"7 ;; comment\")"), LispValue::Number(7.0));
    assert_eq!(eval!("(read-string \";; comment\")"), LispValue::Nil);
}

#[test]
fn test_eval() {
    assert_eq!(eval!("(eval (list + 2 3))"), LispValue::Number(5.0));
    assert_eq!(eval!("(eval (read-string \"(+ 2 3)\"))"), LispValue::Number(5.0));
}

#[test]
fn test_eval_global_scope() {
    assert_eq!(eval!("(let* (b 12) (do (eval (read-string \"(def! aa 7)\")) aa))"), LispValue::Number(7.0));
    let mut env = testing_env();
    eval!("(def! a 1)", &mut env);
    assert_eq!(eval!("(let* (a 2) (eval (read-string \"a\")))", &mut env), LispValue::Number(1.0));
}

#[test]
fn test_slurp() {
    assert_eq!(eval!("(slurp \"test.txt\")"), LispValue::String(
        "A line of text".to_string()
    ));
}

#[test]
fn test_load_file() {
    let mut env = testing_env();
    eval!("(load-file \"inc.mal\")", &mut env);
    assert_eq!(eval!("(inc1 7)", &mut env), LispValue::Number(8.0));
    assert_eq!(eval!("(inc2 7)", &mut env), LispValue::Number(9.0));
    assert_eq!(eval!("(inc3 9)", &mut env), LispValue::Number(12.0));
    eval!("(load-file \"computations.mal\")", &mut env);
    assert_eq!(eval!("(sumdown 2)", &mut env), LispValue::Number(3.0));
    assert_eq!(eval!("(fib 2)", &mut env), LispValue::Number(1.0));
    eval!("(load-file \"incB.mal\")", &mut env);
    assert_eq!(eval!("(inc4 7)", &mut env), LispValue::Number(11.0));
    assert_eq!(eval!("(inc5 7)", &mut env), LispValue::Number(12.0));
    eval!("(load-file \"incC.mal\")", &mut env);
    assert_eq!(eval!("mymap", &mut env), LispValue::Map(hashmap!{
        LispValue::String("a".to_owned()) => LispValue::Number(1.0),
    }));
}

#[test]
fn test_atom() {
    let mut env = testing_env();
    eval!("(def! inc3 (fn* (a) (+ 3 a)))", &mut env);
    eval!("(def! a (atom 2))", &mut env);
    assert_eq!(eval!("(atom? a)", &mut env), LispValue::Bool(true));
    assert_eq!(eval!("(atom? 1)", &mut env), LispValue::Bool(false));
    assert_eq!(eval!("(deref a)", &mut env), LispValue::Number(2.0));
    eval!("(reset! a 3)", &mut env);
    assert_eq!(eval!("@a", &mut env), LispValue::Number(3.0));
    eval!("(swap! a inc3)", &mut env);
    assert_eq!(eval!("@a", &mut env), LispValue::Number(6.0));
    eval!("(swap! a (fn* (a) (* 2 a)))", &mut env);
    assert_eq!(eval!("@a", &mut env), LispValue::Number(12.0));
    eval!("(swap! a (fn* (a b) (* a b)) 10)", &mut env);
    assert_eq!(eval!("@a", &mut env), LispValue::Number(120.0));
    eval!("(swap! a + 3)", &mut env);
    assert_eq!(eval!("@a", &mut env), LispValue::Number(123.0));
}

#[test]
fn atom_closure() {
    let mut env = testing_env();
    eval!("(def! inc-it (fn* (a) (+ 1 a)))", &mut env);
    eval!("(def! atm (atom 7))", &mut env);
    eval!("(def! f (fn* () (swap! atm inc-it)))", &mut env);
    assert_eq!(eval!("(f)", &mut env), LispValue::Number(8.0));
    assert_eq!(eval!("(f)", &mut env), LispValue::Number(9.0));
    eval!("(def! g (let* (atm (atom 0)) (fn* () @atm)))", &mut env);
    assert_eq!(eval!("(f)", &mut env), LispValue::Number(10.0));
    assert_eq!(eval!("(g)", &mut env), LispValue::Number(0.0));
}
