mod common;
use common::*;

#[test]
fn list_functions() {
    assert_eq!(eval!("(list)"), LispValue::List(vector![]));
    assert_eq!(eval!("(list? (list))"), LispValue::Bool(true));
    assert_eq!(eval!("(empty? (list))"), LispValue::Bool(true));
    assert_eq!(eval!("(empty? (list 1))"), LispValue::Bool(false));
    assert_eq!(eval!("(list 1 2 3)"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("(count (list 1 2 3))"), LispValue::Number(3.0));
    assert_eq!(eval!("(count (list))"), LispValue::Number(0.0));
    assert_eq!(eval!("(count nil)"), LispValue::Number(0.0));
    assert_eq!(eval!("(count [1 2 3])"), LispValue::Number(3.0));
    assert_eq!(eval!("(empty? [])"), LispValue::Bool(true));
    assert_eq!(eval!("(empty? [1 2 3])"), LispValue::Bool(false));
    assert_eq!(eval!("(list? [1 2 3])"), LispValue::Bool(false));
}

#[test]
fn if_form() {
    assert_eq!(eval!("(if true 7 8)"), LispValue::Number(7.0));
    assert_eq!(eval!("(if false 7 8)"), LispValue::Number(8.0));
    assert_eq!(eval!("(if nil 7 8)"), LispValue::Number(8.0));
    assert_eq!(eval!("(if 0 7 8)"), LispValue::Number(7.0));
    assert_eq!(eval!("(if (list) 7 8)"), LispValue::Number(7.0));
}

#[test]
fn if_side_effects() {
    let mut env = testing_env();
    eval!("(if true (def! x 4) (def! x 5))", &mut env);
    assert_eq!(eval!("x", &mut env), LispValue::Number(4.0));
    eval!("(if false (def! x 4) (def! x 5))", &mut env);
    assert_eq!(eval!("x", &mut env), LispValue::Number(5.0));
}

#[test]
fn if_one_path() {
    assert_eq!(eval!("(if false 8)"), LispValue::Nil);
    assert_eq!(eval!("(if nil 8)"), LispValue::Nil);
    assert_eq!(eval!("(if true (+ 1 7))"), LispValue::Number(8.0));
}

#[test]
fn equality() {
    assert_eq!(eval!("(= 2 1)"), LispValue::Bool(false));
    assert_eq!(eval!("(= 1 1)"), LispValue::Bool(true));
    assert_eq!(eval!("(= 1 2)"), LispValue::Bool(false));
    assert_eq!(eval!("(= 2 (+ 1 1))"), LispValue::Bool(true));
    assert_eq!(eval!("(= nil nil)"), LispValue::Bool(true));
    assert_eq!(eval!("(= nil (list))"), LispValue::Bool(false));
    assert_eq!(eval!("(= (list) (list))"), LispValue::Bool(true));
    assert_eq!(eval!("(= (list) ())"), LispValue::Bool(true));
    assert_eq!(eval!("(= true true)"), LispValue::Bool(true));
    assert_eq!(eval!("(= false false)"), LispValue::Bool(true));
    assert_eq!(eval!("(= (list 1 2) (list 1 2))"), LispValue::Bool(true));
    assert_eq!(eval!("(= (list 1 2) (list 1))"), LispValue::Bool(false));
    assert_eq!(eval!("(= \"\" \"\")"), LispValue::Bool(true));
    assert_eq!(eval!("(= \"abc\" \"abc\")"), LispValue::Bool(true));
    assert_eq!(eval!("(= \"abc\" \"\")"), LispValue::Bool(false));
    assert_eq!(eval!("(= \"abc\" \"ABC\")"), LispValue::Bool(false));
    assert_eq!(eval!("(= \"\" (list))"), LispValue::Bool(false));
    assert_eq!(eval!("(= [] (list))"), LispValue::Bool(true));
    assert_eq!(eval!("(= [7 8] (list 7 8))"), LispValue::Bool(true));
    assert_eq!(eval!("(= [7 8] [7 8])"), LispValue::Bool(true));
    assert_eq!(eval!("(= [1 2 (list 3 4 [5 6])] (list 1 2 [3 4 (list 5 6)]))"), LispValue::Bool(true));
    assert_eq!(eval!("(= \"abc\" 'abc)"), LispValue::Bool(false));
    assert_eq!(eval!("(= 'abc 'abc)"), LispValue::Bool(true));
    assert_eq!(eval!("(= 'abcd 'abc)"), LispValue::Bool(false));
}

#[test]
fn comparisons() {
    assert_eq!(eval!("(> 2 1)"), LispValue::Bool(true));
    assert_eq!(eval!("(> 1 1)"), LispValue::Bool(false));
    assert_eq!(eval!("(> 1 2)"), LispValue::Bool(false));
    assert_eq!(eval!("(>= 2 1)"), LispValue::Bool(true));
    assert_eq!(eval!("(>= 1 1)"), LispValue::Bool(true));
    assert_eq!(eval!("(>= 1 2)"), LispValue::Bool(false));
    assert_eq!(eval!("(< 2 1)"), LispValue::Bool(false));
    assert_eq!(eval!("(< 1 1)"), LispValue::Bool(false));
    assert_eq!(eval!("(< 1 2)"), LispValue::Bool(true));
    assert_eq!(eval!("(<= 2 1)"), LispValue::Bool(false));
    assert_eq!(eval!("(<= 1 1)"), LispValue::Bool(true));
    assert_eq!(eval!("(<= 1 2)"), LispValue::Bool(true));
}

#[test]
fn user_functions() {
    assert_eq!(eval!("((fn* (a b) (+ b a)) 3 4)"), LispValue::Number(7.0));
    assert_eq!(eval!("((fn* () 4))"), LispValue::Number(4.0));
    assert_eq!(eval!("((fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)"), LispValue::Number(8.0));
    assert_eq!(eval!("((fn* [] 4))"), LispValue::Number(4.0));
}

#[test]
fn closures() {
    let mut env = testing_env();
    assert_eq!(eval!("(((fn* (a) (fn* (b) (+ a b))) 5) 7)"), LispValue::Number(12.0));
    eval!("(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))", &mut env);
    eval!("(def! plus5 (gen-plus5))", &mut env);
    assert_eq!(eval!("(plus5 7)", &mut env), LispValue::Number(12.0));
    eval!("(def! gen-plusX (fn* (x) (fn* (b) (+ x b))))", &mut env);
    eval!("(def! plus7 (gen-plusX 7))", &mut env);
    assert_eq!(eval!("(plus7 8)", &mut env), LispValue::Number(15.0));
}

#[test]
fn do_form() {
    let mut env = testing_env();
    assert_eq!(eval!("(do (def! a 6) 7 (+ a 8))", &mut env), LispValue::Number(14.0));
    assert_eq!(eval!("a", &mut env), LispValue::Number(6.0));
}

#[test]
fn recursive_funcs() {
    let mut env = testing_env();
    eval!("(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown (- N 1))) 0)))", &mut env);
    assert_eq!(eval!("(sumdown 1)", &mut env), LispValue::Number(1.0));
    assert_eq!(eval!("(sumdown 2)", &mut env), LispValue::Number(3.0));
    assert_eq!(eval!("(sumdown 6)", &mut env), LispValue::Number(21.0));
    eval!("(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))", &mut env);
    assert_eq!(eval!("(fib 1)", &mut env), LispValue::Number(1.0));
    assert_eq!(eval!("(fib 2)", &mut env), LispValue::Number(2.0));
    assert_eq!(eval!("(fib 4)", &mut env), LispValue::Number(5.0));
}

#[test]
fn recursive_func_environment() {
    assert_eq!(eval!("(let* (f (fn* () x) x 3) (f))"), LispValue::Number(3.0));
    assert_eq!(eval!("(let* (cst (fn* (n) (if (= n 0) nil (cst (- n 1))))) (cst 1))"), LispValue::Nil);
    assert_eq!(eval!("(let* (f (fn* (n) (if (= n 0) 0 (g (- n 1)))) g (fn* (n) (f n))) (f 2))"), LispValue::Number(0.0));
}

#[test]
fn variadic_function() {
    assert_eq!(eval!("((fn* (& more) `(~@more)) 1 2 3)"), LispValue::List(vector![
        LispValue::Number(1.0),
        LispValue::Number(2.0),
        LispValue::Number(3.0),
    ]));
    assert_eq!(eval!("((fn* (& more) (count more)) 1 2 3)"), LispValue::Number(3.0));
    assert_eq!(eval!("((fn* (& more) (list? more)) 1 2 3)"), LispValue::Bool(true));
    assert_eq!(eval!("((fn* (& more) (count more)))"), LispValue::Number(0.0));
    assert_eq!(eval!("((fn* (& more) (list? more)))"), LispValue::Bool(true));
}

#[test]
fn not() {
    assert_eq!(eval!("(not false)"), LispValue::Bool(true));
    assert_eq!(eval!("(not nil)"), LispValue::Bool(true));
    assert_eq!(eval!("(not true)"), LispValue::Bool(false));
    assert_eq!(eval!("(not ())"), LispValue::Bool(false));
    assert_eq!(eval!("(not 0)"), LispValue::Bool(false));
    assert_eq!(eval!("(not \"\")"), LispValue::Bool(false));
    assert_eq!(eval!("(not [])"), LispValue::Bool(false));
}
