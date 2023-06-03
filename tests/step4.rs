mod common;
use common::*;

#[test]
fn list_functions() {
    assert_eq!(eval!("(list)"), vector![].into());
    assert_eq!(eval!("(list? (list))"), true.into());
    assert_eq!(eval!("(empty? (list))"), true.into());
    assert_eq!(eval!("(empty? (list 1))"), false.into());
    assert_eq!(
        eval!("(list 1 2 3)"),
        vector![1.0.into(), 2.0.into(), 3.0.into(),].into()
    );
    assert_eq!(eval!("(count (list 1 2 3))"), 3.0.into());
    assert_eq!(eval!("(count (list))"), 0.0.into());
    assert_eq!(eval!("(count nil)"), 0.0.into());
    assert_eq!(eval!("(count [1 2 3])"), 3.0.into());
    assert_eq!(eval!("(empty? [])"), true.into());
    assert_eq!(eval!("(empty? [1 2 3])"), false.into());
    assert_eq!(eval!("(list? [1 2 3])"), false.into());
}

#[test]
fn if_form() {
    assert_eq!(eval!("(if true 7 8)"), 7.0.into());
    assert_eq!(eval!("(if false 7 8)"), 8.0.into());
    assert_eq!(eval!("(if nil 7 8)"), 8.0.into());
    assert_eq!(eval!("(if 0 7 8)"), 7.0.into());
    assert_eq!(eval!("(if (list) 7 8)"), 7.0.into());
}

#[test]
fn if_side_effects() {
    let env = testing_env();
    let mut lock = env.write();
    eval!("(if true (def! x 4) (def! x 5))", &mut lock);
    assert_eq!(eval!("x", &mut lock), 4.0.into());
    eval!("(if false (def! x 4) (def! x 5))", &mut lock);
    assert_eq!(eval!("x", &mut lock), 5.0.into());
}

#[test]
fn if_one_path() {
    assert_eq!(eval!("(if false 8)"), LispValue::Nil);
    assert_eq!(eval!("(if nil 8)"), LispValue::Nil);
    assert_eq!(eval!("(if true (+ 1 7))"), 8.0.into());
}

#[test]
fn equality() {
    assert_eq!(eval!("(= 2 1)"), false.into());
    assert_eq!(eval!("(= 1 1)"), true.into());
    assert_eq!(eval!("(= 1 2)"), false.into());
    assert_eq!(eval!("(= 2 (+ 1 1))"), true.into());
    assert_eq!(eval!("(= nil nil)"), true.into());
    assert_eq!(eval!("(= nil (list))"), false.into());
    assert_eq!(eval!("(= (list) (list))"), true.into());
    assert_eq!(eval!("(= (list) ())"), true.into());
    assert_eq!(eval!("(= true true)"), true.into());
    assert_eq!(eval!("(= false false)"), true.into());
    assert_eq!(eval!("(= (list 1 2) (list 1 2))"), true.into());
    assert_eq!(eval!("(= (list 1 2) (list 1))"), false.into());
    assert_eq!(eval!("(= \"\" \"\")"), true.into());
    assert_eq!(eval!("(= \"abc\" \"abc\")"), true.into());
    assert_eq!(eval!("(= \"abc\" \"\")"), false.into());
    assert_eq!(eval!("(= \"abc\" \"ABC\")"), false.into());
    assert_eq!(eval!("(= \"\" (list))"), false.into());
    assert_eq!(eval!("(= [] (list))"), true.into());
    assert_eq!(eval!("(= [7 8] (list 7 8))"), true.into());
    assert_eq!(eval!("(= [7 8] [7 8])"), true.into());
    assert_eq!(
        eval!("(= [1 2 (list 3 4 [5 6])] (list 1 2 [3 4 (list 5 6)]))"),
        true.into()
    );
    assert_eq!(eval!("(= \"abc\" 'abc)"), false.into());
    assert_eq!(eval!("(= 'abc 'abc)"), true.into());
    assert_eq!(eval!("(= 'abcd 'abc)"), false.into());
}

#[test]
fn comparisons() {
    assert_eq!(eval!("(> 2 1)"), true.into());
    assert_eq!(eval!("(> 1 1)"), false.into());
    assert_eq!(eval!("(> 1 2)"), false.into());
    assert_eq!(eval!("(>= 2 1)"), true.into());
    assert_eq!(eval!("(>= 1 1)"), true.into());
    assert_eq!(eval!("(>= 1 2)"), false.into());
    assert_eq!(eval!("(< 2 1)"), false.into());
    assert_eq!(eval!("(< 1 1)"), false.into());
    assert_eq!(eval!("(< 1 2)"), true.into());
    assert_eq!(eval!("(<= 2 1)"), false.into());
    assert_eq!(eval!("(<= 1 1)"), true.into());
    assert_eq!(eval!("(<= 1 2)"), true.into());
}

#[test]
fn user_functions() {
    assert_eq!(eval!("((fn* (a b) (+ b a)) 3 4)"), 7.0.into());
    assert_eq!(eval!("((fn* () 4))"), 4.0.into());
    assert_eq!(eval!("((fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)"), 8.0.into());
    assert_eq!(eval!("((fn* [] 4))"), 4.0.into());
}

#[test]
fn closures() {
    let env = testing_env();
    let mut lock = env.write();
    assert_eq!(eval!("(((fn* (a) (fn* (b) (+ a b))) 5) 7)"), 12.0.into());
    eval!("(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))", &mut lock);
    eval!("(def! plus5 (gen-plus5))", &mut lock);
    assert_eq!(eval!("(plus5 7)", &mut lock), 12.0.into());
    eval!("(def! gen-plusX (fn* (x) (fn* (b) (+ x b))))", &mut lock);
    eval!("(def! plus7 (gen-plusX 7))", &mut lock);
    assert_eq!(eval!("(plus7 8)", &mut lock), 15.0.into());
}

#[test]
fn do_form() {
    let env = testing_env();
    let mut lock = env.write();
    assert_eq!(eval!("(do (def! a 6) 7 (+ a 8))", &mut lock), 14.0.into());
    assert_eq!(eval!("a", &mut lock), 6.0.into());
}

#[test]
fn recursive_funcs() {
    let env = testing_env();
    let mut lock = env.write();
    eval!(
        "(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown (- N 1))) 0)))",
        &mut lock
    );
    assert_eq!(eval!("(sumdown 1)", &mut lock), 1.0.into());
    assert_eq!(eval!("(sumdown 2)", &mut lock), 3.0.into());
    assert_eq!(eval!("(sumdown 6)", &mut lock), 21.0.into());
    eval!(
        "(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))",
        &mut lock
    );
    assert_eq!(eval!("(fib 1)", &mut lock), 1.0.into());
    assert_eq!(eval!("(fib 2)", &mut lock), 2.0.into());
    assert_eq!(eval!("(fib 4)", &mut lock), 5.0.into());
}

#[test]
fn recursive_func_environment() {
    assert_eq!(eval!("(let* (f (fn* () x) x 3) (f))"), 3.0.into());
    assert_eq!(
        eval!("(let* (cst (fn* (n) (if (= n 0) nil (cst (- n 1))))) (cst 1))"),
        LispValue::Nil
    );
    assert_eq!(
        eval!("(let* (f (fn* (n) (if (= n 0) 0 (g (- n 1)))) g (fn* (n) (f n))) (f 2))"),
        0.0.into()
    );
}

#[test]
fn variadic_function() {
    assert_eq!(
        eval!("((fn* (& more) more) 1 2 3)"),
        vector![1.0.into(), 2.0.into(), 3.0.into(),].into()
    );
    assert_eq!(eval!("((fn* (& more) (count more)) 1 2 3)"), 3.0.into());
    assert_eq!(eval!("((fn* (& more) (list? more)) 1 2 3)"), true.into());
    assert_eq!(eval!("((fn* (& more) (count more)))"), 0.0.into());
    assert_eq!(eval!("((fn* (& more) (list? more)))"), true.into());
}

#[test]
fn not() {
    assert_eq!(eval!("(not false)"), true.into());
    assert_eq!(eval!("(not nil)"), true.into());
    assert_eq!(eval!("(not true)"), false.into());
    assert_eq!(eval!("(not ())"), false.into());
    assert_eq!(eval!("(not 0)"), false.into());
    assert_eq!(eval!("(not \"\")"), false.into());
    assert_eq!(eval!("(not [])"), false.into());
}
