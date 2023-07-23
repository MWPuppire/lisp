mod common;
use common::*;

#[test]
fn list_functions() {
    eval_eq!("(list)", vector![]);
    eval_eq!("(list? (list))", true);
    eval_eq!("(empty? (list))", true);
    eval_eq!("(empty? (list 1))", false);
    eval_eq!("(list 1 2 3)", vector![1.0.into(), 2.0.into(), 3.0.into(),]);
    eval_eq!("(count (list 1 2 3))", 3.0);
    eval_eq!("(count (list))", 0.0);
    eval_eq!("(count nil)", 0.0);
    eval_eq!("(count [1 2 3])", 3.0);
    eval_eq!("(empty? [])", true);
    eval_eq!("(empty? [1 2 3])", false);
    eval_eq!("(list? [1 2 3])", false);
}

#[test]
fn if_form() {
    eval_eq!("(if true 7 8)", 7.0);
    eval_eq!("(if false 7 8)", 8.0);
    eval_eq!("(if nil 7 8)", 8.0);
    eval_eq!("(if 0 7 8)", 7.0);
    eval_eq!("(if (list) 7 8)", 7.0);
}

#[test]
fn if_side_effects() {
    let env = testing_env();
    eval!("(if true (def! x 4) (def! x 5))", &env);
    eval_eq!("x", &env, 4.0);
    eval!("(if false (def! x 4) (def! x 5))", &env);
    eval_eq!("x", &env, 5.0);
}

#[test]
fn if_one_path() {
    eval_eq!("(if false 8)", LispValue::Nil);
    eval_eq!("(if nil 8)", LispValue::Nil);
    eval_eq!("(if true (+ 1 7))", 8.0);
}

#[test]
fn equality() {
    eval_eq!("(= 2 1)", false);
    eval_eq!("(= 1 1)", true);
    eval_eq!("(= 1 2)", false);
    eval_eq!("(= 2 (+ 1 1))", true);
    eval_eq!("(= nil nil)", true);
    eval_eq!("(= nil (list))", false);
    eval_eq!("(= (list) (list))", true);
    eval_eq!("(= (list) ())", true);
    eval_eq!("(= true true)", true);
    eval_eq!("(= false false)", true);
    eval_eq!("(= (list 1 2) (list 1 2))", true);
    eval_eq!("(= (list 1 2) (list 1))", false);
    eval_eq!("(= \"\" \"\")", true);
    eval_eq!("(= \"abc\" \"abc\")", true);
    eval_eq!("(= \"abc\" \"\")", false);
    eval_eq!("(= \"abc\" \"ABC\")", false);
    eval_eq!("(= \"\" (list))", false);
    eval_eq!("(= [] (list))", true);
    eval_eq!("(= [7 8] (list 7 8))", true);
    eval_eq!("(= [7 8] [7 8])", true);
    eval_eq!(
        "(= [1 2 (list 3 4 [5 6])] (list 1 2 [3 4 (list 5 6)]))",
        true
    );
    eval_eq!("(= \"abc\" 'abc)", false);
    eval_eq!("(= 'abc 'abc)", true);
    eval_eq!("(= 'abcd 'abc)", false);
}

#[test]
fn comparisons() {
    eval_eq!("(> 2 1)", true);
    eval_eq!("(> 1 1)", false);
    eval_eq!("(> 1 2)", false);
    eval_eq!("(>= 2 1)", true);
    eval_eq!("(>= 1 1)", true);
    eval_eq!("(>= 1 2)", false);
    eval_eq!("(< 2 1)", false);
    eval_eq!("(< 1 1)", false);
    eval_eq!("(< 1 2)", true);
    eval_eq!("(<= 2 1)", false);
    eval_eq!("(<= 1 1)", true);
    eval_eq!("(<= 1 2)", true);
}

#[test]
fn user_functions() {
    eval_eq!("((fn* (a b) (+ b a)) 3 4)", 7.0);
    eval_eq!("((fn* () 4))", 4.0);
    eval_eq!("((fn* (f x) (f x)) (fn* (a) (+ 1 a)) 7)", 8.0);
    eval_eq!("((fn* [] 4))", 4.0);
}

#[test]
fn closures() {
    let env = testing_env();
    eval_eq!("(((fn* (a) (fn* (b) (+ a b))) 5) 7)", 12.0);
    eval!("(def! gen-plus5 (fn* () (fn* (b) (+ 5 b))))", &env);
    eval!("(def! plus5 (gen-plus5))", &env);
    eval_eq!("(plus5 7)", &env, 12.0);
    eval!("(def! gen-plusX (fn* (x) (fn* (b) (+ x b))))", &env);
    eval!("(def! plus7 (gen-plusX 7))", &env);
    eval_eq!("(plus7 8)", &env, 15.0);
}

#[test]
fn do_form() {
    let env = testing_env();
    eval_eq!("(do (def! a 6) 7 (+ a 8))", &env, 14.0);
    eval_eq!("a", &env, 6.0);
}

#[test]
fn recursive_funcs() {
    let env = testing_env();
    eval!(
        "(def! sumdown (fn* (N) (if (> N 0) (+ N (sumdown (- N 1))) 0)))",
        &env
    );
    eval_eq!("(sumdown 1)", &env, 1.0);
    eval_eq!("(sumdown 2)", &env, 3.0);
    eval_eq!("(sumdown 6)", &env, 21.0);
    eval!(
        "(def! fib (fn* (N) (if (= N 0) 1 (if (= N 1) 1 (+ (fib (- N 1)) (fib (- N 2)))))))",
        &env
    );
    eval_eq!("(fib 1)", &env, 1.0);
    eval_eq!("(fib 2)", &env, 2.0);
    eval_eq!("(fib 4)", &env, 5.0);
}

#[test]
fn recursive_func_environment() {
    eval_eq!("(let* (f (fn* () x) x 3) (f))", 3.0);
    eval_eq!(
        "(let* (cst (fn* (n) (if (= n 0) nil (cst (- n 1))))) (cst 1))",
        LispValue::Nil
    );
    eval_eq!(
        "(let* (f (fn* (n) (if (= n 0) 0 (g (- n 1)))) g (fn* (n) (f n))) (f 2))",
        0.0
    );
}

#[test]
fn variadic_function() {
    eval_eq!(
        "((fn* (& more) more) 1 2 3)",
        vector![1.0.into(), 2.0.into(), 3.0.into(),]
    );
    eval_eq!("((fn* (& more) (count more)) 1 2 3)", 3.0);
    eval_eq!("((fn* (& more) (list? more)) 1 2 3)", true);
    eval_eq!("((fn* (& more) (count more)))", 0.0);
    eval_eq!("((fn* (& more) (list? more)))", true);
}

#[test]
fn not() {
    eval_eq!("(not false)", true);
    eval_eq!("(not nil)", true);
    eval_eq!("(not true)", false);
    eval_eq!("(not ())", false);
    eval_eq!("(not 0)", false);
    eval_eq!("(not \"\")", false);
    eval_eq!("(not [])", false);
}
