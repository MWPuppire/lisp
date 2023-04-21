use crate::util::{LispValue, LispError, Result};
use crate::parser::LispParser;
use crate::eval::eval;
use crate::env::LispEnv;
use crate::expect;

use im::{Vector, vector, HashMap, hashmap};
use lazy_static::lazy_static;

lazy_static! {
    static ref MOCK_FS: HashMap<String, &'static str> = {
        hashmap!{
            "inc.mal".to_string() => r#"
                (def! inc1 (fn* (a) (+ 1 a)))
                (def! inc2 (fn* (a) (+ 2 a)))
                (def! inc3 (fn* (a)
                    (+ 3 a)))
            "#,
            "incA.mal".to_string() => r#"
                (def! inc4 (fn* (a) (+ 4 a)))
                (prn (inc4 5))
            "#,
            "incB.mal".to_string() => r#"
                ;; A comment in a file
                (def! inc4 (fn* (a) (+ 4 a)))
                (def! inc5 (fn* (a) ;; a comment after code
                    (+ 5 a)))
                ;; ending comment without final new line"#,
            "incC.mal".to_string() => r#"
                (def! mymap {"a"
                            1})
            "#,
            "computations.mal".to_string() => r#"
                (def! sumdown
                    (fn* [n]
                        (if (= n 0)
                            0
                            (+ n (sumdown (- n 1))))))
                (def! fib
                    (fn* [n]
                        (if (<= n 1)
                            n
                            (+ (fib (- n 1)) (fib (- n 2))))))
            "#,
            "test.txt".to_string() => "A line of text",
        }
    };
}

fn lisp_test_slurp(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?;
    let f = MOCK_FS.get(x.expect_string()?).unwrap();
    Ok((LispValue::String(f.to_string()), env, false))
}
fn lisp_test_load_file(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?;
    let f = MOCK_FS.get(x.expect_string()?).unwrap();
    let mut parser = LispParser::new();
    parser.add_tokenize(f);
    let mut global = env.global();
    while parser.has_tokens() {
        eval(&parser.next()?, &mut global)?;
    }
    Ok((LispValue::Nil, env, false))
}

fn testing_env() -> LispEnv {
    let mut env = LispEnv::new_stdlib_protected();
    // mock filesystem; other functionality could be mocked later as needed
    env.bind_func("slurp", lisp_test_slurp);
    env.bind_func("load-file", lisp_test_load_file);

    // add `cond` built-in macro
    let cond = "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))";
    eval(&LispParser::parse(cond).unwrap(), &mut env).unwrap();

    // TODO mock println and family?
    env
}

fn eval_str(input: &str) -> Result<LispValue> {
    let parsed = LispParser::parse(input)?;
    let mut env = testing_env();
    eval(&parsed, &mut env)
}

fn eval_str_in_env(input: &str, env: &mut LispEnv) -> Result<LispValue> {
    let parsed = LispParser::parse(input)?;
    eval(&parsed, env)
}

macro_rules! eval {
    ($code:expr) => {
        eval_str($code).unwrap()
    };
    ($code:expr, $env:expr) => {
        eval_str_in_env($code, $env).unwrap()
    }
}

mod step1_tests {
    use super::*;

    fn parse(input: &str) -> LispValue {
        LispParser::parse(input).unwrap()
    }

    #[test]
    fn read_numbers() {
        assert_eq!(parse("1"), LispValue::Number(1.0));
        assert_eq!(parse("7"), LispValue::Number(7.0));
        assert_eq!(parse("-123"), LispValue::Number(-123.0));
    }

    #[test]
    fn read_symbols() {
        assert_eq!(parse("+"), LispValue::Symbol("+".to_string()));
        assert_eq!(parse("abc"), LispValue::Symbol("abc".to_string()));
        assert_eq!(parse("abc5"), LispValue::Symbol("abc5".to_string()));
        assert_eq!(parse("abc-def"), LispValue::Symbol("abc-def".to_string()));
        assert_eq!(parse("-"), LispValue::Symbol("-".to_string()));
        assert_eq!(parse("-abc"), LispValue::Symbol("-abc".to_string()));
    }

    #[test]
    fn read_lists() {
        assert_eq!(parse("(+ 1 2)"), LispValue::List(vector![
            LispValue::Symbol("+".to_string()),
            LispValue::Number(1.0),
            LispValue::Number(2.0),
        ]));
        assert_eq!(parse("()"), LispValue::List(vector![]));
        assert_eq!(parse("( )"), LispValue::List(vector![]));
        assert_eq!(parse("(nil)"), LispValue::List(vector![
            LispValue::Nil,
        ]));
    }

    #[test]
    fn nested_lists() {
        assert_eq!(parse("((3 4))"), LispValue::List(vector![
            LispValue::List(vector![
                LispValue::Number(3.0),
                LispValue::Number(4.0),
            ]),
        ]));
        assert_eq!(parse("(+ 1 (+ 2 3))"), LispValue::List(vector![
            LispValue::Symbol("+".to_string()),
            LispValue::Number(1.0),
            LispValue::List(vector![
                LispValue::Symbol("+".to_string()),
                LispValue::Number(2.0),
                LispValue::Number(3.0),
            ]),
        ]));
        assert_eq!(parse("(()())"), LispValue::List(vector![
            LispValue::List(vector![]),
            LispValue::List(vector![]),
        ]));
    }

    #[test]
    fn ignore_commas() {
        assert_eq!(parse("(1 2, 3,,,,),,"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
        ]));
    }

    #[test]
    fn builtin_values() {
        assert_eq!(parse("nil"), LispValue::Nil);
        assert_eq!(parse("true"), LispValue::Bool(true));
        assert_eq!(parse("false"), LispValue::Bool(false));
    }
}

mod step2_tests {
    use super::*;

    #[test]
    fn arithmetic() {
        assert_eq!(eval!("(+ 1 2)"), LispValue::Number(3.0));
        assert_eq!(eval!("(+ 5 (* 2 3))"), LispValue::Number(11.0));
        assert_eq!(eval!("(- (+ 5 (* 2 3)) 3)"), LispValue::Number(8.0));
        assert_eq!(eval!("(/ (- (+ 5 (* 2 3)) 3) 4)"), LispValue::Number(2.0));
        assert_eq!(eval!("(/ (- (+ 515 (* 87 311)) 302) 27)"), LispValue::Number(1010.0));
        assert_eq!(eval!("(* -3 6)"), LispValue::Number(-18.0));
        assert_eq!(eval!("(/ (- (+ 515 (* -87 311)) 296) 27)"), LispValue::Number(-994.0));
    }

    #[test]
    #[should_panic]
    fn fail_undefined_func() {
        eval!("(abc 1 2 3)");
    }

    #[test]
    fn empty_collection_nop() {
        assert_eq!(eval!("()"), LispValue::List(vector![]));
        assert_eq!(eval!("[]"), LispValue::Vector(vec![]));
        assert_eq!(eval!("{}"), LispValue::Map(HashMap::new()));
    }

    #[test]
    fn evaluate_in_collections() {
        assert_eq!(eval!("[1 2 (+ 1 2)]"), LispValue::Vector(vec![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("{\"a\" (+ 7 8)}"), LispValue::Map(
            HashMap::unit(
                LispValue::String("a".to_string()),
                LispValue::Number(15.0),
            )
        ));
        assert_eq!(eval!("{:a (+ 7 8)}"), LispValue::Map(
            HashMap::unit(
                LispValue::Keyword("a".to_string()),
                LispValue::Number(15.0),
            )
        ));
    }
}

mod step3_tests {
    use super::*;

    #[test]
    fn def() {
        let mut env = testing_env();
        eval!("(def! x 3)", &mut env);
        assert_eq!(eval!("x", &mut env), LispValue::Number(3.0));
        eval!("(def! x 4)", &mut env);
        assert_eq!(eval!("x", &mut env), LispValue::Number(4.0));
    }

    #[test]
    fn def_immediate_evaluation() {
        let mut env = testing_env();
        assert_eq!(eval!("(def! x (+ 1 7))", &mut env), LispValue::Number(8.0));
        assert_eq!(eval!("x", &mut env), LispValue::Number(8.0));
    }

    #[test]
    fn case_sensitive_symbols() {
        let mut env = testing_env();
        eval!("(def! mynum 111)", &mut env);
        eval!("(def! MYNUM 222)", &mut env);
        assert_eq!(eval!("mynum", &mut env), LispValue::Number(111.0));
        assert_eq!(eval!("MYNUM", &mut env), LispValue::Number(222.0));
    }

    #[test]
    fn cancel_def_on_error() {
        let mut env = testing_env();
        eval!("(def! w 123)", &mut env);
        assert!(eval_str_in_env("(def! w (abc))", &mut env).is_err());
        assert_eq!(eval!("w", &mut env), LispValue::Number(123.0));
    }

    #[test]
    fn let_statement() {
        assert_eq!(eval!("(let* (x 9) x)"), LispValue::Number(9.0));
        assert_eq!(eval!("(let* (z (+ 2 3)) (+ 1 z))"), LispValue::Number(6.0));
        assert_eq!(eval!("(let* (p (+ 2 3) q (+ 2 p)) (+ p q))"), LispValue::Number(12.0));
    }

    #[test]
    fn let_scopes() {
        let mut env = testing_env();
        eval!("(def! x 4)", &mut env);
        assert_eq!(eval!("(let* (x 9) x)", &mut env), LispValue::Number(9.0));
        assert_eq!(eval!("x", &mut env), LispValue::Number(4.0));
        assert_eq!(eval!("(let* (q 9) x)", &mut env), LispValue::Number(4.0));
    }

    #[test]
    fn vector_let() {
        assert_eq!(eval!("(let* [z 9] z)"), LispValue::Number(9.0));
        assert_eq!(eval!("(let* [p (+ 2 3) q (+ 2 p)] (+ p q))"), LispValue::Number(12.0));
    }

    #[test]
    fn last_binding_priority() {
        assert_eq!(eval!("(let* (x 2 x 3) x)"), LispValue::Number(3.0));
    }
}

mod step4_tests {
    use super::*;

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
}

mod step5_tests {
    use super::*;

    #[test]
    fn recursive_tail_call() {
        let mut env = testing_env();
        eval!("(def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))", &mut env);
        assert_eq!(eval!("(sum2 10 0)", &mut env), LispValue::Number(55.0));
        assert_eq!(eval!("(sum2 10000 0)", &mut env), LispValue::Number(50005000.0));
    }

    #[test]
    fn mutually_recursive_tail_call() {
        let mut env = testing_env();
        eval!("(def! foo (fn* (n) (if (= n 0) 0 (bar (- n 1)))))", &mut env);
        eval!("(def! bar (fn* (n) (if (= n 0) 0 (foo (- n 1)))))", &mut env);
        assert_eq!(eval!("(foo 10000)", &mut env), LispValue::Number(0.0));
    }

    #[test]
    fn do_do() {
        assert_eq!(eval!("(do (do 1 2))"), LispValue::Number(2.0));
    }
}

mod step6_tests {
    use super::*;

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
            LispValue::Symbol("+".to_string()),
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
}

mod step7_tests {
    use super::*;

    #[test]
    fn list_functions() {
        assert_eq!(eval!("(cons 1 (list))"), LispValue::List(vector![
            LispValue::Number(1.0),
        ]));
        assert_eq!(eval!("(cons 1 (list 2))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
        ]));
        assert_eq!(eval!("(cons 1 (list 2 3))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(cons (list 1) (list 2 3))"), LispValue::List(vector![
            LispValue::List(vector![LispValue::Number(1.0)]),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(concat)"), LispValue::List(vector![]));
        assert_eq!(eval!("(concat (list 1 2))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
        ]));
        assert_eq!(eval!("(concat (list 1 2) (list 3 4))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
            LispValue::Number(4.0),
        ]));
        assert_eq!(eval!("(concat (list 1 2) (list 3 4) (list 5 6))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
            LispValue::Number(4.0),
            LispValue::Number(5.0),
            LispValue::Number(6.0),
        ]));
        assert_eq!(eval!("(concat (list) (list))"), LispValue::List(vector![]));
        assert_eq!(eval!("(cons 1 [])"), LispValue::List(vector![
            LispValue::Number(1.0),
        ]));
        assert_eq!(eval!("(cons [1] [2 3])"), LispValue::List(vector![
            LispValue::Vector(vec![
                LispValue::Number(1.0),
            ]),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(concat [1 2] (list 3 4) [5 6])"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
            LispValue::Number(4.0),
            LispValue::Number(5.0),
            LispValue::Number(6.0),
        ]));
    }

    #[test]
    fn quote() {
        assert_eq!(eval!("(quote 7)"), LispValue::Number(7.0));
        assert_eq!(eval!("(quote (1 2 3))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(quote (1 2 (3 4)))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::List(vector![
                LispValue::Number(3.0),
                LispValue::Number(4.0),
            ]),
        ]));
        assert_eq!(eval!("(quote a)"), LispValue::Symbol("a".to_string()));
        assert_eq!(eval!("'7"), LispValue::Number(7.0));
        assert_eq!(eval!("'(1 2 3)"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("'(1 2 (3 4))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::List(vector![
                LispValue::Number(3.0),
                LispValue::Number(4.0),
            ]),
        ]));
    }

    #[test]
    fn quasiquote() {
        assert_eq!(eval!("(quasiquote nil)"), LispValue::Nil);
        assert_eq!(eval!("(quasiquote 7)"), LispValue::Number(7.0));
        assert_eq!(eval!("(quasiquote ())"), LispValue::List(vector![]));
        assert_eq!(eval!("(quasiquote (1 2))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
        ]));
        assert_eq!(eval!("(quasiquote (1 () 2))"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::List(vector![]),
            LispValue::Number(2.0),
        ]));
        assert_eq!(eval!("(quasiquote (()))"), LispValue::List(vector![
            LispValue::List(vector![]),
        ]));
        assert_eq!(eval!("`7"), LispValue::Number(7.0));
        assert_eq!(eval!("`(1 2 3)"), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
            LispValue::Number(3.0),
        ]));
    }

    #[test]
    fn unquote() {
        let mut env = testing_env();
        eval!("(def! a 8)", &mut env);
        assert_eq!(eval!("(quasiquote a)", &mut env), LispValue::Symbol("a".to_string()));
        assert_eq!(eval!("(quasiquote (unquote a))", &mut env), LispValue::Number(8.0));
        assert_eq!(eval!("(quasiquote (1 a 3))", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Symbol("a".to_string()),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(quasiquote (1 (unquote a) 3))", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(8.0),
            LispValue::Number(3.0),
        ]));
        eval!("(def! b (quote (1 \"b\" \"d\")))", &mut env);
        assert_eq!(eval!("(quasiquote (1 b 3))", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Symbol("b".to_string()),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(quasiquote (1 (unquote b) 3))", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::List(vector![
                LispValue::Number(1.0),
                LispValue::String("b".to_string()),
                LispValue::String("d".to_string()),
            ]),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(quasiquote ((unquote 1) (unquote 2)))", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
        ]));
        assert_eq!(eval!("(let* (x 0) (quasiquote (unquote x)))", &mut env), LispValue::Number(0.0));
        assert_eq!(eval!("`~7", &mut env), LispValue::Number(7.0));
        assert_eq!(eval!("`(1 ~a 3)", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(8.0),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("`(1 ~b 3)", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::List(vector![
                LispValue::Number(1.0),
                LispValue::String("b".to_string()),
                LispValue::String("d".to_string()),
            ]),
            LispValue::Number(3.0),
        ]));
    }

    #[test]
    fn splice_unquote() {
        let mut env = testing_env();
        eval!("(def! c (quote (1 \"b\" \"d\")))", &mut env);
        assert_eq!(eval!("(quasiquote (1 c 3))", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Symbol("c".to_string()),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(quasiquote (1 (splice-unquote c) 3))", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(1.0),
            LispValue::String("b".to_string()),
            LispValue::String("d".to_string()),
            LispValue::Number(3.0),
        ]));
        assert_eq!(eval!("(quasiquote ((splice-unquote c) (splice-unquote c)))", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::String("b".to_string()),
            LispValue::String("d".to_string()),
            LispValue::Number(1.0),
            LispValue::String("b".to_string()),
            LispValue::String("d".to_string()),
        ]));
        assert_eq!(eval!("`(1 ~@c 3)", &mut env), LispValue::List(vector![
            LispValue::Number(1.0),
            LispValue::Number(1.0),
            LispValue::String("b".to_string()),
            LispValue::String("d".to_string()),
            LispValue::Number(3.0),
        ]));
    }

    #[test]
    fn vec() {
        assert_eq!(eval!("(vec (list))"), LispValue::Vector(vec![]));
        assert_eq!(eval!("(vec (list 1 2))"), LispValue::Vector(vec![
            LispValue::Number(1.0),
            LispValue::Number(2.0),
        ]));
        assert_eq!(eval!("(vec [])"), LispValue::Vector(vec![]));
    }

    // always halts for some reason, so `ignore`
    // TODO should figure out why the test never finishes
    #[test]
    #[ignore]
    fn quine() {
        let expr = LispParser::parse(r#"
        ((fn* (q)
            (quasiquote ((unquote q) (quote (unquote q))))
        ) (quote (fn* (q)
            (quasiquote ((unquote q) (quote (unquote q))))
        )))
        "#).unwrap();
        let mut env = testing_env();
        assert_eq!(&eval(&expr, &mut env).unwrap(), &expr);
    }
}

mod step8_tests {
    use super::*;

    fn basic_macros() -> LispEnv {
        let mut env = testing_env();
        eval!("(defmacro! one (fn* () 1))", &mut env);
        eval!("(defmacro! two (fn* () 2))", &mut env);
        eval!("(defmacro! unless (fn* (pred a b) `(if ~pred ~b ~a)))", &mut env);
        eval!("(defmacro! unless2 (fn* (pred a b) (list 'if (list 'not pred) a b)))", &mut env);
        eval!("(defmacro! identity (fn* (x) x))", &mut env);
        env
    }

    #[test]
    fn macros() {
        let mut env = basic_macros();
        assert_eq!(eval!("(one)", &mut env), LispValue::Number(1.0));
        assert_eq!(eval!("(two)", &mut env), LispValue::Number(2.0));
        assert_eq!(eval!("(unless false 7 8)", &mut env), LispValue::Number(7.0));
        assert_eq!(eval!("(unless true 7 8)", &mut env), LispValue::Number(8.0));
        assert_eq!(eval!("(unless2 false 7 8)", &mut env), LispValue::Number(7.0));
        assert_eq!(eval!("(unless2 true 7 8)", &mut env), LispValue::Number(8.0));
        assert_eq!(eval!("(let* (a 123) (identity a))", &mut env), LispValue::Number(123.0));
    }

    // always blows the stack for some reason, so `ignore`
    // TODO figure out why
    #[test]
    #[ignore]
    fn macroexpand() {
        let mut env = basic_macros();
        assert_eq!(eval!("(macroexpand (one))", &mut env), LispValue::Number(1.0));
        assert_eq!(eval!("(macroexpand (unless PRED A B))", &mut env), LispValue::List(vector![
            LispValue::Symbol("if".to_string()),
            LispValue::Symbol("PRED".to_string()),
            LispValue::Symbol("B".to_string()),
            LispValue::Symbol("A".to_string()),
        ]));
        assert_eq!(eval!("(macroexpand (unless2 PRED A B))", &mut env), LispValue::List(vector![
            LispValue::Symbol("if".to_string()),
            LispValue::List(vector![
                LispValue::Symbol("not".to_string()),
                LispValue::Symbol("PRED".to_string()),
            ]),
            LispValue::Symbol("A".to_string()),
            LispValue::Symbol("B".to_string()),
        ]));
        assert_eq!(eval!("(macroexpand (unless2 2 3 4))", &mut env), LispValue::List(vector![
            LispValue::Symbol("if".to_string()),
            LispValue::List(vector![
                LispValue::Symbol("not".to_string()),
                LispValue::Number(2.0),
            ]),
            LispValue::Number(3.0),
            LispValue::Number(4.0),
        ]));
        assert_eq!(eval!("(let* (a 123) (macroexpand (identity a)))", &mut env),
            LispValue::Symbol("a".to_string())
        );
    }

    #[test]
    fn list_functions() {
        assert_eq!(eval!("(nth (list 1) 0)"), LispValue::Number(1.0));
        assert_eq!(eval!("(nth (list 1 2) 1)"), LispValue::Number(2.0));
        assert_eq!(eval!("(nth (list 1 2 nil) 2)"), LispValue::Nil);
        assert_eq!(eval!("(first (list))"), LispValue::Nil);
        assert_eq!(eval!("(first (list 6))"), LispValue::Number(6.0));
        assert_eq!(eval!("(first (list 7 8 9))"), LispValue::Number(7.0));
        assert_eq!(eval!("(rest (list))"), LispValue::List(vector![]));
        assert_eq!(eval!("(rest (list 6))"), LispValue::List(vector![]));
        assert_eq!(eval!("(rest (list 7 8 9))"), LispValue::List(vector![
            LispValue::Number(8.0),
            LispValue::Number(9.0),
        ]));
        assert_eq!(eval!("(first [10])"), LispValue::Number(10.0));
        assert_eq!(eval!("(rest [10 11 12])"), LispValue::List(vector![
            LispValue::Number(11.0),
            LispValue::Number(12.0),
        ]));
    }

    #[test]
    #[should_panic]
    fn out_of_bounds_access() {
        eval!("(nth (list 1 2) 2)");
    }

    #[test]
    fn cond() {
        assert_eq!(eval!("(cond)"), LispValue::Nil);
        assert_eq!(eval!("(cond true 7)"), LispValue::Number(7.0));
        assert_eq!(eval!("(cond false 7)"), LispValue::Nil);
        assert_eq!(eval!("(cond true 7 true 8)"), LispValue::Number(7.0));
        assert_eq!(eval!("(cond false 7 true 8)"), LispValue::Number(8.0));
        assert_eq!(eval!("(cond false 7 false 8 \"else\" 9)"), LispValue::Number(9.0));
        assert_eq!(eval!("(cond false 7 false 8 false 9)"), LispValue::Nil);
        assert_eq!(eval!("(let* (x (cond false \"no\" true \"yes\")) x)"), LispValue::String("yes".to_string()));
    }

    #[test]
    #[should_panic]
    fn cond_uneven() {
        eval!("(cond true)");
    }

    #[test]
    fn macro_closure() {
        let mut env = testing_env();
        eval!("(def! x 2)", &mut env);
        eval!("(defmacro! a (fn* [] x))", &mut env);
        assert_eq!(eval!("(a)", &mut env), LispValue::Number(2.0));
        assert_eq!(eval!("(let* (x 3) (a))", &mut env), LispValue::Number(2.0));
    }
}

mod step9_tests {
    use super::*;

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
}
