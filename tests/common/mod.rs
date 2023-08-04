#![allow(unused)]

pub use im::{hashmap, vector, HashMap, Vector};
pub use lisp::{eval, LispEnv, LispError, LispParser, LispValue, Result};
use once_cell::sync::Lazy;
pub use std::sync::Arc;

pub static MOCK_FS: Lazy<HashMap<String, &'static str>> = Lazy::new(|| {
    hashmap! {
        "inc.mal".to_owned() => r#"
                (def! inc1 (fn* (a) (+ 1 a)))
                (def! inc2 (fn* (a) (+ 2 a)))
                (def! inc3 (fn* (a)
                    (+ 3 a)))
            "#,
        "incA.mal".to_owned() => r#"
                (def! inc4 (fn* (a) (+ 4 a)))
                (prn (inc4 5))
            "#,
        "incB.mal".to_owned() => r#"
                ;; A comment in a file
                (def! inc4 (fn* (a) (+ 4 a)))
                (def! inc5 (fn* (a) ;; a comment after code
                    (+ 5 a)))
                ;; ending comment without final new line"#,
        "incC.mal".to_owned() => r#"
                (def! mymap {"a"
                            1})
            "#,
        "computations.mal".to_owned() => r#"
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
        "test.txt".to_owned() => "A line of text",
    }
});

pub fn lisp_test_slurp(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    if args.len() != 1 {
        return Err(LispError::IncorrectArguments(1, args.len()));
    }
    let x = eval(args.pop_front().unwrap(), env)?;
    let file_name = x.expect_string()?;
    let f = MOCK_FS.get(file_name).unwrap();
    Ok(f.to_string().into())
}
pub fn lisp_test_load_file(mut args: Vector<LispValue>, env: &LispEnv) -> Result<LispValue> {
    if args.len() != 1 {
        return Err(LispError::IncorrectArguments(1, args.len()));
    }
    let x = eval(args.pop_front().unwrap(), env)?;
    let file_name = x.expect_string()?;
    let f = MOCK_FS.get(file_name).unwrap();

    let mut parser = LispParser::new();
    parser.add_tokenize(f);
    let mut global = env.global();
    for val in parser {
        eval(val?, &global)?;
    }
    Ok(LispValue::nil())
}

pub fn testing_env() -> Arc<LispEnv> {
    #[cfg(not(feature = "self-implemented"))]
    let env = LispEnv::new_stdlib_protected();
    #[cfg(feature = "self-implemented")]
    let env = LispEnv::new_self_implemented();
    // mock filesystem; other functionality could be mocked later as needed
    env.bind_func("slurp", lisp_test_slurp);
    #[cfg(not(feature = "self-implemented"))]
    env.bind_func("load-file", lisp_test_load_file);
    #[cfg(feature = "self-implemented")]
    eval_str_in_env(
        r#"(def! load-file (fn* (file) (
            eval (read-string (str "(do " (slurp file) "\nnil)" ))
        )))"#,
        &env,
    )
    .unwrap();

    // TODO mock println and family?
    env
}

#[inline]
pub fn eval_str(input: &str) -> Result<LispValue> {
    let parsed = input.parse()?;
    let env = testing_env();
    let x = eval(parsed, &env);
    x
}

#[inline]
pub fn eval_str_in_env(input: &str, env: &LispEnv) -> Result<LispValue> {
    let parsed = input.parse()?;
    eval(parsed, env)
}

#[inline]
pub fn parse(input: &str) -> LispValue {
    LispParser::parse(input).unwrap().unwrap()
}

#[macro_export]
macro_rules! eval {
    ($code:expr $(,)?) => {
        eval_str($code).unwrap()
    };
    ($code:expr, $env:expr $(,)?) => {
        eval_str_in_env($code, $env).unwrap()
    };
}

#[macro_export]
macro_rules! eval_eq {
    ($code:expr, $res:expr $(,)?) => {
        assert_eq!(eval_str($code).unwrap(), $res.into())
    };
    ($code:expr, $env:expr, $res:expr $(,)?) => {
        assert_eq!(eval_str_in_env($code, $env).unwrap(), $res.into())
    };
}
