#![allow(unused)]

pub use im::{hashmap, vector, HashMap, Vector};
pub use lazy_static::lazy_static;
pub use lisp::{eval, LispEnv, LispError, LispParser, LispValue, Result};
pub use std::sync::Arc;

lazy_static! {
    pub static ref MOCK_FS: HashMap<String, &'static str> = {
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
    };
}

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
    // `ptr_eq` explained in `lisp_load_file` (the built-in function). Needed to
    // avoid a dead-lock.
    if Arc::ptr_eq(&global, &env.clone_arc()) {
        for val in parser {
            eval(val?, env)?;
        }
    } else {
        for val in parser {
            eval(val?, &global)?;
        }
    }
    Ok(LispValue::nil())
}

pub fn testing_env() -> Arc<LispEnv> {
    let env = LispEnv::new_stdlib_protected();
    // mock filesystem; other functionality could be mocked later as needed
    env.bind_func("slurp", lisp_test_slurp);
    env.bind_func("load-file", lisp_test_load_file);

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

#[macro_export]
macro_rules! eval {
    ($code:expr) => {
        eval_str($code).unwrap()
    };
    ($code:expr, $env:expr) => {
        eval_str_in_env($code, $env).unwrap()
    };
}
