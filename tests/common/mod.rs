#![allow(unused)]

pub use im::{Vector, vector, HashMap, hashmap};
pub use lazy_static::lazy_static;
pub use lisp::{LispValue, LispError, Result, LispParser, LispEnv, eval};
pub use lisp::expect;

lazy_static! {
    pub static ref MOCK_FS: HashMap<String, &'static str> = {
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

pub fn lisp_test_slurp(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?;
    let file_name = x.expect_string()?;
    let f = MOCK_FS.get(file_name).unwrap();
    Ok((LispValue::String(f.to_string()), env, false))
}
pub fn lisp_test_load_file(args: Vector<LispValue>, mut env: LispEnv) -> Result<(LispValue, LispEnv, bool)> {
    expect!(args.len() == 1, LispError::IncorrectArguments(1, args.len()));
    let x = eval(&args[0], &mut env)?;
    let file_name = x.expect_string()?;
    let f = MOCK_FS.get(file_name).unwrap();

    let mut parser = LispParser::new();
    parser.add_tokenize(f);
    let mut global = env.global();
    for val in parser {
        eval(&val?, &mut global)?;
    }
    Ok((LispValue::Nil, env, false))
}

pub fn testing_env() -> LispEnv {
    let mut env = LispEnv::new_stdlib_protected();
    // mock filesystem; other functionality could be mocked later as needed
    env.bind_func("slurp", lisp_test_slurp);
    env.bind_func("load-file", lisp_test_load_file);

    // add `cond` built-in macro
    let cond = "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))";
    eval(&LispParser::parse(cond).unwrap().unwrap(), &mut env).unwrap();

    // TODO mock println and family?
    env
}

pub fn eval_str(input: &str) -> Result<LispValue> {
    let parsed = LispParser::parse(input).unwrap()?;
    let mut env = testing_env();
    eval(&parsed, &mut env)
}

pub fn eval_str_in_env(input: &str, env: &mut LispEnv) -> Result<LispValue> {
    let parsed = LispParser::parse(input).unwrap()?;
    eval(&parsed, env)
}

#[macro_export]
macro_rules! eval {
    ($code:expr) => {
        eval_str($code).unwrap()
    };
    ($code:expr, $env:expr) => {
        eval_str_in_env($code, $env).unwrap()
    }
}