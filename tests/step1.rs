mod common;
use common::*;

fn parse(input: &str) -> LispValue {
    LispParser::parse(input).unwrap().unwrap()
}

#[test]
fn read_numbers() {
    assert_eq!(parse("1"), 1.0.into());
    assert_eq!(parse("7"), 7.0.into());
    assert_eq!(parse("-123"), (-123.0).into());
}

#[test]
fn read_symbols() {
    assert_eq!(parse("+"), LispValue::symbol_for_static("+"));
    assert_eq!(parse("abc"), LispValue::symbol_for_static("abc"));
    assert_eq!(parse("abc5"), LispValue::symbol_for_static("abc5"));
    assert_eq!(parse("abc-def"), LispValue::symbol_for_static("abc-def"));
    assert_eq!(parse("-"), LispValue::symbol_for_static("-"));
    assert_eq!(parse("-abc"), LispValue::symbol_for_static("-abc"));
}

#[test]
fn read_lists() {
    assert_eq!(parse("(+ 1 2)"), LispValue::list_from(vector![
        LispValue::symbol_for_static("+"),
        1.0.into(),
        2.0.into(),
    ]));
    assert_eq!(parse("()"), LispValue::list_from(vector![]));
    assert_eq!(parse("( )"), LispValue::list_from(vector![]));
    assert_eq!(parse("(nil)"), LispValue::list_from(vector![
        LispValue::Nil,
    ]));
}

#[test]
fn nested_lists() {
    assert_eq!(parse("((3 4))"), LispValue::list_from(vector![
        LispValue::list_from(vector![
            3.0.into(),
            4.0.into(),
        ]),
    ]));
    assert_eq!(parse("(+ 1 (+ 2 3))"), LispValue::list_from(vector![
        LispValue::symbol_for_static("+"),
        1.0.into(),
        LispValue::list_from(vector![
            LispValue::symbol_for_static("+"),
            2.0.into(),
            3.0.into(),
        ]),
    ]));
    assert_eq!(parse("(()())"), LispValue::list_from(vector![
        LispValue::list_from(vector![]),
        LispValue::list_from(vector![]),
    ]));
}

#[test]
fn ignore_commas() {
    assert_eq!(parse("(1 2, 3,,,,),,"), LispValue::list_from(vector![
        1.0.into(),
        2.0.into(),
        3.0.into(),
    ]));
}

#[test]
fn builtin_values() {
    assert_eq!(parse("nil"), LispValue::Nil);
    assert_eq!(parse("true"), true.into());
    assert_eq!(parse("false"), false.into());
}
