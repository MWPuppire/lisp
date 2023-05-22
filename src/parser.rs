use std::collections::VecDeque;
use std::ops::DerefMut;
use im::{vector, Vector};
use string_interner::StringInterner;
use ordered_float::OrderedFloat;
use nom::{
    IResult,
    Finish,
    branch::alt,
    multi::{many0_count, many0, many1_count},
    combinator::{value, opt, recognize, map_opt, map_res, map, peek, not},
    sequence::{preceded, tuple, pair, terminated, delimited},
    bytes::complete::{tag, take_while_m_n},
    character::complete::{multispace1, line_ending, char, none_of, one_of},
};
use crate::{LispValue, LispError, Result, LispEnv};
use crate::util::LispSpecialForm;

// numbers can't start an identifier, but they're valid in one
const IDEN_INVALID_CHARS_START: &str = "~@^{}()[]'\"&`\\,;:0123456789 \t\r\n\u{0}";
const IDEN_INVALID_CHARS: &str = "~@^{}()[]'\"&`\\,;: \t\r\n\u{0}";
// strings can't contain uenscaped backslashes or quotes
const INVALID_STRING_CHARS: &str = "\\\"\u{0}";

#[derive(Clone, Debug, PartialEq)]
enum LispTokenType {
    LParen,
    RParen,
    LBracket,
    RBracket,
    LCurly,
    RCurly,
    Apostrophe,
    Backtick,
    Tilde,
    TildeAtSign,
    AtSign,
    Ampersand,
    Comment,
    Nil,
    True,
    False,
    Special(LispSpecialForm),

    Number(f64),
    String(String),
    Symbol(String),
    Keyword(String),
}

macro_rules! reserved_word_token {
    ($out:expr, $word:literal) => {
        value($out, terminated(
            tag($word),
            // `not(none_of)` instead of `one_of` to include EOF;
            // if the input ends on a reserved word, it still matches
            not(peek(none_of(IDEN_INVALID_CHARS))),
        ))
    }
}
macro_rules! special_form {
    ($name:ident) => {
        LispTokenType::Special(LispSpecialForm::$name)
    }
}

fn line_comment(i: &str) -> IResult<&str, LispTokenType> {
    value(LispTokenType::Comment, tuple((
        char(';'),
        many0_count(none_of("\r\n")),
        // in case the input ends without a newline
        opt(line_ending),
    )))(i)
}

fn identifier(input: &str) -> IResult<&str, LispTokenType> {
    map(recognize(
        pair(
            none_of(IDEN_INVALID_CHARS_START),
            many0_count(none_of(IDEN_INVALID_CHARS)),
        ),
    ), |x: &str| LispTokenType::Symbol(x.to_owned()))(input)
}

fn keyword(input: &str) -> IResult<&str, LispTokenType> {
    map(preceded(
        char(':'),
        recognize(
            pair(
                none_of(IDEN_INVALID_CHARS_START),
                many0_count(none_of(IDEN_INVALID_CHARS)),
            ),
        ),
    ), |x: &str| LispTokenType::Keyword(x.to_owned()))(input)
}

fn number(input: &str) -> IResult<&str, LispTokenType> {
    alt((
        map(recognize( // Case one: .42
            tuple((
                opt(one_of("+-")),
                char('.'),
                decimal,
                opt(tuple((
                    one_of("eE"),
                    opt(one_of("+-")),
                    decimal
                )))
            )),
        ), |x| LispTokenType::Number(x.parse().unwrap())),
        map(recognize( // Case two: 42e42 and 42.42e42
            tuple((
                decimal,
                opt(preceded(
                    char('.'),
                    decimal,
                )),
                one_of("eE"),
                opt(one_of("+-")),
                decimal
            )),
        ), |x| LispTokenType::Number(x.parse().unwrap())),
        map(recognize( // Case three: 42. and 42.42
            tuple((
                decimal,
                char('.'),
                opt(decimal)
            )),
        ), |x| LispTokenType::Number(x.parse().unwrap())),
        // Integers
        map(decimal, |x| {
            LispTokenType::Number(x.into())
        }),
        map(recognize( // Binary integers
            pair(
                opt(one_of::<&str, _, _>("+-")),
                preceded(
                    alt((tag("0b"), tag("0B"))),
                    many1_count(
                        terminated(
                            one_of("01"),
                            opt(pair(
                                many1_count(char('_')),
                                one_of("01"),
                            )),
                        ),
                    ),
                ),
            ),
        ), |x| {
            let sign = if x.as_bytes()[0] == b'-' { -1 } else { 1 };
            let index = if x.as_bytes()[0] == b'0' { 2 } else { 3 };
            let int = sign * i32::from_str_radix(&x[index..], 2).unwrap();
            LispTokenType::Number(int.into())
        }),
        map(recognize( // Octal integers
            pair(
                opt(one_of::<&str, _, _>("+-")),
                preceded(
                    alt((tag("0o"), tag("0o"))),
                    many1_count(
                        terminated(
                            one_of("01234567"),
                            opt(pair(
                                many1_count(char('_')),
                                one_of("01234567"),
                            )),
                        ),
                    ),
                ),
            ),
        ), |x| {
            let sign = if x.as_bytes()[0] == b'-' { -1 } else { 1 };
            let index = if x.as_bytes()[0] == b'0' { 2 } else { 3 };
            let int = sign * i32::from_str_radix(&x[index..], 8).unwrap();
            LispTokenType::Number(int.into())
        }),
        map(recognize( // Hexadecimal integers
            pair(
                opt(one_of::<&str, _, _>("+-")),
                preceded(
                    alt((tag("0x"), tag("0X"))),
                    many1_count(
                        terminated(
                            one_of("0123456789abcdefABCDEF"),
                            opt(pair(
                                many1_count(char('_')),
                                one_of("0123456789abcdefABCDEF"),
                            )),
                        ),
                    ),
                ),
            ),
        ), |x| {
            let sign = if x.as_bytes()[0] == b'-' { -1 } else { 1 };
            let index = if x.as_bytes()[0] == b'0' { 2 } else { 3 };
            let int = sign * i32::from_str_radix(&x[index..], 16).unwrap();
            LispTokenType::Number(int.into())
        }),
    ))(input)
}

fn decimal(input: &str) -> IResult<&str, i32> {
    map(recognize(
        pair(
            opt(one_of::<&str, _, _>("+-")),
            many1_count(
                terminated(
                    one_of("0123456789"),
                    opt(pair(
                        many1_count(char('_')),
                        one_of("0123456789"),
                    )),
                ),
            ),
        ),
    ), |x| x.parse().unwrap())(input)
}

fn parse_unicode(input: &str) -> IResult<&str, char> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());
    let parse_delimited_hex = preceded(
        char('u'),
        delimited(
            char('{'),
            parse_hex,
            char('}'),
        ),
    );
    let parse_u32 = map_res(parse_delimited_hex, move |hex| {
        u32::from_str_radix(hex, 16)
    });
    map_opt(parse_u32, std::char::from_u32)(input)
}

fn parse_escape_sequence(input: &str) -> IResult<&str, char> {
    preceded(
        char('\\'),
        alt((
            parse_unicode,
            value('\n', line_ending),
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('"', char('"')),
            value('\'', char('\'')),
        )),
    )(input)
}

fn parse_string(input: &str) -> IResult<&str, LispTokenType> {
    map(delimited(
        char('"'),
        many0(alt((
            parse_escape_sequence,
            none_of(INVALID_STRING_CHARS)
        ))),
        char('"'),
    ), |x| LispTokenType::String(x.into_iter().collect()))(input)
}

fn parse_lisp(input: &str) -> IResult<&str, LispTokenType> {
    delimited(
        many0_count(alt((
            value((), multispace1),
            value((), char(',')),
        ))),
        alt((
            line_comment,
            number,
            parse_string,
            value(LispTokenType::LParen, char('(')),
            value(LispTokenType::RParen, char(')')),
            value(LispTokenType::TildeAtSign, tag("~@")),
            value(LispTokenType::Tilde, char('~')),
            value(LispTokenType::LCurly, char('{')),
            value(LispTokenType::RCurly, char('}')),
            value(LispTokenType::LBracket, char('[')),
            value(LispTokenType::RBracket, char(']')),
            value(LispTokenType::Apostrophe, char('\'')),
            value(LispTokenType::Ampersand, char('&')),
            value(LispTokenType::AtSign, char('@')),
            value(LispTokenType::Backtick, char('`')),
            alt((
                reserved_word_token!(LispTokenType::Nil, "nil"),
                reserved_word_token!(LispTokenType::True, "true"),
                reserved_word_token!(LispTokenType::False, "false"),
                reserved_word_token!(special_form!(Def), "def!"),
                reserved_word_token!(special_form!(Defmacro), "defmacro!"),
                reserved_word_token!(special_form!(Let), "let*"),
                reserved_word_token!(special_form!(Quote), "quote"),
                reserved_word_token!(special_form!(Quasiquote), "quasiquote"),
                reserved_word_token!(special_form!(Unquote), "unquote"),
                reserved_word_token!(special_form!(SpliceUnquote), "splice-unquote"),
                reserved_word_token!(special_form!(Macroexpand), "macroexpand"),
                reserved_word_token!(special_form!(Try), "try*"),
                reserved_word_token!(special_form!(Catch), "catch*"),
                reserved_word_token!(special_form!(Do), "do"),
                reserved_word_token!(special_form!(If), "if"),
                reserved_word_token!(special_form!(Fn), "fn*"),
                reserved_word_token!(special_form!(Deref), "deref"),
                reserved_word_token!(special_form!(Eval), "eval"),
                reserved_word_token!(special_form!(Apply), "apply"),
                reserved_word_token!(special_form!(Cond), "cond"),
            )),
            identifier,
            keyword,
        )),
        many0_count(alt((
            value((), multispace1),
            value((), char(',')),
        ))),
    )(input)
}

#[inline]
fn some_or_err<T>(opt: Option<Result<T>>, err: LispError) -> Result<T> {
    match opt {
        None => Err(err),
        Some(Ok(x)) => Ok(x),
        Some(Err(x)) => Err(x),
    }
}

macro_rules! token_prefix {
    ($prefix:expr, $name:ident, $rest:expr, $interner:expr $(,)?) => {
        some_or_err(
            LispParser::read_form($rest, $interner),
            LispError::MissingToken($prefix),
        ).map(|inner| {
            LispValue::list_from(vector![
                LispValue::Special(LispSpecialForm::$name),
                inner,
            ])
        })
    }
}

#[derive(Clone, Debug)]
struct LispToken {
    token: LispTokenType,
    row: usize,
    col: usize,
}

#[derive(Clone, Debug)]
pub struct LispParser {
    tokens: VecDeque<LispToken>,
    row: usize,
    col: usize,
}

impl LispParser {
    pub fn new() -> Self {
        LispParser {
            tokens: VecDeque::new(),
            row: 1,
            col: 1,
        }
    }
    pub fn parse(input: &str) -> Option<Result<LispValue>> {
        let mut tokens = VecDeque::new();
        match Self::tokenize(&mut tokens, input, 1, 1) {
            Ok(_) => (),
            Err(e) => return Some(Err(e)),
        }
        Self::read_form(&mut tokens, LispEnv::interner_mut().deref_mut())
    }
    pub fn advance_line(&mut self) {
        self.row += 1;
        self.col = 1;
    }
    #[inline]
    pub fn has_tokens(&self) -> bool {
        !self.tokens.is_empty()
    }
    pub fn add_tokenize(&mut self, input: &str) -> Result<()> {
        let row = self.row;
        let col = self.col;
        let (new_row, new_col) = Self::tokenize(&mut self.tokens, input, row, col)?;
        self.row = new_row;
        self.col = new_col;
        Ok(())
    }
    #[inline]
    pub fn clear_tokens(&mut self) {
        self.tokens.clear();
    }

    fn tokenize(tokens: &mut VecDeque<LispToken>, mut input: &str, mut row: usize, mut col: usize) -> Result<(usize, usize)> {
        loop {
            if input.is_empty() {
                break;
            }
            let parsed = parse_lisp(input);
            match parsed.finish() {
                Ok((new_input, parsed)) => {
                    while !std::ptr::eq(input, new_input) {
                        let ch = input.chars().next().unwrap();
                        if ch == '\n' {
                            col = 1;
                            row += 1;
                        } else {
                            col += 1;
                        }
                        input = &input[ch.len_utf8()..];
                    }
                    tokens.push_back(LispToken {
                        token: parsed,
                        row,
                        col,
                    });
                },
                Err(_) => return Err(LispError::SyntaxError(row, col)),
            }
        }
        if !input.is_empty() {
            for ch in input.chars() {
                if ch == '\n' {
                    col = 1;
                    row += 1;
                } else {
                    col += 1;
                }
            }
        }
        Ok((row, col))
    }

    fn read_form(tokens: &mut VecDeque<LispToken>, strs: &mut StringInterner) -> Option<Result<LispValue>> {
        let Some(LispToken { token, row, col }) = tokens.pop_front() else {
            return None;
        };
        Some(match token {
            LispTokenType::Comment     => Self::read_form(tokens, strs)
                .unwrap_or(Ok(LispValue::Nil)),
            LispTokenType::LParen      => Self::read_list(tokens, strs),
            LispTokenType::RParen      => Err(LispError::SyntaxError(row, col)),
            LispTokenType::LBracket    => Self::read_vec(tokens, strs),
            LispTokenType::RBracket    => Err(LispError::SyntaxError(row, col)),
            LispTokenType::LCurly      => Self::read_map(tokens, strs),
            LispTokenType::RCurly      => Err(LispError::SyntaxError(row, col)),
            LispTokenType::AtSign      => token_prefix!("@", Deref, tokens, strs),
            LispTokenType::Apostrophe  => token_prefix!("'", Quote, tokens, strs),
            LispTokenType::Backtick    => token_prefix!("`", Quasiquote, tokens, strs),
            LispTokenType::Tilde       => token_prefix!("~", Unquote, tokens, strs),
            LispTokenType::TildeAtSign => token_prefix!("~@", SpliceUnquote, tokens, strs),
            LispTokenType::Ampersand   => {
                match some_or_err(
                    Self::read_form(tokens, strs),
                    LispError::MissingToken("&"),
                ) {
                    Ok(LispValue::Symbol(s)) => Ok(LispValue::VariadicSymbol(s)),
                    Ok(_) => Err(LispError::SyntaxError(row, col)),
                    Err(x) => Err(x),
                }
            },
            x => Ok(Self::read_atom(x, strs)),
        })
    }
    fn read_list(tokens: &mut VecDeque<LispToken>, strs: &mut StringInterner) -> Result<LispValue> {
        let mut res = Vector::new();
        loop {
            match tokens.get(0) {
                Some(LispToken { token: LispTokenType::RParen, .. }) => {
                    tokens.pop_front();
                    return Ok(LispValue::list_from(res));
                },
                None => return Err(LispError::UnbalancedDelim(1, ")")),
                _ => (),
            };
            let exp = some_or_err(
                Self::read_form(tokens, strs),
                LispError::UnbalancedDelim(0, ")"),
            ).map_err(|err| match err {
                LispError::UnbalancedDelim(x, ")") => LispError::UnbalancedDelim(x + 1, ")"),
                _ => err,
            })?;
            res.push_back(exp);
        }
    }
    fn read_vec(tokens: &mut VecDeque<LispToken>, strs: &mut StringInterner) -> Result<LispValue> {
        let mut res = vec![];
        loop {
            match tokens.get(0) {
                Some(LispToken { token: LispTokenType::RBracket, .. }) => {
                    tokens.pop_front();
                    return Ok(LispValue::vector_from(res));
                },
                None => return Err(LispError::UnbalancedDelim(1, "]")),
                _ => (),
            };
            let exp = some_or_err(
                Self::read_form(tokens, strs),
                LispError::UnbalancedDelim(0, "]"),
            ).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "]") => LispError::UnbalancedDelim(x + 1, "]"),
                _ => err,
            })?;
            res.push(exp);
        }
    }
    fn read_map(tokens: &mut VecDeque<LispToken>, strs: &mut StringInterner) -> Result<LispValue> {
        let mut res: Vec<(LispValue, LispValue)> = vec![];
        loop {
            match tokens.get(0) {
                Some(LispToken { token: LispTokenType::RCurly, .. }) => {
                    tokens.pop_front();
                    return Ok(LispValue::map_from(res));
                },
                None => return Err(LispError::UnbalancedDelim(1, "}")),
                _ => (),
            };
            let key_exp = some_or_err(
                Self::read_form(tokens, strs),
                LispError::UnbalancedDelim(0, "}"),
            ).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "}") => LispError::UnbalancedDelim(x + 1, "}"),
                _ => err,
            })?;
            let val_exp = some_or_err(
                Self::read_form(tokens, strs),
                LispError::MissingBinding,
            ).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "}") => LispError::UnbalancedDelim(x + 1, "}"),
                _ => err,
            })?;
            res.push((key_exp, val_exp));
        }
    }
    fn read_atom(token: LispTokenType, strs: &mut StringInterner) -> LispValue {
        match token {
            LispTokenType::Number(num) => LispValue::Number(OrderedFloat(num)),
            LispTokenType::String(s) => LispValue::string_for(s),
            LispTokenType::Symbol(sym) => LispValue::Symbol(strs.get_or_intern(&sym)),
            LispTokenType::Keyword(kw) => LispValue::keyword_for(kw),
            LispTokenType::True => LispValue::Bool(true),
            LispTokenType::False => LispValue::Bool(false),
            LispTokenType::Special(form) => LispValue::Special(form),
            _ => LispValue::Nil,
        }
    }
}
impl Default for LispParser {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}
impl Iterator for LispParser {
    type Item = Result<LispValue>;
    fn next(&mut self) -> Option<Result<LispValue>> {
        let mut interner = LispEnv::interner_mut();
        let out = match Self::read_form(&mut self.tokens, interner.deref_mut()) {
            Some(Err(err)) => {
                // forget all processed tokens to clear the error
                self.tokens.clear();
                Some(Err(err))
            },
            x => x,
        };
        out
    }
}
