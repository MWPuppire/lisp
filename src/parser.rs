use std::ops::DerefMut;
use std::pin::Pin;
use im::{vector, Vector};
use string_interner::StringInterner;
use ordered_float::OrderedFloat;
use nom::{
    IResult,
    Finish,
    branch::alt,
    multi::{many0_count, many0, many1_count},
    combinator::{value, opt, recognize, map_opt, map_res, map},
    sequence::{preceded, tuple, pair, terminated, delimited},
    bytes::complete::{tag, take_while_m_n},
    character::complete::{multispace1, line_ending, char, none_of, one_of},
};
use crate::{LispValue, LispError, Result, LispEnv};

// numbers can't start an identifier, but they're valid in one
const IDEN_INVALID_CHARS_START: &str = "~@^{}()[]'\"&`\\,;:0123456789 \t\r\n\u{0}";
const IDEN_INVALID_CHARS: &str = "~@^{}()[]'\"&`\\,;: \t\r\n\u{0}";
// strings can't contain uenscaped backslashes or quotes
const INVALID_STRING_CHARS: &str = "\\\"\u{0}";

#[derive(Clone, Copy, Debug, PartialEq)]
enum LispTokenType<'a> {
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

    Number(f64),
    String(&'a str),
    Symbol(&'a str),
    Keyword(&'a str),
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
    ), LispTokenType::Symbol)(input)
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
    ), LispTokenType::Keyword)(input)
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

fn parse_string(input: &str) -> IResult<&str, String> {
    map(delimited(
        char('"'),
        many0(alt((
            parse_escape_sequence,
            none_of(INVALID_STRING_CHARS)
        ))),
        char('"'),
    ), |x| x.into_iter().collect())(input)
}

fn recognize_string(input: &str) -> IResult<&str, LispTokenType> {
    map(recognize(
        tuple((
            char('"'),
            many0_count(alt((
                parse_escape_sequence,
                none_of(INVALID_STRING_CHARS),
            ))),
            char('"'),
        )),
    ), LispTokenType::String)(input)
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
            recognize_string,
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
            identifier,
            keyword,
        )),
        many0_count(alt((
            value((), multispace1),
            value((), char(',')),
        ))),
    )(input)
}

fn some_or_err<T>(opt: Option<Result<T>>, err: LispError) -> Result<T> {
    match opt {
        None => Err(err),
        Some(Ok(x)) => Ok(x),
        Some(Err(x)) => Err(x),
    }
}

macro_rules! token_prefix {
    ($prefix:expr, $name:expr, $rest:expr, $interner:expr $(,)?) => {
        some_or_err(
            LispParser::read_form($rest, $interner),
            LispError::MissingToken($prefix),
        ).map(|(inner, rest)| {
            (LispValue::List(vector![
                LispValue::Symbol($interner.get_or_intern_static($name)),
                inner,
            ]), rest)
        })
    }
}

#[derive(Clone, Copy, Debug)]
struct LispToken<'a> {
    token: LispTokenType<'a>,
    row: usize,
    col: usize,
}

#[derive(Clone, Debug)]
pub struct LispParser {
    tokens: Vec<LispToken<'static>>,
    buffers: Vec<Pin<String>>,
    row: usize,
    col: usize,
}

impl LispParser {
    pub fn new() -> Self {
        LispParser {
            tokens: vec![],
            buffers: vec![],
            row: 1,
            col: 1,
        }
    }
    pub fn parse(input: &str) -> Option<Result<LispValue>> {
        let mut tokens = vec![];
        let mut buffer = String::with_capacity(input.len());
        match Self::tokenize(&mut tokens, &mut buffer, input, 1, 1) {
            Ok(_) => (),
            Err(e) => return Some(Err(e)),
        }
        // map to drop the leftover tokens from the result
        Self::read_form(&tokens, LispEnv::interner_mut().deref_mut())
            .map(|x| x.map(|(val, _)| val))
    }
    pub(crate) fn parse_with_interner(strs: &mut StringInterner, input: &str) -> Option<Result<LispValue>> {
        let mut tokens = vec![];
        let mut buffer = String::with_capacity(input.len());
        match Self::tokenize(&mut tokens, &mut buffer, input, 1, 1) {
            Ok(_) => (),
            Err(e) => return Some(Err(e)),
        }
        // map to drop the leftover tokens from the result
        Self::read_form(&tokens, strs)
            .map(|x| x.map(|(val, _)| val))
    }
    pub fn advance_line(&mut self) {
        self.row += 1;
        self.col = 1;
    }
    pub fn is_parse_complete(&self) -> bool {
        if let Some(peek) = self.peek() {
            match peek {
                Ok(_) => true,
                Err(LispError::UnbalancedDelim(_, _)) => false,
                Err(LispError::MissingToken(_)) => false,
                Err(_) => true,
            }
        } else {
            true
        }
    }
    pub fn has_tokens(&self) -> bool {
        !self.tokens.is_empty() && self.peek().is_some()
    }
    pub fn add_tokenize(&mut self, input: &str) -> Result<()> {
        let row = self.row;
        let col = self.col;
        let mut new_buf = String::with_capacity(input.len());
        let (new_row, new_col) = Self::tokenize(&mut self.tokens, &mut new_buf, input, row, col)?;
        self.buffers.push(Pin::new(new_buf));
        self.row = new_row;
        self.col = new_col;
        Ok(())
    }
    pub fn peek(&self) -> Option<Result<LispValue>> {
        // map to drop the leftover tokens from the result
        Self::read_form(&self.tokens, LispEnv::interner_mut().deref_mut())
            .map(|x| x.map(|(val, _)| val))
    }
    pub fn clear_tokens(&mut self) {
        self.tokens.clear();
        self.buffers.clear();
    }

    fn tokenize(tokens: &mut Vec<LispToken>, buffer: &mut String, mut input: &str, mut row: usize, mut col: usize) -> Result<(usize, usize)> {
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
                    let parsed = match parsed {
                        LispTokenType::String(s) => {
                            let buf_idx = buffer.len();
                            buffer.push_str(s);
                            LispTokenType::String(&buffer[buf_idx..])
                        },
                        LispTokenType::Symbol(sym) => {
                            let buf_idx = buffer.len();
                            buffer.push_str(sym);
                            LispTokenType::Symbol(&buffer[buf_idx..])
                        },
                        LispTokenType::Keyword(kw) => {
                            let buf_idx = buffer.len();
                            buffer.push_str(kw);
                            LispTokenType::Keyword(&buffer[buf_idx..])
                        },
                        x => x,
                    };
                    tokens.push(LispToken {
                        token: unsafe { std::mem::transmute(parsed) },
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

    fn read_form<'a>(tokens: &'a [LispToken<'a>], strs: &mut StringInterner) -> Option<Result<(LispValue, &'a [LispToken<'a>])>> {
        let Some((LispToken { token, row, col }, rest)) = tokens.split_first() else {
            return None;
        };
        Some(match token {
            LispTokenType::Comment     => Self::read_form(rest, strs)
                .unwrap_or(Ok((LispValue::Nil, rest))),
            LispTokenType::LParen      => Self::read_list(rest, strs),
            LispTokenType::RParen      => Err(LispError::SyntaxError(*row, *col)),
            LispTokenType::LBracket    => Self::read_vec(rest, strs),
            LispTokenType::RBracket    => Err(LispError::SyntaxError(*row, *col)),
            LispTokenType::LCurly      => Self::read_map(rest, strs),
            LispTokenType::RCurly      => Err(LispError::SyntaxError(*row, *col)),
            LispTokenType::AtSign      => token_prefix!("@", "deref", rest, strs),
            LispTokenType::Apostrophe  => token_prefix!("'", "quote", rest, strs),
            LispTokenType::Backtick    => token_prefix!("`", "quasiquote", rest, strs),
            LispTokenType::Tilde       => token_prefix!("~", "unquote", rest, strs),
            LispTokenType::TildeAtSign => token_prefix!("~@", "splice-unquote", rest, strs),
            LispTokenType::Ampersand   => {
                match some_or_err(
                    Self::read_form(rest, strs),
                    LispError::MissingToken("&"),
                ) {
                    Ok((LispValue::Symbol(s), rest)) => Ok((LispValue::VariadicSymbol(s), rest)),
                    Ok(_) => Err(LispError::SyntaxError(*row, *col)),
                    Err(x) => Err(x),
                }
            },
            _ => Self::read_atom(token, strs, *row, *col)
                .map(move |x| (x, rest)),
        })
    }
    fn read_list<'a>(tokens: &'a [LispToken<'a>], strs: &mut StringInterner) -> Result<(LispValue, &'a [LispToken<'a>])> {
        let mut res = Vector::new();
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, ")"))?;
            if *token == LispTokenType::RParen {
                break Ok((LispValue::List(res), rest));
            }
            let (exp, new_xs) = some_or_err(
                Self::read_form(xs, strs),
                LispError::UnbalancedDelim(0, ")"),
            ).map_err(|err| match err {
                LispError::UnbalancedDelim(x, ")") => LispError::UnbalancedDelim(x + 1, ")"),
                _ => err,
            })?;
            res.push_back(exp);
            xs = new_xs;
        }
    }
    fn read_vec<'a>(tokens: &'a [LispToken<'a>], strs: &mut StringInterner) -> Result<(LispValue, &'a [LispToken<'a>])> {
        let mut res = vec![];
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, "]"))?;
            if *token == LispTokenType::RBracket {
                break Ok((LispValue::Vector(res), rest));
            }
            let (exp, new_xs) = some_or_err(
                Self::read_form(xs, strs),
                LispError::UnbalancedDelim(0, "]"),
            ).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "]") => LispError::UnbalancedDelim(x + 1, "]"),
                _ => err,
            })?;
            res.push(exp);
            xs = new_xs;
        }
    }
    fn read_map<'a>(tokens: &'a [LispToken<'a>], strs: &mut StringInterner) -> Result<(LispValue, &'a [LispToken<'a>])> {
        let mut res: Vec<(LispValue, LispValue)> = vec![];
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, "}"))?;
            if *token == LispTokenType::RCurly {
                break Ok((LispValue::Map(res.into()), rest));
            }
            let (key_exp, new_xs) = some_or_err(
                Self::read_form(xs, strs),
                LispError::UnbalancedDelim(0, "}"),
            ).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "}") => LispError::UnbalancedDelim(x + 1, "}"),
                _ => err,
            })?;
            let (val_exp, new_xs) = some_or_err(
                Self::read_form(new_xs, strs),
                LispError::UnbalancedDelim(0, "}"),
            ).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "}") => LispError::UnbalancedDelim(x + 1, "}"),
                _ => err,
            })?;
            res.push((key_exp, val_exp));
            xs = new_xs;
        }
    }
    fn read_atom(token: &LispTokenType, strs: &mut StringInterner, row: usize, col: usize) -> Result<LispValue> {
        if let LispTokenType::Number(num) = token {
            Ok(LispValue::Number(OrderedFloat(*num)))
        } else if let LispTokenType::String(s) = token {
            parse_string(s)
                .finish()
                .map_err(|_| LispError::SyntaxError(row, col))
                .map(|(_, s)| LispValue::String(s))
        } else if *token == LispTokenType::Symbol("true") {
            Ok(LispValue::Bool(true))
        } else if *token == LispTokenType::Symbol("false") {
            Ok(LispValue::Bool(false))
        } else if *token == LispTokenType::Symbol("nil") {
            Ok(LispValue::Nil)
        } else if let LispTokenType::Symbol(sym) = token {
            Ok(LispValue::Symbol(strs.get_or_intern(sym)))
        } else if let LispTokenType::Keyword(kw) = token {
            Ok(LispValue::Keyword(kw.to_string()))
        } else {
            Ok(LispValue::Nil)
        }
    }
}
impl Default for LispParser {
    fn default() -> Self {
        Self::new()
    }
}
impl Iterator for LispParser {
    type Item = Result<LispValue>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut interner = LispEnv::interner_mut();
        match Self::read_form(&self.tokens, interner.deref_mut()) {
            None => None,
            Some(Ok((val, rest))) => {
                let idx = self.tokens.len() - rest.len();
                let _ = self.tokens.drain(0..idx);
                if self.tokens.is_empty() {
                    self.buffers.clear();
                }
                Some(Ok(val))
            },
            Some(Err(err)) => {
                // forget all processed tokens to clear the error
                self.tokens.clear();
                Some(Err(err))
            },
        }
    }
}
