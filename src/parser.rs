use std::ops::DerefMut;
use im::{vector, Vector};
use string_interner::StringInterner;
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
const IDEN_INVALID_CHARS_START: &str = "~@^{}()[]'\"&`\\,;0123456789 \t\r\n";
const IDEN_INVALID_CHARS: &str = "~@^{}()[]'\"&`\\,; \t\r\n";
// strings can't contain uenscaped newlines, backslashes, or quotes
const INVALID_STRING_CHARS: &str = "\r\n\\\"";

fn line_comment(i: &str) -> IResult<&str, &str> {
    recognize(tuple((
        char(';'),
        many0_count(none_of("\r\n")),
        // in case the input ends without a newline
        opt(line_ending),
    )))(i)
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            none_of(IDEN_INVALID_CHARS_START),
            many0_count(none_of(IDEN_INVALID_CHARS)),
        ),
    )(input)
}

fn number(input: &str) -> IResult<&str, &str> {
    alt((
        recognize( // Case one: .42
            tuple((
                char('.'),
                decimal,
                opt(tuple((
                    one_of("eE"),
                    opt(one_of("+-")),
                    decimal
                )))
            ))
        ),
        recognize( // Case two: 42e42 and 42.42e42
            tuple((
                decimal,
                opt(preceded(
                    char('.'),
                    decimal,
                )),
                one_of("eE"),
                opt(one_of("+-")),
                decimal
            ))
        ),
        recognize( // Case three: 42. and 42.42
            tuple((
                decimal,
                char('.'),
                opt(decimal)
            ))
        ),
        // Integers
        recognize(decimal),
        recognize( // Binary integers
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
            )
        ),
        recognize( // Octal integers
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
            )
        ),
        recognize( // Hexadecimal integers
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
            )
        ),
    ))(input)
}

fn decimal(input: &str) -> IResult<&str, &str> {
    recognize(
        many1_count(
            terminated(
                one_of("0123456789"),
                opt(pair(
                    many1_count(char('_')),
                    one_of("0123456789"),
                )),
            ),
        )
    )(input)
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

fn recognize_string(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        char('"'),
        many0_count(alt((
            parse_escape_sequence,
            none_of(INVALID_STRING_CHARS),
        ))),
        char('"'),
    )))(input)
}

fn parse_lisp(input: &str) -> IResult<&str, &str> {
    delimited(
        many0_count(alt((
            value((), multispace1),
            value((), char(',')),
        ))),
        recognize(alt((
            line_comment,
            number,
            recognize_string,
            value("(", char('(')),
            value(")", char(')')),
            tag("~@"),
            value("~", char('~')),
            value("{", char('{')),
            value("}", char('}')),
            value("[", char('[')),
            value("]", char(']')),
            value("'", char('\'')),
            value("&", char('&')),
            value("@", char('@')),
            value("`", char('`')),
            identifier,
        ))),
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

#[derive(Clone, Debug)]
struct LispToken {
    token: String,
    row: usize,
    col: usize,
}

#[derive(Clone, Debug)]
pub struct LispParser {
    tokens: Vec<LispToken>,
    row: usize,
    col: usize,
}
impl LispParser {
    pub fn new() -> Self {
        LispParser {
            tokens: vec![],
            row: 1,
            col: 1,
        }
    }
    pub fn parse(input: &str) -> Option<Result<LispValue>> {
        let mut tokens = vec![];
        match Self::tokenize(&mut tokens, input, 1, 1) {
            Ok(_) => (),
            Err(e) => return Some(Err(e)),
        }
        // map to drop the leftover tokens from the result
        Self::read_form(&tokens, LispEnv::interner_mut().deref_mut())
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
        let (new_row, new_col) = Self::tokenize(&mut self.tokens, input, row, col)?;
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
    }

    fn tokenize(tokens: &mut Vec<LispToken>, mut input: &str, mut row: usize, mut col: usize) -> Result<(usize, usize)> {
        loop {
            if input.is_empty() {
                break;
            }
            let parsed = parse_lisp(input);
            match parsed.finish() {
                Ok((new_input, parsed)) => {
                    let start_pos = unsafe {
                        new_input.as_ptr().offset_from(input.as_ptr()) as usize
                    };
                    for ch in input[..start_pos].chars() {
                        if ch == '\n' {
                            col = 1;
                            row += 1;
                        } else {
                            col += 1;
                        }
                    }
                    input = new_input;
                    tokens.push(LispToken {
                        token: parsed.to_owned(),
                        row,
                        col,
                    });
                    col += parsed.len();
                },
                Err(_) => return Err(LispError::SyntaxError(row, col)),
            }
        }
        if input.len() > 0 {
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

    fn read_form<'a>(tokens: &'a [LispToken], strs: &mut StringInterner) -> Option<Result<(LispValue, &'a [LispToken])>> {
        let Some((LispToken { token, row, col }, rest)) = tokens.split_first() else {
            return None;
        };
        Some(match token.as_str().chars().next() {
            Some('(') => Self::read_list(rest, strs),
            Some(')') => Err(LispError::SyntaxError(*row, *col)),
            Some('[') => Self::read_vec(rest, strs),
            Some(']') => Err(LispError::SyntaxError(*row, *col)),
            Some('{') => Self::read_map(rest, strs),
            Some('}') => Err(LispError::SyntaxError(*row, *col)),
            Some('@') => token_prefix!("@", "deref", rest, strs),
            Some('\'') => token_prefix!("'", "quote", rest, strs),
            Some('`') => token_prefix!("`", "quasiquote", rest, strs),
            Some('~') => token_prefix!(
                if token == "~@" { "~@" } else { "~" },
                if token == "~@" { "splice-unquote" } else { "unquote" },
                rest,
                strs,
            ),
            Some('&') => {
                match some_or_err(
                    Self::read_form(rest, strs),
                    LispError::MissingToken("&"),
                ) {
                    Ok((LispValue::Symbol(s), rest)) => Ok((LispValue::VariadicSymbol(s), rest)),
                    Ok(_) => Err(LispError::SyntaxError(*row, *col)),
                    Err(x) => Err(x),
                }
            },
            Some(';') => {
                // skip comment to next token
                return Self::read_form(rest, strs);
            },
            _ => Self::read_atom(token.clone(), strs, *row, *col)
                .map(move |x| (x, rest)),
        })
    }
    fn read_list<'a>(tokens: &'a [LispToken], strs: &mut StringInterner) -> Result<(LispValue, &'a [LispToken])> {
        let mut res = Vector::new();
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, ")"))?;
            if token == ")" {
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
    fn read_vec<'a>(tokens: &'a [LispToken], strs: &mut StringInterner) -> Result<(LispValue, &'a [LispToken])> {
        let mut res = vec![];
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, "]"))?;
            if token == "]" {
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
    fn read_map<'a>(tokens: &'a [LispToken], strs: &mut StringInterner) -> Result<(LispValue, &'a [LispToken])> {
        let mut res: Vec<(LispValue, LispValue)> = vec![];
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, "}"))?;
            if token == "}" {
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
    fn read_atom(token: String, strs: &mut StringInterner, row: usize, col: usize) -> Result<LispValue> {
        if let Ok(f) = token.parse() {
            Ok(LispValue::Number(f))
        } else if token.starts_with('\"') {
            parse_string(&token)
                .finish()
                .map_err(|_| LispError::SyntaxError(row, col))
                .map(|(_, s)| LispValue::String(s))
        } else if token == "true" {
            Ok(LispValue::Bool(true))
        } else if token == "false" {
            Ok(LispValue::Bool(false))
        } else if token == "nil" {
            Ok(LispValue::Nil)
        } else if let Some(stripped) = token.strip_prefix(':') {
            Ok(LispValue::Keyword(stripped.to_owned()))
        } else if !token.is_empty() {
            Ok(LispValue::Symbol(strs.get_or_intern(token)))
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
