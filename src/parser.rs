use std::ops::DerefMut;
use regex::Regex;
use lazy_static::lazy_static;
use unescape::unescape;
use im::{vector, Vector};
use string_interner::StringInterner;
use crate::{LispValue, LispError, Result, LispEnv};

fn some_or_err<T>(opt: Option<Result<T>>, err: LispError) -> Result<T> {
    match opt {
        None => Err(err),
        Some(Ok(x)) => Ok(x),
        Some(Err(x)) => Err(x),
    }
}

macro_rules! token_prefix {
    ($prefix:literal, $name:expr, $rest:expr, $interner:expr $(,)?) => {
        some_or_err(
            LispParser::read_form($rest, $interner),
            LispError::MissingToken,
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
        Self::tokenize(&mut tokens, input, 1, 1);
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
                Err(LispError::MissingToken) => false,
                Err(_) => true,
            }
        } else {
            true
        }
    }
    pub fn has_tokens(&self) -> bool {
        !self.tokens.is_empty() && self.peek().is_some()
    }
    pub fn add_tokenize(&mut self, input: &str) {
        let row = self.row;
        let col = self.col;
        let (new_row, new_col) = Self::tokenize(&mut self.tokens, input, row, col);
        self.row = new_row;
        self.col = new_col;
    }
    pub fn peek(&self) -> Option<Result<LispValue>> {
        // map to drop the leftover tokens from the result
        Self::read_form(&self.tokens, LispEnv::interner_mut().deref_mut())
            .map(|x| x.map(|(val, _)| val))
    }

    fn tokenize(tokens: &mut Vec<LispToken>, input: &str, mut row: usize, mut col: usize) -> (usize, usize) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#).unwrap();
        }
        if input.is_empty() {
            return (row, col);
        }
        let mut last_idx = 0;
        for found in RE.find_iter(input) {
            let start = found.start();
            let end = found.end();
            for ch in input[last_idx..start].chars() {
                if ch == '\n' {
                    col = 1;
                    row += 1;
                } else {
                    col += 1;
                }
            }
            let found = &input[start..end];
            tokens.push(LispToken {
                token: found.trim_start_matches(|c: char| {
                    c.is_whitespace() || c == ','
                }).to_owned(),
                row,
                col
            });
            col += end - start;
            last_idx = end;
        }
        let last_ch = input.len() - 1;
        if last_ch >= last_idx {
            for ch in input[last_idx..last_ch].chars() {
                if ch == '\n' {
                    col = 1;
                    row += 1;
                } else {
                    col += 1;
                }
            }
        }
        (row, col)
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
            Some('@') => token_prefix!('@', "deref", rest, strs),
            Some('\'') => token_prefix!('\'', "quote", rest, strs),
            Some('`') => token_prefix!('`', "quasiquote", rest, strs),
            Some('~') => token_prefix!(
                '~',
                if token == "~@" { "splice-unquote" } else { "unquote" },
                rest,
                strs,
            ),
            Some('&') => {
                match some_or_err(
                    Self::read_form(rest, strs),
                    LispError::MissingToken,
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
            if token.len() > 1 && token.ends_with('\"') {
                let last = token.len() - 1;
                let unescaped = unescape(&token[1..last]);
                if let Some(s) = unescaped {
                    Ok(LispValue::String(s))
                } else {
                    Err(LispError::SyntaxError(row, col))
                }
            } else {
                Err(LispError::SyntaxError(row, col))
            }
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
