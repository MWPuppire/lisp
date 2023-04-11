use regex::Regex;
use lazy_static::lazy_static;
use unescape::unescape;
use crate::{LispValue, LispError, Result};

struct LispToken {
    token: String,
    row: usize,
    col: usize,
}

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
    pub fn next_line(&mut self) {
        self.row += 1;
        self.col = 1;
    }
    pub fn is_complete(&self) -> bool {
        if let Some(peek) = self.peek() {
            match peek {
                Ok(_) => true,
                Err(LispError::UnbalancedDelim(_, _)) => false,
                Err(_) => true,
            }
        } else {
            true
        }
    }
    pub fn has_tokens(&self) -> bool {
        self.tokens.len() > 0 && self.peek().is_some()
    }
    pub fn add_tokenize(&mut self, input: &str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#).unwrap();
        }
        if input.len() == 0 {
            return;
        }
        let mut row = self.row;
        let mut col = self.col;
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
            self.tokens.push(LispToken {
                token: found.trim_start().to_owned(),
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
        self.row = row;
        self.col = col;
    }
    pub fn next(&mut self) -> Result<LispValue> {
        let read = Self::read_form(&self.tokens);
        if let Ok((val, rest)) = read {
            let idx = self.tokens.len() - rest.len();
            let _ = self.tokens.drain(0..idx);
            Ok(val)
        } else if let Err(err) = read {
            // just forget all processed tokens to clear the error
            self.tokens.clear();
            Err(err)
        } else {
            unreachable!();
        }
    }
    pub fn peek(&self) -> Option<Result<LispValue>> {
        match Self::read_form(&self.tokens) {
            Err(LispError::ParseNoTokens) => None,
            Err(x) => Some(Err(x)),
            Ok((val, _)) => Some(Ok(val)),
        }
    }
    fn read_form<'a>(tokens: &'a [LispToken]) -> Result<(LispValue, &'a [LispToken])> {
        if tokens.len() == 0 {
            return Err(LispError::ParseNoTokens);
        }
        // since we test for `tokens.len() == 0`, this has to be `Some`
        let (LispToken { token, row, col }, rest) = unsafe {
            tokens.split_first().unwrap_unchecked()
        };
        match token.as_str().chars().nth(0) {
            Some('(') => Self::read_list(rest),
            Some(')') => Err(LispError::SyntaxError(*row, *col)),
            Some('[') => Self::read_vec(rest),
            Some(']') => Err(LispError::SyntaxError(*row, *col)),
            Some('{') => Self::read_map(rest),
            Some('}') => Err(LispError::SyntaxError(*row, *col)),
            Some('@') => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol("deref".to_owned()),
                    inner,
                ]), new_rest))
            },
            Some('\'') => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol("quote".to_owned()),
                    inner,
                ]), new_rest))
            },
            Some('`') => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol("quasiquote".to_owned()),
                    inner,
                ]), new_rest))
            },
            Some('~') => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol(if token == "~@" {
                        "splice-unquote".to_owned()
                    } else {
                        "unquote".to_owned()
                    }),
                    inner,
                ]), new_rest))
            },
            Some('&') => {
                let (name, new_rest) = Self::read_form(rest)?;
                match name {
                    LispValue::Symbol(s) => Ok((LispValue::VariadicSymbol(s), new_rest)),
                    _ => Err(LispError::SyntaxError(*row, *col)),
                }
            },
            Some(';') => {
                // skip comment to next token
                Self::read_form(rest)
            },
            _ => Ok((Self::read_atom(token.clone(), *row, *col)?, rest)),
        }
    }
    fn read_list<'a>(tokens: &'a [LispToken]) -> Result<(LispValue, &'a [LispToken])> {
        let mut res: Vec<LispValue> = vec![];
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, ")"))?;
            if token == ")" {
                break Ok((LispValue::List(res), rest));
            }
            let (exp, new_xs) = Self::read_form(&xs).map_err(|err| match err {
                LispError::UnbalancedDelim(x, ")") => LispError::UnbalancedDelim(x + 1, ")"),
                _ => err,
            })?;
            res.push(exp);
            xs = new_xs;
        }
    }
    fn read_vec<'a>(tokens: &'a [LispToken]) -> Result<(LispValue, &'a [LispToken])> {
        let mut res: Vec<LispValue> = vec![];
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, "]"))?;
            if token == "]" {
                break Ok((LispValue::Vector(res), rest));
            }
            let (exp, new_xs) = Self::read_form(&xs).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "]") => LispError::UnbalancedDelim(x + 1, "]"),
                _ => err,
            })?;
            res.push(exp);
            xs = new_xs;
        }
    }
    fn read_map<'a>(tokens: &'a [LispToken]) -> Result<(LispValue, &'a [LispToken])> {
        let mut res: Vec<(LispValue, LispValue)> = vec![];
        let mut xs = tokens;
        loop {
            let (LispToken { token, .. }, rest) = xs.split_first().ok_or(LispError::UnbalancedDelim(1, "}"))?;
            if token == "}" {
                break Ok((LispValue::Map(res.into()), rest));
            }
            let (key_exp, new_xs) = Self::read_form(&xs).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "}") => LispError::UnbalancedDelim(x + 1, "}"),
                _ => err,
            })?;
            let (val_exp, new_xs) = Self::read_form(&new_xs).map_err(|err| match err {
                LispError::UnbalancedDelim(x, "}") => LispError::UnbalancedDelim(x + 1, "}"),
                _ => err,
            })?;
            res.push((key_exp, val_exp));
            xs = new_xs;
        }
    }
    fn read_atom(token: String, row: usize, col: usize) -> Result<LispValue> {
        if let Ok(f) = token.parse() {
            Ok(LispValue::Number(f))
        } else if token.starts_with("\"") {
            if token.len() > 1 && token.ends_with("\"") {
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
        } else if token.starts_with(":") {
            Ok(LispValue::Keyword(token[1..].to_owned()))
        } else if token.len() != 0 {
            Ok(LispValue::Symbol(token))
        } else {
            Ok(LispValue::Nil)
        }
    }
}
