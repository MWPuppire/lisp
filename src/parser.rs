use regex::Regex;
use lazy_static::lazy_static;
use unescape::unescape;
use crate::{LispValue, LispError, Result};

pub struct LispParser {
    tokens: Vec<String>,
}
impl LispParser {
    pub fn new() -> Self {
        LispParser {
            tokens: vec![],
        }
    }
    pub fn is_complete(&self) -> bool {
        match self.peek() {
            Ok(_) => true,
            Err(LispError::UnbalancedParens(_)) => false,
            Err(_) => true,
        }
    }
    pub fn has_tokens(&self) -> bool {
        self.tokens.len() > 0
    }
    pub fn add_tokenize(&mut self, input: &str) {
        lazy_static! {
            static ref RE: Regex = Regex::new(r#"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)"#).unwrap();
        }
        let input = input.trim_end();
        if input.len() == 0 {
            return;
        }
        for found in RE.find_iter(input) {
            let start = found.start();
            let end = found.end();
            let found = &input[start..end];
            self.tokens.push(found.trim_start().to_owned());
        }
    }
    pub fn next(&mut self) -> Result<LispValue> {
        let (val, rest) = Self::read_form(&self.tokens)?;
        let idx = self.tokens.len() - rest.len();
        let _ = self.tokens.drain(0..idx);
        Ok(val)
    }
    pub fn peek(&self) -> Result<LispValue> {
        let (val, _) = Self::read_form(&self.tokens)?;
        Ok(val)
    }
    fn read_form<'a>(tokens: &'a [String]) -> Result<(LispValue, &'a [String])> {
        if tokens.len() == 0 {
            return Ok((LispValue::Nil, tokens));
        }
        // since we test for `tokens.len() == 0`, this has to be `Some`
        let (token, rest) = unsafe { tokens.split_first().unwrap_unchecked() };
        match token.as_str() {
            "(" => Self::read_list(rest),
            ")" => Err(LispError::SyntaxError(0, 0)),
            "@" => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol("deref".to_owned()),
                    inner,
                ]), new_rest))
            },
            "'" => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol("quote".to_owned()),
                    inner,
                ]), new_rest))
            },
            "`" => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol("quasiquote".to_owned()),
                    inner,
                ]), new_rest))
            },
            "~" => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol("unquote".to_owned()),
                    inner,
                ]), new_rest))
            },
            "~@" => {
                let (inner, new_rest) = Self::read_form(rest)?;
                Ok((LispValue::List(vec![
                    LispValue::Symbol("splice-unquote".to_owned()),
                    inner,
                ]), new_rest))
            },
            _ => Ok((Self::read_atom(token.clone())?, rest)),
        }
    }
    fn read_list<'a>(tokens: &'a [String]) -> Result<(LispValue, &'a [String])> {
        let mut res: Vec<LispValue> = vec![];
        let mut xs = tokens;
        loop {
            let (next_token, rest) = xs.split_first().ok_or(LispError::UnbalancedParens(1))?;
            if next_token == ")" {
                break Ok((LispValue::List(res), rest));
            }
            let (exp, new_xs) = Self::read_form(&xs).map_err(|err| match err {
                LispError::UnbalancedParens(x) => LispError::UnbalancedParens(x + 1),
                _ => err,
            })?;
            res.push(exp);
            xs = new_xs;
        }
    }
    fn read_atom(token: String) -> Result<LispValue> {
        if token.starts_with(";") {
            // comment
            Ok(LispValue::Nil)
        } else if let Ok(f) = token.parse() {
            Ok(LispValue::Number(f))
        } else if token.starts_with("\"") {
            if token.ends_with("\"") {
                let last = token.len() - 1;
                let unescaped = unescape(&token[1..last]);
                if let Some(s) = unescaped {
                    Ok(LispValue::String(s))
                } else {
                    Err(LispError::SyntaxError(0, 0))
                }
            } else {
                Err(LispError::SyntaxError(0, 0))
            }
        } else if token.len() != 0 {
            Ok(LispValue::Symbol(token))
        } else {
            Ok(LispValue::Nil)
        }
    }
}
