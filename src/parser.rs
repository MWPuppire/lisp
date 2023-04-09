use regex::Regex;
use lazy_static::lazy_static;
use crate::{LispValue, LispError};

pub struct LispParser {
    tokens: Vec<String>,
    idx: usize,
}
impl LispParser {
    pub fn new() -> Self {
        LispParser {
            tokens: vec![],
            idx: 0,
        }
    }
    pub fn is_complete(&self) -> bool {
        match self.peek() {
            Ok(_) => true,
            Err(LispError::UnbalancedParens(_)) => false,
            Err(_) => true,
        }
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
    pub fn next(&mut self) -> Result<LispValue, LispError> {
        let (val, rest) = self.read_form(&self.tokens[self.idx..])?;
        self.idx = self.tokens.len() - rest.len();
        Ok(val)
    }
    pub fn peek(&self) -> Result<LispValue, LispError> {
        let (val, _) = self.read_form(&self.tokens[self.idx..])?;
        Ok(val)
    }
    fn read_form<'a>(&self, tokens: &'a [String]) -> Result<(LispValue, &'a [String]), LispError> {
        if tokens.len() == 0 {
            return Ok((LispValue::Nil, tokens));
        }
        let (token, rest) = tokens.split_first().ok_or(LispError::SyntaxError(0, 0))?;
        match token.as_str() {
            "(" => self.read_list(rest),
            ")" => Err(LispError::SyntaxError(0, 0)),
            _ => Ok((self.read_atom(token.clone())?, rest)),
        }
    }
    fn read_list<'a>(&self, tokens: &'a [String]) -> Result<(LispValue, &'a [String]), LispError> {
        let mut res: Vec<LispValue> = vec![];
        let mut xs = tokens;
        loop {
            let (next_token, rest) = xs.split_first().ok_or(LispError::UnbalancedParens(1))?;
            if next_token == ")" {
                break Ok((LispValue::List(res), rest));
            }
            let (exp, new_xs) = self.read_form(&xs).map_err(|err| match err {
                LispError::UnbalancedParens(x) => LispError::UnbalancedParens(x + 1),
                _ => err,
            })?;
            res.push(exp);
            xs = new_xs;
        }
    }
    fn read_atom(&self, token: String) -> Result<LispValue, LispError> {
        if token.starts_with(";") {
            // comment
            Ok(LispValue::Nil)
        } else if let Ok(f) = token.parse() {
            Ok(LispValue::Number(f))
        } else if token.starts_with("\"") {
            if token.ends_with("\"") {
                let last = token.len() - 1;
                Ok(LispValue::String(token[1..last].to_owned()))
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
