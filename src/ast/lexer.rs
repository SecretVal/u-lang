#![allow(dead_code)]

use std::i64;

#[derive(Debug, Clone)]
pub enum TokenKind {
    Number(i64),
    String(String),

    Plus,
    Minus,
    Times,
    Divide,

    Let,

    Eof,
    Bad,
    Whitespace,
}
#[derive(Debug, Clone)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
    pub content: String,
}
impl TextSpan {
    pub fn new(start: usize, end: usize, content: String) -> Self {
        Self {
            start,
            end,
            content,
        }
    }
}
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TextSpan,
}
impl Token {
    pub fn new(kind: TokenKind, span: TextSpan) -> Self {
        Self { kind, span }
    }
}

impl From<i64> for TokenKind {
    fn from(value: i64) -> Self {
        Self::Number(value)
    }
}

impl From<String> for TokenKind {
    fn from(value: String) -> Self {
        if value.parse::<i64>().is_ok() {
            return Self::from(value.parse::<i64>().unwrap());
        }
        match value.to_ascii_lowercase().as_str() {
            "+" => Self::Plus,
            "-" => Self::Minus,
            "*" => Self::Times,
            "/" => Self::Divide,
            "let" => Self::Let,
            _ => Self::String(value),
        }
    }
}

impl From<&str> for TokenKind {
    fn from(value: &str) -> Self {
        Self::from(value.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}
impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }
    pub fn next_token(&mut self) -> Option<Token> {
        let ch = self.current_string();
        if ch.is_some() {
            if self.inut_as_vec().len() <= self.pos {
                return None;
            }
            let token = Some(Token::new(
                TokenKind::from(ch.clone().unwrap()),
                TextSpan::new(self.pos, self.pos + 1, ch.clone().unwrap()),
            ));
            self.pos += 1;
            token
        } else {
            None
        }
    }
    fn current_string(&self) -> Option<String> {
        let v = self.inut_as_vec();
        if v.len() <= self.pos {
            None
        } else {
            Some(v[self.pos].to_string())
        }
    }
    fn inut_as_vec(&self) -> Vec<&'a str> {
        self.input.split_whitespace().collect::<Vec<_>>()
    }
}
