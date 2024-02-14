#![allow(dead_code)]

#[derive(Debug, Clone)]
pub enum TokenKind {
    Number(i64),
    String(String),

    Plus,
    Minus,

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
        match value.to_ascii_lowercase().as_str() {
            "+" => Self::Plus,
            "-" => Self::Minus,
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
        let c = self.current_char();
        let mut kind = TokenKind::Bad;
        return c.map(|c| {
            let start = self.pos;
            if c.is_numeric() {
                let num = self.consume_number();
                kind = TokenKind::Number(num);
            } else {
                let str = self.consume();
                kind = TokenKind::String(str.unwrap().to_string());
            }
            if c.is_whitespace() {
                kind = TokenKind::Whitespace;
            }
            let end = self.pos;
            let content = self.input[start..end].to_string();
            let span = TextSpan::new(start, end, content);
            return Token::new(kind, span);
        });
    }

    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }

    fn consume_number(&mut self) -> i64 {
        let mut number: i64 = 0;
        while let Some(c) = self.current_char() {
            println!("c -> {c}");
            if c.is_digit(10) {
                self.consume().unwrap();
                number = number * 10 + c.to_digit(10).unwrap() as i64;
            } else {
                break;
            }
        }
        number
    }

    fn consume(&mut self) -> Option<char> {
        if self.pos >= self.input.len() {
            return None;
        }
        let c = self.current_char();
        self.pos += 1;

        c
    }
}
