#![allow(dead_code, non_snake_case)]

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub(crate) kind: TokenKind,
    pub(crate) span: TextSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: TextSpan) -> Self {
        Self { kind, span }
    }

    pub fn loc(&self) -> String {
        let loc = format!("{}:{}", self.span.row, self.span.col);
        loc
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Identifier,
    Number(i64),
    Plus,
    Minus,
    Bad,
    Whitespace,
    Eof,
    Let,
    Equals,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TextSpan {
    pub(crate) row: usize,
    pub(crate) col: usize,
    pub(crate) literal: String,
}

impl TextSpan {
    fn new(row: usize, col: usize, literal: String) -> Self {
        Self { row, col, literal }
    }
}
#[derive(Debug, PartialEq)]
pub struct Lexer {
    pub(crate) input: String,
    pub(crate) pos: usize,
    pub(crate) row: usize,
    pub(crate) col: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: String::from(input),
            pos: 0,
            row: 1,
            col: 0,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.pos == self.input.len() {
            let eof_char: char = '\0';
            self.pos += 1;
            return Some(Token::new(
                TokenKind::Eof,
                TextSpan::new(0, 0, eof_char.to_string()),
            ));
        }
        if self.pos >= self.input.len() {
            return None;
        }
        let c = self.current_char();
        if c.is_none() {
            return None;
        }
        return c.map(|c| {
            let start = self.pos;
            let mut kind = TokenKind::Bad;
            let _ = kind;
            if c.is_digit(10) {
                let num = self.consume_number();
                kind = TokenKind::Number(num);
            } else if c.is_whitespace() {
                kind = TokenKind::Whitespace;
                if c == '\n' {
                    self.row += 1;
                }
                self.consume();
            } else if c.is_alphabetic() {
                let col = self.col;
                let identifier = self.consume_identifier();
                self.col = col;
                kind = match identifier.as_str() {
                    "let" => TokenKind::Let,
                    _ => TokenKind::Identifier,
                }
            } else {
                kind = self.consume_punctuation();
            }
            if kind == TokenKind::Bad {
                self.consume().unwrap();
            }
            let end = self.pos;
            let literal = self.input[start..end].to_string();
            let span = TextSpan::new(self.row, self.col, literal.to_string());
            Token { kind, span }
        });
    }
    fn consume_identifier(&mut self) -> String {
        let mut str = String::new();
        while let Some(c) = self.current_char() {
            if c.is_alphabetic() {
                self.consume().unwrap();
                str.push(c);
            } else {
                break;
            }
        }
        return str;
    }

    fn consume(&mut self) -> Option<char> {
        if self.pos >= self.input.len() {
            return None;
        }
        let c = self.current_char();
        self.pos += 1;
        self.col += 1;

        c
    }

    fn consume_punctuation(&mut self) -> TokenKind {
        let c = self.consume().unwrap();
        match c {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '=' => TokenKind::Equals,
            _ => TokenKind::Bad,
        }
    }

    fn consume_number(&mut self) -> i64 {
        let mut num: i64 = 0;
        while let Some(c) = self.current_char() {
            if c.is_digit(10) {
                self.consume().unwrap();
                num = num * 10 + c.to_digit(10).unwrap() as i64;
            } else {
                break;
            }
        }
        return num;
    }

    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }
}
