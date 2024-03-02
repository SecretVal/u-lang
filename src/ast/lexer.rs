#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number(i64),

    Plus,
    Minus,
    Times,
    Divide,

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
        let c = &self.current_char();
        if c.is_none() {
            return None;
        }
        let start = self.pos;
        let mut kind = TokenKind::Bad;
        c.map(|c| {
            if c.is_digit(10) {
                let num = self.consume_number();
                kind = TokenKind::Number(num.expect("lexing number"));
            } else if c.is_whitespace() {
                self.consume();
                kind = TokenKind::Whitespace;
            // TODO: Implement this
            // } else if Self::is_identifier_start(c) {
            //     kind = TokenKind::Bad;
            //     kind = match self.consume_identifier().as_str() {}
            } else {
                kind = self.consume_puncutation();
            }
            let end = self.pos;
            let content = &self.input[start..end];
            let span = TextSpan::new(start, end, content.to_string());
            Token::new(kind, span)
        })
    }
    fn consume(&mut self) -> Option<char> {
        let c = self.current_char();
        self.pos += 1;
        c
    }
    fn consume_number(&mut self) -> Option<i64> {
        let mut num: i64 = 0;
        while let Some(c) = self.current_char() {
            if c.is_digit(10) {
                self.consume().unwrap();
                num = num * 10 + c.to_digit(10).unwrap() as i64;
            } else {
                break;
            }
        }
        Some(num)
    }
    fn consume_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some(c) = self.current_char() {
            if Self::is_identifier_start(c) {
                self.consume().unwrap();
                identifier.push(c);
            } else {
                break;
            }
        }
        identifier
    }
    fn current_char(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }

    fn consume_puncutation(&mut self) -> TokenKind {
        match self.consume().unwrap() {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '/' => TokenKind::Divide,
            '*' => TokenKind::Times,
            _ => TokenKind::Bad,
        }
    }
    fn is_identifier_start(ch: char) -> bool {
        ch.is_alphabetic()
    }
}
