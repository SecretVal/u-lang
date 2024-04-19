#![allow(dead_code, non_snake_case)]
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenKind;
#[derive(Debug, PartialEq, Clone)]
pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
    pub(crate) file: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub(crate) kind: StatementKind,
}

impl Statement {
    pub fn expression(expr: Expression) -> Self {
        Self {
            kind: StatementKind::Expression(expr),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    Expression(Expression),
}
#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub(crate) kind: ExpressionKind,
}

impl Expression {
    pub fn number(n: i64) -> Self {
        Self {
            kind: ExpressionKind::NumberExpression(n),
        }
    }
    pub fn binary(expr: BinaryExpression) -> Self {
        Self {
            kind: ExpressionKind::BinaryExpression(expr),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionKind {
    NumberExpression(i64),
    BinaryExpression(BinaryExpression),
}
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression {
    pub(crate) kind: BinaryExpressionKind,
    pub(crate) left: i64,
    pub(crate) right: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryExpressionKind {
    Plus,
    Minus,
}

impl Parser {
    pub fn from_input(input: &str, file: String) -> Self {
        let mut lexer = Lexer::new(input);
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(token) = lexer.next_token() {
            tokens.push(token);
        }
        Self::new(
            tokens
                .into_iter()
                .filter(|t| t.kind != TokenKind::Whitespace)
                .collect(),
            file,
        )
    }

    pub fn new(tokens: Vec<Token>, file: String) -> Self {
        Self {
            tokens,
            pos: 0,
            file,
        }
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expr();
        if expr.is_none() {
            return None;
        }
        Some(Statement::expression(expr.unwrap()))
    }

    pub fn parse_expr(&mut self) -> Option<Expression> {
        let token = self.current();
        return match token.kind {
            TokenKind::Identifier => {
                todo!("not implemented yet")
            }
            TokenKind::Number(num) => {
                if self.peek(1).kind == TokenKind::Plus || self.peek(1).kind == TokenKind::Minus {
                    return Some(Expression::binary(self.parse_binary_expr()));
                }
                self.consume();
                Some(Expression::number(num))
            }
            TokenKind::Plus => {
                eprintln!(
                    "Error: {}:{}: You cannot start a statement with `+`",
                    self.file,
                    self.current().loc(),
                );
                std::process::exit(1);
            }
            TokenKind::Minus => {
                eprintln!(
                    "Error: {}:{}: You cannot start a statement with `+`",
                    self.file,
                    self.current().loc(),
                );
                std::process::exit(1);
            }
            TokenKind::Bad => todo!("bad token"),
            TokenKind::Eof => {
                return None;
            }
            TokenKind::Whitespace => {
                eprintln!(
                    "Error: {}:{}: Still a whitespace token in parsing",
                    self.file,
                    self.current().loc()
                );
                eprintln!("Notice: If you are not the developer please contact create a github issue. This should never happen.");
                std::process::exit(1)
            }
        };
    }

    pub fn parse_binary_expr(&mut self) -> BinaryExpression {
        let left = match self.clone().consume().unwrap().kind {
            TokenKind::Number(num) => num,
            _ => {
                let current = &self.current();
                eprintln!(
                    "Error: {}:{}: `{}` is not a number",
                    self.file,
                    current.loc(),
                    current.span.literal
                );
                std::process::exit(1);
            }
        };
        self.consume();
        let kind = match self.clone().consume().unwrap().kind {
            TokenKind::Plus => BinaryExpressionKind::Plus,
            TokenKind::Minus => BinaryExpressionKind::Minus,
            _ => {
                let current = &self.current();
                eprintln!(
                    "Error: {}:{}: `{}` is not a known operator",
                    self.file,
                    current.loc(),
                    current.span.literal
                );
                std::process::exit(1);
            }
        };
        self.consume();
        let right = match self.clone().consume().unwrap().kind {
            TokenKind::Number(num) => num,
            _ => {
                let current = &self.current();
                eprintln!(
                    "Error: {}:{}: `{}`  is not a number",
                    self.file,
                    current.loc(),
                    current.span.literal
                );
                std::process::exit(1);
            }
        };
        self.consume();
        BinaryExpression { kind, left, right }
    }

    fn consume(&mut self) -> Option<&Token> {
        if self.tokens.len() <= self.pos {
            return None;
        }
        let token = &self.tokens[self.pos];
        self.pos += 1;
        Some(token)
    }

    fn peek(&self, offset: usize) -> &Token {
        let mut index = self.pos + offset;
        if self.pos >= self.tokens.len() {
            index = self.pos;
        }
        &self.tokens[index]
    }
    fn current(&self) -> &Token {
        &self.peek(0)
    }
}
