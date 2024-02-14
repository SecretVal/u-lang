#![allow(dead_code)]

use std::cell::Cell;

use super::{
    lexer::{Token, TokenKind},
    ASTExpression, ASTExpressionKind, ASTStatement,
};

#[derive(Debug, Clone)]
pub struct Counter {
    value: Cell<usize>,
}
impl Counter {
    pub fn new() -> Self {
        Self {
            value: Cell::new(0),
        }
    }

    pub fn increment(&self) {
        let current_value = self.value.get();
        self.value.set(current_value + 1);
    }

    pub fn get_value(&self) -> usize {
        self.value.get()
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    current: Counter,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.iter().map(|token| token.clone()).collect(),
            current: Counter::new(),
        }
    }

    pub fn next_statement(&mut self) -> Option<ASTStatement> {
        println!("{:?}", self.current());
        return self.parse_statement();
    }

    fn parse_statement(&mut self) -> Option<ASTStatement> {
        // let token = self.current();
        let expr = self.parse_expression();
        Some(ASTStatement::expression(expr?))
    }

    fn parse_expression(&self) -> Option<ASTExpression> {
        let token = self.current();
        return match &token.kind {
            TokenKind::Number(num) => Some(ASTExpression::number(*num)),
            TokenKind::String(str) => Some(ASTExpression::string(str.to_string())),
            TokenKind::Plus => Some(ASTExpression::new(ASTExpressionKind::Plus)),
            TokenKind::Minus => Some(ASTExpression::new(ASTExpressionKind::Minus)),
            TokenKind::Let => Some(ASTExpression::new(ASTExpressionKind::Let)),
            _ => None,
        };
    }

    fn peek(&self, offset: isize) -> &Token {
        let mut index = (self.current.get_value() as isize + offset) as usize;
        if index >= self.tokens.len() {
            index = self.tokens.len() - 1;
        }
        self.tokens.get(index).unwrap()
    }

    fn current(&self) -> &Token {
        self.peek(0)
    }
}
