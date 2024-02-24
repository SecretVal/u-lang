#![allow(dead_code)]
use super::{
    lexer::{Lexer, Token, TokenKind},
    BinaryExpression, BinaryExpressionKind, Expression, ExpressionKind, Statement, StatementKind,
};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    statements: Vec<Statement>,
    pos: usize,
}
impl Parser {
    pub fn from_input(input: String) -> Self {
        let mut tokens = Vec::new();
        let mut lexer = Lexer::new(input.as_str());
        while let Some(token) = lexer.next_token() {
            tokens.push(token);
        }
        Self::new(tokens)
    }
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            statements: Vec::new(),
        }
    }
    /* pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.parse_expression()?.kind {
            ExpressionKind::StringExpression(_) => {}
            ExpressionKind::NumberExpression(_) => {}
            ExpressionKind::BinaryExpression(bexpr) => match bexpr {
                BinaryExpression::Plus => {
                    self.pos -= 1;
                    let expr = self.parse_expression();
                    let num1 = match expr?.kind {
                        ExpressionKind::NumberExpression(num) => num,
                        _ => 0,
                    };
                    self.pos += 2;
                    let expr = self.parse_expression();
                    let num2 = match expr?.kind {
                        ExpressionKind::NumberExpression(num) => num,
                        _ => 0,
                    };
                    self.pos -= 1;
                }
                BinaryExpression::Minus => {
                    self.pos -= 1;
                    let expr = self.parse_expression();
                    let num1 = match expr?.kind {
                        ExpressionKind::NumberExpression(num) => num,
                        _ => 0,
                    };
                    self.pos += 2;
                    let expr = self.parse_expression();
                    let num2 = match expr?.kind {
                        ExpressionKind::NumberExpression(num) => num,
                        _ => 0,
                    };
                    self.pos -= 1;
                }
                BinaryExpression::Times => {
                    self.pos -= 1;
                    let expr = self.parse_expression();
                    let num1 = match expr?.kind {
                        ExpressionKind::NumberExpression(num) => num,
                        _ => 0,
                    };
                    self.pos += 2;
                    let expr = self.parse_expression();
                    let num2 = match expr?.kind {
                        ExpressionKind::NumberExpression(num) => num,
                        _ => 0,
                    };
                    self.pos -= 1;
                }
            },
        };
        self.pos += 1;
        None
    } */
    pub fn parse_expression(&mut self) -> Option<Expression> {
        if self.tokens.len() <= self.pos {
            return None;
        }
        let expr: Expression = match &self.tokens[self.pos].kind {
            TokenKind::Number(num) => Expression {
                kind: ExpressionKind::NumberExpression(*num),
            },
            TokenKind::String(str) => Expression {
                kind: ExpressionKind::StringExpression(str.to_string()),
            },
            TokenKind::Plus => Expression {
                kind: ExpressionKind::BinaryExpression(BinaryExpression {
                    kind: BinaryExpressionKind::Plus,
                }),
            },
            TokenKind::Minus => Expression {
                kind: ExpressionKind::BinaryExpression(BinaryExpression {
                    kind: BinaryExpressionKind::Minus,
                }),
            },
            TokenKind::Times => Expression {
                kind: ExpressionKind::BinaryExpression(BinaryExpression {
                    kind: BinaryExpressionKind::Plus,
                }),
            },
            _ => todo!(),
        };
        self.pos += 1;
        Some(expr)
    }
}
