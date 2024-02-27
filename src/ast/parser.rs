#![allow(dead_code)]

use super::{
    lexer::{Lexer, Token, TokenKind},
    BinaryExpression, BinaryExpressionKind, Expression, ExpressionKind, Statement, StatementKind,
};

#[derive(Debug, Clone)]
pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) statements: Vec<Statement>,
    pub(crate) pos: usize,
}
impl Parser {
    pub fn from_input(input: String) -> Self {
        let mut tokens = Vec::new();
        let mut lexer = Lexer::new(input.as_str());
        while let Some(token) = lexer.next_token() {
            tokens.push(token);
        }
        Self::new(
            tokens
                .iter()
                .filter(|x| x.kind != TokenKind::Whitespace)
                .map(|token| token.clone())
                .collect(),
        )
    }
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            pos: 0,
            statements: Vec::new(),
        }
    }
    pub fn parse_statement(&mut self) -> Option<Statement> {
        if self.tokens.len() <= self.pos {
            return None;
        }
        let expr = &self.tokens[self.pos];
        let statement: Statement = match &expr.kind {
            TokenKind::Number(num) => Statement {
                kind: StatementKind::Expression(Expression {
                    kind: ExpressionKind::NumberExpression(*num),
                }),
            },
            TokenKind::Plus => {
                let s = self.clone();
                let s1 = &s.statements[self.pos - 1];
                self.pos += 1;
                let s2 = match self.parse_statement() {
                    Some(s) => s,
                    None => return None,
                };
                let num1 = match self.get_int_by_statement(s1.clone()) {
                    Some(num) => num,
                    None => return None,
                };
                let num2 = match self.get_int_by_statement(s2.clone()) {
                    Some(num) => num,
                    None => return None,
                };
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Plus(num1, num2),
                        }),
                    }),
                }
            }
            TokenKind::Minus => {
                let s = self.clone();
                let s1 = &s.statements[self.pos - 1];
                self.pos += 1;
                let s2 = match self.parse_statement() {
                    Some(s) => s,
                    None => return None,
                };
                let num1 = match self.get_int_by_statement(s1.clone()) {
                    Some(num) => num,
                    None => return None,
                };
                let num2 = match self.get_int_by_statement(s2.clone()) {
                    Some(num) => num,
                    None => return None,
                };
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Minus(num1, num2),
                        }),
                    }),
                }
            }
            TokenKind::Times => {
                let s = self.clone();
                let s1 = &s.statements[self.pos - 1];
                self.pos += 1;
                let s2 = match self.parse_statement() {
                    Some(s) => s,
                    None => return None,
                };
                let num1 = match self.get_int_by_statement(s1.clone()) {
                    Some(num) => num,
                    None => return None,
                };
                let num2 = match self.get_int_by_statement(s2.clone()) {
                    Some(num) => num,
                    None => return None,
                };
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Times(num1, num2),
                        }),
                    }),
                }
            }
            TokenKind::Divide => {
                let s = self.clone();
                let s1 = &s.statements[self.pos - 1];
                self.pos += 1;
                let s2 = match self.parse_statement() {
                    Some(s) => s,
                    None => return None,
                };
                let num1 = match self.get_int_by_statement(s1.clone()) {
                    Some(num) => num,
                    None => return None,
                };
                let num2 = match self.get_int_by_statement(s2.clone()) {
                    Some(num) => num,
                    None => return None,
                };
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Divide(num1, num2),
                        }),
                    }),
                }
            }
            TokenKind::Bad => return None,
            _ => todo!(),
        };
        self.pos += 1;
        self.statements.push(statement.clone());
        Some(statement)
    }
    fn get_int_by_statement(&self, s: Statement) -> Option<i64> {
        let kind = &s.kind;
        match kind {
            StatementKind::Expression(expr) => match expr.kind {
                ExpressionKind::NumberExpression(num) => Some(num),
                _ => None,
            },
        }
    }
}
