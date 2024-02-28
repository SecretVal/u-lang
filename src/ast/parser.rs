#![allow(dead_code)]

use super::{
    lexer::{Lexer, Token, TokenKind},
    BinaryExpression, BinaryExpressionKind, Expression, ExpressionKind, ParserError, Statement,
    StatementKind,
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

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        if self.tokens.len() <= self.pos {
            return Ok(Statement {
                kind: StatementKind::Eof,
            });
        }
        let expr = &self.tokens[self.pos];
        let statement: Statement = match &expr.kind {
            TokenKind::Number(num) => Statement {
                kind: StatementKind::Expression(Expression {
                    kind: ExpressionKind::NumberExpression(*num),
                }),
            },
            TokenKind::Plus => {
                let mut s = self.clone();
                let s1 = &s.clone().statements[self.pos - 1];
                s.pos += 1;
                let s2 = match s.parse_statement() {
                    Ok(s) => s,
                    Err(err) => return Err(err),
                };
                let num1 = match s.get_int_by_statement(s1.clone()) {
                    Some(num) => num,
                    None => return Err(ParserError),
                };
                let num2 = match s.get_int_by_statement(s2.clone()) {
                    Some(num) => num,
                    None => return Err(ParserError),
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
                let mut s = self.clone();
                let s1 = &s.clone().statements[self.pos - 1];
                s.pos += 1;
                let s2 = match s.parse_statement() {
                    Ok(s) => s,
                    Err(err) => return Err(err),
                };
                let num1 = match s.get_int_by_statement(s1.clone()) {
                    Some(num) => num,
                    None => return Err(ParserError),
                };
                let num2 = match s.get_int_by_statement(s2.clone()) {
                    Some(num) => num,
                    None => return Err(ParserError),
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
                let mut s = self.clone();
                let s1 = &s.clone().statements[self.pos - 1];
                s.pos += 1;
                let s2 = match s.parse_statement() {
                    Ok(s) => s,
                    Err(err) => return Err(err),
                };
                let num1 = match s.get_int_by_statement(s1.clone()) {
                    Some(num) => num,
                    None => return Err(ParserError),
                };
                let num2 = match s.get_int_by_statement(s2.clone()) {
                    Some(num) => num,
                    None => return Err(ParserError),
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
                let mut s = self.clone();
                let s1 = &s.clone().statements[self.pos - 1];
                s.pos += 1;
                let s2 = match s.parse_statement() {
                    Ok(s) => s,
                    Err(err) => return Err(err),
                };
                let num1 = match s.get_int_by_statement(s1.clone()) {
                    Some(num) => num,
                    None => return Err(ParserError),
                };
                let num2 = match s.get_int_by_statement(s2.clone()) {
                    Some(num) => num,
                    None => return Err(ParserError),
                };
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Divide(num1, num2),
                        }),
                    }),
                }
            }
            TokenKind::Bad => return Err(ParserError),
            _ => todo!(),
        };
        self.pos += 1;
        self.statements.push(statement.clone());
        Ok(statement)
    }
    fn get_int_by_statement(&self, s: Statement) -> Option<i64> {
        let kind = &s.kind;
        match kind {
            StatementKind::Expression(expr) => match expr.kind {
                ExpressionKind::NumberExpression(num) => Some(num),
                _ => None,
            },
            StatementKind::Eof => None,
        }
    }
}
