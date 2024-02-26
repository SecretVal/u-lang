#![allow(dead_code)]
use super::{
    lexer::{Lexer, Token, TokenKind},
    BinaryExpression, BinaryExpressionKind, Expression, ExpressionKind, Statement, StatementKind,
};

#[derive(Debug, Clone)]
pub struct Parser {
    tokens: Vec<Token>,
    statements: Vec<Statement>,
    pos: usize,
}
impl Parser {
    pub fn from_input(input: String) -> Self {
        let mut tokens = Vec::new();
        let mut lexer = Lexer::new(input.as_str());
        while let Ok(Some(token)) = lexer.next_token() {
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
            TokenKind::String(str) => Statement {
                kind: StatementKind::Expression(Expression {
                    kind: ExpressionKind::StringExpression(str.to_string()),
                }),
            },
            TokenKind::Plus => {
                let statement1 = &self.statements[self.pos - 1];
                let mut s = self.clone();
                s.pos += 1;
                s.parse_statement();
                let statement2 = &s.statements[s.pos - 2];
                let num1 = self.get_int_by_statement(statement1.clone());
                let num2 = self.get_int_by_statement(statement2.clone());
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Plus(num1, num2),
                        }),
                    }),
                }
            }
            TokenKind::Minus => {
                let statement1 = &self.statements[self.pos - 1];
                let mut s = self.clone();
                s.pos += 1;
                s.parse_statement();
                let statement2 = &s.statements[s.pos - 2];
                let num1 = self.get_int_by_statement(statement1.clone());
                let num2 = self.get_int_by_statement(statement2.clone());
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Minus(num1, num2),
                        }),
                    }),
                }
            }
            TokenKind::Times => {
                let statement1 = &self.statements[self.pos - 1];
                let mut s = self.clone();
                s.pos += 1;
                s.parse_statement();
                let statement2 = &s.statements[s.pos - 2];
                let num1 = self.get_int_by_statement(statement1.clone());
                let num2 = self.get_int_by_statement(statement2.clone());
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Times(num1, num2),
                        }),
                    }),
                }
            }
            TokenKind::Divide => {
                let statement1 = &self.statements[self.pos - 1];
                let mut s = self.clone();
                s.pos += 1;
                s.parse_statement();
                let statement2 = &s.statements[s.pos - 2];
                let num1 = self.get_int_by_statement(statement1.clone());
                let num2 = self.get_int_by_statement(statement2.clone());
                Statement {
                    kind: StatementKind::Expression(Expression {
                        kind: ExpressionKind::BinaryExpression(BinaryExpression {
                            kind: BinaryExpressionKind::Divide(num1, num2),
                        }),
                    }),
                }
            }
            _ => todo!(),
        };
        self.pos += 1;
        self.statements.push(statement.clone());
        Some(statement)
    }
    fn get_int_by_statement(&self, s: Statement) -> i64 {
        let kind = &s.kind;
        match kind {
            StatementKind::Expression(expr) => match expr.kind {
                ExpressionKind::NumberExpression(num) => num,
                _ => 0,
            },
        }
    }
}
