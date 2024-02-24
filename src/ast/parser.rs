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
    pub fn parse_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression();
        if expr.is_none() {
            return None;
        }
        let statement: Statement = match expr?.kind {
            ExpressionKind::StringExpression(str) => Statement {
                kind: StatementKind::Expression(Expression {
                    kind: ExpressionKind::StringExpression(str),
                }),
            },
            ExpressionKind::NumberExpression(num) => Statement {
                kind: StatementKind::Expression(Expression {
                    kind: ExpressionKind::NumberExpression(num),
                }),
            },
            ExpressionKind::BinaryExpression(bexpr) => match bexpr.kind {
                BinaryExpressionKind::Plus => todo!(),
                BinaryExpressionKind::Minus => todo!(),
                BinaryExpressionKind::Times => todo!(),
            },
        };
        self.pos += 1;
        self.statements.push(statement.clone());
        Some(statement)
    }
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
        Some(expr)
    }
}
