#![allow(dead_code)]
pub mod lexer;
pub mod parser;

#[derive(Debug)]
pub struct Ast {
    pub statements: Vec<ASTStatement>,
}

#[derive(Debug)]
pub struct ASTStatement {
    kind: ASTStatementKind,
}

impl ASTStatement {
    pub fn new(kind: ASTStatementKind) -> Self {
        Self { kind }
    }
    pub fn expression(expr: ASTExpression) -> Self {
        ASTStatement::new(ASTStatementKind::Expression(expr))
    }
}
#[derive(Debug)]
pub enum ASTStatementKind {
    Expression(ASTExpression),
}

#[derive(Debug)]
pub enum ASTExpressionKind {
    Number(i64),
    String(String),

    Plus,
    Minus,

    Let,
}

#[derive(Debug)]
pub struct ASTExpression {
    kind: ASTExpressionKind,
}

impl ASTExpression {
    pub fn new(kind: ASTExpressionKind) -> Self {
        Self { kind }
    }

    pub fn number(num: i64) -> Self {
        Self::new(ASTExpressionKind::Number(num))
    }
    pub fn string(str: String) -> Self {
        Self::new(ASTExpressionKind::String(str))
    }
}
