#![allow(dead_code)]
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Statement {
    kind: StatementKind,
}
#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression),
}
#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
}
#[derive(Debug, Clone)]
pub enum ExpressionKind {
    StringExpression(String),
    NumberExpression(i64),
    BinaryExpression(BinaryExpression),
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    kind: BinaryExpressionKind,
}
#[derive(Debug, Clone)]
pub enum BinaryExpressionKind {
    Plus,
    Minus,
    Times,
}
