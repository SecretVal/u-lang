#![allow(dead_code)]
pub mod errors;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Statement {
    pub(crate) kind: StatementKind,
}
#[derive(Debug, Clone)]
pub enum StatementKind {
    Expression(Expression),
}
#[derive(Debug, Clone)]
pub struct Expression {
    pub(crate) kind: ExpressionKind,
}
#[derive(Debug, Clone)]
pub enum ExpressionKind {
    StringExpression(String),
    NumberExpression(i64),
    BinaryExpression(BinaryExpression),
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub(crate) kind: BinaryExpressionKind,
}
#[derive(Debug, Clone)]
pub enum BinaryExpressionKind {
    Plus(i64, i64),
    Minus(i64, i64),
    Times(i64, i64),
    Divide(i64, i64),
}
