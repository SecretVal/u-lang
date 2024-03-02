use std::fmt::Display;
pub mod lexer;
pub mod parser;

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub(crate) kind: StatementKind,
}
#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Expression(Expression),
    Eof,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub(crate) kind: ExpressionKind,
}
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    NumberExpression(i64),
    BinaryExpression(BinaryExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub(crate) kind: BinaryExpressionKind,
}
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryExpressionKind {
    Plus(i64, i64),
    Minus(i64, i64),
    Times(i64, i64),
    Divide(i64, i64),
}

// Errors

#[derive(Debug)]
pub struct ParserError;
impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "There was an error while parsing")
    }
}
