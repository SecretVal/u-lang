use std::fmt::Display;

#[derive(Debug)]
pub struct LexerError;

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "There was an error while lexing")
    }
}
