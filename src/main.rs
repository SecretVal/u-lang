mod ast;
use std::{env, fs};

use ast::{
    lexer::Lexer,
    parser::{self, Parser},
};

fn main() {
    let mut lexer = Lexer::new("");
    let token = lexer.next_token();
    println!("{token:?}");
    let token = lexer.next_token();
    println!("{token:?}");
    let token = lexer.next_token();
    println!("{token:?}");
}
