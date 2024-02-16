mod ast;
use std::{env, fs, process};

use ast::lexer::Lexer;
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.clone().len() < 2 {
        println!("Usage: ");
        println!("  <file_name>             tokenize the file");
        process::exit(1);
    }
    let file = fs::read_to_string(args[1].clone()).unwrap();
    let mut lexer = Lexer::new(&file);
    while let Some(token) = lexer.next_token() {
        println!("{token:?}");
    }
}
