mod ast;
use std::{env, fs, process};

use ast::parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.clone().len() < 2 {
        println!("Usage: ");
        println!("  <file_name>             parse the file");
        process::exit(1);
    }
    let file = fs::read_to_string(args[1].clone()).unwrap();
    let mut parser = Parser::from_input(file);
    while let Some(expr) = parser.parse_expression() {
        println!("{expr:?}");
    }
}
