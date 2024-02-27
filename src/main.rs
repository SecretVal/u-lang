mod ast;
use std::{
    env, fs,
    io::{stdin, stdout, BufRead, Write},
    process,
};

use ast::{parser::Parser, Statement};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.clone().len() < 2 {
        println!("Usage: ");
        println!("  <file_name>             parse the file");
        println!("  shell                   Get dropped into an interactive shell.");
        process::exit(1);
    }
    match args.clone()[1].as_str() {
        "shell" => {
            let mut stdout = stdout();
            let mut exit = false;
            while !exit {
                let _ = stdout.write(b"->");
                let _ = stdout.flush();
                let mut handler = stdin().lock();
                let mut input = String::new();
                let _ = handler.read_line(&mut input);
                match input.as_str().trim() {
                    "exit" => exit = true,
                    _ => {
                        let mut parser = Parser::from_input(input);
                        while let Some(stmt) = parser.parse_statement() {
                            print_statement(stmt);
                        }
                    }
                }
            }
        }
        _ => {
            let file = fs::read_to_string(args[1].clone()).unwrap();
            let mut parser = Parser::from_input(file);
            while let Some(stmt) = parser.parse_statement() {
                print_statement(stmt);
            }
            println!("{:?}", parser.statements);
        }
    }
}
fn print_statement(s: Statement) {
    match s.kind {
        ast::StatementKind::Expression(expr) => match expr.kind {
            ast::ExpressionKind::StringExpression(str) => {
                println!("String: {str}");
            }
            ast::ExpressionKind::NumberExpression(num) => {
                println!("Number: {num}");
            }
            ast::ExpressionKind::BinaryExpression(b) => match b.kind {
                ast::BinaryExpressionKind::Plus(n1, n2) => {
                    println!("Plus:");
                    println!("      {n1}");
                    println!("      {n2}");
                }
                ast::BinaryExpressionKind::Minus(n1, n2) => {
                    println!("Minus:");
                    println!("      {n1}");
                    println!("      {n2}");
                }
                ast::BinaryExpressionKind::Times(n1, n2) => {
                    println!("Times:");
                    println!("      {n1}");
                    println!("      {n2}");
                }
                ast::BinaryExpressionKind::Divide(n1, n2) => {
                    println!("Divide:");
                    println!("      {n1}");
                    println!("      {n2}");
                }
            },
        },
    }
}
