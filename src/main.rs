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
    while let Some(stmt) = parser.parse_statement() {
        match stmt.kind {
            ast::StatementKind::Expression(expr) => match expr.kind {
                ast::ExpressionKind::StringExpression(_) => {}
                ast::ExpressionKind::NumberExpression(_) => {}
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
}
