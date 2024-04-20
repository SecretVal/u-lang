mod parser;
use parser::Parser;
use parser::Statement;
mod lexer;
use std::{env, fs, process};
mod generator;
use generator::Generator;
use std::process::Command;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.clone().len() < 2 {
        println!("Usage:");
        println!("<file-name>    Compile the file");
        eprintln!("ERROR: Did not provide file name");
        process::exit(1);
    }
    let file = fs::read_to_string(args[1].clone()).unwrap();
    let mut parser = Parser::from_input(&file, args[1].clone());
    let mut statements: Vec<Statement> = vec![];
    while let Some(stmt) = parser.parse_statement() {
        //println!("{stmt:?}");
        statements.push(stmt);
    }
    let mut generator = Generator::new(statements);
    let stripped_assembly_path = format!("{}.asm", args[1].clone().split(".").collect::<Vec<_>>()[0]);
    fs::write(&stripped_assembly_path, generator.generate()).unwrap();
    let _ = Command::new("fasm").args([&stripped_assembly_path]).output();
}
