#![allow(dead_code, non_snake_case)]
use crate::parser::StatementKind;
use crate::parser::ExpressionKind;
use crate::parser::BinaryExpressionKind;
use crate::parser::Statement;

#[derive(Debug, PartialEq, Clone)]
pub struct Generator {
    statements: Vec<Statement>,
    output: String,
    pos: usize,
}

impl Generator {
    pub fn new(statements: Vec<Statement>) -> Self {
        let mut output = String::new();
        output.push_str(".intel_syntax noprefix\n");
        output.push_str(".global _start\n");
        output.push_str("_start:\n");
        Self {
            statements,
            output,
            pos: 0,
        }
    }

    pub fn generate(&mut self) -> String {
        while let Some(_) = self.generate_next_statement() {};
        self.add_to_output("mov rax, 60");
        self.add_to_output("syscall");
        return self.output.clone();
    }

    fn generate_next_statement(&mut self) -> Option<()> {
        let statement = self.consume();
        if statement.is_none() {
            return None;
        }
        match statement?.kind {
            StatementKind::Expression(expr) => match expr.kind {
                ExpressionKind::NumberExpression(num) => {
                    self.add_to_output(format!("mov rdi, {}", num).as_str());
                }
                ExpressionKind::BinaryExpression(bexpr) => match bexpr.kind {
                    BinaryExpressionKind::Plus => {
                        self.add_to_output(format!("mov rdi, {}", bexpr.left).as_str());
                        self.add_to_output(format!("add rdi, {}", bexpr.right).as_str());
                    }
                    BinaryExpressionKind::Minus => {
                        self.add_to_output(format!("mov rdi, {}", bexpr.left).as_str());
                        self.add_to_output(format!("sub rdi, {}", bexpr.right).as_str());
                    }
                }
                _ => todo!(),
            },
        };
        Some(())
    }

    fn consume(&mut self) -> Option<Statement> {
        let s = self.current();
        if s.is_none() {
            return None;
        }
        self.pos += 1;
        s
    }

    fn current(&mut self) -> Option<Statement> {
        if self.pos >= self.statements.len() {
            return None;
        }
        Some(self.statements[self.pos].clone())
    }
    fn add_to_output(&mut self, str: &str) {
        self.output.push_str(format!("    {}\n", str).as_str());
    }
}
