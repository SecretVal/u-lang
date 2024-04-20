#![allow(dead_code, non_snake_case)]
use crate::parser::BinaryExpressionKind;
use crate::parser::DeclarationKind;
use crate::parser::Expression;
use crate::parser::ExpressionKind;
use crate::parser::Statement;
use crate::parser::StatementKind;
use crate::parser::VariableDeclaration;

#[derive(Debug, PartialEq, Clone)]
pub struct Generator {
    statements: Vec<Statement>,
    output: String,
    pos: usize,
    variables: Vec<VariableDeclaration>,
}

impl Generator {
    pub fn new(statements: Vec<Statement>) -> Self {
        let mut output = String::new();
        output.push_str("format ELF64 executable 3\n");
        output.push_str("entry start\n");
        output.push_str("segment readable executable\n");
        output.push_str("start:\n");
        Self {
            statements,
            output,
            pos: 0,
            variables: vec![],
        }
    }

    pub fn generate(&mut self) -> String {
        while let Some(_) = self.generate_next_statement() {}
        self.add_to_output("mov rax, 60");
        self.add_to_output("syscall");
        self.add_to_output("segment readable writeable");
        for var in &self.variables.clone() {
            self.add_to_output(format!("{} dq 0", var.name).as_str());
        }
        return self.output.clone();
    }

    fn generate_next_statement(&mut self) -> Option<()> {
        let statement = self.consume();
        if statement.is_none() {
            return None;
        }
        match statement?.kind {
            StatementKind::Expression(expr) => self.generate_expression(expr),
            StatementKind::Declaration(decl) => match decl.kind {
                DeclarationKind::VariableDeclaration(var_decl) => {
                    self.variables.push(var_decl.clone());
                    self.generate_expression(var_decl.clone().value);
                    self.add_to_output(format!("mov [{}], rdi", var_decl.clone().name).as_str());
                    self.add_to_output("xor rdi, rdi");
                }
            },
        };
        Some(())
    }

    fn generate_expression(&mut self, expr: Expression) {
        match expr.kind {
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
            },
            ExpressionKind::ExitExpression(exit_expr) => {
                self.add_to_output(format!("mov rdi, [{}]", exit_expr.value).as_str())
            }
        }
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
