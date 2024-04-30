#![allow(dead_code, non_snake_case)]
use crate::parser::BinaryExpressionKind;
use crate::parser::Condition;
use crate::parser::DeclarationKind;
use crate::parser::Expression;
use crate::parser::ExpressionKind;
use crate::parser::Statement;
use crate::parser::StatementKind;
use crate::parser::VariableDeclaration;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Generator {
    statements: Vec<Statement>,
    output: String,
    pos: usize,
    stmt_pos: usize,
    variables: HashMap<String, VariableDeclaration>,
}

impl Generator {
    pub fn new(statements: Vec<Statement>) -> Self {
        let mut output = String::new();
        output.push_str("format ELF64 executable 3\n");
        output.push_str("entry start\n");
        output.push_str("segment readable executable\n");
        output.push_str("start:\n");
        let variables: HashMap<String, VariableDeclaration> = HashMap::new();
        Self {
            statements,
            output,
            pos: 0,
            stmt_pos: 0,
            variables,
        }
    }

    pub fn generate(&mut self) -> String {
        let mut current = self.current();
        self.output
            .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
        while let Some(_) = self.generate_statement(current.unwrap()) {
            self.consume();
            self.stmt_pos += 1;
            current = self.current();
            if current.is_none() {
                self.output
                    .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
                self.add_to_output("mov rax, 60");
                self.add_to_output("syscall");
                self.output.push_str("segment readable writeable\n");
                for (name, _) in self.variables.clone().into_iter() {
                    self.add_to_output(format!("{} dq 0", name).as_str());
                }
                break;
            }
            self.output
                .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
        }
        self.output.clone()
    }

    fn generate_statement(&mut self, statement: Statement) -> Option<()> {
        match statement.kind {
            StatementKind::Expression(expr) => self.generate_expression(expr),
            StatementKind::Declaration(decl) => match decl.kind {
                DeclarationKind::VariableDeclaration(var_decl) => {
                    self.variables
                        .insert(var_decl.clone().name, var_decl.clone());
                    self.generate_expression(var_decl.clone().value);
                    self.add_to_output(format!("mov [{}], rdi", var_decl.clone().name).as_str());
                    self.add_to_output("xor rdi, rdi");
                }
                DeclarationKind::VariableRedeclaration(var_decl) => {
                    if !self.variables.contains_key(&var_decl.name) {
                        panic!("variable not found");
                    }
                    self.generate_expression(var_decl.clone().value);
                    self.add_to_output(format!("mov [{}], rdi", var_decl.clone().name).as_str());
                    self.add_to_output("xor rdi, rdi");
                }
            },
            StatementKind::IfStatement(if_stmt) => {
                self.add_to_output(";; -- if --- ;;");
                self.generate_condition(if_stmt.condition);
                self.add_to_output(format!("je addr_{}", self.stmt_pos + 1).as_str());
                self.add_to_output("xor rdi, rdi");
                self.add_to_output("xor rdx, rdx");
                self.add_to_output(
                    format!("jmp addr_{}", self.stmt_pos + 1 + if_stmt.stmt_count).as_str(),
                );
                for stmt in if_stmt.body {
                    self.stmt_pos += 1;
                    self.output
                        .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
                    self.generate_statement(*stmt.clone());
                }
            }
            StatementKind::WhileStatement(while_stmt) => {
                self.add_to_output(";; -- while --- ;;");
                self.generate_condition(while_stmt.condition);
                self.add_to_output(format!("je addr_{}", self.stmt_pos + 1).as_str());
                self.add_to_output("xor rdi, rdi");
                self.add_to_output("xor rdx, rdx");
                self.add_to_output(
                    format!("jmp addr_{}", self.stmt_pos + 1 + while_stmt.stmt_count).as_str(),
                );
                let start = self.stmt_pos;
                for stmt in while_stmt.body {
                    self.stmt_pos += 1;
                    self.output
                        .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
                    self.generate_statement(*stmt.clone());
                }
                self.add_to_output(format!("jmp addr_{start}").as_str());
            }
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
                    self.generate_expression(*bexpr.left.clone());
                    self.add_to_output("mov rdx, rdi");
                    self.generate_expression(*bexpr.right.clone());
                    self.add_to_output("add rdx, rdi");
                    self.add_to_output("mov rdi, rdx");
                }
                BinaryExpressionKind::Minus => {
                    self.generate_expression(*bexpr.left.clone());
                    self.add_to_output("mov rdx, rdi");
                    self.generate_expression(*bexpr.right.clone());
                    self.add_to_output("sub rdx, rdi");
                    self.add_to_output("mov rdi, rdx");
                }
            },
            ExpressionKind::CallExpression(call_expr) => {
                for (i, arg) in call_expr.args.iter().enumerate() {
                    let reg = match i {
                        0 => "rax",
                        1 => "rdi",
                        2 => "rsi",
                        3 => "rdx",
                        4 => "r10",
                        5 => "r8",
                        6 => "r9",
                        _ => todo!("a function can only have 6 parameters"),
                    };
                    self.generate_expression(*arg.clone());
                    self.add_to_output(format!("mov {reg}, rdi").as_str());
                }
                if call_expr.name == "syscall".to_string() {
                    self.add_to_output("syscall");
                }
            }
            ExpressionKind::Identifier(i) => {
                if !self.variables.contains_key(&i) {
                    panic!("variable not found");
                }
                self.add_to_output(format!("mov rdi, [{i}]").as_str());
            }
        }
    }

    fn generate_condition(&mut self, c: Condition) {
        self.generate_expression(c.left);
        self.add_to_output("mov rdx, rdi");
        self.generate_expression(c.right);
        self.add_to_output("cmp rdi, rdx");
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
