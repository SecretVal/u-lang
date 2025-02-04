#![allow(dead_code, non_snake_case)]

use crate::parser::BinaryExpressionKind;
use crate::parser::Condition;
use crate::parser::DeclarationKind;
use crate::parser::EqualKind;
use crate::parser::Expression;
use crate::parser::ExpressionKind;
use crate::parser::FunctionDeclaration;
use crate::parser::Operator;
use crate::parser::Statement;
use crate::parser::StatementKind;

#[derive(Debug, Clone)]
pub struct Generator {
    statements: Vec<Statement>,
    output: String,
    pos: usize,
    stmt_pos: usize,
    variables: Vec<String>,
    strings: Vec<String>,
    subroutines: Vec<FunctionDeclaration>,
}

impl Generator {
    pub fn new(statements: Vec<Statement>) -> Self {
        let mut output = String::new();
        output.push_str("format ELF64 executable 3\n");
        output.push_str("entry start\n");
        output.push_str("segment readable executable\n");
        output.push_str("start:\n");
        let variables = vec![];
        let strings = vec![];
        let subroutines = vec![];
        Self {
            statements,
            output,
            pos: 0,
            stmt_pos: 0,
            variables,
            strings,
            subroutines,
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
                self.atp("mov rax, 60");
                self.atp("syscall");
                for sub in self.subroutines.clone() {
                    self.output.push_str(format!("{}:\n", sub.name).as_str());
		    let mut a2 = sub.args.clone();
		    a2.reverse();
                    for (i, a) in a2.iter().enumerate() {
                        self.variables.push(a.name.clone());
			self.atp(&format!("mov rdi, [rsp + {}]", 8 * (i+1)));
                        self.atp(&format!("mov [{}], rdi", a.name));
                    }
                    for stmt in sub.body {
                        self.stmt_pos += 1;
                        self.output
                            .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
                        self.generate_statement(stmt);
                    }
		    self.stmt_pos += 1;
		    self.output
                        .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
                    self.atp("ret");
                }
                self.output.push_str("segment readable writeable\n");
                for name in self.variables.clone().into_iter() {
                    self.atp(format!("{} dq 0", name).as_str());
                }
                for (i, str) in self.strings.clone().iter().enumerate() {
                    self.atp(format!("str_{i} db \"{str}\", 0Dh, 0Ah, \"$\"").as_str());
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
                    self.variables.push(var_decl.clone().name);
                    self.generate_expression(var_decl.clone().value);
                    self.atp(format!("mov [{}], rdi", var_decl.clone().name).as_str());
                    self.atp("xor rdi, rdi");
                }
                DeclarationKind::VariableRedeclaration(var_decl) => {
                    if !self.variables.contains(&var_decl.name) {
                        panic!("variable not found");
                    }
                    self.generate_expression(var_decl.clone().value);
                    match var_decl.kind {
                        EqualKind::Equals => {
                            self.atp(format!("mov [{}], rdi", var_decl.clone().name).as_str())
                        }
                        EqualKind::MinusEquals => {
                            self.atp(format!("sub [{}], rdi", var_decl.clone().name).as_str())
                        }
                        EqualKind::PlusEquals => {
                            self.atp(format!("add [{}], rdi", var_decl.clone().name).as_str())
                        }
                    }
                    self.atp("xor rdi, rdi");
                }
                DeclarationKind::FunctionDeclaration(sub) => {
                    self.subroutines.push(sub);
                }
            },
            StatementKind::IfStatement(if_stmt) => {
                self.atp(";; -- if --- ;;");
                self.generate_condition(if_stmt.condition.clone());
                self.atp("xor rdi, rdi");
                self.atp("xor rdx, rdx");
                self.atp(format!("jmp addr_{}", self.stmt_pos + 1 + if_stmt.stmt_count).as_str());
                for stmt in if_stmt.body {
                    self.stmt_pos += 1;
                    self.output
                        .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
                    self.generate_statement(stmt);
                }
                self.atp(
                    format!("jmp addr_{}", self.stmt_pos + 1 + if_stmt.else_stmt_count).as_str(),
                );
                if if_stmt.else_body.is_some() {
                    for stmt in if_stmt.else_body.unwrap() {
                        self.stmt_pos += 1;
                        self.output
                            .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
                        self.generate_statement(stmt);
                    }
                }
            }
            StatementKind::WhileStatement(while_stmt) => {
                self.atp(";; -- while --- ;;");
                self.generate_condition(while_stmt.condition.clone());
                self.atp("xor rdi, rdi");
                self.atp("xor rdx, rdx");
                let start = self.stmt_pos;
                self.atp(
                    format!("jmp addr_{}", self.stmt_pos + 1 + while_stmt.stmt_count).as_str(),
                );
                for stmt in while_stmt.body {
                    self.stmt_pos += 1;
                    self.output
                        .push_str(format!("addr_{}:\n", self.stmt_pos).as_str());
                    self.generate_statement(*stmt.clone());
                }
                self.atp(format!("jmp addr_{start}").as_str());
            }
        };
        Some(())
    }

    fn generate_expression(&mut self, expr: Expression) {
        match expr.kind {
            ExpressionKind::NumberExpression(num) => {
                self.atp(format!("mov rdi, {}", num).as_str());
            }
            ExpressionKind::BinaryExpression(bexpr) => match bexpr.kind {
                BinaryExpressionKind::Plus => {
                    self.generate_expression(*bexpr.left.clone());
                    self.atp("mov rdx, rdi");
                    self.generate_expression(*bexpr.right.clone());
                    self.atp("add rdx, rdi");
                    self.atp("mov rdi, rdx");
                }
                BinaryExpressionKind::Minus => {
                    self.generate_expression(*bexpr.left.clone());
                    self.atp("mov rdx, rdi");
                    self.generate_expression(*bexpr.right.clone());
                    self.atp("sub rdx, rdi");
                    self.atp("mov rdi, rdx");
                }
            },
            ExpressionKind::CallExpression(call_expr) => {
                if call_expr.name == "syscall".to_string() {
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
                        if i == 1 {
                            self.atp("push rdi");
                            continue;
                        }
                        self.atp(format!("mov {reg}, rdi").as_str());
                    }
                    self.atp("pop rdi");
                    self.atp("syscall");
                } else {
                    for arg in call_expr.args {
                        self.generate_expression(*arg);
                        self.atp("push rdi");
                    }
                    self.atp(&format!("call {}", call_expr.name));
                }

                self.atp("xor rdi, rdi");
            }
            ExpressionKind::Variable(var) => {
                if !self.variables.contains(&var) {
                    panic!("variable ({}) not found", var);
                }
                self.atp(format!("mov rdi, [{var}]").as_str());
            }
            ExpressionKind::StringLiteral(str) => {
                self.strings.push(str);
                self.atp(format!("lea rdi, [str_{}]", self.strings.len() - 1).as_str());
            }
            ExpressionKind::Comment(_) => {}
        }
    }

    fn generate_condition(&mut self, c: Condition) {
        self.generate_expression(c.left);
        self.atp("mov rdx, rdi");
        self.generate_expression(c.right);
        self.atp("cmp rdx, rdi");
        self.generate_operator(c.operator);
    }

    fn generate_operator(&mut self, op: Operator) {
        match op {
            Operator::Equals => self.atp(format!("je addr_{}", self.stmt_pos + 1).as_str()),
            Operator::NotEquals => self.atp(format!("jne addr_{}", self.stmt_pos + 1).as_str()),
            Operator::GreaterThan => self.atp(format!("jg addr_{}", self.stmt_pos + 1).as_str()),
            Operator::LessThan => self.atp(format!("jl addr_{}", self.stmt_pos + 1).as_str()),
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

    fn atp(&mut self, str: &str) {
        self.output.push_str(format!("    {}\n", str).as_str());
    }
}
