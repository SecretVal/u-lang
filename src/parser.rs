#![allow(dead_code, non_snake_case)]
use crate::lexer::Lexer;
use crate::lexer::Token;
use crate::lexer::TokenKind;
#[derive(Debug, PartialEq, Clone)]
pub struct Parser {
    pub(crate) tokens: Vec<Token>,
    pub(crate) pos: usize,
    pub(crate) file: String,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub(crate) kind: StatementKind,
}

impl Statement {
    pub fn expression(expr: Expression) -> Self {
        Self {
            kind: StatementKind::Expression(expr),
        }
    }

    pub fn declaration(decl: Declaration) -> Self {
        match decl.kind {
            DeclarationKind::VariableDeclaration(var_decl) => Self {
                kind: StatementKind::Declaration(Declaration {
                    kind: DeclarationKind::VariableDeclaration(var_decl),
                }),
            },
            DeclarationKind::VariableRedeclaration(var_re_decl) => Self {
                kind: StatementKind::Declaration(Declaration {
                    kind: DeclarationKind::VariableDeclaration(var_re_decl),
                }),
            },
        }
    }

    pub fn if_statemnt(stmt: IfStatement) -> Self {
        Self {
            kind: StatementKind::IfStatement(stmt),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    Expression(Expression),
    Declaration(Declaration),
    IfStatement(IfStatement),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfStatement {
    pub(crate) left: Expression,
    pub(crate) right: Expression,
    pub(crate) body: Vec<Box<Statement>>,
    pub(crate) stmt_count: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Declaration {
    pub(crate) kind: DeclarationKind,
}
#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationKind {
    VariableDeclaration(VariableDeclaration),
    VariableRedeclaration(VariableDeclaration),
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclaration {
    pub(crate) name: String,
    pub(crate) value: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub(crate) kind: ExpressionKind,
}

impl Expression {
    pub fn number(n: i64) -> Self {
        Self {
            kind: ExpressionKind::NumberExpression(n),
        }
    }
    pub fn binary(expr: BinaryExpression) -> Self {
        Self {
            kind: ExpressionKind::BinaryExpression(expr),
        }
    }
    pub fn call(expr: CallExpression) -> Self {
        Self {
            kind: ExpressionKind::CallExpression(expr),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionKind {
    NumberExpression(i64),
    BinaryExpression(BinaryExpression),
    CallExpression(CallExpression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpression {
    pub(crate) kind: BinaryExpressionKind,
    pub(crate) left: i64,
    pub(crate) right: i64,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryExpressionKind {
    Plus,
    Minus,
}
#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub(crate) name: String,
    pub(crate) args: Vec<Box<Expression>>,
}

impl Parser {
    pub fn from_input(input: &str, file: String) -> Self {
        let mut lexer = Lexer::new(input);
        let mut tokens: Vec<Token> = Vec::new();
        while let Some(token) = lexer.next_token() {
            tokens.push(token);
        }
        Self::new(
            tokens
                .into_iter()
                .filter(|t| t.kind != TokenKind::Whitespace)
                .collect(),
            file,
        )
    }

    pub fn new(tokens: Vec<Token>, file: String) -> Self {
        Self {
            tokens,
            pos: 0,
            file,
        }
    }

    pub fn parse_statement(&mut self) -> Option<Statement> {
        match self.current().kind {
            TokenKind::Let => return Some(Statement::declaration(self.parse_declaration())),
            TokenKind::Identifier => return Some(Statement::declaration(self.parse_declaration())),
            TokenKind::If => return Some(Statement::if_statemnt(self.parse_if_statement())),
            _ => {
                let expr = self.parse_expr();
                if expr.is_none() {
                    return None;
                }
                return Some(Statement::expression(expr.unwrap()));
            }
        }
    }

    pub fn parse_expr(&mut self) -> Option<Expression> {
        let token = self.current();
        return match token.kind {
            TokenKind::Syscall => Some(Expression::call(self.parse_syscall())),
            TokenKind::Number(num) => {
                if self.peek(1).kind == TokenKind::Plus || self.peek(1).kind == TokenKind::Minus {
                    return Some(Expression::binary(self.parse_binary_expr()));
                }
                self.consume().unwrap();
                Some(Expression::number(num))
            }
            TokenKind::Plus => {
                eprintln!(
                    "Error: {}:{}: You cannot start a statement with `+`",
                    self.file,
                    self.current().loc(),
                );
                std::process::exit(1);
            }
            TokenKind::Minus => {
                eprintln!(
                    "Error: {}:{}: You cannot start a statement with `-`",
                    self.file,
                    self.current().loc(),
                );
                std::process::exit(1);
            }
            TokenKind::Bad => {
                eprintln!("Error: {}:{}: Bad token", self.file, self.current().loc(),);
                std::process::exit(1);
            }
            TokenKind::Eof => {
                return None;
            }
            TokenKind::Whitespace => {
                eprintln!(
                    "Error: {}:{}: Still a whitespace token in parsing",
                    self.file,
                    self.current().loc()
                );
                eprintln!("Notice: If you are not the developer please contact create a github issue. This should never happen.");
                std::process::exit(1);
            }
            _ => {
                eprintln!(
                    "Error: {}:{}: Trying to parse this as an Expression",
                    self.file,
                    self.current().loc()
                );
                std::process::exit(1);
            }
        };
    }

    fn parse_if_statement(&mut self) -> IfStatement {
        self.consume().unwrap();
        let left = self.parse_expr();
        if left.is_none() {
            eprintln!("error");
            std::process::exit(1);
        }
        match self.current().kind {
            TokenKind::DoubleEquals => self.consume().unwrap(),
            _ => {
                eprintln!(
                    "Error: {}:{}: Expected `==`",
                    self.file,
                    self.current().loc()
                );
                std::process::exit(1);
            }
        };
        let right = self.parse_expr();
        if right.is_none() {
            eprintln!("error");
            std::process::exit(1);
        }
        match self.current().kind {
            TokenKind::OpenParen => self.consume().unwrap(),
            _ => {
                eprintln!(
                    "Error: {}:{}: Expected `{{`",
                    self.file,
                    self.current().loc()
                );
                std::process::exit(1);
            }
        };
        let mut statements: Vec<Box<Statement>> = vec![];
        while let Some(stmt) = self.parse_statement() {
            statements.push(Box::new(stmt));
            if self.current().kind == TokenKind::CloseParen {
                break;
            }
        }
        match self.current().kind {
            TokenKind::CloseParen => self.consume().unwrap(),
            _ => {
                eprintln!(
                    "Error: {}:{}: Expected `}}`",
                    self.file,
                    self.current().loc()
                );
                std::process::exit(1);
            }
        };
        IfStatement {
            left: left.unwrap(),
            right: right.unwrap(),
            body: statements.clone(),
            stmt_count: statements.len(),
        }
    }

    fn parse_syscall(&mut self) -> CallExpression {
        // syscall
        self.consume();
        let mut args = vec![];
        for _ in 0..7 {
            args.push(match self.current().kind {
                TokenKind::Number(num) => Box::new(Expression::number(num)),
                TokenKind::Eof => break,
                _ => Box::new(Expression::number(0)),
            });
            self.consume().unwrap();
            match self.current().kind {
                TokenKind::Comma => self.consume().unwrap(),
                TokenKind::Eof => break,
                _ => break,
            };
        }
        CallExpression {
            name: "syscall".to_string(),
            args,
        }
    }

    fn parse_declaration(&mut self) -> Declaration {
        return match self.current().kind {
            TokenKind::Let => Declaration {
                kind: DeclarationKind::VariableDeclaration(self.parse_var_declaration()),
            },
            TokenKind::Identifier => {
                if self.peek(1).kind == TokenKind::Equals {
                    Declaration {
                        kind: DeclarationKind::VariableRedeclaration(
                            self.parse_variable_redleclaration(),
                        ),
                    }
                } else {
                    todo!();
                }
            }
            _ => todo!(),
        };
    }

    fn parse_var_declaration(&mut self) -> VariableDeclaration {
        // `let`
        self.consume().unwrap();
        // name
        let literal = &self.clone().current().span.literal.clone();
        let name = match self.clone().consume().unwrap().kind {
            TokenKind::Identifier => literal,
            _ => {
                let current = self.current();
                eprintln!(
                    "Error: {}:{}: `{}` is not a valid variable name",
                    self.file,
                    current.loc(),
                    current.span.literal
                );
                std::process::exit(1);
            }
        };
        // identifier
        self.consume().unwrap();
        // =
        self.consume().unwrap();
        // expression
        let value = self.clone().parse_expr().unwrap();
        self.parse_expr();
        VariableDeclaration {
            name: name.to_string(),
            value: value.clone(),
        }
    }

    fn parse_variable_redleclaration(&mut self) -> VariableDeclaration {
        let a = self.consume().unwrap();
        let name = match a.kind {
            TokenKind::Identifier => a.span.literal.clone(),
            _ => todo!(),
        }
        .to_string();
        self.consume();
        let value = self.parse_expr().unwrap();
        return VariableDeclaration { name, value };
    }

    fn parse_binary_expr(&mut self) -> BinaryExpression {
        let left = match self.clone().consume().unwrap().kind {
            TokenKind::Number(num) => num,
            _ => {
                let current = &self.current();
                println!("{}", current.span.literal.len());
                if current.span.literal.len() <= 1 {
                    eprintln!(
                        "Error: {}:{}: Please provide the first number",
                        self.file,
                        current.loc(),
                    );
                } else {
                    eprintln!(
                        "Error: {}:{}: `{}` is not a number",
                        self.file,
                        current.loc(),
                        current.span.literal
                    );
                }
                std::process::exit(1);
            }
        };
        self.consume();
        let kind = match self.clone().consume().unwrap().kind {
            TokenKind::Plus => BinaryExpressionKind::Plus,
            TokenKind::Minus => BinaryExpressionKind::Minus,
            _ => {
                let current = &self.current();
                eprintln!(
                    "Error: {}:{}: `{}` is not a known operator",
                    self.file,
                    current.loc(),
                    current.span.literal
                );
                std::process::exit(1);
            }
        };
        self.consume();
        let right = match self.clone().consume().unwrap().kind {
            TokenKind::Number(num) => num,
            _ => {
                let current = &self.current();
                if current.span.literal.len() <= 1 {
                    eprintln!(
                        "Error: {}:{}: Please provide a second number",
                        self.file,
                        current.loc(),
                    );
                } else {
                    eprintln!(
                        "Error: {}:{}: `{}` is not a number",
                        self.file,
                        current.loc(),
                        current.span.literal
                    );
                }
                std::process::exit(1);
            }
        };
        self.consume();
        BinaryExpression { kind, left, right }
    }

    fn consume(&mut self) -> Option<&Token> {
        if self.tokens.len() <= self.pos {
            return None;
        }
        let token = &self.tokens[self.pos];
        self.pos += 1;
        Some(token)
    }

    fn peek(&self, offset: usize) -> &Token {
        let mut index = self.pos + offset;
        if self.pos >= self.tokens.len() {
            index = self.pos;
        }
        &self.tokens[index]
    }
    fn current(&self) -> &Token {
        &self.peek(0)
    }
}
