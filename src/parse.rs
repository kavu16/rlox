use crate::{lex::Token, Lexer};
use core::str;
use miette::miette;
use std::{borrow::Cow, fmt::Display, iter::Peekable};

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de> {
    String(Cow<'de, str>),
    Number(f64),
    Nil,
    Bool(bool),
    Ident(&'de str),
    Super,
    This,
}

impl Display for Atom<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Atom::String(Cow::Owned(str)) => str.to_owned(),
                Atom::String(Cow::Borrowed(str)) => (*str).to_owned(),
                Atom::Number(num) => num.to_string(),
                Atom::Nil => String::from("nil"),
                Atom::Bool(bool) => bool.to_string(),
                Atom::Ident(str) => (*str).to_owned(),
                Atom::Super => String::from("super"),
                Atom::This => String::from("this"),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression<'de> {
    Unary {
        op: Token<'de>,
        right: Box<Expression<'de>>,
    },
    Binary {
        left: Box<Expression<'de>>,
        op: Token<'de>,
        right: Box<Expression<'de>>,
    },
    Grouping {
        expression: Box<Expression<'de>>,
    },
    Literal {
        literal: Atom<'de>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<'de> {
    Expression(Expression<'de>),
    Print(Expression<'de>),
}

pub trait Visitor<'de, T> {
    fn visit_expression(&self, exp: Expression<'de>) -> miette::Result<T>;
    fn visit_statement(&self, stmt: Statement<'de>) -> miette::Result<()>;
}

pub struct Parser<'de> {
    // whole: &'de str,
    lex: Peekable<Lexer<'de>>,
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            // whole: input,
            lex: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> miette::Result<Vec<Statement<'de>>> {
        let mut statements = Vec::new();
        while match self.lex.peek() {
            Some(Err(_)) => return Err(self.lex.next().unwrap().unwrap_err()),
            Some(Ok(Token::Eof)) => false,
            _ => true,
        } {
            statements.push(self.parse_statement()?)
        }

        Ok(statements)
    }

    pub fn parse_statement(&mut self) -> miette::Result<Statement<'de>> {
        match self.lex.peek() {
            Some(Ok(Token::Print)) => {
                self.lex.next();
                self.parse_print_statement()
            }
            Some(Err(_)) => Err(self.lex.next().unwrap().unwrap_err()),
            Some(_) => self.parse_exp_statement(),
            None => todo!()
        }
    }

    pub fn parse_exp_statement(&mut self) -> miette::Result<Statement<'de>> {
        let expression = self.parse_expression()?;
        if let Some(Ok(Token::SemiColon)) = self.lex.next() {
            Ok(Statement::Expression(expression))
        } else {
            Err(miette!("Expected ; after value"))
        }
    }

    pub fn parse_print_statement(&mut self) -> miette::Result<Statement<'de>> {
        let expression = self.parse_expression()?;
        if let Some(Ok(Token::SemiColon)) = self.lex.next() {
            Ok(Statement::Print(expression))
        } else {
            Err(miette!("Expected ; after value"))
        }
    }

    pub fn parse_expression(&mut self) -> miette::Result<Expression<'de>> {
        // let mut lexer = self.lex.clone().peekable();
        let expression = self.equality();
        expression
    }

    fn equality(&mut self) -> miette::Result<Expression<'de>> {
        let mut expr = self.comparison()?;

        while match self.lex.peek() {
            Some(Err(_)) => return Err(self.lex.next().unwrap().unwrap_err()),
            Some(Ok(token)) if *token == Token::BangEqual || *token == Token::EqualEqual => true,
            _ => false,
        } {
            let operator = self.lex.next().unwrap()?;
            let right = self.comparison()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                op: operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> miette::Result<Expression<'de>> {
        let mut expr = self.term()?;

        while match self.lex.peek() {
            Some(Err(_)) => return Err(self.lex.next().unwrap().unwrap_err()),
            Some(Ok(token))
                if *token == Token::Less
                    || *token == Token::LessEqual
                    || *token == Token::Greater
                    || *token == Token::GreaterEqual =>
            {
                true
            }
            _ => false,
        } {
            let operator = self.lex.next().unwrap()?;
            let right = self.term()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                op: operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> miette::Result<Expression<'de>> {
        let mut expr = self.factor()?;

        while match self.lex.peek() {
            Some(Err(_)) => return Err(self.lex.next().unwrap().unwrap_err()),
            Some(Ok(token)) if *token == Token::Plus || *token == Token::Minus => true,
            _ => false,
        } {
            let operator = self.lex.next().unwrap()?;
            let right = self.factor()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                op: operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> miette::Result<Expression<'de>> {
        let mut expr = self.unary()?;

        while match self.lex.peek() {
            Some(Err(_)) => return Err(self.lex.next().unwrap().unwrap_err()),
            Some(Ok(token)) if *token == Token::Slash || *token == Token::Star => true,
            _ => false,
        } {
            let operator = self.lex.next().unwrap()?;
            let right = self.unary()?;
            expr = Expression::Binary {
                left: Box::new(expr),
                op: operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> miette::Result<Expression<'de>> {
        match self.lex.peek() {
            Some(Ok(Token::Minus)) | Some(Ok(Token::Bang)) => {
                let operator = self.lex.next().unwrap()?;
                Ok(Expression::Unary {
                    op: operator,
                    right: Box::new(self.unary()?),
                })
            }
            Some(Err(_)) => Err(self.lex.next().unwrap().unwrap_err()),

            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> miette::Result<Expression<'de>> {
        match self.lex.next() {
            Some(Ok(Token::False)) => Ok(Expression::Literal {
                literal: Atom::Bool(false),
            }),
            Some(Ok(Token::True)) => Ok(Expression::Literal {
                literal: Atom::Bool(true),
            }),
            Some(Ok(Token::Nil)) => Ok(Expression::Literal { literal: Atom::Nil }),

            Some(Ok(Token::Number(_, num))) => Ok(Expression::Literal {
                literal: Atom::Number(num),
            }),
            Some(Ok(Token::String(str))) => Ok(Expression::Literal {
                literal: Atom::String(Cow::Borrowed(str)),
            }),

            Some(Ok(Token::LParen)) => {
                let exp = self.equality()?;
                match self.lex.next() {
                    Some(Ok(Token::RParen)) => Ok(Expression::Grouping {
                        expression: Box::new(exp),
                    }),
                    _ => Err(miette!("Missing RParen to close grouping")),
                }
            }
            Some(Ok(Token::RParen)) => Err(miette!("Missing Opening Lparen")),
            Some(Err(error)) => Err(miette!(error)),
            _ => Err(miette!("Expected expression")),
        }
    }
}

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, statements: Vec<Statement>) -> miette::Result<()> {
        for stmt in statements {
            self.visit_statement(stmt)?;
        }
        Ok(())
    }
}

impl<'de> Visitor<'de, Atom<'de>> for Interpreter {
    fn visit_expression(&self, exp: Expression<'de>) -> miette::Result<Atom<'de>> {
        match exp {
            Expression::Unary { op, right } => {
                let right = self.visit_expression(*right)?;
                match op {
                    Token::Minus => {
                        match right {
                            Atom::Number(num) => Ok(Atom::Number(-num)),
                            _ => Err(miette!("Cannot negate non-number")), //Error case
                        }
                    }
                    Token::Bang => match right {
                        Atom::Bool(bool) => Ok(Atom::Bool(!bool)),
                        _ => Ok(Atom::Bool(true)),
                    },
                    _ => Err(miette!("Unsupported Unary expression")), // Theoretically unreachable
                }
            }
            Expression::Binary { left, op, right } => {
                let left = self.visit_expression(*left)?;
                let right = self.visit_expression(*right)?;

                match op {
                    Token::Greater => match (left, right) {
                        (Atom::Number(n1), Atom::Number(n2)) => Ok(Atom::Bool(n1 > n2)),
                        _ => todo!(),
                    },
                    Token::GreaterEqual => match (left, right) {
                        (Atom::Number(n1), Atom::Number(n2)) => Ok(Atom::Bool(n1 >= n2)),
                        _ => todo!(),
                    },
                    Token::Less => match (left, right) {
                        (Atom::Number(n1), Atom::Number(n2)) => Ok(Atom::Bool(n1 < n2)),
                        _ => todo!(),
                    },
                    Token::LessEqual => match (left, right) {
                        (Atom::Number(n1), Atom::Number(n2)) => Ok(Atom::Bool(n1 <= n2)),
                        _ => todo!(),
                    },
                    Token::Plus => match (left, right) {
                        (Atom::Number(n1), Atom::Number(n2)) => Ok(Atom::Number(n1 + n2)),
                        (Atom::String(str1), Atom::String(str2)) => {
                            Ok(Atom::String(Cow::Owned(format!("{str1}{str2}"))))
                        }
                        _ => todo!(),
                    },
                    Token::Minus => match (left, right) {
                        (Atom::Number(n1), Atom::Number(n2)) => Ok(Atom::Number(n1 - n2)),
                        _ => todo!(),
                    },
                    Token::Slash => match (left, right) {
                        (Atom::Number(n1), Atom::Number(n2)) => Ok(Atom::Number(n1 / n2)),
                        _ => todo!(),
                    },
                    Token::Star => match (left, right) {
                        (Atom::Number(n1), Atom::Number(n2)) => Ok(Atom::Number(n1 * n2)),
                        _ => todo!(),
                    },
                    _ => Err(miette!("Unsupported Binary Expression")),
                }
            }
            Expression::Grouping { expression } => self.visit_expression(*expression),
            Expression::Literal { literal } => Ok(literal),
        }
    }

    fn visit_statement(&self, stmt: Statement<'de>) -> miette::Result<()> {
        match stmt {
            Statement::Expression(exp) => { self.visit_expression(exp)?; }
            Statement::Print(exp) => {
                let val = self.visit_expression(exp)?;
                println!("{val}");
            }
        }
        Ok(())
    }
}