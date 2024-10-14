use core::str;
use std::{borrow::Cow, fmt::Display, iter::Peekable};
use miette::miette;
use crate::{lex::Token, Lexer};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Minus,
    Plus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Bang,
    And,
    Or,
    Call,
    For,
    Class,
    Print,
    Return,
    Field,
    Var,
    While,
    Group,
}
impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Operator::Minus => "-",
                Operator::Plus => "+",
                Operator::Star => "*",
                Operator::BangEqual => "!=",
                Operator::EqualEqual => "==",
                Operator::LessEqual => "<=",
                Operator::GreaterEqual => ">=",
                Operator::Less => "<",
                Operator::Greater => ">",
                Operator::Slash => "/",
                Operator::Bang => "!",
                Operator::And => "and",
                Operator::Or => "or",
                Operator::Call => "call",
                Operator::For => "for",
                Operator::Class => "class",
                Operator::Print => "print",
                Operator::Return => "return",
                Operator::Field => "field",
                Operator::Var => "var",
                Operator::While => "while",
                Operator::Group => "group",
            }
        )
    }
}

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

pub trait Visitor<'de, T> {
    fn visit_expression(&self, exp: Expression<'de>) -> T;
}

pub struct Parser<'de> {
    // whole: &'de str,
    lex: Lexer<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            // whole: input,
            lex: Lexer::new(input)
        }
    }

    pub fn expression(&self) -> miette::Result<Expression<'_>> {
        let mut lexer = self.lex.clone().peekable();
        self.equality(&mut lexer)
    }

    fn equality(&self, lexer: &mut Peekable<Lexer<'de>>) -> miette::Result<Expression<'_>> {
        let mut expr = self.comparison(lexer)?;

        while match lexer.peek() {
            Some(Err(_)) => return Err(lexer.next().unwrap().unwrap_err()),
            Some(Ok(token)) if *token == Token::BangEqual || *token == Token::EqualEqual => true,
            _ => false,
        } {
            let operator = lexer.next().unwrap()?;
            let right = self.comparison(lexer)?;
            expr = Expression::Binary { left: Box::new(expr), op: operator, right: Box::new(right) }
        };

        Ok(expr)
    }

    fn comparison(&self, lexer: &mut Peekable<Lexer<'de>>) -> miette::Result<Expression<'_>> {
        let mut expr = self.term(lexer)?;

        while match lexer.peek() {
            Some(Err(_)) => return Err(lexer.next().unwrap().unwrap_err()),
            Some(Ok(token)) if *token == Token::Less || *token == Token::LessEqual || *token == Token::Greater || *token == Token::GreaterEqual => true,
            _ => false,
        } {
            let operator = lexer.next().unwrap()?;
            let right = self.term(lexer)?;
            expr = Expression::Binary { left: Box::new(expr), op: operator, right: Box::new(right) }
        };

        Ok(expr)
    }

    fn term(&self, lexer: &mut Peekable<Lexer<'de>>) -> miette::Result<Expression<'_>> {
        let mut expr = self.factor(lexer)?;

        while match lexer.peek() {
            Some(Err(_)) => return Err(lexer.next().unwrap().unwrap_err()),
            Some(Ok(token)) if *token == Token::Plus || *token == Token::Minus => true,
            _ => false,
        } {
            let operator = lexer.next().unwrap()?;
            let right = self.factor(lexer)?;
            expr = Expression::Binary { left: Box::new(expr), op: operator, right: Box::new(right) }
        }

        Ok(expr)
    }

    fn factor(&self, lexer: &mut Peekable<Lexer<'de>>) -> miette::Result<Expression<'_>> {
        let mut expr = self.unary(lexer)?;

        while match lexer.peek() {
            Some(Err(_)) => return Err(lexer.next().unwrap().unwrap_err()),
            Some(Ok(token)) if *token == Token::Slash || *token == Token::Star => true,
            _ => false,
        } {
            let operator = lexer.next().unwrap()?;
            let right = self.unary(lexer)?;
            expr = Expression::Binary { left: Box::new(expr), op: operator, right: Box::new(right) }
        }

        Ok(expr)
    }

    fn unary(&self, lexer: &mut Peekable<Lexer<'de>>) -> miette::Result<Expression<'_>> {
        match lexer.peek() {
            Some(Ok(Token::Minus)) | Some(Ok(Token::Bang)) => {
                let operator = lexer.next().unwrap()?;
                Ok(Expression::Unary { op: operator, right: Box::new(self.unary(lexer)?) })
            }
            Some(Err(_)) => Err(lexer.next().unwrap().unwrap_err()),
            _ => self.primary(lexer),
        }
    }

    fn primary(&self, lexer: &mut Peekable<Lexer<'de>>) -> miette::Result<Expression<'_>> {
        match lexer.next() {
            Some(Ok(Token::False)) => Ok(Expression::Literal { literal: Atom::Bool(false) }),
            Some(Ok(Token::True)) => Ok(Expression::Literal { literal: Atom::Bool(true) }),
            Some(Ok(Token::Nil)) => Ok(Expression::Literal { literal: Atom::Nil }),

            Some(Ok(Token::Number(_, num))) => Ok(Expression::Literal { literal: Atom::Number(num) }),
            Some(Ok(Token::String(str))) => Ok(Expression::Literal { literal: Atom::String(Cow::Borrowed(str)) }),

            Some(Ok(Token::LParen)) => {
                let exp = self.equality(lexer)?;
                match lexer.next() {
                    Some(Ok(Token::RParen)) => {
                        Ok(Expression::Grouping { expression: Box::new(exp) })
                    }
                    _ => Err(miette!("Missing RParen to close grouping"))
                }
            }
            Some(Err(error)) => Err(miette!(error)),
            _ => Err(miette!("Expected expression"))
        }
    }
}

pub struct Interpreter {}

impl<'de> Visitor<'de, Atom<'de>> for Interpreter {
    fn visit_expression(&self, exp: Expression<'de>) -> Atom<'de> {
        match exp {
            Expression::Unary { op, right } => {
                let right = self.visit_expression(*right);
                match op {
                    Token::Minus => {
                        match right {
                            Atom::Number(num) => Atom::Number(-num),
                            _ => todo!() //Error case
                        }
                    }
                    Token::Bang => {
                        match right {
                            Atom::Bool(bool) => Atom::Bool(!bool),
                            _ => Atom::Bool(true),
                        }
                    }
                    _ => todo!() // Theoretically unreachable
                }
            }
            Expression::Binary { left, op, right } => {
                let left = self.visit_expression(*left);
                let right = self.visit_expression(*right);

                match op {
                    Token::Greater => {
                        match (left, right) {
                            (Atom::Number(n1), Atom::Number(n2)) => Atom::Bool(n1 > n2),
                            _ => todo!(),
                        }
                    }
                    Token::GreaterEqual => {
                        match (left, right) {
                            (Atom::Number(n1), Atom::Number(n2)) => Atom::Bool(n1 >= n2),
                            _ => todo!(),
                        }
                    }
                    Token::Less => {
                        match (left, right) {
                            (Atom::Number(n1), Atom::Number(n2)) => Atom::Bool(n1 < n2),
                            _ => todo!(),
                        }
                    }
                    Token::LessEqual => {
                        match (left, right) {
                            (Atom::Number(n1), Atom::Number(n2)) => Atom::Bool(n1 <= n2),
                            _ => todo!(),
                        }
                    }
                    Token::Plus => {
                        match (left, right) {
                            (Atom::Number(n1), Atom::Number(n2)) => Atom::Number(n1 + n2),
                            (Atom::String(str1), Atom::String(str2)) => Atom::String(Cow::Owned(format!("{str1}{str2}"))),
                            _ => todo!(),
                        }
                    }
                    Token::Minus => {
                        match (left, right) {
                            (Atom::Number(n1), Atom::Number(n2)) => Atom::Number(n1 - n2),
                            _ => todo!(),
                        }
                    }
                    Token::Slash => {
                        match (left, right) {
                            (Atom::Number(n1), Atom::Number(n2)) => Atom::Number(n1 / n2),
                            _ => todo!(),
                        }
                    }
                    Token::Star => {
                        match (left, right) {
                            (Atom::Number(n1), Atom::Number(n2)) => Atom::Number(n1 * n2),
                            _ => todo!(),
                        }
                    }
                    _ => todo!(),
                }
            }
            Expression::Grouping { expression } => self.visit_expression(*expression),
            Expression::Literal { literal } => literal,
        }
    }
}