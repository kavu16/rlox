use core::fmt;
use miette::{Error, LabeledSpan};
use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'de> {
    // SINGLE CHAR TOKENS
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    // ONE OR TWO CHAR TOKENS
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // LITERALS
    Identifier(&'de str),
    String(&'de str),
    Number(&'de str, f64),

    // KEYWORDS
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::LParen => write!(f, "LEFT_PAREN ( null"),
            Token::RParen => write!(f, "RIGHT_PAREN ) null"),
            Token::LBrace => write!(f, "LEFT_BRACE {{ null"),
            Token::RBrace => write!(f, "RIGHT_BRACE }} null"),
            Token::Comma => write!(f, "COMMA , null"),
            Token::Dot => write!(f, "DOT . null"),
            Token::Minus => write!(f, "MINUS - null"),
            Token::Plus => write!(f, "PLUS + null"),
            Token::SemiColon => write!(f, "SEMICOLON ; null"),
            Token::Slash => write!(f, "SLASH / null"),
            Token::Star => write!(f, "STAR * null"),
            Token::Bang => write!(f, "BANG ! null"),
            Token::BangEqual => write!(f, "BANG_EQUAL != null"),
            Token::Equal => write!(f, "EQUAL = null"),
            Token::EqualEqual => write!(f, "EQUAL_EQUAL == null"),
            Token::Greater => write!(f, "GREATER > null"),
            Token::GreaterEqual => write!(f, "GREATER_EQUAL >= null"),
            Token::Less => write!(f, "LESS < null"),
            Token::LessEqual => write!(f, "LESS_EQUAL <= null"),
            Token::Identifier(i) => write!(f, "IDENTIFIER {i} null"),
            Token::String(s) => write!(f, "STRING \"{s}\" {}", Token::unescape(s)),
            Token::Number(lit, num) => write!(f, "NUMBER {lit} {num:?}"),
            Token::And => write!(f, "AND and null"),
            Token::Class => write!(f, "CLASS class null"),
            Token::Else => write!(f, "ELSE else null"),
            Token::False => write!(f, "FALSE false null"),
            Token::Fun => write!(f, "FUN fun null"),
            Token::For => write!(f, "FOR for null"),
            Token::If => write!(f, "IF if null"),
            Token::Nil => write!(f, "NIL nil null"),
            Token::Or => write!(f, "OR or null"),
            Token::Print => write!(f, "PRINT print null"),
            Token::Return => write!(f, "RETURN return null"),
            Token::Super => write!(f, "SUPER super null"),
            Token::This => write!(f, "THIS this null"),
            Token::True => write!(f, "TRUE true null"),
            Token::Var => write!(f, "VAR var null"),
            Token::While => write!(f, "WHILE while null"),
            Token::Eof => write!(f, "EOF null"),
        }
    }
}

impl Token<'_> {
    fn unescape(s: &str) -> Cow<'_, str> {
        // No escaping in Lox
        Cow::Borrowed(s)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
    eof: bool,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
            eof: false,
        }
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.rest.chars();
        while let Some(c) = chars.next() {
            let start_byte = self.byte;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Started<'de> {
                String,
                Number,
                Ident,
                MaybeComment,
                IfEqualElse(Token<'de>, Token<'de>),
            }

            let started =
                match c {
                    '(' => return Some(Ok(Token::LParen)),
                    ')' => return Some(Ok(Token::RParen)),
                    '{' => return Some(Ok(Token::LBrace)),
                    '}' => return Some(Ok(Token::RBrace)),
                    ',' => return Some(Ok(Token::Comma)),
                    '.' => return Some(Ok(Token::Dot)),
                    '-' => return Some(Ok(Token::Minus)),
                    '+' => return Some(Ok(Token::Plus)),
                    ';' => return Some(Ok(Token::SemiColon)),
                    '*' => return Some(Ok(Token::Star)),
                    '/' => Started::MaybeComment,
                    '!' => Started::IfEqualElse(Token::BangEqual, Token::Bang),
                    '<' => Started::IfEqualElse(Token::LessEqual, Token::Less),
                    '>' => Started::IfEqualElse(Token::GreaterEqual, Token::Greater),
                    '=' => Started::IfEqualElse(Token::EqualEqual, Token::Equal),
                    '"' => Started::String,
                    'a'..='z' | '_' => Started::Ident,
                    '0'..='9' => Started::Number,
                    c if c.is_whitespace() => continue,
                    c => return Some(Err(miette::miette! {
                        labels = vec![
                            LabeledSpan::at(self.byte - c.len_utf8()..self.byte, "this character"),
                        ],
                        "Unexpected Token '{c}' in input"
                    }
                    .with_source_code(self.whole.to_string()))),
                };

            match started {
                Started::String => {
                    let mut closed = false;
                    while let Some(c) = chars.next() {
                        self.rest = chars.as_str();
                        self.byte += 1;
                        if c == '"' {
                            closed = true;
                            break;
                        }
                    }
                    if !closed {
                        return Some(Err(miette::miette! {
                            labels = vec![
                                LabeledSpan::at(self.byte-1, "end of file"),
                            ],
                            "Unclosed string"
                        }
                        .with_source_code(self.whole.to_string())));
                    }
                    return Some(Ok(Token::String(
                        &self.whole[start_byte + 1..self.byte - 1],
                    )));
                }
                Started::Number => {
                    let mut curr_num = c.to_digit(10).unwrap() as f64;
                    let mut curr_c = c;

                    while let Some(c) = chars.next() {
                        if !c.is_numeric() {
                            curr_c = c;
                            break;
                        }
                        self.rest = chars.as_str();
                        self.byte += 1;

                        curr_num *= 10.0;
                        curr_num += c.to_digit(10).unwrap() as f64;
                    }
                    if curr_c == '.'
                        && self.rest[1..]
                            .starts_with(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])
                    {
                        self.rest = chars.as_str();
                        self.byte += 1;

                        let mut tenths = 1.0;

                        while let Some(c) = chars.next() {
                            if !c.is_numeric() {
                                curr_c = c;
                                break;
                            }
                            self.rest = chars.as_str();
                            self.byte += 1;

                            tenths *= 10.0;
                            curr_num += c.to_digit(10).unwrap() as f64 / tenths;
                        }
                        if curr_c == '.' {
                            return Some(Err(miette::miette! {
                                labels = vec![
                                    LabeledSpan::at(self.byte, "this character"),
                                ],
                                "Malformed number"
                            }
                            .with_source_code(self.whole.to_string())));
                        }
                    }
                    self.rest = &self.whole[self.byte..];

                    return Some(Ok(Token::Number(
                        &self.whole[start_byte..self.byte],
                        curr_num,
                    )));
                }
                Started::Ident => {
                    while let Some(c) = chars.next() {
                        if !c.is_alphanumeric() && c != '_' {
                            break;
                        }
                        self.rest = chars.as_str();
                        self.byte += 1;
                    }
                    return match &self.whole[start_byte..self.byte] {
                        "and" => Some(Ok(Token::And)),
                        "class" => Some(Ok(Token::Class)),
                        "else" => Some(Ok(Token::Else)),
                        "false" => Some(Ok(Token::False)),
                        "for" => Some(Ok(Token::For)),
                        "fun" => Some(Ok(Token::Fun)),
                        "if" => Some(Ok(Token::If)),
                        "nil" => Some(Ok(Token::Nil)),
                        "or" => Some(Ok(Token::Or)),
                        "print" => Some(Ok(Token::Print)),
                        "return" => Some(Ok(Token::Return)),
                        "super" => Some(Ok(Token::Super)),
                        "this" => Some(Ok(Token::This)),
                        "true" => Some(Ok(Token::True)),
                        "var" => Some(Ok(Token::Var)),
                        "while" => Some(Ok(Token::While)),
                        ident => Some(Ok(Token::Identifier(ident))),
                    };
                }
                Started::IfEqualElse(yes, no) => {
                    if self.rest.trim_start().starts_with('=') {
                        self.rest = &self.rest.trim_start()[1..];
                        self.byte += 1;
                        return Some(Ok(yes));
                    } else {
                        return Some(Ok(no));
                    }
                }
                Started::MaybeComment => {
                    if self.rest.starts_with('/') {
                        while let Some(c) = chars.next() {
                            self.rest = chars.as_str();
                            self.byte += 1;

                            if c == '\n' {
                                break;
                            }
                        }
                    } else {
                        return Some(Ok(Token::Slash));
                    }
                }
            }
        }
        if !self.eof {
            self.eof = true;
            Some(Ok(Token::Eof))
        } else {
            None
        }
    }
}
