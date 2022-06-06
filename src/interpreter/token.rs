use std::fmt;
use std::fmt::{Formatter};

/// A discrete part of an expression
#[derive(Clone, PartialEq)]
pub enum Token {
    Literal(f64),
    Identifier(String),
    Plus,
    Minus,
    Star,
    ForwardSlash,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Literal(value) => write!(f, "{}", value),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::ForwardSlash => write!(f, "/"),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}