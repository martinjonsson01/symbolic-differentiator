use crate::interpreter::operator::{Operator, OPERATORS};
use anyhow::{anyhow, bail, Context, Result};
use std::fmt;
use std::fmt::Formatter;
use std::str;

/// A discrete part of an expression
#[derive(Clone, PartialEq)]
pub enum Token {
    Literal(f64),
    Identifier(String),
    Plus,
    Minus,
    Star,
    Caret,
    ForwardSlash,
    OpenParenthesis,
    CloseParenthesis,
}

pub static SYMBOLS: [char; 7] = ['+', '-', '*', '/', '^', '(', ')'];

impl Token {
    pub fn to_operator(&self) -> Result<&'static Operator> {
        for operator in OPERATORS.iter() {
            if &operator.token == self {
                return Ok(operator);
            }
        }
        Err(anyhow!("Token {} is not an operator", self))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Literal(value) => write!(f, "{}", value),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Caret => write!(f, "^"),
            Token::ForwardSlash => write!(f, "/"),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
        }
    }
}

impl str::FromStr for Token {
    type Err = ();

    fn from_str(input: &str) -> Result<Token, Self::Err> {
        match input {
            "+" => Ok(Token::Plus),
            "-" => Ok(Token::Minus),
            "*" => Ok(Token::Star),
            "/" => Ok(Token::ForwardSlash),
            "^" => Ok(Token::Caret),
            "(" => Ok(Token::OpenParenthesis),
            ")" => Ok(Token::CloseParenthesis),
            input => Ok(parse_literal_or_identifier(input)),
        }
    }
}

fn parse_literal_or_identifier(text: &str) -> Token {
    let number = text.parse::<f64>();
    match number {
        Ok(value) => Token::Literal(value),
        Err(_) => Token::Identifier(text.to_string()),
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
