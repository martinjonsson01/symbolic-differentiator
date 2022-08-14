use anyhow::Result;
use std::fmt;
use std::fmt::Formatter;
use std::str;

/// A discrete part of an expression
#[derive(Clone, PartialEq, Eq)]
pub enum Token {
    LiteralInteger(i32),
    Identifier(String),
    Plus,
    Dash,
    Asterisk,
    ForwardSlash,
    Caret,
    LeftParentheses,
    RightParentheses,
}

pub static SYMBOLS: [char; 7] = ['+', '-', '*', '/', '^', '(', ')'];

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::LiteralInteger(value) => write!(f, "{}", value),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::Plus => write!(f, "+"),
            Token::Dash => write!(f, "-"),
            Token::Asterisk => write!(f, "*"),
            Token::ForwardSlash => write!(f, "/"),
            Token::Caret => write!(f, "^"),
            Token::LeftParentheses => write!(f, "("),
            Token::RightParentheses => write!(f, ")"),
        }
    }
}

impl str::FromStr for Token {
    type Err = ();

    fn from_str(input: &str) -> Result<Token, Self::Err> {
        match input {
            "+" => Ok(Token::Plus),
            "-" => Ok(Token::Dash),
            "*" => Ok(Token::Asterisk),
            "/" => Ok(Token::ForwardSlash),
            "^" => Ok(Token::Caret),
            "(" => Ok(Token::LeftParentheses),
            ")" => Ok(Token::RightParentheses),
            input => Ok(parse_literal_or_identifier(input)),
        }
    }
}

fn parse_literal_or_identifier(text: &str) -> Token {
    let number = text.parse::<i32>();
    match number {
        Ok(value) => Token::LiteralInteger(value),
        Err(_) => Token::Identifier(text.to_string()),
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}
