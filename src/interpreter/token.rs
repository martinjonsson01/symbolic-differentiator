use crate::interpreter::operator::{Associativity, Operator};
use anyhow::Result;
use std::fmt;
use std::fmt::Formatter;
use std::str;

/// A discrete part of an expression
#[derive(Clone, PartialEq)]
pub enum Token {
    Literal(f64),
    Identifier(String),
    Operator(Operator),
    OpenParenthesis,
    CloseParenthesis,
}

pub static SYMBOLS: [char; 7] = ['+', '-', '*', '/', '^', '(', ')'];

impl Token {
    /// A 'value' is a token that either represents, contains or is a numerical value.
    /// E.g. a literal or identifier.
    pub fn is_value(&self) -> bool {
        matches!(self, Token::Literal(_)) || matches!(self, Token::Identifier(_))
    }
    
    pub fn is_caret(&self) -> bool {
        let caret = "^".to_string();
        matches!(self, Token::Operator(Operator { symbol: caret, .. }))
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Token::Literal(value) => write!(f, "{}", value),
            Token::Identifier(name) => write!(f, "{}", name),
            Token::Operator(operator) => write!(f, "{}", operator),
            Token::OpenParenthesis => write!(f, "("),
            Token::CloseParenthesis => write!(f, ")"),
        }
    }
}

impl str::FromStr for Token {
    type Err = ();

    fn from_str(input: &str) -> Result<Token, Self::Err> {
        match input {
            "+" => Ok(Token::Operator(Operator {
                symbol: "+".into(),
                precedence: 0,
                associativity: Associativity::Left,
                evaluate: |a, b| a + b,
            })),
            "-" => Ok(Token::Operator(Operator {
                symbol: "-".into(),
                precedence: 0,
                associativity: Associativity::Left,
                evaluate: |a, b| a - b,
            })),
            "*" => Ok(Token::Operator(Operator {
                symbol: "*".into(),
                precedence: 1,
                associativity: Associativity::Left,
                evaluate: |a, b| a * b,
            })),
            "/" => Ok(Token::Operator(Operator {
                symbol: "/".into(),
                precedence: 1,
                associativity: Associativity::Left,
                evaluate: |a, b| a / b,
            })),
            "^" => Ok(Token::Operator(Operator {
                symbol: "^".into(),
                precedence: 2,
                associativity: Associativity::Right,
                evaluate: |a, b| f64::powf(a, b),
            })),
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
