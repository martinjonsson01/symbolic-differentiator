use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;
use crate::Token;

/// A mathematical operator.
#[derive(Debug, Copy, Clone)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponentiate,
}

impl Operator {
    pub fn token(&self) -> Token {
        match self {
            Operator::Add => Token::Plus,
            Operator::Subtract => Token::Dash,
            Operator::Multiply => Token::Asterisk,
            Operator::Divide => Token::ForwardSlash,
            Operator::Exponentiate => Token::Caret,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            Operator::Add |
            Operator::Subtract => 0,
            Operator::Multiply |
            Operator::Divide => 1,
            Operator::Exponentiate => 2,
        }
    }

    pub(crate) fn associativity(&self) -> Associativity {
        match self {
            Operator::Add |
            Operator::Subtract |
            Operator::Multiply |
            Operator::Divide => Associativity::Left,
            Operator::Exponentiate => Associativity::Right,
        }
    }

    pub fn evaluate(&self, a: i32, b: i32) -> i32 {
        match self {
            Operator::Add => a+b,
            Operator::Subtract =>a-b,
            Operator::Multiply =>a*b,
            Operator::Divide =>a/b,
            Operator::Exponentiate => a.pow(b as u32),
        }
    }
    
    pub fn identity_operand(&self) -> i32 {
        match self {
            Operator::Add |
            Operator::Subtract => 0,
            Operator::Multiply |
            Operator::Divide => 1,
            Operator::Exponentiate => 1,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        &self.precedence() == &other.precedence()
    }
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.precedence().partial_cmp(&other.precedence())
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operator_compares_correspond_with_precedence() {
        let greater = Operator::Multiply;
        let lesser = Operator::Add;
        assert!(greater > lesser)
    }

    #[test]
    fn operator_equality_correspond_with_precedence() {
        let greater = Operator::Multiply;
        let lesser = Operator::Add;
        assert_ne!(greater, lesser)
    }
}
