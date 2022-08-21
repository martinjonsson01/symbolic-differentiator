use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;
use crate::Token;

/// A binary mathematical operator.
#[derive(Debug, Copy, Clone, Eq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponentiate,
}

/// An unary mathematical operator.
#[derive(Debug, Copy, Clone, Eq)]
pub enum UnaryOperator {
    PositiveSquareRoot,
}

impl UnaryOperator {
    pub fn token(&self) -> Token {
        match self {
            UnaryOperator::PositiveSquareRoot => Token::Sqrt,
        }
    }

    fn associativity(&self) -> Associativity {
        match self {
            UnaryOperator::PositiveSquareRoot => Associativity::Right,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            UnaryOperator::PositiveSquareRoot => 3
        }
    }
    
    pub fn evaluate(&self, x: i32) -> i32 {
        match self {
            UnaryOperator::PositiveSquareRoot => f32::sqrt(x as f32) as i32
        }
    }
}

impl BinaryOperator {
    pub fn token(&self) -> Token {
        match self {
            BinaryOperator::Add => Token::Plus,
            BinaryOperator::Subtract => Token::Dash,
            BinaryOperator::Multiply => Token::Asterisk,
            BinaryOperator::Divide => Token::ForwardSlash,
            BinaryOperator::Exponentiate => Token::Caret,
        }
    }

    pub(crate) fn associativity(&self) -> Associativity {
        match self {
            BinaryOperator::Add |
            BinaryOperator::Subtract |
            BinaryOperator::Multiply |
            BinaryOperator::Divide => Associativity::Left,
            BinaryOperator::Exponentiate => Associativity::Right,
        }
    }

    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Add |
            BinaryOperator::Subtract => 0,
            BinaryOperator::Multiply |
            BinaryOperator::Divide => 1,
            BinaryOperator::Exponentiate => 2,
        }
    }
    
    pub fn evaluate(&self, a: i32, b: i32) -> i32 {
        match self {
            BinaryOperator::Add => a+b,
            BinaryOperator::Subtract =>a-b,
            BinaryOperator::Multiply =>a*b,
            BinaryOperator::Divide =>a/b,
            BinaryOperator::Exponentiate => a.pow(b as u32),
        }
    }

    pub fn identity_operand(&self) -> i32 {
        match self {
            BinaryOperator::Add |
            BinaryOperator::Subtract => 0,
            BinaryOperator::Multiply |
            BinaryOperator::Divide => 1,
            BinaryOperator::Exponentiate => 1,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

impl PartialEq for BinaryOperator {
    fn eq(&self, other: &Self) -> bool {
        self.precedence() == other.precedence()
    }
}

impl Ord for BinaryOperator {
    fn cmp(&self, other: &Self) -> Ordering {
        self.precedence().cmp(&other.precedence())
    }
}

impl PartialOrd for BinaryOperator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token())
    }
}

impl PartialEq for UnaryOperator {
    fn eq(&self, other: &Self) -> bool {
        self.precedence() == other.precedence()
    }
}

impl Ord for UnaryOperator {
    fn cmp(&self, other: &Self) -> Ordering {
        self.precedence().cmp(&other.precedence())
    }
}

impl PartialOrd for UnaryOperator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operator_equality_correspond_with_precedence() {
        let greater = BinaryOperator::Multiply;
        let lesser = BinaryOperator::Divide;
        assert_eq!(greater, lesser)
    }

    #[test]
    fn operator_compares_correspond_with_precedence() {
        let greater = BinaryOperator::Multiply;
        let lesser = BinaryOperator::Add;
        assert!(greater > lesser)
    }

    #[test]
    fn operator_non_equality_correspond_with_precedence() {
        let greater = BinaryOperator::Multiply;
        let lesser = BinaryOperator::Add;
        assert_ne!(greater, lesser)
    }
}
