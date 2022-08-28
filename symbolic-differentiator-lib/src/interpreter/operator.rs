use crate::interpreter::token::Token;
use std::fmt;
use std::fmt::Formatter;

/// A binary mathematical operator.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Exponentiate,
}

/// An unary mathematical operator.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOperator {
    PositiveSquareRoot,
    NaturalLogarithm,
}

impl UnaryOperator {
    pub fn token(&self) -> Token {
        match self {
            UnaryOperator::PositiveSquareRoot => Token::Sqrt,
            UnaryOperator::NaturalLogarithm => Token::Ln,
        }
    }

    fn associativity(&self) -> Associativity {
        Associativity::Right
    }

    fn precedence(&self) -> u8 {
        3
    }

    pub(crate) fn precedence_eq(&self, other: &Self) -> bool {
        self.precedence().eq(&other.precedence())
    }

    pub(crate) fn precedence_ne(&self, other: &Self) -> bool {
        self.precedence().ne(&other.precedence())
    }

    pub(crate) fn precedence_gt(&self, other: &Self) -> bool {
        self.precedence().gt(&other.precedence())
    }

    pub(crate) fn precedence_ge(&self, other: &Self) -> bool {
        self.precedence().ge(&other.precedence())
    }

    pub(crate) fn precedence_lt(&self, other: &Self) -> bool {
        self.precedence().lt(&other.precedence())
    }

    pub(crate) fn precedence_le(&self, other: &Self) -> bool {
        self.precedence().le(&other.precedence())
    }

    pub fn evaluate(&self, x: i32) -> i32 {
        match self {
            UnaryOperator::PositiveSquareRoot => f32::sqrt(x as f32) as i32,
            UnaryOperator::NaturalLogarithm => f32::ln(x as f32) as i32,
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
            BinaryOperator::Add
            | BinaryOperator::Subtract
            | BinaryOperator::Multiply
            | BinaryOperator::Divide => Associativity::Left,
            BinaryOperator::Exponentiate => Associativity::Right,
        }
    }

    pub(crate) fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Add | BinaryOperator::Subtract => 0,
            BinaryOperator::Multiply | BinaryOperator::Divide => 1,
            BinaryOperator::Exponentiate => 2,
        }
    }

    pub(crate) fn precedence_eq(&self, other: &Self) -> bool {
        self.precedence().eq(&other.precedence())
    }

    pub(crate) fn precedence_ne(&self, other: &Self) -> bool {
        self.precedence().ne(&other.precedence())
    }

    pub(crate) fn precedence_gt(&self, other: &Self) -> bool {
        self.precedence().gt(&other.precedence())
    }

    pub(crate) fn precedence_ge(&self, other: &Self) -> bool {
        self.precedence().ge(&other.precedence())
    }

    pub(crate) fn precedence_lt(&self, other: &Self) -> bool {
        self.precedence().lt(&other.precedence())
    }

    pub(crate) fn precedence_le(&self, other: &Self) -> bool {
        self.precedence().le(&other.precedence())
    }

    pub fn evaluate(&self, a: i32, b: i32) -> i32 {
        match self {
            BinaryOperator::Add => a + b,
            BinaryOperator::Subtract => a - b,
            BinaryOperator::Multiply => a * b,
            BinaryOperator::Divide => a / b,
            BinaryOperator::Exponentiate => a.pow(b as u32),
        }
    }

    pub fn identity_operand(&self) -> i32 {
        match self {
            BinaryOperator::Add | BinaryOperator::Subtract => 0,
            BinaryOperator::Multiply | BinaryOperator::Divide => 1,
            BinaryOperator::Exponentiate => 1,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token())
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
        let equal1 = BinaryOperator::Multiply;
        let equal2 = BinaryOperator::Divide;
        assert!(equal1.precedence_eq(&equal2))
    }

    #[test]
    fn operator_non_equality_correspond_with_precedence() {
        let greater = BinaryOperator::Multiply;
        let lesser = BinaryOperator::Add;
        assert!(greater.precedence_ne(&lesser))
    }

    #[test]
    fn operator_gt_correspond_with_precedence() {
        let greater = BinaryOperator::Multiply;
        let lesser = BinaryOperator::Add;
        assert!(greater.precedence_gt(&lesser))
    }

    #[test]
    fn operator_ge_correspond_with_precedence() {
        let equal1 = BinaryOperator::Multiply;
        let equal2 = BinaryOperator::Divide;
        assert!(equal1.precedence_ge(&equal2))
    }

    #[test]
    fn operator_lt_correspond_with_precedence() {
        let greater = BinaryOperator::Multiply;
        let lesser = BinaryOperator::Add;
        assert!(lesser.precedence_lt(&greater))
    }

    #[test]
    fn operator_le_correspond_with_precedence() {
        let equal1 = BinaryOperator::Multiply;
        let equal2 = BinaryOperator::Divide;
        assert!(equal1.precedence_le(&equal2))
    }
}
