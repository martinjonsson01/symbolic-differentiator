use crate::interpreter::token::Token;
use std::cmp::Ordering;

pub static OPERATORS: [Operator; 5] = [
    Operator {
        token: Token::Plus,
        precedence: 0,
        associativity: Associativity::Left,
        evaluate: |a, b| a + b,
    },
    Operator {
        token: Token::Minus,
        precedence: 0,
        associativity: Associativity::Left,
        evaluate: |a, b| a - b,
    },
    Operator {
        token: Token::Star,
        precedence: 1,
        associativity: Associativity::Left,
        evaluate: |a, b| a * b,
    },
    Operator {
        token: Token::ForwardSlash,
        precedence: 1,
        associativity: Associativity::Left,
        evaluate: |a, b| a / b,
    },
    Operator {
        token: Token::Caret,
        precedence: 2,
        associativity: Associativity::Right,
        evaluate: |a, b| f64::powf(a, b),
    },
];

/// A binary mathematical operator.
pub struct Operator {
    pub token: Token,
    pub precedence: i32,
    pub associativity: Associativity,
    pub evaluate: fn(f64, f64) -> f64,
}

#[derive(PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        &self.precedence == &other.precedence
    }
}

impl PartialOrd for Operator {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.precedence.partial_cmp(&other.precedence)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::token::Token;

    #[test]
    fn operator_compares_correspond_with_precedence() {
        let greater = Operator {
            token: Token::Plus,
            precedence: 100,
            associativity: Associativity::Left,
            evaluate: |a, _| a,
        };
        let lesser = Operator {
            token: Token::Plus,
            precedence: 1,
            associativity: Associativity::Left,
            evaluate: |a, _| a,
        };
        assert!(greater > lesser)
    }

    #[test]
    fn operator_equality_correspond_with_precedence() {
        let greater = Operator {
            token: Token::Plus,
            precedence: 100,
            associativity: Associativity::Left,
            evaluate: |a, _| a,
        };
        let lesser = Operator {
            token: Token::Plus,
            precedence: 1,
            associativity: Associativity::Left,
            evaluate: |a, _| a,
        };
        assert!(greater != lesser)
    }
}
