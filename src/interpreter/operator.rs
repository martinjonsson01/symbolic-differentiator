use std::cmp::Ordering;
use std::fmt;
use std::fmt::Formatter;

/// A binary mathematical operator.
#[derive(Clone)]
pub struct Operator {
    pub symbol: String,
    pub precedence: i32,
    pub associativity: Associativity,
    pub evaluate: fn(f64, f64) -> f64,
}

#[derive(Clone, PartialEq)]
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

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.symbol)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn operator_compares_correspond_with_precedence() {
        let greater = Operator {
            symbol: "+".into(),
            precedence: 100,
            associativity: Associativity::Left,
            evaluate: |a, _| a,
        };
        let lesser = Operator {
            symbol: "+".into(),
            precedence: 1,
            associativity: Associativity::Left,
            evaluate: |a, _| a,
        };
        assert!(greater > lesser)
    }

    #[test]
    fn operator_equality_correspond_with_precedence() {
        let greater = Operator {
            symbol: "+".into(),
            precedence: 100,
            associativity: Associativity::Left,
            evaluate: |a, _| a,
        };
        let lesser = Operator {
            symbol: "+".into(),
            precedence: 1,
            associativity: Associativity::Left,
            evaluate: |a, _| a,
        };
        assert!(greater != lesser)
    }
}
