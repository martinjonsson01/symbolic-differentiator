use std::cmp::Ordering;

/// A binary mathematical operator.
pub struct Operator {
    pub precedence: i32,
    pub evaluate: fn(f64, f64) -> f64,
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

    #[test]
    fn operator_compares_correspond_with_precedence() {
        let greater = Operator { precedence: 100, evaluate: |a, _| a };
        let lesser = Operator { precedence: 1, evaluate: |a, _| a };
        assert!(greater > lesser)
    }

    #[test]
    fn operator_equality_correspond_with_precedence() {
        let greater = Operator { precedence: 100, evaluate: |a, _| a };
        let lesser = Operator { precedence: 1, evaluate: |a, _| a };
        assert!(greater != lesser)
    }
}
