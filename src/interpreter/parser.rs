use std::rc::Rc;
use ::phf::{Map, phf_map};

use super::token::Token;

/// A part of an expression tree
pub struct TokenNode {
    left: Rc<TokenNode>,
    right: Rc<TokenNode>,
    value: Option<Token>,
}

struct Operator {
    precedence: i32,
    evaluate: fn(f64, f64) -> f64,
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

fn token_to_operator(token: Token) -> Result<Operator> {
    match token {
        Token::Plus => Ok(Operator { precedence: 0, evaluate: |a, b| a + b }),
        Token::Minus => Ok(Operator { precedence: 0, evaluate: |a, b| a - b }),
        Token::Star => Ok(Operator { precedence: 1, evaluate: |a, b| a * b }),
        Token::ForwardSlash => Ok(Operator { precedence: 1, evaluate: |a, b| a / b }),
        token => Err(anyhow!("Token {} is not an operator",token))
    }
}

/*pub fn create_expression_tree(tokens: Vec<Token>) -> Result<TokenNode> {
    // Implemented as an operator precedence parser.
    Ok(1)
}*/


#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

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

    #[test]
    fn token_is_converted_to_correct_operator() {
        let operator = token_to_operator(Token::Plus).unwrap();
        assert_eq!((operator.evaluate)(10f64, 12f64), 10f64 + 12f64)
    }

    /*#[test]
    fn simple_expression_returns_tree() {
        let tokens = [
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
        ].to_vec();
        let tree_root = create_expression_tree(tokens).unwrap();
        assert_eq!(actual_tokens, expected_tokens);
    }*/
}
