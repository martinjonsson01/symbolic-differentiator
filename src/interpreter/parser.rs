use std::rc::Rc;
use crate::interpreter::operator::Operator;

use super::token::Token;

/// A part of an expression tree
pub struct TokenNode {
    left: Rc<TokenNode>,
    right: Rc<TokenNode>,
    value: Option<Token>,
}


/*pub fn create_expression_tree(tokens: Vec<Token>) -> Result<TokenNode> {
    // Implemented as an operator precedence parser (Pratt Parsing).
    Ok(1)
}*/

fn token_to_operator(token: Token) -> Result<Operator> {
    match token {
        Token::Plus => Ok(Operator { precedence: 0, evaluate: |a, b| a + b }),
        Token::Minus => Ok(Operator { precedence: 0, evaluate: |a, b| a - b }),
        Token::Star => Ok(Operator { precedence: 1, evaluate: |a, b| a * b }),
        Token::ForwardSlash => Ok(Operator { precedence: 1, evaluate: |a, b| a / b }),
        token => Err(anyhow!("Token {} is not an operator",token))
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

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
