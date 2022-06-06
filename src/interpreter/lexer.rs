use anyhow::anyhow;
use anyhow::Result;

use super::token::Token;

//Err(anyhow!(
//            "Incorrect formatting of expression: {}",
//           expression
//        ))

pub fn tokenize(expression: String) -> Result<Vec<Token>> {
    Ok([Token::Plus].to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn correct_expression_returns_tokens() {
        let expected_tokens = [
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
        ];
        let actual_tokens = tokenize("x+y".to_string()).unwrap();
        assert_eq!(actual_tokens, expected_tokens);
    }
}
