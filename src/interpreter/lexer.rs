use anyhow::anyhow;
use anyhow::Result;

use super::token::Token;

//Err(anyhow!(
//            "Incorrect formatting of expression: {}",
//           expression
//        ))

/// Turns the input string into individual tokens containing values
pub fn tokenize(expression: String) -> Result<Vec<Token>> {
    let mut tokens: Vec<Token> = vec![];
    for char in expression.chars() {
        let token: Token = match char {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::ForwardSlash,
            name => Token::Identifier(name.to_string()),
        };
        tokens.push(token);
    }
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn simple_expression_returns_tokens() {
        let expected_tokens = [
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
        ];
        let actual_tokens = tokenize("x+y".to_string()).unwrap();
        assert_eq!(actual_tokens, expected_tokens);
    }

    #[test]
    fn long_expression_returns_tokens() {
        let expected_tokens = [
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::Minus,
            Token::Identifier("c".to_string()),
            Token::ForwardSlash,
            Token::Identifier("d".to_string()),
            Token::Star,
            Token::Identifier("e".to_string()),
            Token::Plus,
            Token::Identifier("f".to_string()),
        ];
        let actual_tokens = tokenize("a+b-c/d*e+f".to_string()).unwrap();
        assert_eq!(actual_tokens, expected_tokens);
    }
}
