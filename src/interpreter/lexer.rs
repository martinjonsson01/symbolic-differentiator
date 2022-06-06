use anyhow::Result;
use std::str::FromStr;

use super::token::Token;
use super::token::symbols as symbols;

/// Turns the input string into individual tokens containing values
pub fn tokenize(expression: String) -> Result<Vec<Token>> {
    let mut tokens: Vec<Token> = vec![];

    let mut start = 0;
    let mut length = 0;
    while start < expression.len() {

        // Run until next symbol
        loop {
            let char = expression.chars().nth(start + length);
            let next_char = expression.chars().nth(start + length + 1);

            match next_char {
                None => {
                    // Next char is out of bounds, but we still need to include the current one
                    length += 1;
                    break;
                }
                Some(next_char) => {
                    let char = char.unwrap();
                    if symbols.contains(&char) {
                        length = 1;
                        break;
                    }

                    length += 1;
                    if symbols.contains(&next_char) {
                        break;
                    }
                }
            }
        }

        let text = &expression[start..start + length];
        let token: Token = Token::from_str(text).unwrap();
        tokens.push(token);

        start += length;
        length = 0;
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

    #[test]
    fn expression_with_literals_and_identifiers_returns_both_types_of_tokens() {
        let expected_tokens = [
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Literal(1.0),
        ];
        let actual_tokens = tokenize("x+1".to_string()).unwrap();
        assert_eq!(actual_tokens, expected_tokens);
    }

    #[test]
    fn simple_multi_character_expression_returns_multi_character_tokens() {
        let expected_tokens = [
            Token::Identifier("foo".to_string()),
            Token::Plus,
            Token::Literal(123.4),
        ];
        let actual_tokens = tokenize("foo+123.4".to_string()).unwrap();
        assert_eq!(actual_tokens, expected_tokens);
    }
}
