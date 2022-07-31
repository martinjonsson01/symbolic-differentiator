use crate::interpreter::operator::{Associativity, Operator};
use anyhow::{anyhow, Context, Result};
use std::collections::VecDeque;
use std::rc::Rc;

use super::token::Token;

/// A part of an expression tree
pub struct TokenNode {
    left: Rc<TokenNode>,
    right: Rc<TokenNode>,
    value: Option<Token>,
}

/*pub fn create_expression_tree(tokens: Vec<Token>) -> Result<TokenNode> {
    // Implemented using recursive descent.
    Ok(1)
}*/

fn convert_to_postfix(original_tokens: Vec<Token>) -> Result<Vec<Token>> {
    let mut tokens: VecDeque<Token> = VecDeque::from(original_tokens);
    let mut operators: Vec<Token> = vec![];
    let mut output: Vec<Token> = vec![];
    while let Some(token) = tokens.pop_front() {
        match token {
            Token::Literal(_) | Token::Identifier(_) => output.push(token),
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::ForwardSlash
            | Token::OpenParenthesis => {
                match operators.last() {
                    None => {}
                    Some(other_token) => {
                        if *other_token == Token::OpenParenthesis {
                            break;
                        }
                        let operator = token_to_operator(&token)?;
                        let other_operator = token_to_operator(other_token)?;
                        if (other_operator > operator)
                            || (other_operator == operator
                                && operator.associativity == Associativity::Left)
                        {
                            let other_operator_token =
                                operators.pop().with_context(|| "No operators left.")?; // Pop other_operator
                            operators.push(other_operator_token.clone()); // Push other_operator
                        }
                    }
                };
                operators.push(token.clone()); // Push operator
            }
            Token::CloseParenthesis => loop {
                match operators.pop() {
                    None => {
                        /* Throw mismatched parenthesis exception */
                        todo!();
                    }
                    Some(operator_token) => {
                        output.push(operator_token.clone());

                        if operator_token != Token::CloseParenthesis {
                            continue;
                        }

                        // Found the matching parenthesis, safe to break.
                        break;
                    }
                }
            },
        };
    }

    while let Some(operator) = operators.pop() {
        match operator {
            Token::OpenParenthesis | Token::CloseParenthesis => {
                /* Throw mismatched parenthesis exception */
                todo!();
            }
            operator => output.push(operator),
        }
    }

    Ok(output)
}

fn token_to_operator(token: &Token) -> Result<Operator> {
    match token {
        Token::Plus => Ok(Operator {
            precedence: 0,
            associativity: Associativity::Left,
            evaluate: |a, b| a + b,
        }),
        Token::Minus => Ok(Operator {
            precedence: 0,
            associativity: Associativity::Left,
            evaluate: |a, b| a - b,
        }),
        Token::Star => Ok(Operator {
            precedence: 1,
            associativity: Associativity::Left,
            evaluate: |a, b| a * b,
        }),
        Token::ForwardSlash => Ok(Operator {
            precedence: 1,
            associativity: Associativity::Left,
            evaluate: |a, b| a / b,
        }),
        token => Err(anyhow!("Token {} is not an operator", token)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn token_is_converted_to_correct_operator() {
        let operator = token_to_operator(&Token::Plus).unwrap();
        assert_eq!((operator.evaluate)(10f64, 12f64), 10f64 + 12f64)
    }

    #[test]
    fn infix_to_postfix_simple_expression() {
        let infix = [
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Plus,
        ]
        .to_vec();

        let actual = convert_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
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
