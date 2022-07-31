use crate::interpreter::operator::{Associativity, Operator};
use anyhow::{anyhow, bail, Context, Result};
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

fn infix_to_postfix(original_tokens: Vec<Token>) -> Result<Vec<Token>> {
    let mut tokens: VecDeque<Token> = VecDeque::from(original_tokens);
    let mut operators: VecDeque<Token> = VecDeque::new();
    let mut output: Vec<Token> = vec![];
    while let Some(token) = tokens.pop_front() {
        match token {
            Token::Literal(_) | Token::Identifier(_) => output.push(token),
            Token::OpenParenthesis => operators.push_front(token),
            Token::Plus | Token::Minus | Token::Star | Token::ForwardSlash | Token::Caret => {
                parse_operator_token(&mut operators, &mut output, &token)?
            }
            Token::CloseParenthesis => {
                parse_closing_parenthesis_token(&mut operators, &mut output)?
            }
        };
    }

    transfer_leftover_operators(&mut operators, &mut output)?;

    Ok(output)
}

fn transfer_leftover_operators(
    operators: &mut VecDeque<Token>,
    output: &mut Vec<Token>,
) -> Result<()> {
    while let Some(operator) = operators.pop_front() {
        match operator {
            Token::OpenParenthesis | Token::CloseParenthesis => {
                bail!("Mismatched parenthesis");
            }
            operator => output.push(operator),
        }
    }
    Ok(())
}

fn parse_closing_parenthesis_token(
    operators: &mut VecDeque<Token>,
    output: &mut Vec<Token>,
) -> Result<()> {
    loop {
        match operators.front() {
            None => {
                bail!("Mismatched parenthesis");
            }
            Some(top_of_operator_stack) => {
                if Token::OpenParenthesis.eq(top_of_operator_stack) {
                    break;
                }
                let operator = operators
                    .pop_front()
                    .with_context(|| "No operators left.")?;
                output.push(operator);
            }
        }
    }
    match operators.pop_front() {
        None => {
            bail!("Mismatched parenthesis");
        }
        Some(top_of_operator_stack) => {
            if top_of_operator_stack != Token::OpenParenthesis {
                bail!("Mismatched parenthesis");
            }
            // Discard the open parenthesis.
        }
    }
    Ok(())
}

fn parse_operator_token(
    operators: &mut VecDeque<Token>,
    output: &mut Vec<Token>,
    token: &Token,
) -> Result<()> {
    loop {
        match operators.front() {
            None => {
                break;
            }
            Some(top_of_operator_stack) => {
                let other_token = top_of_operator_stack.clone();

                if other_token == Token::OpenParenthesis {
                    break;
                }

                let operator = token_to_operator(&token)?;
                let other_operator = token_to_operator(top_of_operator_stack)?;
                if (other_operator <= operator)
                    && !(other_operator == operator
                        && operator.associativity == Associativity::Left)
                {
                    break;
                }

                let other_operator_token = operators
                    .pop_front()
                    .with_context(|| "No operators left.")?; // Pop other_operator
                output.push(other_operator_token.clone()); // Push other_operator
            }
        }
    }

    operators.push_front(token.clone()); // Push operator
    Ok(())
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
        Token::Caret => Ok(Operator {
            precedence: 2,
            associativity: Associativity::Right,
            evaluate: |a, b| f64::powf(a, b),
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

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_simple_parenthesised_expression() {
        let infix = [
            Token::Identifier("x".to_string()),
            Token::Minus,
            Token::OpenParenthesis,
            Token::Identifier("y".to_string()),
            Token::Plus,
            Token::Identifier("z".to_string()),
            Token::CloseParenthesis,
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Identifier("z".to_string()),
            Token::Plus,
            Token::Minus,
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_complex_expression() {
        let infix = [
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::Star,
            Token::Identifier("c".to_string()),
            Token::ForwardSlash,
            Token::OpenParenthesis,
            Token::Identifier("d".to_string()),
            Token::Minus,
            Token::Identifier("e".to_string()),
            Token::CloseParenthesis,
            Token::Caret,
            Token::Identifier("f".to_string()),
            Token::Caret,
            Token::Identifier("g".to_string()),
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Identifier("c".to_string()),
            Token::Star,
            Token::Identifier("d".to_string()),
            Token::Identifier("e".to_string()),
            Token::Minus,
            Token::Identifier("f".to_string()),
            Token::Identifier("g".to_string()),
            Token::Caret,
            Token::Caret,
            Token::ForwardSlash,
            Token::Plus,
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_multi_operator_expression() {
        let infix = [
            Token::Identifier("A".to_string()),
            Token::Plus,
            Token::Identifier("B".to_string()),
            Token::Star,
            Token::Identifier("C".to_string()),
            Token::Minus,
            Token::Identifier("D".to_string()),
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("A".to_string()),
            Token::Identifier("B".to_string()),
            Token::Identifier("C".to_string()),
            Token::Star,
            Token::Plus,
            Token::Identifier("D".to_string()),
            Token::Minus,
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_mismatched_parenthesis_should_return_err() {
        let infix = [
            Token::OpenParenthesis,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::CloseParenthesis,
            Token::CloseParenthesis,
        ]
        .to_vec();

        infix_to_postfix(infix).expect_err("Should return Err");
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
