use crate::interpreter::operator::{Associativity, Operator};
use anyhow::{bail, Context, Result};
use std::collections::VecDeque;

use super::token::Token;

fn infix_to_postfix(original_tokens: Vec<Token>) -> Result<Vec<Token>> {
    let mut tokens: VecDeque<Token> = VecDeque::from(original_tokens);
    let mut operators: VecDeque<Token> = VecDeque::new();
    let mut output: Vec<Token> = vec![];
    while let Some(token) = tokens.pop_front() {
        match token {
            Token::Literal(_) | Token::Identifier(_) => output.push(token),
            Token::OpenParenthesis => operators.push_front(token),
            Token::Operator(ref operator) => {
                parse_operator_token(&mut operators, &mut output, &token, &operator)?
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
    operator: &Operator,
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

                let other_operator = match top_of_operator_stack {
                    Token::Operator(operator) => operator,
                    _ => {
                        bail!("Found non-operator in operator stack")
                    }
                };
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn infix_to_postfix_simple_expression() {
        // x + y
        let infix = [
            Token::Identifier("x".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("y".to_string()),
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            "+".parse().unwrap(),
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_simple_parenthesised_expression() {
        // x - (y + z)
        let infix = [
            Token::Identifier("x".to_string()),
            "-".parse().unwrap(),
            Token::OpenParenthesis,
            Token::Identifier("y".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("z".to_string()),
            Token::CloseParenthesis,
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Identifier("z".to_string()),
            "+".parse().unwrap(),
            "-".parse().unwrap(),
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_complex_expression() {
        // a + b * c / (d - e)^f^g
        let infix = [
            Token::Identifier("a".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("b".to_string()),
            "*".parse().unwrap(),
            Token::Identifier("c".to_string()),
            "/".parse().unwrap(),
            Token::OpenParenthesis,
            Token::Identifier("d".to_string()),
            "-".parse().unwrap(),
            Token::Identifier("e".to_string()),
            Token::CloseParenthesis,
            "^".parse().unwrap(),
            Token::Identifier("f".to_string()),
            "^".parse().unwrap(),
            Token::Identifier("g".to_string()),
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Identifier("c".to_string()),
            "*".parse().unwrap(),
            Token::Identifier("d".to_string()),
            Token::Identifier("e".to_string()),
            "-".parse().unwrap(),
            Token::Identifier("f".to_string()),
            Token::Identifier("g".to_string()),
            "^".parse().unwrap(),
            "^".parse().unwrap(),
            "/".parse().unwrap(),
            "+".parse().unwrap(),
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_multi_operator_expression() {
        // A + B * C - D
        let infix = [
            Token::Identifier("A".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("B".to_string()),
            "*".parse().unwrap(),
            Token::Identifier("C".to_string()),
            "-".parse().unwrap(),
            Token::Identifier("D".to_string()),
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("A".to_string()),
            Token::Identifier("B".to_string()),
            Token::Identifier("C".to_string()),
            "*".parse().unwrap(),
            "+".parse().unwrap(),
            Token::Identifier("D".to_string()),
            "-".parse().unwrap(),
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_nested_parenthesis_expression() {
        // a + ((b + c) * d)
        let infix = [
            Token::Identifier("a".to_string()),
            "+".parse().unwrap(),
            Token::OpenParenthesis,
            Token::OpenParenthesis,
            Token::Identifier("b".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("c".to_string()),
            Token::CloseParenthesis,
            "*".parse().unwrap(),
            Token::Identifier("d".to_string()),
            Token::CloseParenthesis,
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Identifier("c".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("d".to_string()),
            "*".parse().unwrap(),
            "+".parse().unwrap(),
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_mismatched_parenthesis_should_return_err() {
        // (x + y))
        let infix = [
            Token::OpenParenthesis,
            Token::Identifier("x".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("y".to_string()),
            Token::CloseParenthesis,
            Token::CloseParenthesis,
        ]
        .to_vec();

        infix_to_postfix(infix).expect_err("Should return Err");
    }
}
