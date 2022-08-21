use crate::interpreter::operator::{Associativity, BinaryOperator};
use anyhow::{anyhow, bail, Context, Result};
use std::collections::VecDeque;

use super::token::Token;

pub(super) fn infix_to_postfix(original_tokens: Vec<Token>) -> Result<Vec<Token>> {
    let mut tokens: VecDeque<Token> = VecDeque::from(original_tokens);
    let mut operators: VecDeque<Token> = VecDeque::new();
    let mut output: Vec<Token> = vec![];
    while let Some(token) = tokens.pop_front() {
        match token {
            Token::LiteralInteger(_) | Token::Identifier(_) => output.push(token),
            Token::Sqrt => operators.push_front(token),
            Token::Plus | Token::Dash | Token::Asterisk | Token::ForwardSlash | Token::Caret => {
                let operator = token_to_operator(&token)?;
                parse_operator_token(&mut operators, &mut output, &token, operator)?
            }
            Token::LeftParentheses => operators.push_front(token),
            Token::RightParentheses => {
                parse_closing_parenthesis_token(&mut operators, &mut output)?
            }
        }
    }

    transfer_leftover_operators(&mut operators, &mut output)?;

    Ok(output)
}


fn token_to_operator(token: &Token) -> Result<BinaryOperator> {
    match token {
        Token::Plus => Ok(BinaryOperator::Add),
        Token::Dash => Ok(BinaryOperator::Subtract),
        Token::Asterisk => Ok(BinaryOperator::Multiply),
        Token::ForwardSlash => Ok(BinaryOperator::Divide),
        Token::Caret => Ok(BinaryOperator::Exponentiate),
        _ => Err(anyhow!("Operator not recognized")),
    }
}

fn transfer_leftover_operators(
    operators: &mut VecDeque<Token>,
    output: &mut Vec<Token>,
) -> Result<()> {
    while let Some(operator) = operators.pop_front() {
        match operator {
            Token::LeftParentheses | Token::RightParentheses => {
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
                if Token::LeftParentheses.eq(top_of_operator_stack) {
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
            if top_of_operator_stack != Token::LeftParentheses {
                bail!("Mismatched parenthesis");
            }
            // Discard the open parenthesis.
        }
    }
    match operators.front() {
        Some(Token::Sqrt) => {
            let function = operators
                .pop_front()
                .with_context(|| "No operators left.")?;
            output.push(function);
        }
        _ => {}
    }
    Ok(())
}

fn parse_operator_token(
    operators: &mut VecDeque<Token>,
    output: &mut Vec<Token>,
    token: &Token,
    operator: BinaryOperator,
) -> Result<()> {
    loop {
        match operators.front() {
            None => {
                break;
            }
            Some(top_of_operator_stack) => {
                let other_token = top_of_operator_stack.clone();

                if other_token == Token::LeftParentheses {
                    break;
                }

                let other_operator = match top_of_operator_stack {
                    Token::Plus
                    | Token::Dash
                    | Token::Asterisk
                    | Token::ForwardSlash
                    | Token::Caret => token_to_operator(top_of_operator_stack)?,
                    _ => {
                        bail!("Found non-operator in operator stack")
                    }
                };
                if (other_operator <= operator)
                    && !(other_operator == operator
                        && operator.associativity() == Associativity::Left)
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
        // x - (y + z)
        let infix = [
            Token::Identifier("x".to_string()),
            Token::Dash,
            Token::LeftParentheses,
            Token::Identifier("y".to_string()),
            Token::Plus,
            Token::Identifier("z".to_string()),
            Token::RightParentheses,
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Identifier("z".to_string()),
            Token::Plus,
            Token::Dash,
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_simple_function_expression() {
        // sqrt(x)
        let infix = vec![
            Token::Sqrt,
            Token::LeftParentheses,
            Token::Identifier("x".to_string()),
            Token::RightParentheses,
        ];
        let postfix = vec![
            Token::Identifier("x".to_string()),
            Token::Sqrt,
        ];

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_complex_expression() {
        // a + b * c / (d - e)^f^g
        let infix = [
            Token::Identifier("a".to_string()),
            Token::Plus,
            Token::Identifier("b".to_string()),
            Token::Asterisk,
            Token::Identifier("c".to_string()),
            Token::ForwardSlash,
            Token::LeftParentheses,
            Token::Identifier("d".to_string()),
            Token::Dash,
            Token::Identifier("e".to_string()),
            Token::RightParentheses,
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
            Token::Asterisk,
            Token::Identifier("d".to_string()),
            Token::Identifier("e".to_string()),
            Token::Dash,
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
        // A + B * C - D
        let infix = [
            Token::Identifier("A".to_string()),
            Token::Plus,
            Token::Identifier("B".to_string()),
            Token::Asterisk,
            Token::Identifier("C".to_string()),
            Token::Dash,
            Token::Identifier("D".to_string()),
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("A".to_string()),
            Token::Identifier("B".to_string()),
            Token::Identifier("C".to_string()),
            Token::Asterisk,
            Token::Plus,
            Token::Identifier("D".to_string()),
            Token::Dash,
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
            Token::Plus,
            Token::LeftParentheses,
            Token::LeftParentheses,
            Token::Identifier("b".to_string()),
            Token::Plus,
            Token::Identifier("c".to_string()),
            Token::RightParentheses,
            Token::Asterisk,
            Token::Identifier("d".to_string()),
            Token::RightParentheses,
        ]
        .to_vec();
        let postfix = [
            Token::Identifier("a".to_string()),
            Token::Identifier("b".to_string()),
            Token::Identifier("c".to_string()),
            Token::Plus,
            Token::Identifier("d".to_string()),
            Token::Asterisk,
            Token::Plus,
        ]
        .to_vec();

        let actual = infix_to_postfix(infix).unwrap();

        assert_eq!(actual, postfix)
    }

    #[test]
    fn infix_to_postfix_mismatched_parenthesis_should_return_err() {
        // (x + y))
        let infix = [
            Token::LeftParentheses,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::RightParentheses,
            Token::RightParentheses,
        ]
        .to_vec();

        infix_to_postfix(infix).expect_err("Should return Err");
    }
}
