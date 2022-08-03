pub mod lexer;
mod operator;
mod parser;
mod token;

use crate::interpreter::parser::expression_tree::ExpressionTree;
use crate::interpreter::token::Token;
use anyhow::{Context, Result};
use string_builder::Builder;

/// Converts the given input string into an equivalent expression tree,
/// which is easier to manipulate than the original string.
///
/// # Arguments
///
/// * `expression`: The text-representation of the infix expression.
///
/// returns: The equivalent expression tree.
///
/// # Examples
///
/// ```
/// let tree = convert(expression.into())?;
/// let regenerated_tokens = tree.to_infix();
/// ```
pub fn convert(expression: String) -> Result<ExpressionTree> {
    let tokens = lexer::tokenize(expression)?;
    let expression_tree = parser::parse(tokens)?;
    Ok(expression_tree)
}

/// Pretty-prints the given vector of tokens with added whitespace.
///
/// # Arguments
///
/// * `tokens`: The tokens to print.
///
/// returns: A pretty-printed text-version of the given tokens.
///
/// # Examples
///
/// ```
/// let pretty_printed_tokens = tokens_to_string(tokens);
/// print!("{}", pretty_printed_tokens);
/// ```
pub fn tokens_to_string(tokens: Vec<Token>) -> Result<String> {
    let mut builder = Builder::new(tokens.len());

    for token in tokens {
        match token {
            Token::Literal(value) => builder.append(format!("{:.0}", value)),
            Token::Operator(operator) => {
                if operator.symbol == "^".to_string() {
                    builder.append(operator.to_string());
                } else {
                    builder.append(" ");
                    builder.append(operator.to_string());
                    builder.append(" ");
                }
            }
            _ => builder.append(token.to_string()),
        }
    }

    builder.string().context("Failed to build token string")
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;

    #[test]
    fn simple_expression_regenerates_to_itself() {
        let expression = "a + b";

        let tree = convert(expression.into()).unwrap();
        let regenerated_tokens = tree.to_infix();
        let regenerated_expression = tokens_to_string(regenerated_tokens).unwrap();

        assert_eq!(regenerated_expression, expression)
    }

    #[test]
    fn complex_expression_regenerates_to_itself() {
        let expression = "a + b * (c - d) / e^2";

        let tree = convert(expression.into()).unwrap();
        let regenerated_tokens = tree.to_infix();
        let regenerated_expression = tokens_to_string(regenerated_tokens).unwrap();

        assert_eq!(regenerated_expression, expression)
    }
}
