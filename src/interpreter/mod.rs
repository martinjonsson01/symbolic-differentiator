pub mod differentiator;
pub mod lexer;
mod operator;
mod parser;
pub mod token;

use crate::find_derivative;
use crate::interpreter::parser::expression_tree::{ExpressionTree, Valid};
use crate::interpreter::token::Token;
use anyhow::{bail, Context, Result};
use string_builder::Builder;

/// Calculates the derivative of the given expression with respect to the given variable.
///
/// # Arguments
///
/// * `expression`: A text expression in infix format.
/// * `with_respect_to`: Name of a variable present in the expression.
///
/// returns: The derivative of the expression, in text.
///
/// # Examples
///
/// ```
/// let expression = "x^2";
/// let derivative = differentiate(expression.to_string(), "x".to_string());
/// match derivative {
///     Ok(result) => print!("{}", derivative),
///     Err(_) => {}
/// }
/// ```
pub fn differentiate(expression: String, with_respect_to: String) -> Result<String> {
    let expression_tree = convert(expression)?;
    let variable_token: Token = match with_respect_to.parse::<Token>() {
        Ok(token) => token,
        Err(_) => bail!(
            "Failed to convert {} into a variable token",
            with_respect_to
        ),
    };
    let derivative_tree = find_derivative(expression_tree, &variable_token)?;
    let derivative_tokens = derivative_tree.to_infix()?;
    tokens_to_string(derivative_tokens)
}

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
fn convert(expression: String) -> Result<ExpressionTree<Valid>> {
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
    use parameterized_macro::parameterized;

    #[test]
    fn simple_expression_regenerates_to_itself() {
        let expression = "a + b";

        let tree = convert(expression.into()).unwrap();
        let regenerated_tokens = tree.to_infix().unwrap();
        let regenerated_expression = tokens_to_string(regenerated_tokens).unwrap();

        assert_eq!(regenerated_expression, expression)
    }

    #[test]
    fn complex_expression_regenerates_to_itself() {
        let expression = "a + b * (c - d) / e^2";

        let tree = convert(expression.into()).unwrap();
        let regenerated_tokens = tree.to_infix().unwrap();
        let regenerated_expression = tokens_to_string(regenerated_tokens).unwrap();

        assert_eq!(regenerated_expression, expression)
    }

    #[test]
    fn redundant_parentheses_are_stripped_when_regenerating() {
        let redundant_expression = "a + ((b) * ((c - d)) / (e^2))";
        let expected_expression = "a + b * (c - d) / e^2";

        let tree = convert(redundant_expression.into()).unwrap();
        let regenerated_tokens = tree.to_infix().unwrap();
        let regenerated_expression = tokens_to_string(regenerated_tokens).unwrap();

        assert_eq!(regenerated_expression, expected_expression)
    }

    #[parameterized(
        expression = {
            "x^3",
            "x^1",
            "3 * x^4",
        },
        expected_derivative = {
            "3 * x^2",
            "1 * x^0", // will be fixed to just '1' later
            "3 * 4 * x^3",
        }
    )]
    fn differentiate_expression_returns_correct_derivative(
        expression: &str,
        expected_derivative: &str,
    ) {
        let actual_derivative = differentiate(expression.to_string(), "x".to_string()).unwrap();
        assert_eq!(actual_derivative, expected_derivative);
    }
}
