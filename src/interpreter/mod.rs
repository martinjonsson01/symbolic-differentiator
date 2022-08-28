pub mod differentiator;
pub mod lexer;
mod operator;
mod parser;
mod simplifier;
mod syntax;
pub mod token;

use crate::find_derivative;
use crate::interpreter::simplifier::simplify;
use crate::interpreter::token::Token;
use anyhow::{Context, Result};
use string_builder::Builder;
use syntax::expression_tree::Node;

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
    let variable = Node::new_identifier(with_respect_to);
    let simplified_expression = simplify(expression_tree)?;
    print!("{}", simplified_expression);
    let derivative = find_derivative(simplified_expression, &variable)?;
    print!("{}", derivative);
    let simplified_derivative = simplify(derivative)?;
    print!("{}", simplified_derivative);
    let derivative_tokens = simplified_derivative.to_infix()?;
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
fn convert(expression: String) -> Result<Node> {
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
            Token::LiteralInteger(value) => builder.append(format!("{:.0}", value)),
            Token::Caret => builder.append(token.to_string()),
            Token::Plus | Token::Dash | Token::Asterisk | Token::ForwardSlash => {
                builder.append(" ");
                builder.append(token.to_string());
                builder.append(" ");
            }
            _ => builder.append(token.to_string()),
        }
    }

    builder.string().context("Failed to build token string")
}

fn find_matching_node<'a, F>(nodes: impl Iterator<Item = &'a Node>, predicate: F) -> Option<&'a Node>
where
    F: Fn(&Node) -> bool,
{
    for node in nodes {
        if predicate(node) {
            return Some(node);
        }
    }
    None
}

#[cfg(test)]
mod interpreter_tests {
    use super::*;
    use parameterized_macro::parameterized;
    extern crate test;
    use test::Bencher;

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
    "y",
    "x^y",
    "3 * x^4",
    "x^3 + 3 * x^12",
    },
    expected_derivative = {
    "3 * x^2",
    "1",
    "y",
    "y * x^(y - 1)",
    "12 * x^3",
    "3 * x^2 + 36 * x^11",
    }
    )]
    fn differentiate_expression_returns_correct_derivative(
        expression: &str,
        expected_derivative: &str,
    ) {
        let actual_derivative = differentiate(expression.to_string(), "x".to_string()).unwrap();
        assert_eq!(actual_derivative, expected_derivative);
    }

    #[test]
    fn test() {
        let actual_derivative =
            differentiate("ln((1 + x)^3)".to_string(), "x".to_string()).unwrap();
        assert_eq!(actual_derivative, "3 * (1 + x)^-1".to_string());
    }

    #[parameterized(
    expression = {
    "x",
    "3",
    "y",
    "x + 7",
    "x * 5",
    "5 * x",
    "x^3 + 2 * x^2 - 4 * x + 3",
    "sqrt(2 + x^2)",
    "ln((1 + x)^3)",
    },
    expected_derivative = {
    "1",
    "0",
    "y",
    "1",
    "5",
    "5",
    "3 * x^2 + 4 * x - 4",
    "x * (2 + x^2)^(-1 / 2)",
    "3 * (1 + x)^-1",
    }
    )]
    fn cs381k_differentiate_expression_returns_correct_derivative(
        expression: &str,
        expected_derivative: &str,
    ) {
        let actual_derivative = differentiate(expression.to_string(), "x".to_string()).unwrap();
        assert_eq!(actual_derivative, expected_derivative);
    }

    #[bench]
    fn bench_differentiate_simple_expression(bencher: &mut Bencher) {
        bencher.iter(|| differentiate("x^2".to_string(), "x".to_string()));
    }

    #[bench]
    fn bench_differentiate_complex_expression(bencher: &mut Bencher) {
        bencher.iter(|| differentiate("(x^2+z)^(y*z)+(a+b+c^x)-(8*x^2)".to_string(), "x".to_string()));
    }
}
