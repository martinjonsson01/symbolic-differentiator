pub mod lexer;
mod operator;
pub mod parser;
mod token;

use crate::interpreter::parser::expression_tree::ExpressionTree;
use crate::interpreter::token::Token;
use anyhow::Result;

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