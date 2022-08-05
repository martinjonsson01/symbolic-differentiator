pub mod expression_tree;
mod infix_converter;

use crate::interpreter::parser::expression_tree::{ExpressionTree, Valid};
use crate::interpreter::parser::infix_converter::infix_to_postfix;
use crate::interpreter::token;
use crate::interpreter::token::Token;
use anyhow::Result;

/// Parses the given input string into an equivalent expression tree,
/// which is easier to manipulate than the original string.
///
/// # Arguments
///
/// * `infix_tokens`: The tokens to parse, in infix format.
///
/// returns: The equivalent expression tree.
///
/// # Examples
///
/// ```
/// let tree = parse(infix_tokens)?;
/// let regenerated_tokens = tree.to_infix();
/// ```
pub fn parse(infix_tokens: Vec<Token>) -> Result<ExpressionTree<Valid>> {
    let postfix_tokens = infix_to_postfix(infix_tokens)?;
    let tree = ExpressionTree::<Valid>::new(postfix_tokens)?;
    Ok(tree)
}
