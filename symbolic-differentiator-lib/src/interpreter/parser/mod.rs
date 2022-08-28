mod infix_converter;

use crate::interpreter::parser::infix_converter::infix_to_postfix;
use crate::interpreter::syntax::expression_tree;
use crate::interpreter::syntax::expression_tree::Node;
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
/// # use anyhow::Result;
/// # fn main() -> Result<()> {
/// use symbolic_differentiator::interpreter::parser::parse;
/// use symbolic_differentiator::interpreter::token::Token;
///
/// let infix_tokens = vec![
///     Token::Identifier("x".to_string()),
///     Token::Caret,
///     Token::LiteralInteger(2),
/// ];
/// let tree = parse(infix_tokens)?;
/// let regenerated_tokens = tree.to_infix();
/// # Ok::<(), anyhow::Error>(()) }
/// ```
pub fn parse(infix_tokens: Vec<Token>) -> Result<Node> {
    let postfix_tokens = infix_to_postfix(infix_tokens)?;
    let tree = expression_tree::new_tree(postfix_tokens)?;
    Ok(tree)
}
