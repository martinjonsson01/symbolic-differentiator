use crate::interpreter::parser::expression_tree::{ExpressionTree, TokenKey, Valid};
use crate::interpreter::Token;
use anyhow::{bail, Context, Result};
use std::borrow::Borrow;
use std::cell::RefCell;

/// Differentiates the given expression tree with respect to the given variable.
///
/// # Arguments
///
/// * `tree`: The expression to differentiate, represented as an expression tree.
/// * `with_respect_to`: The token to differentiate with respect to. Usually an identifier.
///
/// returns: The derivative of the input expression tree represented as an expression tree.
///
/// # Examples
///
/// ```
/// let variable = Token::Identifier("x".to_string());
/// let tokens = [
/// variable,
/// Token::Identifier("2".to_string()),
/// "^".parse().unwrap(),
/// ]
/// .to_vec();
/// let tree = ExpressionTree::new(tokens).unwrap();
///
/// let derived_tree = find_derivative(tree, variable).unwrap();
/// ```
pub fn find_derivative(
    mut tree: ExpressionTree<Valid>,
    with_respect_to: &Token,
) -> Result<ExpressionTree<Valid>> {
    let root_key = tree.root_key();
    let new_root = differentiate_subtree(&mut tree, root_key, with_respect_to)?;
    let derivative = tree.set_root(new_root);
    Ok(derivative)
}

fn differentiate_subtree(
    tree: &mut ExpressionTree<Valid>,
    node: TokenKey,
    with_respect_to: &Token,
) -> Result<TokenKey> {
    let mut root = tree.root_key();
    match tree.token_of(node) {
        Ok(Token::Operator(operator)) => {
            if operator.symbol == "^" {
                let left_operand = tree
                    .left_child_of(node)
                    .context("Expected a left operand")?;
                let right_operand = tree
                    .right_child_of(node)
                    .context("Expected a right operand")?;

                let left_token = tree
                    .token_of(left_operand)?;
                if left_token != with_respect_to {
                    bail!("Can not differentiate with respect to this variable")
                }

                let maybe_parent = tree.get_parent_of(node);

                let cloned_right_operand = tree.clone_node_of(right_operand)?;
                let multiply_node =
                    tree.add_node_children("*".parse().unwrap(), cloned_right_operand, node)?;

                match maybe_parent {
                    Ok(parent) => tree.set_parent(multiply_node, parent)?,
                    Err(_) => {
                        // This means that we're at the root node.
                        root = multiply_node;
                    }
                }

                let right_operand_token = tree.mut_token_of(right_operand)?;
                if let Token::Literal(exponent) = right_operand_token {
                    *exponent -= 1f64
                }
            }
        }
        _ => todo![],
    }
    Ok(root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::Token;

    #[test]
    fn simple_single_term_is_differentiated_correctly() {
        // x^2 (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = [
            variable.clone(),
            Token::Literal(2f64),
            "^".parse().unwrap(),
        ]
        .to_vec();
        let tree = ExpressionTree::<Valid>::new(tokens).unwrap();
        tree.print().unwrap();

        let actual_tree = find_derivative(tree, &variable).unwrap();
        actual_tree.print().unwrap();

        // 2 * x (but in postfix notation)
        let expected_tokens = [
            Token::Literal(2f64),
            Token::Identifier("x".to_string()),
            Token::Literal(1f64),
            "^".parse().unwrap(),
            "*".parse().unwrap(),
        ]
        .to_vec();
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();
        expected_tree.print().unwrap();

        assert_eq!(actual_tree, expected_tree);
    }
}
