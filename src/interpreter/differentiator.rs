use crate::interpreter::find_matching_node;
use crate::interpreter::operator::Operator;
use crate::interpreter::parser::expression_tree::{
    CompositeData, ExpressionTree, Node, NodeKey, Valid,
};
use anyhow::{anyhow, bail, Context, Result};

/// Differentiates the given expression tree with respect to the given variable.
///
/// # Arguments
///
/// * `tree`: The expression to differentiate, represented as an expression tree.
/// * `with_respect_to`: The expression node to differentiate with respect to. Usually an identifier.
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
/// Operator::Exponentiate.token(),
/// ]
/// .to_vec();
/// let tree = ExpressionTree::new(tokens).unwrap();
///
/// let derived_tree = find_derivative(tree, variable).unwrap();
/// ```
pub fn find_derivative(
    mut tree: ExpressionTree<Valid>,
    with_respect_to: &Node,
) -> Result<ExpressionTree<Valid>> {
    let root_key = tree.root_key();
    let new_root = differentiate_subtree(&mut tree, root_key, with_respect_to)?;
    let derivative = tree.set_root(new_root);
    Ok(derivative)
}

fn differentiate_subtree(
    tree: &mut ExpressionTree<Valid>,
    node: NodeKey,
    with_respect_to: &Node,
) -> Result<NodeKey> {
    match tree.get_node(node) {
        Some(Node::BinaryOperation {
            operator,
            left_operand,
            right_operand,
        }) => {
            let left_child = tree
                .get_node(*left_operand)
                .context("Expected a left operand")?;

            let right_key = *right_operand;

            if *operator == Operator::Exponentiate {
                if left_child != with_respect_to {
                    bail!("Can not differentiate with respect to this variable")
                }

                let exponent = tree.clone_node_of(right_key)?;
                let multiply_node = Node::new_binary_multiplication(exponent, node);
                let multiply_key = tree.add_node(multiply_node);

                let subtraction = create_subtract_one(tree, right_key)?;
                tree.replace_child_of(node, right_key, subtraction)?;

                Ok(multiply_key)
            } else {
                Err(anyhow!("Could not differentiate expression"))
            }
        }
        Some(Node::Composite(CompositeData {
            operator: Operator::Multiply,
            left: multiply,
            right: divide,
            ..
        })) => {
            if multiply.len() == 2 && divide.is_empty() {
                let multiply = multiply.clone();

                find_matching_node(tree, multiply.iter(), |token| token.is_value())
                    .context("Expected a value child")?;
                let exponentiate_key = find_matching_node(tree, multiply.iter(), |token| {
                    token.is_specific_operator(Operator::Exponentiate)
                })
                .context("Expected an exponentiation child")?;

                let new_root = differentiate_subtree(tree, exponentiate_key, with_respect_to)?;
                tree.replace_child_of(node, exponentiate_key, new_root)?;
                Ok(node)
            } else {
                Err(anyhow!("Could not differentiate expression"))
            }
        }
        Some(Node::Identifier(variable_name)) => {
            if with_respect_to.is_identifier(variable_name) {
                let one_node = tree.add_node(Node::new_literal_integer(1));
                Ok(one_node)
            } else {
                Ok(node)
            }
        }
        _ => bail!("The given node is not an operator token in the given expression tree"),
    }
}

/// Returns node 'from' with a subtraction node: from - 1
fn create_subtract_one(tree: &mut ExpressionTree<Valid>, from: NodeKey) -> Result<NodeKey> {
    let one = tree.add_node(Node::LiteralInteger(1));
    let subtraction = Node::new_binary_subtraction(from, one);
    Ok(tree.add_node(subtraction))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::Token;

    #[test]
    fn simple_exponent_term_is_differentiated_correctly() {
        // x^2 (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = vec![
            variable.clone(),
            Token::LiteralInteger(2),
            Operator::Exponentiate.token(),
        ];
        let tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        let with_respect_to = Node::new_identifier(variable.to_string());
        let actual_tree = find_derivative(tree, &with_respect_to).unwrap();

        // 2 * x^(2 - 1) (but in postfix notation)
        let expected_tokens = vec![
            Token::LiteralInteger(2),
            variable.clone(),
            Token::LiteralInteger(2),
            Token::LiteralInteger(1),
            Operator::Subtract.token(),
            Operator::Exponentiate.token(),
            Token::Asterisk,
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn complex_exponent_term_is_differentiated_correctly() {
        // 3 * x^y (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = vec![
            Token::LiteralInteger(3),
            variable.clone(),
            Token::Identifier("y".to_string()),
            Operator::Exponentiate.token(),
            Token::Asterisk,
        ];
        let tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        let with_respect_to = Node::new_identifier(variable.to_string());
        let actual_tree = find_derivative(tree, &with_respect_to).unwrap();

        // 3 * (y * x^(y-1)) (but in postfix notation)
        let expected_tokens = vec![
            Token::LiteralInteger(3),
            Token::Identifier("y".to_string()),
            variable.clone(),
            Token::Identifier("y".to_string()),
            Token::LiteralInteger(1),
            Operator::Subtract.token(),
            Operator::Exponentiate.token(),
            Token::Asterisk,
            Token::Asterisk,
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn complex_exponent_term_reversed_is_differentiated_correctly() {
        // x^4 * 3 (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = vec![
            variable.clone(),
            Token::LiteralInteger(4),
            Operator::Exponentiate.token(),
            Token::LiteralInteger(3),
            Token::Asterisk,
        ];
        let tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        let with_respect_to = Node::new_identifier(variable.to_string());
        let actual_tree = find_derivative(tree, &with_respect_to).unwrap();

        // (4 * x^(4-1)) * 3 (but in postfix notation)
        let expected_tokens = vec![
            Token::LiteralInteger(4),
            variable.clone(),
            Token::LiteralInteger(4),
            Token::LiteralInteger(1),
            Operator::Subtract.token(),
            Operator::Exponentiate.token(),
            Token::Asterisk,
            Token::LiteralInteger(3),
            Token::Asterisk,
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }
}
