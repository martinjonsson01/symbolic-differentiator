use crate::interpreter::find_matching_node;
use crate::interpreter::operator::{BinaryOperator, UnaryOperator};
use crate::interpreter::parser::expression_tree::{
    CompositeData, ExpressionTree, Node, NodeKey, Valid,
};
use anyhow::{anyhow, bail, Result};
use std::mem::discriminant;

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
        Some(Node::LiteralInteger(_)) => Ok(tree.add_node(Node::LiteralInteger(0))),
        Some(Node::Identifier(variable_name)) => {
            if with_respect_to.is_identifier(variable_name) {
                let one_node = tree.add_node(Node::new_literal_integer(1));
                Ok(one_node)
            } else {
                Ok(node)
            }
        }
        Some(Node::BinaryOperation {
            operator,
            left_operand,
            right_operand,
        }) => {
            let left_key = *left_operand;
            let right_key = *right_operand;

            if *operator == BinaryOperator::Exponentiate {
                // f(g(x)), f(x) = x^y -> g'(x) * y * x^(y - 1) chain rule
                let base = tree.clone_node_of(left_key)?;
                let exponent = tree.clone_node_of(right_key)?;
                let differentiated_base = differentiate_subtree(tree, left_key, with_respect_to)?;

                let subtraction = create_subtract_one(tree, right_key)?;
                let new_exponentiation =
                    tree.add_node(Node::new_binary_exponentiation(base, subtraction));

                let multiply_node = Node::new_composite_fraction(
                    vec![differentiated_base, exponent, new_exponentiation],
                    vec![],
                );
                let multiply_key = tree.add_node(multiply_node);

                Ok(multiply_key)
            } else {
                Err(anyhow!("Could not differentiate expression"))
            }
        }
        Some(Node::Composite(CompositeData {
            operator: BinaryOperator::Multiply,
            left: multiply,
            right: divide,
            ..
        })) => {
            if multiply.len() == 2 && divide.is_empty() {
                let multiply = multiply.clone();

                let maybe_value = find_matching_node(tree, multiply.iter(), |node| {
                    node.is_value() && node != with_respect_to
                });
                let maybe_non_value = find_matching_node(tree, multiply.iter(), |node| {
                    !node.is_value() || node == with_respect_to
                });

                if maybe_value.is_some() {
                    if let Some(non_value) = maybe_non_value {
                        let new_root = differentiate_subtree(tree, non_value, with_respect_to)?;
                        tree.replace_child_of(node, non_value, new_root)?;
                        return Ok(node);
                    }
                    return Err(anyhow!("Could not differentiate expression"));
                }
            }
            Err(anyhow!(
                "Can not currently differentiate product with >2 factors"
            ))
        }
        Some(Node::Composite(data)) => {
            let data = data.clone();
            let CompositeData {
                left: adds,
                right: subtracts,
                ..
            } = data;
            let new_adds: Vec<NodeKey> = adds
                .into_iter()
                .filter_map(|key| differentiate_subtree(tree, key, with_respect_to).ok())
                .collect();
            let new_subtracts: Vec<NodeKey> = subtracts
                .into_iter()
                .filter_map(|key| differentiate_subtree(tree, key, with_respect_to).ok())
                .collect();

            let new_node = Node::Composite(CompositeData {
                left: new_adds,
                right: new_subtracts,
                ..data
            });
            Ok(tree.add_node(new_node))
        }
        Some(Node::UnaryOperation { operator, operand }) => {
            let operand = *operand;
            let operator_discriminant = discriminant(operator);
            if operator_discriminant == discriminant(&UnaryOperator::PositiveSquareRoot) {
                // Easier to convert sqrt(x) into x^(1/2) and differentiate that.
                let one = tree.add_node(Node::new_literal_integer(1));
                let two = tree.add_node(Node::new_literal_integer(2));
                let one_half = tree.add_node(Node::new_composite_fraction(vec![one], vec![two]));
                let power = tree.add_node(Node::new_binary_exponentiation(operand, one_half));
                let derivative = differentiate_subtree(tree, power, with_respect_to)?;
                Ok(derivative)
            } else if operator_discriminant == discriminant(&UnaryOperator::NaturalLogarithm) {
                // d/dx ln(f(x)) = f'(x) / f(x)
                let denominator = tree.clone_node_of(operand)?;
                let numerator = differentiate_subtree(tree, operand, with_respect_to)?;
                let derivative = tree.add_node(Node::new_composite_fraction(
                    vec![numerator],
                    vec![denominator],
                ));
                Ok(derivative)
            } else {
                unimplemented!(
                    "This unary operation has not yet been implemented: {}",
                    operator
                )
            }
        }
        None => bail!("The given node is not an operator token in the given expression tree"),
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
    use crate::interpreter::simplifier::simplify;
    use crate::interpreter::Token;

    #[test]
    fn simple_exponent_term_is_differentiated_correctly() {
        // x^2 (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = vec![
            variable.clone(),
            Token::LiteralInteger(2),
            BinaryOperator::Exponentiate.token(),
        ];
        let tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        let with_respect_to = Node::new_identifier(variable.to_string());
        let actual_tree = find_derivative(tree, &with_respect_to).unwrap();

        // 1 * 2 * x^(2 - 1) (but in postfix notation)
        let expected_tokens = vec![
            Token::LiteralInteger(1),
            Token::LiteralInteger(2),
            variable.clone(),
            Token::LiteralInteger(2),
            Token::LiteralInteger(1),
            BinaryOperator::Subtract.token(),
            BinaryOperator::Exponentiate.token(),
            Token::Asterisk,
            Token::Asterisk,
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(
            simplify(actual_tree).unwrap(),
            simplify(expected_tree).unwrap()
        );
    }

    #[test]
    fn complex_exponent_term_is_differentiated_correctly() {
        // 3 * x^y (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = vec![
            Token::LiteralInteger(3),
            variable.clone(),
            Token::Identifier("y".to_string()),
            BinaryOperator::Exponentiate.token(),
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
            BinaryOperator::Subtract.token(),
            BinaryOperator::Exponentiate.token(),
            Token::Asterisk,
            Token::Asterisk,
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(
            simplify(actual_tree).unwrap(),
            simplify(expected_tree).unwrap()
        );
    }

    #[test]
    fn complex_exponent_term_reversed_is_differentiated_correctly() {
        // x^4 * 3 (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = vec![
            variable.clone(),
            Token::LiteralInteger(4),
            BinaryOperator::Exponentiate.token(),
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
            BinaryOperator::Subtract.token(),
            BinaryOperator::Exponentiate.token(),
            Token::Asterisk,
            Token::LiteralInteger(3),
            Token::Asterisk,
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(
            simplify(actual_tree).unwrap(),
            simplify(expected_tree).unwrap()
        );
    }
}
