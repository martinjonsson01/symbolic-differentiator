use crate::interpreter::find_matching_node;
use crate::interpreter::operator::{BinaryOperator, UnaryOperator};
use crate::interpreter::syntax::composite::CompositeData;
use crate::interpreter::syntax::expression_tree::Node;
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
pub fn find_derivative(mut node: Node, with_respect_to: &Node) -> Result<Node> {
    match node {
        Node::LiteralInteger(_) => Ok(Node::LiteralInteger(0)),
        Node::Identifier(ref variable_name) => {
            if with_respect_to.is_identifier(variable_name) {
                let one_node = Node::new_literal_integer(1);
                Ok(one_node)
            } else {
                Ok(node)
            }
        }
        Node::BinaryOperation {
            operator,
            left_operand,
            right_operand,
        } => {
            if operator == BinaryOperator::Exponentiate {
                // f(g(x)), f(x) = x^y -> g'(x) * y * x^(y - 1) chain rule
                let base = *left_operand.clone();
                let exponent = *right_operand.clone();
                let differentiated_base = find_derivative(*left_operand, with_respect_to)?;

                let subtraction = create_subtract_one(*right_operand)?;
                let new_exponentiation = Node::new_binary_exponentiation(base, subtraction);

                let multiply_node = Node::new_composite_fraction(
                    vec![differentiated_base, exponent, new_exponentiation],
                    vec![],
                );

                Ok(multiply_node)
            } else {
                Err(anyhow!("Could not differentiate expression"))
            }
        }
        Node::Composite(CompositeData {
            operator: BinaryOperator::Multiply,
            left: ref multiply,
            right: ref divide,
            ..
        }) => {
            if multiply.len() == 2 && divide.is_empty() {

                let multiply = multiply.clone();
                
                let maybe_value = find_matching_node(multiply.iter(), |node| {
                    node.is_value() && node != with_respect_to
                });
                let maybe_non_value = find_matching_node(multiply.iter(), |node| {
                    !node.is_value() || node == with_respect_to
                });

                if maybe_value.is_some() {
                    if let Some(non_value) = maybe_non_value {
                        let derivative = find_derivative(non_value.clone(), with_respect_to)?;
                        node.replace_child(non_value, derivative)?;
                        return Ok(node.clone());
                    }
                    return Err(anyhow!("Could not differentiate expression"));
                }
            }
            Err(anyhow!(
                "Can not currently differentiate product with >2 factors"
            ))
        }
        Node::Composite(data) => {
            let data = data.clone();
            let CompositeData {
                left: adds,
                right: subtracts,
                ..
            } = data;
            let new_adds: Vec<Node> = adds
                .into_iter()
                .filter_map(|node| find_derivative(node, with_respect_to).ok())
                .collect();
            let new_subtracts: Vec<Node> = subtracts
                .into_iter()
                .filter_map(|node| find_derivative(node, with_respect_to).ok())
                .collect();

            let new_node = Node::Composite(CompositeData {
                left: new_adds,
                right: new_subtracts,
                ..data
            });
            Ok(new_node)
        }
        Node::UnaryOperation { operator, operand } => {
            let operator_discriminant = discriminant(&operator);
            if operator_discriminant == discriminant(&UnaryOperator::PositiveSquareRoot) {
                // Easier to convert sqrt(x) into x^(1/2) and differentiate that.
                let one = Node::new_literal_integer(1);
                let two = Node::new_literal_integer(2);
                let one_half = Node::new_composite_fraction(vec![one], vec![two]);
                let power = Node::new_binary_exponentiation(*operand, one_half);
                let derivative = find_derivative(power, with_respect_to)?;
                Ok(derivative)
            } else if operator_discriminant == discriminant(&UnaryOperator::NaturalLogarithm) {
                // d/dx ln(f(x)) = f'(x) / f(x)
                let denominator = *operand.clone();
                let numerator = find_derivative(*operand, with_respect_to)?;
                let derivative = Node::new_composite_fraction(vec![numerator], vec![denominator]);
                Ok(derivative)
            } else {
                unimplemented!(
                    "This unary operation has not yet been implemented: {}",
                    operator
                )
            }
        }
    }
}

/// Returns node 'from' with a subtraction node: from - 1
fn create_subtract_one(from: Node) -> Result<Node> {
    let one = Node::LiteralInteger(1);
    let subtraction = Node::new_binary_subtraction(from, one);
    Ok(subtraction)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::simplifier::simplify;
    use crate::interpreter::syntax::expression_tree;
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
        let tree = expression_tree::new_tree(tokens).unwrap();

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
        let expected_tree = expression_tree::new_tree(expected_tokens).unwrap();

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
        let tree = expression_tree::new_tree(tokens).unwrap();

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
        let expected_tree = expression_tree::new_tree(expected_tokens).unwrap();

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
        let tree = expression_tree::new_tree(tokens).unwrap();

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
        let expected_tree = expression_tree::new_tree(expected_tokens).unwrap();

        assert_eq!(
            simplify(actual_tree).unwrap(),
            simplify(expected_tree).unwrap()
        );
    }
}
