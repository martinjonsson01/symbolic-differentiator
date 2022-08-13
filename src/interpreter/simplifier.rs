use crate::interpreter::find_matching_node;
use crate::interpreter::operator::Operator;
use crate::interpreter::parser::expression_tree::{
    CompositeData, ExpressionTree, Node, NodeKey, Valid,
};
use crate::Token;
use anyhow::{bail, Context, Error, Result};

/// Simplifies a given expression tree.
///
/// **NOTE:**
/// This does not guarantee a complete mathematical simplification, as not all possible
/// types of simplifiable patterns are implemented.
///
/// # Arguments
///
/// * `tree`: A valid expression tree.
///
/// returns: A simplified (or the input) expression tree.
///
/// # Examples
///
/// ```
/// let expression_tree = convert(expression.to_string())?;
///
/// let simplified_tree = simplify(expression_tree)?;
/// ```
pub fn simplify(mut tree: ExpressionTree<Valid>) -> Result<ExpressionTree<Valid>> {
    let root_key = tree.root_key();
    let new_root = simplify_subtree(&mut tree, root_key)?;
    let simplification = tree.set_root(new_root);
    Ok(simplification)
}

fn simplify_subtree(mut tree: &mut ExpressionTree<Valid>, node: NodeKey) -> Result<NodeKey> {
    match tree.get_node(node) {
        Some(Node::LiteralInteger(_) | Node::Identifier(_)) => {
            return Ok(node);
        }
        Some(Node::Composite(data)) => {
            let data = data.clone();
            let mut new_left = vec![];
            let mut new_right = vec![];

            let mut left_leftovers =
                simplify_composite_children(tree, &data.left, &mut new_left, &mut new_right)?;
            let mut right_leftovers =
                simplify_composite_children(tree, &data.right, &mut new_left, &mut new_right)?;
            new_left.append(&mut left_leftovers);
            new_right.append(&mut right_leftovers);

            let (filtered_left, filtered_right) = if data.is_summation() {
                let nonzero_adds = new_left
                    .iter()
                    .filter_map(|key| to_nonzero(tree, key))
                    .collect();
                let nonzero_subtracts = new_right
                    .iter()
                    .filter_map(|key| to_nonzero(tree, key))
                    .collect();
                (nonzero_adds, nonzero_subtracts)
            } else if data.is_fraction() {
                let adds_without_ones = new_left
                    .iter()
                    .filter_map(|key| to_not_one(tree, key))
                    .collect();
                let subtracts_without_ones = new_right
                    .iter()
                    .filter_map(|key| to_not_one(tree, key))
                    .collect();
                (adds_without_ones, subtracts_without_ones)
            } else {
                (new_left, new_right)
            };

            let new_node = tree.add_node(Node::Composite(CompositeData {
                left: filtered_left,
                right: filtered_right,
                ..data
            }));
            Ok(new_node)
        }
        Some(Node::BinaryOperation {
            operator,
            left_operand,
            right_operand,
        }) => {
            let operator = operator.clone();
            let left_key = left_operand.clone();
            let right_key = right_operand.clone();
            let left_simplified = simplify_subtree(tree, left_key)?;
            let right_simplified = simplify_subtree(tree, right_key)?;

            let left_node = tree
                .get_node(left_simplified)
                .context("Expected a left operand")?;
            let right_node = tree
                .get_node(right_simplified)
                .context("Expected a right operand")?;

            if operator == Operator::Exponentiate {
                // x^0 -> 1
                if right_node.is_literal_integer(0) && !left_node.is_literal_integer(0) {
                    let one = tree.add_node(Node::new_literal_integer(1));
                    return Ok(one);
                }
                // x^1 -> x
                else if right_node.is_literal_integer(1)
                    && !left_node.is_literal_integer(0)
                    && left_node.is_value()
                {
                    let base = tree.add_node(left_node.clone());
                    return Ok(base);
                }
            }

            return try_evaluate_as_literals(
                &mut tree,
                node,
                &operator,
                left_simplified,
                right_simplified,
            );
        }
        None => bail!("The given node does not exist in the given expression tree"),
    }
}

fn simplify_composite_children(
    tree: &mut ExpressionTree<Valid>,
    children: &Vec<NodeKey>,
    new_left: &mut Vec<NodeKey>,
    new_right: &mut Vec<NodeKey>,
) -> Result<Vec<NodeKey>> {
    let mut leftovers = vec![];
    for key in children {
        let simplified = simplify_subtree(tree, *key)?;
        let node = tree
            .get_node(simplified)
            .context("Expected node to exist in tree")?;

        match node {
            // Flatten any direct child composite nodes into this one.
            Node::Composite(CompositeData {
                operator: Operator::Add,
                left,
                right,
                ..
            }) => {
                new_left.append(&mut left.clone());
                new_right.append(&mut right.clone());
            }
            _ => leftovers.push(*key),
        }
    }
    Ok(leftovers)
}

fn try_evaluate_as_literals(
    tree: &mut ExpressionTree<Valid>,
    node: NodeKey,
    operator: &Operator,
    left: NodeKey,
    right: NodeKey,
) -> Result<NodeKey, Error> {
    match tree.get_node(left) {
        Some(Node::LiteralInteger(left_value)) => {
            match tree.get_node(right) {
                Some(Node::LiteralInteger(right_value)) => {
                    // literal op literal -> evaluate
                    let evaluation = operator.evaluate(*left_value, *right_value);
                    let literal = Node::LiteralInteger(evaluation);
                    let evaluated_node = tree.add_node(literal);
                    Ok(evaluated_node)
                }
                // x op y -> x op y
                _ => Ok(node),
            }
        }
        // x op y -> x op y
        _ => Ok(node),
    }
}

fn to_nonzero(tree: &ExpressionTree<Valid>, key: &NodeKey) -> Option<NodeKey> {
    node_matches(tree, key, |node| !node.is_zero())
}

fn to_not_one(tree: &ExpressionTree<Valid>, key: &NodeKey) -> Option<NodeKey> {
    node_matches(tree, key, |node| !node.is_one())
}

fn node_matches(
    tree: &ExpressionTree<Valid>,
    key: &NodeKey,
    predicate: impl Fn(&Node) -> bool,
) -> Option<NodeKey> {
    if let Some(node) = tree.get_node(*key) {
        if predicate(node) {
            return Some(*key);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::{convert, tokens_to_string};
    use parameterized_macro::parameterized;

    fn simplify_expression_returns_expected(expression: &str, expected_simplification: &str) {
        /* Not part of test, only used to simplify parameters by not using tree structs. */
        let expression_tree = convert(expression.to_string()).unwrap();
        /* End */

        let actual_simplification_tree = simplify(expression_tree).unwrap();

        /* Not part of test, only used to simplify parameters by not using tree structs. */
        let actual_simplification =
            tokens_to_string(actual_simplification_tree.to_infix().unwrap()).unwrap();
        /* End */

        assert_eq!(actual_simplification, expected_simplification);
    }

    #[test]
    fn simplify_expression_returns_expected_example() {
        simplify_expression_returns_expected("x + 0", "x")
    }

    #[parameterized(
    expression = {
    "x + y",
    "(x + y) * (z + a)"
    }
    )]
    fn simplify_non_simplifiable_expression_returns_original(expression: &str) {
        simplify_expression_returns_expected(expression, expression)
    }

    #[parameterized(
    expression = {
    "x^0",
    "1^0",
    "2349872^0",
    }
    )]
    fn simplify_power_of_zero_returns_one(expression: &str) {
        simplify_expression_returns_expected(expression, "1")
    }

    #[parameterized(
    expression = {
    "1 * x",
    "x^1",
    "x + 0",
    "0 + x",
    "x - 0",
    "0 + 0 + 0 + x",
    "1 * 1 * 1 * x",
    }
    )]
    fn simplify_identity_operation_returns_original(expression: &str) {
        simplify_expression_returns_expected(expression, "x")
    }

    #[parameterized(
    expression = {
    "0 * x",
    "x * 0",
    "0 / x",
    "x * 0 * 0 * 0",
    "0 * ((x * (y^b + 1))^(1+1) * (z + a))",
    }
    )]
    fn simplify_zero_property_returns_zero(expression: &str) {
        simplify_expression_returns_expected(expression, "0")
    }

    #[parameterized(
    expression = {
    "1 + 1",
    "2 - 1",
    "3 * 4",
    "1 * 1",
    "8 / 4",
    "1 / 1",
    "0 / 1433",
    "10 ^ 2",
    "1 + 1 + 1 + 1"
    },
    expected_simplification = {
    "2",
    "1",
    "12",
    "1",
    "2",
    "1",
    "0",
    "100",
    "4"
    }
    )]
    fn simplify_literal_expression_evaluates_it(expression: &str, expected_simplification: &str) {
        simplify_expression_returns_expected(expression, expected_simplification)
    }

    #[parameterized(
    expression = {
    "(x + y) * (z + 0)",
    "(x * (y^0 + 1))^2 * (z + a)",
    "(x + 0)^(10^2 * 100) - 0 * (x + y) / (z - 0)",
    "(x * y) - 0",
    "(x * y) + 0",
    "0 + (x * y)",
    "1 * (x + y)",
    },
    expected_simplification = {
    "(x + y) * z",
    "(x * 2)^2 * (z + a)",
    "x^10000",
    "x * y",
    "x * y",
    "x * y",
    "x + y",
    }
    )]
    fn simplify_nested_expressions_returns_expected(
        expression: &str,
        expected_simplification: &str,
    ) {
        simplify_expression_returns_expected(expression, expected_simplification)
    }
}
