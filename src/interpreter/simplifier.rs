use crate::interpreter::operator::Operator;
use crate::interpreter::parser::expression_tree::{
    CompositeData, ExpressionTree, Node, NodeKey, Valid,
};

use anyhow::{bail, Context, Result};

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

            let mut left_leftovers = simplify_composite_children(
                tree,
                &data,
                &data.left,
                &mut new_left,
                &mut new_right,
            )?;
            let mut right_leftovers = simplify_composite_children(
                tree,
                &data,
                &data.right,
                &mut new_left,
                &mut new_right,
            )?;
            new_left.append(&mut left_leftovers);
            new_right.append(&mut right_leftovers);

            // 0 * [anything] -> 0
            if data.is_fraction() {
                for left_key in &new_left {
                    let left_node = tree
                        .get_node(*left_key)
                        .context("Expected node to be in tree")?;
                    if let Node::LiteralInteger(0) = left_node {
                        return Ok(*left_key);
                    }
                }
            }

            let new_data = CompositeData {
                left: new_left,
                right: new_right,
                ..data
            };
            return try_evaluate_composites_as_literals(tree, new_data);
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

            if left_key != left_simplified {
                tree.replace_child_of(node, left_key, left_simplified)?;
            }
            if right_key != right_simplified {
                tree.replace_child_of(node, right_key, right_simplified)?;
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
    parent: &CompositeData,
    children: &Vec<NodeKey>,
    new_left: &mut Vec<NodeKey>,
    new_right: &mut Vec<NodeKey>,
) -> Result<Vec<NodeKey>> {
    let mut leftovers = vec![];
    for key in children {
        let simplified_child_key = simplify_subtree(tree, *key)?;
        let simplified_child = tree
            .get_node(simplified_child_key)
            .context("Expected node to exist in tree")?;

        try_flatten_composite_child(
            parent,
            simplified_child_key,
            simplified_child,
            new_left,
            new_right,
            &mut leftovers,
        )
    }
    Ok(leftovers)
}

fn try_flatten_composite_child(
    parent: &CompositeData,
    key: NodeKey,
    child: &Node,
    new_left: &mut Vec<NodeKey>,
    new_right: &mut Vec<NodeKey>,
    leftovers: &mut Vec<NodeKey>,
) {
    match child {
        // Flatten any direct child composite nodes into this one.
        Node::Composite(CompositeData {
            operator: Operator::Add,
            left,
            right,
            ..
        }) if parent.is_summation() => {
            new_left.append(&mut left.clone());
            new_right.append(&mut right.clone());
        }
        // Flatten any direct child composite nodes into this one.
        Node::Composite(CompositeData {
            operator: Operator::Multiply,
            left,
            ..
        }) if parent.is_fraction() => {
            new_left.append(&mut left.clone());
            // TODO: implement fraction / fraction simplification
        }
        _ => leftovers.push(key),
    }
}

fn try_evaluate_as_literals(
    tree: &mut ExpressionTree<Valid>,
    node: NodeKey,
    operator: &Operator,
    left: NodeKey,
    right: NodeKey,
) -> Result<NodeKey> {
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

fn try_evaluate_composites_as_literals(
    tree: &mut ExpressionTree<Valid>,
    data: CompositeData,
) -> Result<NodeKey> {
    let accumulated_left = accumulate_composite_literals(tree, data.left, data.operator)?;
    let accumulated_right = accumulate_composite_literals(tree, data.right, data.operator)?;

    let cancelled_left = cancel_composite_terms(tree, &accumulated_left, &accumulated_right)?;
    let cancelled_right = cancel_composite_terms(tree, &accumulated_right, &accumulated_left)?;

    match (&cancelled_left[..], &cancelled_right[..]) {
        ([_], [_]) => {
            let left = tree
                .get_node(cancelled_left[0])
                .context("Expected node to exist in tree")?;
            let right = tree
                .get_node(cancelled_right[0])
                .context("Expected node to exist in tree")?;

            if let Node::LiteralInteger(left_value) = left {
                if let Node::LiteralInteger(right_value) = right {
                    let evaluation = data.inverse_operator.evaluate(*left_value, *right_value);
                    let evaluated_node = tree.add_node(Node::LiteralInteger(evaluation));
                    return Ok(evaluated_node);
                }
            }
        }
        ([], []) => {
            let identity_node =
                tree.add_node(Node::LiteralInteger(data.operator.identity_operand()));
            return Ok(identity_node);
        }
        ([remaining_key], []) => {
            return Ok(*remaining_key);
        }
        ([], [remaining_key]) => {
            return Ok(*remaining_key);
        }
        _ => {}
    }

    let new_node = Node::Composite(CompositeData {
        left: cancelled_left,
        right: cancelled_right,
        ..data
    });
    Ok(tree.add_node(new_node))
}

fn cancel_composite_terms(
    tree: &mut ExpressionTree<Valid>,
    first: &Vec<NodeKey>,
    second: &Vec<NodeKey>,
) -> Result<Vec<NodeKey>> {
    let second_nodes = second
        .into_iter()
        .map(|key| {
            tree.get_node(*key)
                .context("Expected node to exist in tree")
        })
        .collect::<Result<Vec<_>>>()?;

    let cancelled_first = first
        .into_iter()
        .filter(|key| contains_node_of_key(tree, &second_nodes, key))
        .map(|key| *key)
        .collect();

    Ok(cancelled_first)
}

fn contains_node_of_key(tree: &ExpressionTree<Valid>, nodes: &Vec<&Node>, key: &NodeKey) -> bool {
    match tree.get_node(*key) {
        None => false,
        Some(node) => !nodes.contains(&node),
    }
}

fn accumulate_composite_literals(
    tree: &mut ExpressionTree<Valid>,
    keys: Vec<NodeKey>,
    operator: Operator,
) -> Result<Vec<NodeKey>> {
    let mut sum = operator.identity_operand();
    let mut others = vec![];
    for key in keys {
        let node = tree
            .get_node(key)
            .context("Expected node to exist in tree")?;
        match node {
            Node::LiteralInteger(value) => sum = operator.evaluate(sum, *value),
            _ => others.push(key),
        }
    }
    let new_keys = if sum != operator.identity_operand() {
        let left_sum_literal = tree.add_node(Node::new_literal_integer(sum));
        vec![left_sum_literal]
            .into_iter()
            .chain(others.into_iter())
            .collect()
    } else {
        others
    };
    Ok(new_keys)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::{convert, tokens_to_string};
    use parameterized_macro::parameterized;

    fn simplify_expression_returns_expected(expression: &str, expected_simplification: &str) {
        /* Not part of test, only used to simplify parameters by not using tree structs. */
        let expression_tree = convert(expression.to_string()).unwrap();
        // print!("{}", expression_tree);
        /* End */

        let actual_simplification_tree = simplify(expression_tree).unwrap();
        // print!("{}", actual_simplification_tree);

        /* Not part of test, only used to simplify parameters by not using tree structs. */
        let actual_simplification =
            tokens_to_string(actual_simplification_tree.to_infix().unwrap()).unwrap();
        /* End */

        assert_eq!(actual_simplification, expected_simplification);
    }

    #[test]
    fn simplify_expression_returns_expected_example() {
        simplify_expression_returns_expected(
            "(x + 0)^(10^2 * 100) - 0 * (x + y) / (z - 0)",
            "x^10000",
        )
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
    "(2 * x)^2 * (z + a)",
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
