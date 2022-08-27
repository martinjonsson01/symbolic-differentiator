use crate::interpreter::operator::BinaryOperator;
use crate::interpreter::syntax::expression_tree::{ExpressionTree, Node, NodeKey, Valid};

use crate::interpreter::syntax::composite::{CompositeChild, CompositeData};
use anyhow::{anyhow, Context, Result};

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

fn simplify_subtree(tree: &mut ExpressionTree<Valid>, node: NodeKey) -> Result<NodeKey> {
    match tree.get_node(node) {
        Some(Node::LiteralInteger(_) | Node::Identifier(_)) => Ok(node),
        Some(Node::Composite(data)) => {
            let data = data.clone();
            let mut new_left = vec![];
            let mut new_right = vec![];

            simplify_composite_children(
                tree,
                &data,
                CompositeChild::Left,
                &mut new_left,
                &mut new_right,
            )?;
            simplify_composite_children(
                tree,
                &data,
                CompositeChild::Right,
                &mut new_left,
                &mut new_right,
            )?;

            let new_data = if data.is_summation() {
                simplify_sum_of_fractions(tree, data, new_left, new_right)?
            } else if data.is_fraction() {
                simplify_fraction(tree, &new_left, &new_right)?
            } else {
                CompositeData {
                    left: new_left,
                    right: new_right,
                    ..data
                }
            };

            try_evaluate_composites_as_literals(tree, new_data)
        }
        Some(Node::BinaryOperation {
            operator,
            left_operand,
            right_operand,
        }) => {
            let operator = *operator;
            let left_key = *left_operand;
            let right_key = *right_operand;
            let left_simplified = simplify_subtree(tree, left_key)?;
            let right_simplified = simplify_subtree(tree, right_key)?;

            let left_node = tree
                .get_node(left_simplified)
                .context("Expected a left operand")?;
            let right_node = tree
                .get_node(right_simplified)
                .context("Expected a right operand")?;

            if operator == BinaryOperator::Exponentiate {
                // x^0 -> 1
                if right_node.is_literal_integer(0) && !left_node.is_literal_integer(0) {
                    let one = tree.add_node(Node::new_literal_integer(1));
                    return Ok(one);
                }
                // x^1 -> x
                else if right_node.is_literal_integer(1) {
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

            try_evaluate_as_literals(tree, node, &operator, left_simplified, right_simplified)
        }
        Some(Node::UnaryOperation { operand, .. }) => {
            let operand_key = *operand;
            let operand_simplified = simplify_subtree(tree, operand_key)?;

            if operand_key != operand_simplified {
                tree.replace_child_of(node, operand_key, operand_simplified)?;
            }

            Ok(node)
        }
        None => Err(anyhow!(
            "The given node does not exist in the given expression tree"
        )),
    }
}

fn simplify_fraction(
    tree: &mut ExpressionTree<Valid>,
    numerator: &[NodeKey],
    denominator: &[NodeKey],
) -> Result<CompositeData> {
    let mut new_numerator = vec![];
    let mut exclude_from_denominator = vec![];

    for numerator_factor_key in numerator {
        let numerator_factor_node = tree
            .get_node(*numerator_factor_key)
            .context("Expected node to be in tree")?;
        // 0 * [anything] -> 0
        if let Node::LiteralInteger(0) = numerator_factor_node {
            return Ok(CompositeData::new_multiplied(vec![*numerator_factor_key]));
        }

        // [expression]^n / [expression]^k -> [expression]^(n-k)
        if let Node::BinaryOperation {
            operator: BinaryOperator::Exponentiate,
            left_operand: base,
            right_operand: exponent,
        } = numerator_factor_node.clone()
        {
            let denominator_nodes: Vec<(_, _)> = denominator
                .iter()
                .filter_map(|key| tree.get_node_with_key(*key))
                .map(|(key, node)| (key, node.clone()))
                .collect();
            let matching_exponentiation_in_denominator =
                denominator_nodes.iter().find_map(|(key, node)| match node {
                    Node::BinaryOperation {
                        operator: BinaryOperator::Exponentiate,
                        left_operand: other_base,
                        right_operand: other_exponent,
                    } => {
                        if tree.nodes_eq(*other_base, base) {
                            Some((key, other_exponent))
                        } else {
                            None
                        }
                    }
                    _ => None,
                });

            if let Some((other_exponentiation, other_exponent)) =
                matching_exponentiation_in_denominator
            {
                let new_exponent =
                    tree.add_node(Node::new_binary_subtraction(exponent, *other_exponent));

                let new_exponentiation =
                    tree.add_node(Node::new_binary_exponentiation(base, new_exponent));

                let simplified_exponentiation = simplify_subtree(tree, new_exponentiation)?;

                new_numerator.push(simplified_exponentiation);
                // Exclude from denominator because it's been merged with the one in the numerator.
                exclude_from_denominator.push(*other_exponentiation);

                continue;
            }
        }

        new_numerator.push(*numerator_factor_key)
    }

    let new_denominator: Vec<_> = denominator
        .iter()
        .filter(|factor| !exclude_from_denominator.contains(factor))
        .copied()
        .collect();

    Ok(CompositeData::new_fraction(new_numerator, new_denominator))
}

fn simplify_sum_of_fractions(
    tree: &mut ExpressionTree<Valid>,
    data: CompositeData,
    new_left: Vec<NodeKey>,
    new_right: Vec<NodeKey>,
) -> Result<CompositeData> {
    // Disjunctive form (fraction + fraction - fraction)

    let left_keyed_fractions = tree.nodes_into_fractions(&new_left)?;
    let right_keyed_fractions = tree.nodes_into_fractions(&new_right)?;
    let left_fractions: Vec<&CompositeData> =
        left_keyed_fractions.iter().map(|(_, data)| data).collect();
    let right_fractions: Vec<&CompositeData> =
        right_keyed_fractions.iter().map(|(_, data)| data).collect();

    let common_denominator: Vec<NodeKey> = left_fractions
        .iter()
        .chain(right_fractions.iter())
        .flat_map(|data| data.right.clone())
        .collect();

    // An empty common denominator means there's no point in extending them to a common
    // fraction, as it'll just be equal to the original expression once simplified.
    // E.g. (a * b)/() - (c * d)/() =  (a * b - (c * d)) / ()
    // but since any multiplication implicitly has an empty denominator, it's actually
    // = ((a * b)/() - ((c * d)/())) / () which can be simplified to
    // = (a * b)/() - (c * d)/() and we're back to where we started.
    if common_denominator.is_empty() {
        return Ok(CompositeData {
            left: new_left,
            right: new_right,
            ..data
        });
    }

    let extend_nominator_of = |data: &&CompositeData| {
        let numerator_factors = &data.left;
        let denominator_factors = &data.right;
        let other_denominators = common_denominator
            .iter()
            .filter(|key| !denominator_factors.contains(key));

        // This chaining effectively multiplies
        // the other denominators into the numerator.
        let extended_numerator = numerator_factors
            .iter()
            .chain(other_denominators)
            .copied()
            .collect::<Vec<_>>();
        Node::new_multiplied(extended_numerator)
    };
    let left_extended_nominators = left_fractions
        .iter()
        .map(extend_nominator_of)
        .map(|node| tree.add_node(node))
        .collect();
    let right_extended_nominators = right_fractions
        .iter()
        .map(extend_nominator_of)
        .map(|node| tree.add_node(node))
        .collect();

    let nominator_sum_data =
        CompositeData::new_summation(left_extended_nominators, right_extended_nominators);
    let nominator_sum = tree.add_node(Node::Composite(nominator_sum_data.clone()));

    let nominator_sum = simplify_subtree(tree, nominator_sum)?;

    if common_denominator.is_empty() {
        Ok(nominator_sum_data)
    } else {
        Ok(CompositeData::new_fraction(
            vec![nominator_sum],
            common_denominator,
        ))
    }
}

fn simplify_composite_children(
    tree: &mut ExpressionTree<Valid>,
    parent: &CompositeData,
    which_child: CompositeChild,
    new_left: &mut Vec<NodeKey>,
    new_right: &mut Vec<NodeKey>,
) -> Result<()> {
    let mut leftovers = vec![];
    for key in parent.child(which_child) {
        let simplified_child_key = simplify_subtree(tree, *key)?;
        let simplified_child = tree
            .get_node(simplified_child_key)
            .context("Expected node to exist in tree")?;

        try_flatten_composite_child(
            parent,
            simplified_child_key,
            simplified_child,
            which_child,
            new_left,
            new_right,
            &mut leftovers,
        )
    }
    match which_child {
        CompositeChild::Left => new_left.append(&mut leftovers),
        CompositeChild::Right => new_right.append(&mut leftovers),
    }
    Ok(())
}

/// If a child can't be flattened, then it is returned in leftovers.
fn try_flatten_composite_child(
    parent: &CompositeData,
    key: NodeKey,
    child: &Node,
    which_child: CompositeChild,
    new_left: &mut Vec<NodeKey>,
    new_right: &mut Vec<NodeKey>,
    leftovers: &mut Vec<NodeKey>,
) {
    // Flatten any direct child composite nodes into the parent.
    match child {
        Node::Composite(CompositeData {
            operator: BinaryOperator::Add,
            left,
            right,
            ..
        }) if parent.is_summation() => {
            // x + (a - b) -> x + a - b
            if which_child == CompositeChild::Left {
                new_left.append(&mut left.clone());
                new_right.append(&mut right.clone());
            }
            // x - (a - b) -> x - a + b
            else if which_child == CompositeChild::Right {
                new_left.append(&mut right.clone());
                new_right.append(&mut left.clone());
            }
        }
        Node::Composite(CompositeData {
            operator: BinaryOperator::Multiply,
            left,
            right,
            ..
        }) if parent.is_fraction() && which_child == CompositeChild::Left => {
            // (x / y) / a -> x / (a * y)
            new_left.append(&mut left.clone()); // x
            new_right.append(&mut right.clone()); // (a * y)
        }
        Node::Composite(CompositeData {
            operator: BinaryOperator::Multiply,
            left,
            right,
            ..
        }) if parent.is_fraction() && which_child == CompositeChild::Right => {
            // a / (x / y) -> (a * y) / x
            new_left.append(&mut right.clone()); // (a * y)
            new_right.append(&mut left.clone()); // x
        }
        _ => leftovers.push(key),
    }
}

fn try_evaluate_as_literals(
    tree: &mut ExpressionTree<Valid>,
    node: NodeKey,
    operator: &BinaryOperator,
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
    let accumulated_left = accumulate_composite_literals(tree, &data.left, &data.operator)?;
    let accumulated_right = accumulate_composite_literals(tree, &data.right, &data.operator)?;

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
                    // Temporary ugly workaround: since support for floating point numbers
                    // has not been implemented, don't try to divide numbers that don't evenly
                    // divide into each other.
                    if !data.is_fraction() || divides_evenly(*left_value, *right_value) {
                        let evaluation = data.inverse_operator.evaluate(*left_value, *right_value);
                        let evaluated_node = tree.add_node(Node::LiteralInteger(evaluation));
                        return Ok(evaluated_node);
                    }
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
        _ => {}
    }

    let new_node = Node::Composite(CompositeData {
        left: cancelled_left,
        right: cancelled_right,
        ..data
    });
    Ok(tree.add_node(new_node))
}

fn divides_evenly(a: i32, b: i32) -> bool {
    let a_float = a as f64;
    let b_float = b as f64;
    let integer_division = (a / b) as f64;
    integer_division == a_float / b_float
}

fn cancel_composite_terms(
    tree: &mut ExpressionTree<Valid>,
    first: &[NodeKey],
    second: &[NodeKey],
) -> Result<Vec<NodeKey>> {
    let second_nodes = second
        .iter()
        .copied()
        .map(|key| tree.get_node(key).context("Expected node to exist in tree"))
        .collect::<Result<Vec<_>>>()?;

    let cancelled_first = first
        .iter()
        .filter(|key| contains_node_of_key(tree, &second_nodes, key))
        .copied()
        .collect();

    Ok(cancelled_first)
}

fn contains_node_of_key(tree: &ExpressionTree<Valid>, nodes: &[&Node], key: &NodeKey) -> bool {
    match tree.get_node(*key) {
        None => false,
        Some(node) => !nodes.contains(&node),
    }
}

fn accumulate_composite_literals(
    tree: &mut ExpressionTree<Valid>,
    keys: &[NodeKey],
    operator: &BinaryOperator,
) -> Result<Vec<NodeKey>> {
    let mut sum = operator.identity_operand();
    let mut others = vec![];
    for key in keys {
        let node = tree
            .get_node(*key)
            .context("Expected node to exist in tree")?;
        match node {
            Node::LiteralInteger(value) => sum = operator.evaluate(sum, *value),
            _ => others.push(*key),
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
        print!("{}", expression_tree);
        /* End */

        let actual_simplification_tree = simplify(expression_tree).unwrap();
        print!("{}", actual_simplification_tree);

        /* Not part of test, only used to simplify parameters by not using tree structs. */
        let actual_simplification =
            tokens_to_string(actual_simplification_tree.to_infix().unwrap()).unwrap();
        /* End */

        assert_eq!(actual_simplification, expected_simplification);
    }

    #[test]
    fn simplify_expression_returns_expected_example() {
        simplify_expression_returns_expected("3 * x^2 / x^3", "3 * x^-1")
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
    "x - (x - 1)",
    "x + (x - 1)",
    "3 * (1 + x)^(3 - 1) / (1 + x)^3",
    "3 * x^2 / x^3",
    },
    expected_simplification = {
    "(x + y) * z",
    "(2 * x)^2 * (z + a)",
    "x^10000",
    "x * y",
    "x * y",
    "x * y",
    "x + y",
    "1",
    "x + x - 1",
    "3 * (1 + x)^-1",
    "3 * x^-1",
    }
    )]
    fn simplify_nested_expressions_returns_expected(
        expression: &str,
        expected_simplification: &str,
    ) {
        simplify_expression_returns_expected(expression, expected_simplification)
    }

    #[parameterized(
    expression = {
    "x / x",
    "(y * x) / (x * y)",
    "y * x / (x * y)",
    "x * (1 / y)",
    },
    expected_simplification = {
    "1",
    "1",
    "1",
    "x / y",
    }
    )]
    fn simplify_fractions_returns_expected(expression: &str, expected_simplification: &str) {
        simplify_expression_returns_expected(expression, expected_simplification)
    }

    #[parameterized(
    expression = {
    "(x / 3) / (y / 2)",
    "1 / 2 - 1",
    "1 / 2 + 1 / 2",
    },
    expected_simplification = {
    "2 * x / 3 * y",
    "-1 / 2",
    "1",
    }
    )]
    fn fraction_literal_simplifies_to_expected(expression: &str, expected_simplification: &str) {
        simplify_expression_returns_expected(expression, expected_simplification)
    }

    #[parameterized(
    expression = {
    "(x + y)^3 / (x + y)^2",
    "(x - z + 2 * 3)^z / (x - z + 2 * 3)^(z - 1)",
    "(x + 1)^(y + z) / (1 + x)^(a - b)",
    "x^3 * y^4 * z^5 / (x^2 * y^3 * z^4)",
    },
    expected_simplification = {
    "x + y",
    "6 + x - z",
    "(1 + x)^(y + z + b - a)",
    "x * y * z",
    }
    )]
    fn exponentiation_division_subtracts_exponents(
        expression: &str,
        expected_simplification: &str,
    ) {
        simplify_expression_returns_expected(expression, expected_simplification)
    }
}
