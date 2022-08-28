use crate::interpreter::operator::BinaryOperator;
use crate::interpreter::syntax::expression_tree::{nodes_into_fractions, Node};

use crate::interpreter::syntax::composite::{CompositeChild, CompositeData};
use anyhow::Result;
use itertools::Itertools;

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
/// # use anyhow::Result;
/// # fn main() -> Result<()> {
/// use symbolic_differentiator::interpreter::convert;
/// use symbolic_differentiator::interpreter::simplifier::simplify;
///
/// let expression = "x^2";
/// let expression_tree = convert(expression.to_string())?;
///
/// let simplified_tree = simplify(expression_tree)?;
/// # Ok::<(), anyhow::Error>(()) }
/// ```
pub fn simplify(mut node: Node) -> Result<Node> {
    match node {
        Node::LiteralInteger(_) | Node::Identifier(_) => Ok(node),
        Node::Composite(data) => {
            let mut new_left = vec![];
            let mut new_right = vec![];

            simplify_composite_children(
                &data,
                CompositeChild::Left,
                &mut new_left,
                &mut new_right,
            )?;
            simplify_composite_children(
                &data,
                CompositeChild::Right,
                &mut new_left,
                &mut new_right,
            )?;

            let new_data = if data.is_summation() {
                simplify_sum_of_fractions(data, new_left, new_right)?
            } else if data.is_fraction() {
                simplify_fraction(new_left, new_right)?
            } else {
                CompositeData {
                    left: new_left,
                    right: new_right,
                    ..data
                }
            };

            try_evaluate_composites_as_literals(new_data)
        }
        Node::BinaryOperation {
            operator,
            left_operand,
            right_operand,
        } => {
            let left = *left_operand.clone();
            let right = *right_operand.clone();
            let left_simplified = simplify(left)?;
            let right_simplified = simplify(right)?;

            if operator == BinaryOperator::Exponentiate {
                // x^0 -> 1
                if right_simplified.is_literal_integer(0) && !left_simplified.is_literal_integer(0)
                {
                    let one = Node::new_literal_integer(1);
                    return Ok(one);
                }
                // x^1 -> x
                else if right_simplified.is_literal_integer(1) {
                    let base = left_simplified.clone();
                    return Ok(base);
                }
            }

            if let Some(evaluated) =
                try_evaluate_as_literals(&operator, &left_simplified, &right_simplified)
            {
                Ok(evaluated)
            } else {
                Ok(Node::BinaryOperation {
                    operator,
                    left_operand: Box::new(left_simplified),
                    right_operand: Box::new(right_simplified),
                })
            }
        }
        Node::UnaryOperation { ref operand, .. } => {
            let old_operand = operand.clone();
            let operand_simplified = simplify(*operand.clone())?;

            if *old_operand != operand_simplified {
                node.replace_child(&old_operand, operand_simplified)?;
            }

            Ok(node)
        }
    }
}

fn simplify_fraction(numerator: Vec<Node>, denominator: Vec<Node>) -> Result<CompositeData> {
    let mut new_numerator = vec![];
    let mut exclude_from_denominator = vec![];

    for numerator_factor_node in numerator {
        // 0 * [anything] -> 0
        if let Node::LiteralInteger(0) = numerator_factor_node {
            return Ok(CompositeData::new_multiplied(vec![numerator_factor_node]));
        }

        // [expression]^n / [expression]^k -> [expression]^(n-k)
        if let Node::BinaryOperation {
            operator: BinaryOperator::Exponentiate,
            left_operand: base,
            right_operand: exponent,
        } = &numerator_factor_node
        {
            let matching_exponentiation_in_denominator =
                denominator.iter().find_map(|node| match node {
                    Node::BinaryOperation {
                        operator: BinaryOperator::Exponentiate,
                        left_operand: other_base,
                        right_operand: other_exponent,
                    } => {
                        if base == other_base {
                            Some((node, other_exponent))
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
                    Node::new_binary_subtraction(*exponent.clone(), *other_exponent.clone());

                let new_exponentiation =
                    Node::new_binary_exponentiation(*base.clone(), new_exponent);

                let simplified_exponentiation = simplify(new_exponentiation)?;

                new_numerator.push(simplified_exponentiation);
                // Exclude from denominator because it's been merged with the one in the numerator.
                exclude_from_denominator.push(other_exponentiation);

                continue;
            }
        }

        new_numerator.push(numerator_factor_node)
    }

    let new_denominator: Vec<_> = denominator
        .clone()
        .into_iter()
        .filter(|factor| !exclude_from_denominator.contains(&factor))
        .collect();

    Ok(CompositeData::new_fraction(new_numerator, new_denominator))
}

fn simplify_sum_of_fractions(
    data: CompositeData,
    new_left: Vec<Node>,
    new_right: Vec<Node>,
) -> Result<CompositeData> {
    // Disjunctive form (fraction + fraction - fraction)

    let left_fractions = nodes_into_fractions(&new_left)?;
    let right_fractions = nodes_into_fractions(&new_right)?;

    let common_denominator: Vec<Node> = left_fractions
        .iter()
        .chain(right_fractions.iter())
        .flat_map(|data| data.right.clone())
        .unique()
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

    let extend_nominator_of = |data: CompositeData| {
        let numerator_factors = data.left;
        let denominator_factors = data.right;
        let other_denominators = common_denominator
            .clone()
            .into_iter()
            .filter(|key| !denominator_factors.contains(key));

        // This chaining effectively multiplies
        // the other denominators into the numerator.
        let extended_numerator = numerator_factors
            .into_iter()
            .chain(other_denominators)
            .collect::<Vec<_>>();
        Node::new_multiplied(extended_numerator)
    };
    let left_extended_nominators = left_fractions
        .into_iter()
        .map(extend_nominator_of)
        .collect();
    let right_extended_nominators = right_fractions
        .into_iter()
        .map(extend_nominator_of)
        .collect();

    let nominator_sum_data =
        CompositeData::new_summation(left_extended_nominators, right_extended_nominators);
    let nominator_sum = Node::Composite(nominator_sum_data.clone());

    let nominator_sum = simplify(nominator_sum)?;

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
    parent: &CompositeData,
    which_child: CompositeChild,
    new_left: &mut Vec<Node>,
    new_right: &mut Vec<Node>,
) -> Result<()> {
    let mut leftovers = vec![];
    for node in parent.clone().child_owned(which_child) {
        let simplified_child = simplify(node)?;

        try_flatten_composite_child(
            parent,
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
    child: Node,
    which_child: CompositeChild,
    new_left: &mut Vec<Node>,
    new_right: &mut Vec<Node>,
    leftovers: &mut Vec<Node>,
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
        _ => leftovers.push(child),
    }
}

fn try_evaluate_as_literals(operator: &BinaryOperator, left: &Node, right: &Node) -> Option<Node> {
    match left {
        Node::LiteralInteger(left_value) => {
            match right {
                Node::LiteralInteger(right_value) => {
                    // literal op literal -> evaluate
                    let evaluation = operator.evaluate(*left_value, *right_value);
                    let evaluated = Node::LiteralInteger(evaluation);
                    Some(evaluated)
                }
                // x op y -> x op y
                _ => None,
            }
        }
        // x op y -> x op y
        _ => None,
    }
}

fn try_evaluate_composites_as_literals(data: CompositeData) -> Result<Node> {
    let accumulated_left = accumulate_composite_literals(data.left, &data.operator)?;
    let accumulated_right = accumulate_composite_literals(data.right, &data.operator)?;

    let (cancelled_left, cancelled_right) =
        cancel_composite_terms(accumulated_left, accumulated_right)?;

    match (&cancelled_left[..], &cancelled_right[..]) {
        ([Node::LiteralInteger(left_value)], [Node::LiteralInteger(right_value)]) => {
            // Temporary ugly workaround: since support for floating point numbers
            // has not been implemented, don't try to divide numbers that don't evenly
            // divide into each other.
            if !is_fraction(data.operator, data.inverse_operator)
                || divides_evenly(*left_value, *right_value)
            {
                let evaluation = data.inverse_operator.evaluate(*left_value, *right_value);
                let evaluated_node = Node::LiteralInteger(evaluation);
                return Ok(evaluated_node);
            }
        }
        ([], []) => {
            let identity_node = Node::LiteralInteger(data.operator.identity_operand());
            return Ok(identity_node);
        }
        ([remaining_node], []) => {
            return Ok(remaining_node.clone());
        }
        _ => {}
    }

    let new_node = Node::Composite(CompositeData {
        left: cancelled_left,
        right: cancelled_right,
        ..data
    });
    Ok(new_node)
}

fn is_fraction(operator: BinaryOperator, inverse_operator: BinaryOperator) -> bool {
    operator == BinaryOperator::Multiply && inverse_operator == BinaryOperator::Divide
}

fn divides_evenly(a: i32, b: i32) -> bool {
    let a_float = a as f64;
    let b_float = b as f64;
    let integer_division = (a / b) as f64;
    integer_division == a_float / b_float
}

fn cancel_composite_terms(
    mut first: Vec<Node>,
    mut second: Vec<Node>,
) -> Result<(Vec<Node>, Vec<Node>)> {
    let cancelled: Vec<_> = first
        .iter()
        .chain(second.iter())
        .filter(|node| first.contains(node) && second.contains(node))
        .cloned()
        .collect();
    first.retain(|node| !cancelled.contains(node));
    second.retain(|node| !cancelled.contains(node));
    Ok((first, second))
}

fn accumulate_composite_literals(nodes: Vec<Node>, operator: &BinaryOperator) -> Result<Vec<Node>> {
    let mut sum = operator.identity_operand();
    let mut others = vec![];
    for node in nodes {
        match node {
            Node::LiteralInteger(value) => sum = operator.evaluate(sum, value),
            _ => others.push(node),
        }
    }
    let new_keys = if sum != operator.identity_operand() {
        let left_sum_literal = Node::new_literal_integer(sum);
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
        simplify_expression_returns_expected("1 / 2 + 1 / 2", "1")
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
