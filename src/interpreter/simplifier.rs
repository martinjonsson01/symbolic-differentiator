use crate::interpreter::operator::Operator;
use crate::interpreter::parser::expression_tree::{ExpressionTree, TokenKey, Valid};
use crate::Token;
use anyhow::{anyhow, bail, Context, Error, Result};

pub fn simplify(mut tree: ExpressionTree<Valid>) -> Result<ExpressionTree<Valid>> {
    let root_key = tree.root_key();
    let new_root = simplify_subtree(&mut tree, root_key)?;
    let simplification = tree.set_root(new_root);
    Ok(simplification)
}

fn simplify_subtree(mut tree: &mut ExpressionTree<Valid>, node: TokenKey) -> Result<TokenKey> {
    match tree.token_of(node) {
        Ok(Token::Operator(operator)) => {
            let left_child = tree
                .left_child_of(node)
                .context("Expected a left operand")?;
            let left_token = tree.token_of(left_child)?;
            let right_child = tree
                .right_child_of(node)
                .context("Expected a right operand")?;
            let right_token = tree.token_of(right_child)?;
            let children = vec![left_child, right_child];

            let evaluate = operator.evaluate;
            let left_token = left_token.clone();
            let right_token = right_token.clone();

            let zero = Token::Literal(0f64);
            let one = Token::Literal(1f64);
            return if operator.symbol == "^" {
                // x^0 -> 1
                if right_token == zero && left_token != zero {
                    let new_root = tree.add_node(one);
                    Ok(new_root)
                }
                // x^1 -> x
                else if right_token == one && left_token != zero && left_token.is_value() {
                    let base = tree.add_node(left_token.clone());
                    Ok(base)
                } else {
                    try_evaluate_as_literals(&mut tree, node, evaluate, left_token, right_token)
                }
            } else if operator.symbol == "*" {
                let zero_node = find_matching_node(tree, &children, |token| *token == zero);
                let one_node = find_matching_node(tree, &children, |token| *token == one);
                let value_node =
                    find_matching_node(tree, &children, |token| token.is_value() && *token != one);
                // x * 1 || 1 * x -> x
                if one_node.is_some() && value_node.is_some() {
                    let value_token = tree.token_of(value_node.unwrap())?;
                    let new_root = tree.add_node(value_token.clone());
                    Ok(new_root)
                }
                // x * 0 || 0 * x -> x
                else if zero_node.is_some() && value_node.is_some() {
                    let new_root = tree.add_node(zero);
                    Ok(new_root)
                } else {
                    try_evaluate_as_literals(&mut tree, node, evaluate, left_token, right_token)
                }
            } else {
                try_evaluate_as_literals(&mut tree, node, evaluate, left_token, right_token)
            };
        }
        _ => bail!("The given node is not an operator token in the given expression tree"),
    }
}

fn try_evaluate_as_literals<F>(
    tree: &mut ExpressionTree<Valid>,
    node: TokenKey,
    evaluate: F,
    left_token: Token,
    right_token: Token,
) -> Result<TokenKey, Error>
where
    F: Fn(f64, f64) -> f64,
{
    match left_token {
        Token::Literal(left_value) => {
            match right_token {
                Token::Literal(right_value) => {
                    // k^n -> evaluate
                    let evaluation = evaluate(left_value, right_value);
                    let literal = Token::Literal(evaluation);
                    let evaluated_node = tree.add_node(literal);
                    Ok(evaluated_node)
                }
                // x^y -> x^y
                _ => Ok(node),
            }
        }
        // x^y -> x^y
        _ => Ok(node),
    }
}

fn find_matching_node<F>(
    tree: &mut ExpressionTree<Valid>,
    keys: &Vec<TokenKey>,
    predicate: F,
) -> Option<TokenKey>
where
    F: Fn(&Token) -> bool,
{
    for child_key in keys {
        let token = tree.token_of(*child_key).ok()?;
        if predicate(token) {
            return Some(*child_key);
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

    #[parameterized(
    expression = {
    "x + y",
    }
    )]
    fn simplify_non_simplifiable_expression_returns_original(expression: &str) {
        simplify_expression_returns_expected(expression, expression)
    }

    #[parameterized(
    expression = {
    "x^0",
    }
    )]
    fn simplify_power_of_zero_returns_one(expression: &str) {
        simplify_expression_returns_expected(expression, "1")
    }

    #[parameterized(
    expression = {
    "1 * x",
    "x^1",
    }
    )]
    fn simplify_identity_operation_returns_original(expression: &str) {
        simplify_expression_returns_expected(expression, "x")
    }

    #[parameterized(
    expression = {
    "0 * x",
    "x * 0",
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
    "8 / 4",
    "10 ^ 2",
    },
    expected_simplification = {
    "2",
    "1",
    "12",
    "2",
    "100",
    }
    )]
    fn simplify_literal_expression_evaluates_it(expression: &str, expected_simplification: &str) {
        simplify_expression_returns_expected(expression, expected_simplification)
    }
}
