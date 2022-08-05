use crate::interpreter::operator::Operator;
use crate::interpreter::parser::expression_tree::{ExpressionTree, TokenKey, Valid};
use crate::interpreter::Token;
use anyhow::{anyhow, bail, Context, Result};

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
            let left_child = tree
                .left_child_of(node)
                .context("Expected a left operand")?;
            let right_child = tree
                .right_child_of(node)
                .context("Expected a right operand")?;
            let children = vec![left_child, right_child];

            if operator.symbol == "^" {
                let left_token = tree.token_of(left_child)?;
                if left_token != with_respect_to {
                    bail!("Can not differentiate with respect to this variable")
                }

                let maybe_parent = tree.get_parent_of(node);

                let cloned_right_operand = tree.clone_node_of(right_child)?;
                let multiply_node =
                    tree.add_node_children("*".parse().unwrap(), cloned_right_operand, node)?;

                match maybe_parent {
                    Ok(parent) => tree.set_parent(multiply_node, parent)?,
                    Err(_) => {
                        // This means that we're at the root node.
                        root = multiply_node;
                    }
                }

                mutate_literal(tree, right_child, |exponent| *exponent -= 1f64)?;
            } else if operator.symbol == "*" {
                let is_literal = |key: &TokenKey| {
                    if let Ok(token) = tree.token_of(*key) {
                        if matches!(token, Token::Literal(_)) {
                            return Some(*key);
                        }
                    }
                    None
                };
                let is_caret = |key: &TokenKey| {
                    if let Ok(token) = tree.token_of(*key) {
                        let _caret = "^".to_string();
                        if matches!(token, Token::Operator(Operator { symbol: _caret, .. })) {
                            return Some(*key);
                        }
                    }
                    None
                };

                let factor_literal_key = find_matching_node(&children, is_literal)
                    .context("Expected a literal child")?;
                let caret_key = find_matching_node(&children, is_caret)
                    .context("Expected a caret operator child")?;

                let exponent_literal_key = tree
                    .right_child_of(caret_key)
                    .context("Expected a right operand")?;
                let exponent_literal_key = is_literal(&exponent_literal_key)
                    .context("Expected a literal right operand")?;

                let variable_key = tree
                    .left_child_of(caret_key)
                    .context("Expected a left operand")?;
                let variable_token = tree.token_of(variable_key)?;
                if variable_token != with_respect_to {
                    bail!("Can not differentiate with respect to this variable")
                }

                let mut maybe_exponent_value = None;
                mutate_literal(tree, exponent_literal_key, |exponent|{
                    maybe_exponent_value = Some(exponent.clone());
                    *exponent -= 1f64
                } )?;
                let exponent_value = match maybe_exponent_value {
                    None => bail!("Expected exponent to be a literal"),
                    Some(value) => value
                };
                    
                mutate_literal(tree, factor_literal_key, |factor| *factor *= exponent_value)?;
            }
        }
        _ => todo![],
    }
    Ok(root)
}

fn mutate_literal<F>(tree: &mut ExpressionTree<Valid>, key: TokenKey, mut mutate: F) -> Result<()>
where
    F: FnMut(&mut f64),
{
    let token = tree.mut_token_of(key)?;
    if let Token::Literal(literal) = token {
        mutate(literal);
        return Ok(());
    }
    Err(anyhow!("Token is not a literal"))
}

fn find_matching_node<F>(keys: &Vec<TokenKey>, matcher: F) -> Option<TokenKey>
where
    F: Fn(&TokenKey) -> Option<TokenKey>,
{
    keys.iter().filter_map(matcher).next()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::Token;

    #[test]
    fn simple_exponent_term_is_differentiated_correctly() {
        // x^2 (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = vec![variable.clone(), Token::Literal(2f64), "^".parse().unwrap()];
        let tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        let actual_tree = find_derivative(tree, &variable).unwrap();

        // 2 * x (but in postfix notation)
        let expected_tokens = vec![
            Token::Literal(2f64),
            variable.clone(),
            Token::Literal(1f64),
            "^".parse().unwrap(),
            "*".parse().unwrap(),
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn complex_exponent_term_is_differentiated_correctly() {
        // 3 * x^4 (but in postfix notation)
        let variable = Token::Identifier("x".to_string());
        let tokens = vec![
            Token::Literal(3f64),
            variable.clone(),
            Token::Literal(4f64),
            "^".parse().unwrap(),
            "*".parse().unwrap(),
        ];
        let tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        let actual_tree = find_derivative(tree, &variable).unwrap();

        // 12 * x^3 (but in postfix notation)
        let expected_tokens = vec![
            Token::Literal(12f64),
            variable.clone(),
            Token::Literal(3f64),
            "^".parse().unwrap(),
            "*".parse().unwrap(),
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
            Token::Literal(4f64),
            "^".parse().unwrap(),
            Token::Literal(3f64),
            "*".parse().unwrap(),
        ];
        let tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        let actual_tree = find_derivative(tree, &variable).unwrap();

        // x^3 * 12 (but in postfix notation)
        let expected_tokens = vec![
            variable.clone(),
            Token::Literal(3f64),
            "^".parse().unwrap(),
            Token::Literal(12f64),
            "*".parse().unwrap(),
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }
}
