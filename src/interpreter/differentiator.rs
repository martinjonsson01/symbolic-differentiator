use crate::interpreter::operator::Operator;
use crate::interpreter::parser::expression_tree::{ExpressionTree, TokenKey, Valid};
use crate::interpreter::{find_matching_node, Token};
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
    match tree.token_of(node) {
        Ok(Token::Operator(operator)) => {
            let left_child = tree
                .left_child_of(node)
                .context("Expected a left operand")?;
            let right_child = tree
                .right_child_of(node)
                .context("Expected a right operand")?;
            let children = vec![left_child, right_child];

            return if operator.symbol == "^" {
                let base = tree.token_of(left_child)?;
                if base != with_respect_to {
                    bail!("Can not differentiate with respect to this variable")
                }

                let exponent = tree.clone_node_of(right_child)?;
                let multiply_node = tree.add_node_children("*".parse().unwrap(), exponent, node)?;

                mutate_literal(tree, right_child, |exponent| *exponent -= 1f64)?;

                Ok(multiply_node)
            } else if operator.symbol == "*" {
                find_matching_node(tree, &children, |token| token.is_value())
                    .context("Expected a value child")?;
                let caret_key = find_matching_node(tree, &children, |token| token.is_caret())
                    .context("Expected an exponentiation child")?;

                let new_root = differentiate_subtree(tree, caret_key, with_respect_to)?;
                tree.replace_child_of(node, caret_key, new_root)?;
                Ok(node)
            } else {
                Err(anyhow!("Could not differentiate expression"))
            };
        }
        Ok(Token::Identifier(variable_name)) => {
            if *variable_name == with_respect_to.to_string() {
                let one = Token::Literal(1f64);
                let one_node = tree.add_node(one);
                Ok(one_node)
            } else {
                Ok(node)
            }
        }
        _ => bail!("The given node is not an operator token in the given expression tree"),
    }
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

        // 3 * (4 * x^3) (but in postfix notation)
        let expected_tokens = vec![
            Token::Literal(3f64),
            Token::Literal(4f64),
            variable.clone(),
            Token::Literal(3f64),
            "^".parse().unwrap(),
            "*".parse().unwrap(),
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

        // (4 * x^3) * 3 (but in postfix notation)
        let expected_tokens = vec![
            Token::Literal(4f64),
            variable.clone(),
            Token::Literal(3f64),
            "^".parse().unwrap(),
            "*".parse().unwrap(),
            Token::Literal(3f64),
            "*".parse().unwrap(),
        ];
        let expected_tree = ExpressionTree::<Valid>::new(expected_tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }
}
