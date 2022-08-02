use crate::interpreter::token::Token;
use anyhow::{anyhow, bail, Context, Result};
use std::rc::Rc;
use string_builder::Builder;

#[derive(PartialEq, Debug)]
pub struct ExpressionTree {
    root: TokenNode,
}

#[derive(PartialEq, Debug)]
struct TokenNode {
    value: Option<Token>,
    left: Option<Rc<TokenNode>>,
    right: Option<Rc<TokenNode>>,
}
impl ExpressionTree {
    /// Generates an expression tree based off of the given tokens.
    ///
    /// # Arguments
    ///
    /// * `postfix_tokens`: Tokens, ordered in postfix notation, to convert to an expression tree.
    ///
    /// returns: The generated expression tree.
    pub fn new(postfix_tokens: Vec<Token>) -> Result<ExpressionTree> {
        let mut tokens = postfix_tokens.clone();
        tokens.reverse();
        let mut operands: Vec<TokenNode> = Vec::new();

        while let Some(token) = tokens.pop() {
            match token {
                Token::Operator(_) => {
                    let second_operand = operands.pop().context("Expected a second operand")?;
                    let first_operand = operands.pop().context("Expected a first operand")?;

                    let mut operator_node = TokenNode::new(token);
                    operator_node.set_left(first_operand);
                    operator_node.set_right(second_operand);

                    operands.push(operator_node);
                }
                Token::Literal(_) | Token::Identifier(_) => operands.push(TokenNode::new(token)),
                Token::OpenParenthesis | Token::CloseParenthesis => {
                    bail!("There should not be any parenthesis present in the input")
                }
            }
        }

        Ok(ExpressionTree {
            root: operands.pop().context("No tree root found")?,
        })
    }
}

impl TokenNode {
    fn new(token: Token) -> TokenNode {
        TokenNode {
            value: Some(token),
            left: None,
            right: None,
        }
    }

    fn set_left(&mut self, node: TokenNode) {
        self.left = Some(Rc::new(node));
    }

    fn set_right(&mut self, node: TokenNode) {
        self.right = Some(Rc::new(node));
    }

    fn print(&self) -> String {
        if self.left.is_none() && self.right.is_none() {
            match &self.value {
                None => "None".to_string(),
                Some(value) => value.to_string(),
            }
        } else {
            let mut builder = Builder::default();
            match &self.value {
                None => {}
                Some(value) => builder.append(format!("{}: ", value)),
            }
            builder.append("[");
            match &self.left {
                None => {}
                Some(reference) => builder.append(reference.print()),
            }
            builder.append(",");
            match &self.right {
                None => {}
                Some(reference) => builder.append(reference.print()),
            }
            builder.append("]");
            return builder.string().unwrap_or("Error".to_string());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_expression_returns_correct_tree() {
        // x + y (but in postfix notation)
        let tokens = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            "+".parse().unwrap(),
        ]
        .to_vec();

        let mut expected_tree = ExpressionTree {
            root: TokenNode::new("+".parse().unwrap()),
        };
        expected_tree
            .root
            .set_left(TokenNode::new(Token::Identifier("x".into())));
        expected_tree
            .root
            .set_right(TokenNode::new(Token::Identifier("y".into())));

        let actual_tree = ExpressionTree::new(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn complex_expression_returns_correct_tree() {
        // x + ((y + z) * a) (but in postfix notation)
        let tokens = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Identifier("z".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("a".to_string()),
            "*".parse().unwrap(),
            "+".parse().unwrap(),
        ]
        .to_vec();

        let mut expected_tree = ExpressionTree {
            root: TokenNode::new("+".parse().unwrap()),
        };
        expected_tree
            .root
            .set_left(TokenNode::new(Token::Identifier("x".into())));

        let mut right_node = TokenNode::new("*".parse().unwrap());
        right_node.set_right(TokenNode::new(Token::Identifier("a".into())));

        let mut right_left_node = TokenNode::new("+".parse().unwrap());
        right_left_node.set_left(TokenNode::new(Token::Identifier("y".into())));
        right_left_node.set_right(TokenNode::new(Token::Identifier("z".into())));

        right_node.set_left(right_left_node);

        expected_tree.root.set_right(right_node);

        let actual_tree = ExpressionTree::new(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }
}
