use crate::interpreter::token::Token;
use anyhow::{bail, Context, Result};
use ptree::{print_tree, TreeBuilder};
use std::io;
use std::rc::Rc;

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

    fn print(&self) -> io::Result<()> {
        let mut builder = TreeBuilder::new("expression".into());
        write_node(&self.root, &mut builder);
        print_tree(&builder.build())
    }
}

fn write_node(node: &TokenNode, builder: &mut TreeBuilder) {
    match &node.value {
        None => return,
        Some(value) => {
            let node_name = format!("{}", value);

            if node.is_leaf() {
                builder.add_empty_child(node_name);
                return;
            }

            builder.begin_child(node_name)
        }
    };
    match &node.left {
        None => {}
        Some(left_node) => write_node(&left_node, builder),
    };
    match &node.right {
        None => {}
        Some(right_node) => write_node(&right_node, builder),
    };
    builder.end_child();
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

    fn is_leaf(&self) -> bool {
        self.left.is_none() && self.right.is_none()
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
        let expected_tree = expected_tree;

        let actual_tree = ExpressionTree::new(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn complex_expression_returns_correct_tree() {
        let tokens = create_complex_tokens();
        let expected_tree = create_complex_tree();

        let actual_tree = ExpressionTree::new(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn print_succeeds() {
        let tree = create_complex_tree();

        tree.print().unwrap();
    }

    fn create_complex_tokens() -> Vec<Token> {
        // x + ((y + z) * a) (but in postfix notation)
        [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Identifier("z".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("a".to_string()),
            "*".parse().unwrap(),
            "+".parse().unwrap(),
        ]
        .to_vec()
    }

    fn create_complex_tree() -> ExpressionTree {
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

        expected_tree
    }
}
