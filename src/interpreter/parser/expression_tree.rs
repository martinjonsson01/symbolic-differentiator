use crate::interpreter::token::Token;
use anyhow::{bail, Context, Result};
use ptree::{print_tree, TreeBuilder};
use std::io;
use std::rc::Rc;

#[derive(PartialEq, Debug)]
pub struct ExpressionTree {
    root: TokenNode,
}

#[derive(Eq, PartialEq, Copy, Clone)]
pub(self) enum TraverseOrder {
    PreOrder,
    InOrder,
    PostOrder,
}

#[derive(PartialEq, Debug)]
struct TokenNode {
    value: Token,
    left: Option<Rc<TokenNode>>,
    right: Option<Rc<TokenNode>>,
}

struct Wrapper<F: FnMut(&Token)> {
    function: F,
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

    fn traverse<F>(&self, order: TraverseOrder, wrapper: &mut Wrapper<F>)
    where
        F: FnMut(&Token),
    {
        Self::traverse_nodes(&self.root, order, wrapper);
    }

    fn traverse_nodes<F>(node: &TokenNode, order: TraverseOrder, wrapper: &mut Wrapper<F>)
    where
        F: FnMut(&Token),
    {
        if order == TraverseOrder::PreOrder {
            (wrapper.function)(&node.value);
        }
        match &node.left {
            None => {}
            Some(left_node) => Self::traverse_nodes(&left_node, order, wrapper),
        };
        if order == TraverseOrder::InOrder {
            (wrapper.function)(&node.value);
        }
        match &node.right {
            None => {}
            Some(right_node) => Self::traverse_nodes(&right_node, order, wrapper),
        };
        if order == TraverseOrder::PostOrder {
            (wrapper.function)(&node.value);
        }
    }

    pub fn to_infix(&self) -> Vec<Token> {
        Self::build_expression(&self.root)
    }

    fn build_expression(node: &TokenNode) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        if let Some(left_node) = &node.left {
            let mut subtree_tokens = Self::build_expression_subtree(&node, &left_node);
            tokens.append(&mut subtree_tokens);
        }

        tokens.push(node.value.clone());

        if let Some(right_node) = &node.right {
            let mut subtree_tokens = Self::build_expression_subtree(&node, &right_node);
            tokens.append(&mut subtree_tokens);
        }

        tokens
    }

    fn build_expression_subtree(node: &TokenNode, left_node: &TokenNode) -> Vec<Token> {
        let mut subtree_tokens: Vec<Token> = Vec::new();
        if let Token::Operator(other_operator) = &left_node.value {
            let mut close_parentheses = false;
            if let Token::Operator(operator) = &node.value {
                // When a child operator has lower precedence, it and its operands needs
                // to be wrapped in parentheses.
                if operator > other_operator {
                    subtree_tokens.push(Token::OpenParenthesis);
                    close_parentheses = true;
                }
            }

            // Recurse so that the entire subtree will be contained within
            // any potential parentheses.
            let mut left_tokens = Self::build_expression(&left_node);
            subtree_tokens.append(&mut left_tokens);

            if close_parentheses {
                subtree_tokens.push(Token::CloseParenthesis);
            }
        } else {
            subtree_tokens.push(left_node.value.clone());
        }
        subtree_tokens
    }

    fn print(&self) -> io::Result<()> {
        let mut builder = TreeBuilder::new("expression".into());
        write_node(&self.root, &mut builder);
        print_tree(&builder.build())
    }
}

fn write_node(node: &TokenNode, builder: &mut TreeBuilder) {
    let node_name = format!("{}", &node.value);

    if node.is_leaf() {
        builder.add_empty_child(node_name);
        return;
    }

    builder.begin_child(node_name);

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
            value: token,
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
        let tokens = create_simple_postfix_tokens();
        let expected_tree = create_simple_tree();

        let actual_tree = ExpressionTree::new(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn complex_expression_returns_correct_tree() {
        let tokens = create_complex_postfix_tokens();
        let expected_tree = create_complex_tree();

        let actual_tree = ExpressionTree::new(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn print_succeeds() {
        let tree = create_complex_tree();

        tree.print().unwrap();
    }

    #[test]
    fn traverse_preorder_returns_nodes_in_preorder() {
        let tree = create_complex_tree();
        let expected_order = vec![
            "+".to_string(),
            "x".to_string(),
            "*".to_string(),
            "+".to_string(),
            "y".to_string(),
            "z".to_string(),
            "a".to_string(),
        ];

        let mut actual_order = Vec::new();
        let collect = |token: &Token| actual_order.push(token.to_string());
        let mut wrapper = Wrapper { function: collect };
        tree.traverse(TraverseOrder::PreOrder, &mut wrapper);

        assert_eq!(actual_order, expected_order);
    }

    #[test]
    fn traverse_inorder_returns_nodes_in_order() {
        let tree = create_complex_tree();
        let expected_order = vec![
            "x".to_string(),
            "+".to_string(),
            "y".to_string(),
            "+".to_string(),
            "z".to_string(),
            "*".to_string(),
            "a".to_string(),
        ];

        let mut actual_order = Vec::new();
        let collect = |token: &Token| actual_order.push(token.to_string());
        let mut wrapper = Wrapper { function: collect };
        tree.traverse(TraverseOrder::InOrder, &mut wrapper);

        assert_eq!(actual_order, expected_order);
    }

    #[test]
    fn traverse_postorder_returns_nodes_in_postorder() {
        let tree = create_complex_tree();
        let expected_order = vec![
            "x".to_string(),
            "y".to_string(),
            "z".to_string(),
            "+".to_string(),
            "a".to_string(),
            "*".to_string(),
            "+".to_string(),
        ];

        let mut actual_order = Vec::new();
        let collect = |token: &Token| actual_order.push(token.to_string());
        let mut wrapper = Wrapper { function: collect };
        tree.traverse(TraverseOrder::PostOrder, &mut wrapper);

        assert_eq!(actual_order, expected_order);
    }

    #[test]
    fn simple_tree_converts_back_to_simple_expression() {
        let expected_tokens = create_simple_infix_tokens();
        let tree = create_simple_tree();

        let actual_tokens = tree.to_infix();

        assert_eq!(actual_tokens, expected_tokens);
    }

    #[test]
    fn complex_tree_converts_back_to_simple_expression() {
        let expected_tokens = create_complex_infix_tokens();
        let tree = create_complex_tree();

        let actual_tokens = tree.to_infix();

        assert_eq!(actual_tokens, expected_tokens);
    }

    fn create_simple_tree() -> ExpressionTree {
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
        expected_tree
    }

    fn create_simple_postfix_tokens() -> Vec<Token> {
        // x + y (but in postfix notation)
        let tokens = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            "+".parse().unwrap(),
        ]
        .to_vec();
        tokens
    }

    fn create_simple_infix_tokens() -> Vec<Token> {
        // x + y
        let tokens = [
            Token::Identifier("x".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("y".to_string()),
        ]
        .to_vec();
        tokens
    }

    fn create_complex_postfix_tokens() -> Vec<Token> {
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

    fn create_complex_infix_tokens() -> Vec<Token> {
        // x + (y + z) * a
        [
            Token::Identifier("x".to_string()),
            "+".parse().unwrap(),
            Token::OpenParenthesis,
            Token::Identifier("y".to_string()),
            "+".parse().unwrap(),
            Token::Identifier("z".to_string()),
            Token::CloseParenthesis,
            "*".parse().unwrap(),
            Token::Identifier("a".to_string()),
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
