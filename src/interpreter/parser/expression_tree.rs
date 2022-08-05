use crate::interpreter::token::Token;
use anyhow::{bail, Context, Result};
use ptree::{print_tree, TreeBuilder};
use slotmap::{new_key_type, SlotMap};

new_key_type! { struct TokenKey; }

#[derive(Debug)]
pub struct ExpressionTree {
    nodes: SlotMap<TokenKey, TokenNode>,
    root_key: TokenKey,
}

#[derive(Debug)]
pub struct TokenNode {
    token: Token,
    left: Option<TokenKey>,
    right: Option<TokenKey>,
}

impl PartialEq for ExpressionTree {
    fn eq(&self, other: &Self) -> bool {
        node_eq(self, other, Some(self.root()), Some(other.root()))
    }
}

fn node_eq(
    tree1: &ExpressionTree,
    tree2: &ExpressionTree,
    maybe_node1: Option<&TokenNode>,
    maybe_node2: Option<&TokenNode>,
) -> bool {
    if maybe_node1.is_none() && maybe_node2.is_none() {
        return true;
    }
    if maybe_node1.is_none() || maybe_node2.is_none() {
        return false;
    }
    let node1 = maybe_node1.expect("Value should logically exist by now");
    let node2 = maybe_node2.expect("Value should logically exist by now");
    if node1.token != node2.token {
        return false;
    }

    let node1_left = tree1.left_child_of(node1);
    let node1_right = tree1.right_child_of(node1);
    let node2_left = tree2.left_child_of(node2);
    let node2_right = tree2.right_child_of(node2);

    node_eq(tree1, tree2, node1_left, node2_left) && node_eq(tree1, tree2, node1_right, node2_right)
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
        let mut nodes = SlotMap::with_key();

        let mut tokens = postfix_tokens.clone();
        tokens.reverse();
        let mut operand_keys: Vec<TokenKey> = Vec::new();

        while let Some(token) = tokens.pop() {
            match token {
                Token::Operator(_) => {
                    let second_operand_key =
                        operand_keys.pop().context("Expected a second operand")?;
                    let first_operand_key =
                        operand_keys.pop().context("Expected a first operand")?;

                    let operator_node =
                        TokenNode::new_children(token, first_operand_key, second_operand_key);
                    let operator_key = nodes.insert(operator_node);

                    operand_keys.push(operator_key);
                }
                Token::Literal(_) | Token::Identifier(_) => {
                    let node = TokenNode::new(token);
                    let node_key = nodes.insert(node);
                    operand_keys.push(node_key);
                }
                Token::OpenParenthesis | Token::CloseParenthesis => {
                    bail!("There should not be any parenthesis present in the input")
                }
            }
        }

        let root_key = operand_keys.pop().context("No tree root found")?;
        Ok(ExpressionTree { nodes, root_key })
    }

    pub fn root(&self) -> &TokenNode {
        self.nodes
            .get(self.root_key)
            .expect("Tree is missing root node")
    }

    pub fn left_child_of(&self, node: &TokenNode) -> Option<&TokenNode> {
        let left_key = node.left?;
        self.nodes.get(left_key)
    }

    pub fn right_child_of(&self, node: &TokenNode) -> Option<&TokenNode> {
        let right_key = node.right?;
        self.nodes.get(right_key)
    }

    pub fn to_infix(&self) -> Vec<Token> {
        self.build_expression(self.root())
    }

    fn build_expression(&self, node: &TokenNode) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        if let Some(left_node) = self.left_child_of(node) {
            let mut subtree_tokens = self.build_expression_subtree(&node, &left_node);
            tokens.append(&mut subtree_tokens);
        }

        tokens.push(node.token.clone());

        if let Some(right_node) = self.right_child_of(node) {
            let mut subtree_tokens = self.build_expression_subtree(&node, &right_node);
            tokens.append(&mut subtree_tokens);
        }

        tokens
    }

    fn build_expression_subtree(&self, node: &TokenNode, left_node: &TokenNode) -> Vec<Token> {
        let mut subtree_tokens: Vec<Token> = Vec::new();
        if let Token::Operator(other_operator) = &left_node.token {
            let mut close_parentheses = false;
            if let Token::Operator(operator) = &node.token {
                // When a child operator has lower precedence, it and its operands needs
                // to be wrapped in parentheses.
                if operator > other_operator {
                    subtree_tokens.push(Token::OpenParenthesis);
                    close_parentheses = true;
                }
            }

            // Recurse so that the entire subtree will be contained within
            // any potential parentheses.
            let mut left_tokens = self.build_expression(&left_node);
            subtree_tokens.append(&mut left_tokens);

            if close_parentheses {
                subtree_tokens.push(Token::CloseParenthesis);
            }
        } else {
            subtree_tokens.push(left_node.token.clone());
        }
        subtree_tokens
    }

    pub fn print(&self) -> Result<()> {
        let mut builder = TreeBuilder::new("expression".into());
        self.write_node(self.root(), &mut builder);
        print_tree(&builder.build()).context("Failed to build tree string")
    }

    fn write_node(&self, node: &TokenNode, builder: &mut TreeBuilder) {
        let node_name = format!("{}", &node.token);

        if node.is_leaf() {
            builder.add_empty_child(node_name);
            return;
        }

        builder.begin_child(node_name);

        match self.left_child_of(node) {
            None => {}
            Some(left_node) => self.write_node(&left_node, builder),
        };
        match self.right_child_of(node) {
            None => {}
            Some(right_node) => self.write_node(&right_node, builder),
        };
        builder.end_child();
    }
}

impl TokenNode {
    fn new(token: Token) -> TokenNode {
        TokenNode {
            token,
            left: None,
            right: None,
        }
    }

    fn new_children(token: Token, left: TokenKey, right: TokenKey) -> TokenNode {
        TokenNode {
            token,
            left: Some(left),
            right: Some(right),
        }
    }

    fn set_left(&mut self, node_key: TokenKey) {
        self.left = Some(node_key);
    }

    fn set_right(&mut self, node_key: TokenKey) {
        self.right = Some(node_key);
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
        let mut nodes = SlotMap::with_key();
        let x = nodes.insert(TokenNode::new(Token::Identifier("x".into())));
        let y = nodes.insert(TokenNode::new(Token::Identifier("y".into())));
        let plus = nodes.insert(TokenNode::new_children("+".parse().unwrap(), x, y));

        ExpressionTree {
            nodes,
            root_key: plus,
        }
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
        let mut nodes = SlotMap::with_key();
        let x = nodes.insert(TokenNode::new(Token::Identifier("x".into())));
        let y = nodes.insert(TokenNode::new(Token::Identifier("y".into())));
        let z = nodes.insert(TokenNode::new(Token::Identifier("z".into())));
        let a = nodes.insert(TokenNode::new(Token::Identifier("a".into())));
        let second_plus = nodes.insert(TokenNode::new_children("+".parse().unwrap(), y, z));
        let star = nodes.insert(TokenNode::new_children(
            "*".parse().unwrap(),
            second_plus,
            a,
        ));
        let first_plus = nodes.insert(TokenNode::new_children("+".parse().unwrap(), x, star));

        ExpressionTree {
            nodes,
            root_key: first_plus,
        }
    }
}
