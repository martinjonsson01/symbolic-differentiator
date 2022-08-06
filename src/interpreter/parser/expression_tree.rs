use crate::interpreter::token::Token;
use anyhow::{anyhow, bail, Context, Result};
use ptree::{write_tree, TreeBuilder};
use slotmap::{new_key_type, SlotMap};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

new_key_type! { pub struct TokenKey; }

#[derive(Debug)]
struct Empty {}

#[derive(Debug)]
pub struct Valid {
    root_key: TokenKey,
}

pub struct ExpressionTree<S: Debug> {
    nodes: SlotMap<TokenKey, TokenNode>,
    state: S,
}

#[derive(Debug, Clone)]
struct TokenNode {
    token: Token,
    parent: Option<TokenKey>,
    left: Option<TokenKey>,
    right: Option<TokenKey>,
}

impl PartialEq for ExpressionTree<Valid> {
    fn eq(&self, other: &Self) -> bool {
        node_eq(self, other, Some(self.root_key()), Some(other.root_key()))
    }
}

fn node_eq(
    tree1: &ExpressionTree<Valid>,
    tree2: &ExpressionTree<Valid>,
    maybe_node1: Option<TokenKey>,
    maybe_node2: Option<TokenKey>,
) -> bool {
    if maybe_node1.is_none() && maybe_node2.is_none() {
        return true;
    }
    if maybe_node1.is_none() || maybe_node2.is_none() {
        return false;
    }
    let node1 = maybe_node1.expect("Value should logically exist by now");
    let node2 = maybe_node2.expect("Value should logically exist by now");
    if let Ok(token1) = tree1.token_of(node1) {
        if let Ok(token2) = tree2.token_of(node2) {
            if token1 != token2 {
                return false;
            }
        }
    }

    let node1_left = tree1.left_child_of(node1);
    let node1_right = tree1.right_child_of(node1);
    let node2_left = tree2.left_child_of(node2);
    let node2_right = tree2.right_child_of(node2);

    node_eq(tree1, tree2, node1_left, node2_left) && node_eq(tree1, tree2, node1_right, node2_right)
}

impl<S: Debug> ExpressionTree<S> {
    fn empty() -> ExpressionTree<Empty> {
        ExpressionTree {
            nodes: SlotMap::with_key(),
            state: Empty {},
        }
    }

    /// Generates an expression tree based off of the given tokens.
    ///
    /// # Arguments
    ///
    /// * `postfix_tokens`: Tokens, ordered in postfix notation, to convert to an expression tree.
    ///
    /// returns: The generated expression tree.
    pub fn new(postfix_tokens: Vec<Token>) -> Result<ExpressionTree<Valid>> {
        let mut tree = Self::empty();

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

                    let operator_key =
                        tree.add_node_children(token, first_operand_key, second_operand_key)?;

                    operand_keys.push(operator_key);
                }
                Token::Literal(_) | Token::Identifier(_) => operand_keys.push(tree.add_node(token)),
                Token::OpenParenthesis | Token::CloseParenthesis => {
                    bail!("There should not be any parenthesis present in the input")
                }
            }
        }

        let root_key = operand_keys.pop().context("No tree root found")?;
        let valid_tree = tree.set_root(root_key);
        Ok(valid_tree)
    }

    pub fn set_root(self, new_root: TokenKey) -> ExpressionTree<Valid> {
        // TODO: implement "root-node" that is always present, with a single child
        // TODO: being the start of the real tree 
        ExpressionTree {
            nodes: self.nodes,
            state: Valid { root_key: new_root },
        }
    }

    pub fn set_parent(&mut self, key: TokenKey, parent: TokenKey) -> Result<()> {
        let node = self
            .nodes
            .get_mut(key)
            .context("Could not find key in tree")?;
        node.set_parent(parent);
        Ok(())
    }

    pub fn left_child_of(&self, node: TokenKey) -> Option<TokenKey> {
        let node = self.nodes.get(node)?;
        node.left
    }

    pub fn right_child_of(&self, node: TokenKey) -> Option<TokenKey> {
        let node = self.nodes.get(node)?;
        node.right
    }

    pub fn add_node(&mut self, token: Token) -> TokenKey {
        let node = TokenNode::new(token);
        self.nodes.insert(node)
    }

    pub fn add_node_children(
        &mut self,
        token: Token,
        left: TokenKey,
        right: TokenKey,
    ) -> Result<TokenKey> {
        let node = TokenNode::new_children(token, left, right);
        let parent_key = self.nodes.insert(node);
        self.set_parent(left, parent_key)?;
        self.set_parent(right, parent_key)?;
        Ok(parent_key)
    }
}

impl ExpressionTree<Valid> {
    pub fn root_key(&self) -> TokenKey {
        self.state.root_key
    }

    pub fn token_of(&self, key: TokenKey) -> Result<&Token> {
        let node = self.node_of(key)?;
        Ok(&node.token)
    }

    pub fn mut_token_of(&mut self, key: TokenKey) -> Result<&mut Token> {
        let node = self.mut_node_of(key)?;
        Ok(&mut node.token)
    }

    fn node_of(&self, key: TokenKey) -> Result<&TokenNode> {
        let node = self.nodes.get(key).context("Could not find node in tree")?;
        Ok(&node)
    }

    fn mut_node_of(&mut self, key: TokenKey) -> Result<&mut TokenNode> {
        let node = self
            .nodes
            .get_mut(key)
            .context("Could not find node in tree")?;
        Ok(node)
    }

    pub fn clone_node_of(&mut self, key: TokenKey) -> Result<TokenKey> {
        let node = self.node_of(key)?;
        let cloned = node.clone();
        let cloned_key = self.nodes.insert(cloned);
        Ok(cloned_key)
    }

    pub fn is_leaf(&self, key: TokenKey) -> bool {
        match self.nodes.get(key) {
            None => false,
            Some(node) => node.left.is_none() && node.right.is_none(),
        }
    }

    pub fn get_parent_of(&self, key: TokenKey) -> Result<TokenKey> {
        let node = self.node_of(key)?;
        node.parent.context("Node has not parent")
    }
    
    pub fn replace_child_of(&mut self, key: TokenKey, old_child: TokenKey, new_child: TokenKey) -> Result<()> {
        let node = self.mut_node_of(key)?;
        let child_ref = if node.left == Some(old_child) {
            &mut node.left
        } else if node.right == Some(old_child){
            &mut node.right
        } else {
            return Err(anyhow!("old_child is not a child of the given node"))
        };
        *child_ref = Some(new_child);
        Ok(())
    }

    pub fn to_infix(&self) -> Result<Vec<Token>> {
        self.build_expression(self.root_key())
    }

    fn build_expression(&self, node: TokenKey) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

        if let Some(left_node) = self.left_child_of(node) {
            let mut subtree_tokens = self.build_expression_subtree(node, left_node)?;
            tokens.append(&mut subtree_tokens);
        }

        let token = self.token_of(node)?;
        tokens.push(token.clone());

        if let Some(right_node) = self.right_child_of(node) {
            let mut subtree_tokens = self.build_expression_subtree(node, right_node)?;
            tokens.append(&mut subtree_tokens);
        }

        Ok(tokens)
    }

    fn build_expression_subtree(&self, node: TokenKey, child_node: TokenKey) -> Result<Vec<Token>> {
        let mut subtree_tokens: Vec<Token> = Vec::new();
        if let Ok(Token::Operator(other_operator)) = self.token_of(child_node) {
            let mut close_parentheses = false;
            if let Ok(Token::Operator(operator)) = self.token_of(node) {
                // When a child operator has lower precedence, it and its operands needs
                // to be wrapped in parentheses.
                if operator > other_operator {
                    subtree_tokens.push(Token::OpenParenthesis);
                    close_parentheses = true;
                }
            }

            // Recurse so that the entire subtree will be contained within
            // any potential parentheses.
            let mut child_tokens = self.build_expression(child_node)?;
            subtree_tokens.append(&mut child_tokens);

            if close_parentheses {
                subtree_tokens.push(Token::CloseParenthesis);
            }
        } else {
            let child_token = self.token_of(child_node)?;
            subtree_tokens.push(child_token.clone());
        }
        Ok(subtree_tokens)
    }

    fn write_node(&self, node: TokenKey, builder: &mut TreeBuilder) {
        let node_name = match self.token_of(node) {
            Err(_) => return,
            Ok(token) => {
                format!("{}", token)
            }
        };

        if self.is_leaf(node) {
            builder.add_empty_child(node_name);
            return;
        }

        builder.begin_child(node_name);

        match self.left_child_of(node) {
            None => {}
            Some(left_node) => self.write_node(left_node, builder),
        };
        match self.right_child_of(node) {
            None => {}
            Some(right_node) => self.write_node(right_node, builder),
        };
        builder.end_child();
    }
}

impl Display for ExpressionTree<Valid> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format_tree(self, f)
    }
}

impl Debug for ExpressionTree<Valid> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        format_tree(self, f)
    }
}

fn format_tree(tree: &ExpressionTree<Valid>, f: &mut Formatter<'_>) -> fmt::Result {
    let mut builder = TreeBuilder::new("expression".into());
    tree.write_node(tree.root_key(), &mut builder);
    let mut buffer: Vec<u8> = Vec::new();
    match write_tree(&builder.build(), &mut buffer) {
        Ok(_) => {}
        Err(_) => return Err(fmt::Error),
    }
    let text = match std::str::from_utf8(&buffer) {
        Ok(text) => text,
        Err(_) => return Err(fmt::Error),
    };
    f.write_str(text)
}

impl TokenNode {
    fn new(token: Token) -> TokenNode {
        TokenNode {
            token,
            parent: None,
            left: None,
            right: None,
        }
    }

    fn new_children(token: Token, left: TokenKey, right: TokenKey) -> TokenNode {
        TokenNode {
            token,
            parent: None,
            left: Some(left),
            right: Some(right),
        }
    }

    fn set_parent(&mut self, node_key: TokenKey) {
        self.parent = Some(node_key);
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

        let actual_tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn complex_expression_returns_correct_tree() {
        let tokens = create_complex_postfix_tokens();
        let expected_tree = create_complex_tree();

        let actual_tree = ExpressionTree::<Valid>::new(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn print_succeeds() {
        let tree = create_complex_tree();

        print!("{}", tree);
    }

    #[test]
    fn simple_tree_converts_back_to_simple_expression() {
        let expected_tokens = create_simple_infix_tokens();
        let tree = create_simple_tree();

        let actual_tokens = tree.to_infix().unwrap();

        assert_eq!(actual_tokens, expected_tokens);
    }

    #[test]
    fn complex_tree_converts_back_to_simple_expression() {
        let expected_tokens = create_complex_infix_tokens();
        let tree = create_complex_tree();

        let actual_tokens = tree.to_infix().unwrap();

        assert_eq!(actual_tokens, expected_tokens);
    }

    fn create_simple_tree() -> ExpressionTree<Valid> {
        let mut nodes = SlotMap::with_key();
        let x = nodes.insert(TokenNode::new(Token::Identifier("x".into())));
        let y = nodes.insert(TokenNode::new(Token::Identifier("y".into())));
        let plus = nodes.insert(TokenNode::new_children("+".parse().unwrap(), x, y));

        ExpressionTree {
            nodes,
            state: Valid { root_key: plus },
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

    fn create_complex_tree() -> ExpressionTree<Valid> {
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
            state: Valid {
                root_key: first_plus,
            },
        }
    }
}
