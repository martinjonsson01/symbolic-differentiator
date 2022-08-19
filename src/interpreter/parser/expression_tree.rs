use crate::interpreter::operator::Operator;
use crate::interpreter::token::Token;
use anyhow::{anyhow, bail, Context, Result};
use itertools::Itertools;
use ptree::{write_tree, TreeBuilder};
use slotmap::{new_key_type, SlotMap};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

new_key_type! { pub struct NodeKey; }

#[derive(Debug)]
struct Empty {}

#[derive(Debug)]
pub struct Valid {
    root_key: NodeKey,
}

pub struct ExpressionTree<S: Debug> {
    nodes: SlotMap<NodeKey, Node>,
    state: S,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompositeData {
    pub(crate) operator: Operator,
    pub(crate) inverse_operator: Operator,
    pub(crate) left: Vec<NodeKey>,
    pub(crate) right: Vec<NodeKey>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CompositeChild {
    Left,
    Right,
}

impl CompositeData {
    pub fn is_summation(&self) -> bool {
        self.operator == Operator::Add && self.inverse_operator == Operator::Subtract
    }

    pub fn is_fraction(&self) -> bool {
        self.operator == Operator::Multiply && self.inverse_operator == Operator::Divide
    }

    pub fn child(&self, which: CompositeChild) -> &[NodeKey] {
        match which {
            CompositeChild::Left => &self.left,
            CompositeChild::Right => &self.right,
        }
    }

    fn node_name(&self) -> String {
        match self.operator {
            Operator::Add => "Summation".into(),
            Operator::Multiply => "Fraction".into(),
            _ => "Unknown".into(),
        }
    }

    fn left_node_name(&self) -> String {
        self.operator.to_string()
    }

    fn right_node_name(&self) -> String {
        self.inverse_operator.to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Node {
    // Terminal symbols (leaves)
    LiteralInteger(i32),
    Identifier(String),
    // Non-terminal symbols (non-leaves)
    Composite(CompositeData),
    BinaryOperation {
        operator: Operator,
        left_operand: NodeKey,
        right_operand: NodeKey,
    },
}

impl Node {
    pub fn new_literal_integer(value: i32) -> Node {
        Node::LiteralInteger(value)
    }

    pub fn new_identifier(name: String) -> Node {
        Node::Identifier(name)
    }

    pub fn new_binary_addition(left_operand: NodeKey, right_operand: NodeKey) -> Node {
        Self::new_composite_summation(vec![left_operand, right_operand], Vec::new())
    }

    pub fn new_binary_subtraction(left_operand: NodeKey, right_operand: NodeKey) -> Node {
        Self::new_composite_summation(vec![left_operand], vec![right_operand])
    }

    pub fn new_binary_multiplication(left_operand: NodeKey, right_operand: NodeKey) -> Node {
        Self::new_composite_fraction(vec![left_operand, right_operand], Vec::new())
    }

    pub fn new_binary_division(left_operand: NodeKey, right_operand: NodeKey) -> Node {
        Self::new_composite_fraction(vec![left_operand], vec![right_operand])
    }

    pub fn new_binary_exponentiation(left_operand: NodeKey, right_operand: NodeKey) -> Node {
        Node::BinaryOperation {
            operator: Operator::Exponentiate,
            left_operand,
            right_operand,
        }
    }

    pub fn new_composite_summation(left: Vec<NodeKey>, right: Vec<NodeKey>) -> Node {
        Node::Composite(CompositeData {
            operator: Operator::Add,
            inverse_operator: Operator::Subtract,
            left,
            right,
        })
    }

    pub fn new_composite_fraction(left: Vec<NodeKey>, right: Vec<NodeKey>) -> Node {
        Node::Composite(CompositeData {
            operator: Operator::Multiply,
            inverse_operator: Operator::Divide,
            left,
            right,
        })
    }

    pub fn is_specific_operator(&self, _check_operator: Operator) -> bool {
        matches!(
            self,
            Node::BinaryOperation {
                operator: _check_operator,
                ..
            }
        )
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Node::BinaryOperation { .. }) || matches!(self, Node::Composite { .. })
    }

    pub fn try_get_operator(&self) -> Option<Operator> {
        match self {
            Node::LiteralInteger(_) | Node::Identifier(_) => None,
            Node::Composite(CompositeData { operator, .. })
            | Node::BinaryOperation { operator, .. } => Some(*operator),
        }
    }

    pub fn is_value(&self) -> bool {
        matches!(self, Node::Identifier(_) | Node::LiteralInteger(_))
    }

    pub fn is_literal_integer(&self, compare_to: i32) -> bool {
        match self {
            Node::LiteralInteger(value) => *value == compare_to,
            _ => false,
        }
    }

    pub fn is_identifier(&self, compare_to: &str) -> bool {
        match self {
            Node::Identifier(name) => *name == compare_to,
            _ => false,
        }
    }

    pub fn get_operator(&self) -> Option<Operator> {
        match self {
            Node::LiteralInteger(_) | Node::Identifier(_) => None,
            Node::Composite(CompositeData { operator, .. }) => Some(*operator),
            Node::BinaryOperation { operator, .. } => Some(*operator),
        }
    }
}

impl PartialEq for ExpressionTree<Valid> {
    fn eq(&self, other: &Self) -> bool {
        node_eq(self, other, self.root_key(), other.root_key())
    }
}

fn node_eq(
    tree1: &ExpressionTree<Valid>,
    tree2: &ExpressionTree<Valid>,
    key1: NodeKey,
    key2: NodeKey,
) -> bool {
    let maybe_node1 = tree1.get_node(key1);
    let maybe_node2 = tree2.get_node(key2);
    if maybe_node1.is_none() && maybe_node2.is_none() {
        return true;
    }
    if maybe_node1.is_none() || maybe_node2.is_none() {
        return false;
    }
    let node1 = maybe_node1.expect("Value should logically exist by now");
    let node2 = maybe_node2.expect("Value should logically exist by now");

    return match (node1, node2) {
        (Node::LiteralInteger(value1), Node::LiteralInteger(value2)) => value1 == value2,
        (Node::Identifier(name1), Node::Identifier(name2)) => name1 == name2,
        (
            Node::Composite(CompositeData {
                left: first1,
                right: second1,
                ..
            }),
            Node::Composite(CompositeData {
                left: first2,
                right: second2,
                ..
            }),
        ) => {
            if first1.len() != first2.len() {
                return false;
            }
            if second1.len() != second2.len() {
                return false;
            }
            return nodes_eq(tree1, tree2, first1.iter(), first2.iter())
                && nodes_eq(tree1, tree2, second1.iter(), second2.iter());
        }
        (
            Node::BinaryOperation {
                operator: operator1,
                left_operand: left_operand1,
                right_operand: right_operand1,
            },
            Node::BinaryOperation {
                operator: operator2,
                left_operand: left_operand2,
                right_operand: right_operand2,
            },
        ) => {
            if operator1 != operator2 {
                return false;
            }

            node_eq(tree1, tree2, *left_operand1, *left_operand2)
                && node_eq(tree1, tree2, *right_operand1, *right_operand2)
        }
        _ => false, // node1 and node2 are different variants
    };
}

fn nodes_eq<'a>(
    tree1: &ExpressionTree<Valid>,
    tree2: &ExpressionTree<Valid>,
    keys1: impl Iterator<Item = &'a NodeKey>,
    keys2: impl Iterator<Item = &'a NodeKey>,
) -> bool {
    keys1
        .zip(keys2)
        .map(|(key1, key2)| node_eq(tree1, tree2, *key1, *key2))
        .all(|equal| equal)
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
    pub fn new(mut tokens: Vec<Token>) -> Result<ExpressionTree<Valid>> {
        let mut tree = Self::empty();

        tokens.reverse();
        let mut operand_keys: Vec<NodeKey> = Vec::new();

        while let Some(token) = tokens.pop() {
            match token {
                Token::LiteralInteger(value) => {
                    operand_keys.push(tree.add_node(Node::new_literal_integer(value)))
                }
                Token::Identifier(name) => {
                    operand_keys.push(tree.add_node(Node::new_identifier(name)))
                }
                Token::LeftParentheses | Token::RightParentheses => {
                    bail!("There should not be any parenthesis present in the input")
                }
                _ => {
                    let operand_two = operand_keys.pop().context("Expected a second operand")?;
                    let operand_one = operand_keys.pop().context("Expected a first operand")?;

                    let node = match token {
                        Token::Plus => Node::new_binary_addition(operand_one, operand_two),
                        Token::Dash => Node::new_binary_subtraction(operand_one, operand_two),
                        Token::Asterisk => Node::new_binary_multiplication(operand_one, operand_two),
                        Token::ForwardSlash => Node::new_binary_division(operand_one, operand_two),
                        Token::Caret => Node::new_binary_exponentiation(operand_one, operand_two),
                        _ => bail!("Should be unreachable. If this occurs, check the other match-expression above.")
                    };
                    operand_keys.push(tree.add_node(node));
                }
            }
        }

        let root_key = operand_keys.pop().context("No tree root found")?;
        let valid_tree = tree.set_root(root_key);
        Ok(valid_tree)
    }

    pub fn set_root(self, new_root: NodeKey) -> ExpressionTree<Valid> {
        // TODO: implement "root-node" that is always present, with a single child
        // TODO: being the start of the real tree
        ExpressionTree {
            nodes: self.nodes,
            state: Valid { root_key: new_root },
        }
    }

    pub fn add_node(&mut self, node: Node) -> NodeKey {
        self.nodes.insert(node)
    }
}

impl ExpressionTree<Valid> {
    pub fn root_key(&self) -> NodeKey {
        self.state.root_key
    }

    pub fn get_node(&self, key: NodeKey) -> Option<&Node> {
        self.nodes.get(key)
    }

    pub fn get_mut_node(&mut self, key: NodeKey) -> Option<&mut Node> {
        self.nodes.get_mut(key)
    }

    pub fn clone_node_of(&mut self, key: NodeKey) -> Result<NodeKey> {
        let node = self.get_node(key).context("Could not find node in tree")?;
        let cloned = node.clone();
        let cloned_key = self.nodes.insert(cloned);
        Ok(cloned_key)
    }

    pub fn replace_child_of(
        &mut self,
        key: NodeKey,
        old_child: NodeKey,
        new_child: NodeKey,
    ) -> Result<()> {
        let node = self
            .get_mut_node(key)
            .context("Expected node to exist in tree")?;
        match node {
            Node::LiteralInteger(_) | Node::Identifier(_) => {
                Err(anyhow!("Value nodes do not have children"))
            }
            Node::Composite(CompositeData {
                ref mut left,
                ref mut right,
                ..
            }) => {
                let child_ref = left
                    .iter_mut()
                    .chain(right.iter_mut())
                    .find(|key| **key == old_child)
                    .context("Could not find child of node")?;
                *child_ref = new_child;
                Ok(())
            }
            Node::BinaryOperation {
                ref mut left_operand,
                ref mut right_operand,
                ..
            } => {
                let child_ref = if left_operand == &old_child {
                    left_operand
                } else if right_operand == &old_child {
                    right_operand
                } else {
                    return Err(anyhow!("Old child is not a child of the given node"));
                };
                *child_ref = new_child;
                Ok(())
            }
        }
    }

    pub fn to_infix(&self) -> Result<Vec<Token>> {
        let root_node = self
            .get_node(self.root_key())
            .context("Expected a root node")?;
        self.build_expression(None, root_node)
    }

    fn build_expression(&self, parent_node: Option<&Node>, node: &Node) -> Result<Vec<Token>> {
        match node {
            Node::LiteralInteger(value) => Ok(vec![Token::LiteralInteger(*value)]),
            Node::Identifier(name) => Ok(vec![Token::Identifier(name.to_string())]),
            Node::Composite(CompositeData {
                operator: Operator::Add,
                left: add,
                right: subtract,
                ..
            }) => {
                let mut tokens = Vec::new();
                let mut add_tokens = self.build_group_tokens(Some(node), add.iter(), Token::Plus);
                let mut subtract_tokens =
                    self.build_group_tokens(Some(node), subtract.iter(), Token::Dash);

                Self::parenthesize_if_precedence(
                    parent_node,
                    Operator::Add,
                    &mut tokens,
                    |tokens| {
                        tokens.append(&mut add_tokens);
                        if !subtract_tokens.is_empty() {
                            tokens.push(Token::Dash);
                            tokens.append(&mut subtract_tokens);
                        }
                    },
                );

                Ok(tokens)
            }
            Node::Composite(CompositeData {
                operator: Operator::Multiply,
                left: multiply,
                right: divide,
                ..
            }) => {
                let divides_operator = self.get_child_operator(divide);
                let mut tokens = Vec::new();
                let mut multiply_tokens =
                    self.build_group_tokens(Some(node), multiply.iter(), Token::Asterisk);
                let mut divide_tokens =
                    self.build_group_tokens(Some(node), divide.iter(), Token::Asterisk);

                let build_interior = |tokens: &mut Vec<Token>| {
                    tokens.append(&mut multiply_tokens);
                    if let Some(divides_operator) = divides_operator {
                        tokens.push(Token::ForwardSlash);

                        Self::parenthesize_if_precedence(
                            Some(node),
                            divides_operator,
                            tokens,
                            |tokens| tokens.append(&mut divide_tokens),
                        );
                    } else if !divide_tokens.is_empty() {
                        tokens.push(Token::ForwardSlash);
                        tokens.append(&mut divide_tokens);
                    }
                };
                Self::parenthesize_if_precedence(
                    parent_node,
                    Operator::Multiply,
                    &mut tokens,
                    build_interior,
                );

                Ok(tokens)
            }
            Node::Composite(_) => Err(anyhow!("This composite node has not been implemented yet")),
            Node::BinaryOperation {
                operator,
                left_operand,
                right_operand,
            } => {
                let mut tokens = Vec::new();
                let left_node = self
                    .get_node(*left_operand)
                    .context("Expected left operand to exist")?;
                let right_node = self
                    .get_node(*right_operand)
                    .context("Expected right operand to exist")?;

                let mut left_tokens = self.build_expression(Some(node), left_node)?;
                let mut right_tokens = self.build_expression(Some(node), right_node)?;

                Self::parenthesize_if_precedence(
                    parent_node,
                    Operator::Multiply,
                    &mut tokens,
                    |tokens| {
                        tokens.append(&mut left_tokens);
                        tokens.push(operator.token());
                        tokens.append(&mut right_tokens);
                    },
                );

                Ok(tokens)
            }
        }
    }

    /// If there are multiple child operators, it returns the one with highest precedence.
    fn get_child_operator(&self, children: &[NodeKey]) -> Option<Operator> {
        children
            .iter()
            .filter_map(|key| self.get_node(*key))
            .filter_map(Node::try_get_operator)
            .sorted()
            .rev()
            .next()
    }

    fn parenthesize_if_necessary(
        tokens: &mut Vec<Token>,
        predicate: impl Fn() -> bool,
        mut build_interior: impl FnMut(&mut Vec<Token>),
    ) {
        let mut close_parentheses = false;

        if predicate() {
            tokens.push(Token::LeftParentheses);
            close_parentheses = true;
        }

        build_interior(tokens);

        if close_parentheses {
            tokens.push(Token::RightParentheses);
        }
    }

    fn parenthesize_if_precedence(
        parent_node: Option<&Node>,
        operator: Operator,
        tokens: &mut Vec<Token>,
        build_interior: impl FnMut(&mut Vec<Token>),
    ) {
        let predicate = || {
            if let Some(parent) = parent_node {
                if let Some(parent_operator) = parent.get_operator() {
                    // When a child operator has lower precedence, it and its operands needs
                    // to be wrapped in parentheses.
                    if parent_operator > operator {
                        return true;
                    }
                }
            }
            false
        };

        Self::parenthesize_if_necessary(tokens, predicate, build_interior);
    }

    fn build_group_tokens<'a>(
        &self,
        parent_node: Option<&Node>,
        iterator: impl Iterator<Item = &'a NodeKey>,
        intersperse_with: Token,
    ) -> Vec<Token> {
        iterator
            .filter_map(|key| self.get_node(*key))
            .filter_map(|node| self.build_expression(parent_node, node).ok())
            .intersperse(vec![intersperse_with])
            .flatten()
            .collect()
    }

    fn write_node(&self, node: NodeKey, builder: &mut TreeBuilder) {
        match self.get_node(node) {
            None => {}
            Some(node) => match node {
                Node::LiteralInteger(value) => {
                    builder.add_empty_child(format!("{}", value));
                }
                Node::Identifier(name) => {
                    builder.add_empty_child(name.to_string());
                }
                Node::Composite(data) => {
                    builder.begin_child(data.node_name());
                    if !data.left.is_empty() {
                        builder.begin_child(data.left_node_name());
                        for key in &data.left {
                            self.write_node(*key, builder)
                        }
                        builder.end_child();
                    }
                    if !data.right.is_empty() {
                        builder.begin_child(data.right_node_name());
                        for key in &data.right {
                            self.write_node(*key, builder)
                        }
                        builder.end_child();
                    }
                    builder.end_child();
                }
                Node::BinaryOperation {
                    operator,
                    left_operand,
                    right_operand,
                } => {
                    builder.begin_child(format!("{}", operator));
                    self.write_node(*left_operand, builder);
                    self.write_node(*right_operand, builder);
                    builder.end_child();
                }
            },
        }
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
    fn get_child_operator_returns_child_with_highest_precedence() {
        let tree = create_complex_tree();
        let children = tree.nodes.clone();

        let operator = tree.get_child_operator(&children.keys().collect::<Vec<NodeKey>>());

        assert_eq!(operator, Some(Operator::Multiply))
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
        let x = nodes.insert(Node::new_identifier("x".into()));
        let y = nodes.insert(Node::new_identifier("y".into()));
        let plus = nodes.insert(Node::new_binary_addition(x, y));

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
            Token::Plus,
        ]
        .to_vec();
        tokens
    }

    fn create_simple_infix_tokens() -> Vec<Token> {
        // x + y
        let tokens = [
            Token::Identifier("x".to_string()),
            Token::Plus,
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
            Token::Plus,
            Token::Identifier("a".to_string()),
            Token::Asterisk,
            Token::Plus,
        ]
        .to_vec()
    }

    fn create_complex_infix_tokens() -> Vec<Token> {
        // x + (y + z) * a
        [
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::LeftParentheses,
            Token::Identifier("y".to_string()),
            Token::Plus,
            Token::Identifier("z".to_string()),
            Token::RightParentheses,
            Token::Asterisk,
            Token::Identifier("a".to_string()),
        ]
        .to_vec()
    }

    fn create_complex_tree() -> ExpressionTree<Valid> {
        let mut nodes = SlotMap::with_key();
        let x = nodes.insert(Node::new_identifier("x".into()));
        let y = nodes.insert(Node::new_identifier("y".into()));
        let z = nodes.insert(Node::new_identifier("z".into()));
        let a = nodes.insert(Node::new_identifier("a".into()));
        let second_plus = nodes.insert(Node::new_binary_addition(y, z));
        let star = nodes.insert(Node::new_binary_multiplication(second_plus, a));
        let first_plus = nodes.insert(Node::new_binary_addition(x, star));

        ExpressionTree {
            nodes,
            state: Valid {
                root_key: first_plus,
            },
        }
    }
}
