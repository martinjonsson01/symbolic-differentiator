use crate::interpreter::operator::{BinaryOperator, UnaryOperator};
use crate::interpreter::syntax::composite::CompositeData;
use crate::interpreter::syntax::syntax_visitor::{
    walk_binary_operation, walk_composite, walk_unary_operation, SyntaxVisitor,
};
use crate::interpreter::token::Token;
use anyhow::{anyhow, bail, Context, Result};
use itertools::Itertools;
use ptree::{write_tree, TreeBuilder};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Clone, Eq)]
pub enum Node {
    // Terminal symbols (leaves)
    LiteralInteger(i32),
    Identifier(String),
    // Non-terminal symbols (non-leaves)
    Composite(CompositeData),
    BinaryOperation {
        operator: BinaryOperator,
        left_operand: Box<Node>,
        right_operand: Box<Node>,
    },
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Node>,
    },
}

impl Node {
    pub fn new_literal_integer(value: i32) -> Node {
        Node::LiteralInteger(value)
    }

    pub fn new_identifier(name: String) -> Node {
        Node::Identifier(name)
    }

    pub fn new_binary_addition(left_operand: Node, right_operand: Node) -> Node {
        Self::new_composite_summation(vec![left_operand, right_operand], Vec::new())
    }

    pub fn new_binary_subtraction(left_operand: Node, right_operand: Node) -> Node {
        Self::new_composite_summation(vec![left_operand], vec![right_operand])
    }

    pub fn new_binary_multiplication(left_operand: Node, right_operand: Node) -> Node {
        Self::new_composite_fraction(vec![left_operand, right_operand], Vec::new())
    }

    pub fn new_binary_division(left_operand: Node, right_operand: Node) -> Node {
        Self::new_composite_fraction(vec![left_operand], vec![right_operand])
    }

    pub fn new_binary_exponentiation(left_operand: Node, right_operand: Node) -> Node {
        Node::BinaryOperation {
            operator: BinaryOperator::Exponentiate,
            left_operand: Box::new(left_operand),
            right_operand: Box::new(right_operand),
        }
    }

    pub fn new_composite_summation(left: Vec<Node>, right: Vec<Node>) -> Node {
        Node::Composite(CompositeData::new_summation(left, right))
    }

    pub fn new_composite_fraction(left: Vec<Node>, right: Vec<Node>) -> Node {
        Node::Composite(CompositeData::new_fraction(left, right))
    }

    pub fn new_summed(terms: Vec<Node>) -> Node {
        Node::Composite(CompositeData::new_summed(terms))
    }

    pub fn new_multiplied(factors: Vec<Node>) -> Node {
        Node::Composite(CompositeData::new_multiplied(factors))
    }

    fn new_sqrt(operand: Node) -> Node {
        Node::UnaryOperation {
            operator: UnaryOperator::PositiveSquareRoot,
            operand: Box::new(operand),
        }
    }

    fn new_natural_log(operand: Node) -> Node {
        Node::UnaryOperation {
            operator: UnaryOperator::NaturalLogarithm,
            operand: Box::new(operand),
        }
    }

    pub fn is_specific_operator(&self, _check_operator: BinaryOperator) -> bool {
        matches!(
            self,
            Node::BinaryOperation {
                operator: _check_operator,
                ..
            }
        )
    }

    pub fn is_operator(&self) -> bool {
        matches!(self, Node::BinaryOperation { .. })
            || matches!(self, Node::Composite { .. })
            || matches!(self, Node::UnaryOperation { .. })
    }

    pub fn as_binary_operator(&self) -> Option<BinaryOperator> {
        match self {
            Node::LiteralInteger(_) | Node::Identifier(_) | Node::UnaryOperation { .. } => None,
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

    /// Calls the correct visitor method for the node variant on the given visitor.
    pub(crate) fn accept(&self, visitor: &mut impl SyntaxVisitor) {
        match self {
            Node::LiteralInteger(value) => visitor.visit_literal_integer(*value),
            Node::Identifier(name) => visitor.visit_identifier(name),
            Node::Composite(data) => visitor.visit_composite(data),
            Node::BinaryOperation {
                operator,
                left_operand,
                right_operand,
            } => visitor.visit_binary_operation(operator, left_operand, right_operand),
            Node::UnaryOperation { operator, operand } => {
                visitor.visit_unary_operation(operator, operand)
            }
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.format_tree(f)
    }
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Node::LiteralInteger(value) => write!(f, "{:?}", value),
            Node::Identifier(name) => write!(f, "{:?}", name),
            Node::Composite(data) => write!(f, "{:?}", data.node_name()),
            Node::BinaryOperation { operator, .. } => write!(f, "{:?}", operator),
            Node::UnaryOperation { operator, .. } => write!(f, "{:?}", operator),
        }
    }
}

impl Hash for Node {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Node::LiteralInteger(value) => value.hash(state),
            Node::Identifier(name) => name.hash(state),
            Node::Composite(data) => data.hash(state),
            Node::BinaryOperation {
                operator,
                left_operand,
                right_operand,
            } => {
                operator.hash(state);
                left_operand.hash(state);
                right_operand.hash(state);
            }
            Node::UnaryOperation { operator, operand } => {
                operator.hash(state);
                operand.hash(state);
            }
        }
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        node_eq(self, other)
    }
}

fn node_eq(node1: &Node, node2: &Node) -> bool {
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
            return nodes_eq(first1.iter(), first2.iter())
                && nodes_eq(second1.iter(), second2.iter());
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

            node_eq(left_operand1, left_operand2) && node_eq(right_operand1, right_operand2)
        }
        _ => false, // node1 and node2 are different variants
    };
}

fn nodes_eq<'a>(
    nodes1: impl Iterator<Item = &'a Node>,
    nodes2: impl Iterator<Item = &'a Node>,
) -> bool {
    nodes1
        .zip(nodes2)
        .map(|(key1, key2)| node_eq(key1, key2))
        .all(|equal| equal)
}

/// Generates an expression tree based off of the given tokens.
///
/// # Arguments
///
/// * `postfix_tokens`: Tokens, ordered in postfix notation, to convert to an expression tree.
///
/// returns: The root of the generated expression tree.
pub fn new_tree(mut tokens: Vec<Token>) -> Result<Node> {
    tokens.reverse();
    let mut operands: Vec<Node> = Vec::new();

    while let Some(token) = tokens.pop() {
        match token {
            Token::LiteralInteger(value) => operands.push(Node::new_literal_integer(value)),
            Token::Identifier(name) => operands.push(Node::new_identifier(name)),
            Token::Sqrt => {
                let operand = operands.pop().context("Expected an inner expression")?;
                let node = Node::new_sqrt(operand);
                operands.push(node);
            }
            Token::Ln => {
                let operand = operands.pop().context("Expected an inner expression")?;
                let node = Node::new_natural_log(operand);
                operands.push(node);
            }
            Token::LeftParentheses | Token::RightParentheses => {
                bail!("There should not be any parenthesis present in the input")
            }
            _ => {
                let operand_two = operands.pop().context("Expected a second operand")?;
                let operand_one = operands.pop().context("Expected a first operand")?;

                let node = match token {
                    Token::Plus => Node::new_binary_addition(operand_one, operand_two),
                    Token::Dash => Node::new_binary_subtraction(operand_one, operand_two),
                    Token::Asterisk => Node::new_binary_multiplication(operand_one, operand_two),
                    Token::ForwardSlash => Node::new_binary_division(operand_one, operand_two),
                    Token::Caret => Node::new_binary_exponentiation(operand_one, operand_two),
                    _ => bail!("Should be unreachable. If this occurs, check the other match-expression above.")
                };
                operands.push(node);
            }
        }
    }

    let root = operands.pop().context("No tree root found")?;
    Ok(root)
}

pub fn nodes_into_fractions(nodes: &[Node]) -> Result<Vec<CompositeData>> {
    let mut fractions = vec![];
    for node in nodes {
        match node {
            Node::LiteralInteger(_)
            | Node::Identifier(_)
            | Node::BinaryOperation { .. }
            | Node::UnaryOperation { .. }
            | Node::Composite(CompositeData {
                operator: BinaryOperator::Add,
                ..
            }) => {
                let fraction_data = CompositeData::new_multiplied(vec![node.clone()]);
                fractions.push(fraction_data);
            }
            Node::Composite(data) if data.is_fraction() => fractions.push(data.clone()),
            Node::Composite(_) => {
                return Err(anyhow!("Unable to handle composite of unknown type"))
            }
        }
    }
    Ok(fractions)
}

impl Node {
    pub fn replace_child(&mut self, old_child: &Node, new_child: Node) -> Result<()> {
        match self {
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
                    .find(|node| *node == old_child)
                    .context("Could not find child of node")?;
                *child_ref = new_child;
                Ok(())
            }
            Node::BinaryOperation {
                ref mut left_operand,
                ref mut right_operand,
                ..
            } => {
                let child_ref = if **left_operand == *old_child {
                    left_operand
                } else if **right_operand == *old_child {
                    right_operand
                } else {
                    return Err(anyhow!("Old child is not a child of the given node"));
                };
                *child_ref = Box::new(new_child);
                Ok(())
            }
            Node::UnaryOperation {
                ref mut operand, ..
            } => {
                let child_ref = if **operand == *old_child {
                    operand
                } else {
                    return Err(anyhow!("Old child is not a child of the given node"));
                };
                *child_ref = Box::new(new_child);
                Ok(())
            }
        }
    }

    pub fn to_infix(&self) -> Result<Vec<Token>> {
        self.build_expression(None)
    }

    fn build_expression(&self, parent_node: Option<&Node>) -> Result<Vec<Token>> {
        match self {
            Node::LiteralInteger(value) => Ok(vec![Token::LiteralInteger(*value)]),
            Node::Identifier(name) => Ok(vec![Token::Identifier(name.to_string())]),
            Node::Composite(CompositeData {
                operator: BinaryOperator::Add,
                left: add,
                right: subtract,
                ..
            }) => {
                let mut tokens = Vec::new();
                let mut add_tokens = build_group_tokens(Some(self), add.iter(), Token::Plus);
                let mut subtract_tokens =
                    build_group_tokens(Some(self), subtract.iter(), Token::Dash);

                parenthesize_if_precedence(
                    parent_node,
                    BinaryOperator::Add,
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
                operator: BinaryOperator::Multiply,
                left: multiply,
                right: divide,
                ..
            }) => {
                let divides_operator = get_child_operator(divide);
                let mut tokens = Vec::new();
                let mut multiply_tokens =
                    build_group_tokens(Some(self), multiply.iter(), Token::Asterisk);
                let mut divide_tokens =
                    build_group_tokens(Some(self), divide.iter(), Token::Asterisk);

                // Put a 1 as numerator if it's empty and there's a denominator, for readability.
                if multiply_tokens.is_empty() && !divide_tokens.is_empty() {
                    multiply_tokens.push(Token::LiteralInteger(1));
                }

                let build_interior = |tokens: &mut Vec<Token>| {
                    tokens.append(&mut multiply_tokens);
                    if let Some(divides_operator) = divides_operator {
                        tokens.push(Token::ForwardSlash);

                        parenthesize_if_precedence(
                            Some(self),
                            divides_operator,
                            tokens,
                            |tokens| tokens.append(&mut divide_tokens),
                        );
                    } else if !divide_tokens.is_empty() {
                        tokens.push(Token::ForwardSlash);
                        tokens.append(&mut divide_tokens);
                    }
                };
                parenthesize_if_precedence(
                    parent_node,
                    BinaryOperator::Multiply,
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

                let mut left_tokens = left_operand.build_expression(Some(self))?;
                let mut right_tokens = right_operand.build_expression(Some(self))?;

                parenthesize_if_precedence(
                    parent_node,
                    BinaryOperator::Multiply,
                    &mut tokens,
                    |tokens| {
                        tokens.append(&mut left_tokens);
                        tokens.push(operator.token());
                        tokens.append(&mut right_tokens);
                    },
                );

                Ok(tokens)
            }
            Node::UnaryOperation { operator, operand } => {
                let mut tokens = Vec::new();
                let mut operand_tokens = operand.build_expression(Some(self))?;

                tokens.push(operator.token());
                tokens.push(Token::LeftParentheses);
                tokens.append(&mut operand_tokens);
                tokens.push(Token::RightParentheses);

                Ok(tokens)
            }
        }
    }

    fn format_tree(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut visitor = TreeBuilderVisitor {
            builder: TreeBuilder::new("expression".into()),
        };
        self.accept(&mut visitor);

        let mut buffer: Vec<u8> = Vec::new();
        match write_tree(&visitor.builder.build(), &mut buffer) {
            Ok(_) => {}
            Err(_) => return Err(fmt::Error),
        }
        let text = match std::str::from_utf8(&buffer) {
            Ok(text) => text,
            Err(_) => return Err(fmt::Error),
        };
        f.write_str(text)
    }
}

struct TreeBuilderVisitor {
    builder: TreeBuilder,
}

impl SyntaxVisitor for TreeBuilderVisitor {
    fn visit_literal_integer(&mut self, value: i32) {
        self.builder.add_empty_child(format!("{}", value));
    }
    fn visit_identifier(&mut self, name: &str) {
        self.builder.add_empty_child(name.to_string());
    }
    fn visit_composite(&mut self, data: &CompositeData) {
        self.builder.begin_child(data.node_name());
        if !data.left.is_empty() {
            self.builder.begin_child(data.left_node_name());
            data.left.iter().for_each(|node| node.accept(self));
            self.builder.end_child();
        }
        if !data.right.is_empty() {
            self.builder.begin_child(data.right_node_name());
            data.right.iter().for_each(|node| node.accept(self));
            self.builder.end_child();
        }
        self.builder.end_child();
    }
    fn visit_binary_operation(
        &mut self,
        operator: &BinaryOperator,
        left_operand: &Node,
        right_operand: &Node,
    ) {
        self.builder.begin_child(format!("{}", operator));
        walk_binary_operation(self, left_operand, right_operand);
        self.builder.end_child();
    }
    fn visit_unary_operation(&mut self, operator: &UnaryOperator, operand: &Node) {
        self.builder.begin_child(format!("{}", operator));
        walk_unary_operation(self, operand);
        self.builder.end_child();
    }
}

/// If there are multiple child operators, it returns the one with highest precedence.
fn get_child_operator(children: &[Node]) -> Option<BinaryOperator> {
    children
        .iter()
        .filter_map(Node::as_binary_operator)
        .sorted_by(|a, b| a.precedence().cmp(&b.precedence()))
        .rev()
        .next()
}

fn parenthesize_if(
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
    operator: BinaryOperator,
    tokens: &mut Vec<Token>,
    build_interior: impl FnMut(&mut Vec<Token>),
) {
    let predicate = || {
        if let Some(parent) = parent_node {
            if let Some(parent_operator) = parent.as_binary_operator() {
                // When a child operator has lower precedence, it and its operands needs
                // to be wrapped in parentheses.
                if parent_operator.precedence_gt(&operator) {
                    return true;
                }
            }
        }
        false
    };

    parenthesize_if(tokens, predicate, build_interior);
}

fn build_group_tokens<'a>(
    parent_node: Option<&Node>,
    iterator: impl Iterator<Item = &'a Node>,
    intersperse_with: Token,
) -> Vec<Token> {
    let built_expressions = iterator.filter_map(|node| node.build_expression(parent_node).ok());
    // Have to use fully qualified syntax here until 'intersperse' is added into stdlib
    itertools::Itertools::intersperse(built_expressions, vec![intersperse_with])
        .flatten()
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_expression_returns_correct_tree() {
        let tokens = create_simple_postfix_tokens();
        let expected_tree = create_simple_tree();

        let actual_tree = new_tree(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn complex_expression_returns_correct_tree() {
        let tokens = create_complex_postfix_tokens();
        let expected_tree = create_complex_tree();

        let actual_tree = new_tree(tokens).unwrap();

        assert_eq!(actual_tree, expected_tree);
    }

    #[test]
    fn print_succeeds() {
        let tree = create_complex_tree();

        print!("{}", tree);
    }

    #[test]
    fn get_child_operator_returns_child_with_highest_precedence() {
        let children = vec![
            Node::new_multiplied(vec![Node::new_identifier("x".into())]),
            Node::new_summed(vec![Node::new_identifier("y".into())]),
            Node::new_binary_exponentiation(
                Node::new_identifier("x".into()),
                Node::new_literal_integer(2),
            ),
        ];

        let operator = get_child_operator(&children);

        assert_eq!(operator, Some(BinaryOperator::Exponentiate))
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

    fn create_simple_tree() -> Node {
        let x = Node::new_identifier("x".into());
        let y = Node::new_identifier("y".into());
        Node::new_binary_addition(x, y)
    }

    fn create_simple_postfix_tokens() -> Vec<Token> {
        // x + y (but in postfix notation)
        vec![
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Plus,
        ]
    }

    fn create_simple_infix_tokens() -> Vec<Token> {
        // x + y
        vec![
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
        ]
    }

    fn create_complex_postfix_tokens() -> Vec<Token> {
        // x + ((y + z) * a) (but in postfix notation)
        vec![
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            Token::Identifier("z".to_string()),
            Token::Plus,
            Token::Identifier("a".to_string()),
            Token::Asterisk,
            Token::Plus,
        ]
    }

    fn create_complex_infix_tokens() -> Vec<Token> {
        // x + (y + z) * a
        vec![
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
    }

    fn create_complex_tree() -> Node {
        let x = Node::new_identifier("x".into());
        let y = Node::new_identifier("y".into());
        let z = Node::new_identifier("z".into());
        let a = Node::new_identifier("a".into());
        let second_plus = Node::new_binary_addition(y, z);
        let star = Node::new_binary_multiplication(second_plus, a);
        Node::new_binary_addition(x, star)
    }
}
