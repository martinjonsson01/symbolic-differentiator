use crate::interpreter::operator::BinaryOperator;
use crate::interpreter::syntax::expression_tree::Node;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompositeData {
    pub(crate) operator: BinaryOperator,
    pub(crate) inverse_operator: BinaryOperator,
    pub(crate) left: Vec<Node>,
    pub(crate) right: Vec<Node>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CompositeChild {
    Left,
    Right,
}

impl CompositeData {
    pub(crate) fn is_summation(&self) -> bool {
        self.operator == BinaryOperator::Add && self.inverse_operator == BinaryOperator::Subtract
    }

    pub(crate) fn is_fraction(&self) -> bool {
        self.operator == BinaryOperator::Multiply && self.inverse_operator == BinaryOperator::Divide
    }

    pub(crate) fn child(&self, which: CompositeChild) -> &[Node] {
        match which {
            CompositeChild::Left => &self.left,
            CompositeChild::Right => &self.right,
        }
    }

    pub(crate) fn child_owned(self, which: CompositeChild) -> Vec<Node> {
        match which {
            CompositeChild::Left => self.left,
            CompositeChild::Right => self.right,
        }
    }

    pub(crate) fn new_summation(adds: Vec<Node>, subtracts: Vec<Node>) -> CompositeData {
        CompositeData {
            operator: BinaryOperator::Add,
            inverse_operator: BinaryOperator::Subtract,
            left: adds,
            right: subtracts,
        }
    }

    pub(crate) fn new_fraction(numerator: Vec<Node>, denominator: Vec<Node>) -> CompositeData {
        CompositeData {
            operator: BinaryOperator::Multiply,
            inverse_operator: BinaryOperator::Divide,
            left: numerator,
            right: denominator,
        }
    }

    pub(crate) fn new_summed(terms: Vec<Node>) -> CompositeData {
        Self::new_summation(terms, vec![])
    }

    pub(crate) fn new_multiplied(factors: Vec<Node>) -> CompositeData {
        Self::new_fraction(factors, vec![])
    }

    pub(super) fn node_name(&self) -> String {
        match self.operator {
            BinaryOperator::Add => "Summation".into(),
            BinaryOperator::Multiply => "Fraction".into(),
            _ => "Unknown".into(),
        }
    }

    pub(super) fn left_node_name(&self) -> String {
        self.operator.to_string()
    }

    pub(super) fn right_node_name(&self) -> String {
        self.inverse_operator.to_string()
    }
}
