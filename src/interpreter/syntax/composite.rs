use crate::interpreter::operator::BinaryOperator;
use crate::interpreter::syntax::expression_tree::NodeKey;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompositeData {
    pub(crate) operator: BinaryOperator,
    pub(crate) inverse_operator: BinaryOperator,
    pub(crate) left: Vec<NodeKey>,
    pub(crate) right: Vec<NodeKey>,
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

    pub(crate) fn child(&self, which: CompositeChild) -> &[NodeKey] {
        match which {
            CompositeChild::Left => &self.left,
            CompositeChild::Right => &self.right,
        }
    }

    pub(crate) fn new_summation(adds: Vec<NodeKey>, subtracts: Vec<NodeKey>) -> CompositeData {
        CompositeData {
            operator: BinaryOperator::Add,
            inverse_operator: BinaryOperator::Subtract,
            left: adds,
            right: subtracts,
        }
    }

    pub(crate) fn new_fraction(
        numerator: Vec<NodeKey>,
        denominator: Vec<NodeKey>,
    ) -> CompositeData {
        CompositeData {
            operator: BinaryOperator::Multiply,
            inverse_operator: BinaryOperator::Divide,
            left: numerator,
            right: denominator,
        }
    }

    pub(crate) fn new_summed(terms: Vec<NodeKey>) -> CompositeData {
        Self::new_summation(terms, vec![])
    }

    pub(crate) fn new_multiplied(factors: Vec<NodeKey>) -> CompositeData {
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
