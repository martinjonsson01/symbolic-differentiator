use crate::interpreter::operator::{BinaryOperator, UnaryOperator};
use crate::interpreter::syntax::composite::CompositeData;
use crate::interpreter::syntax::expression_tree::Node;

/// If a method is not implemented, the default implementation will continue in a pre-order
/// traversal of the tree.
pub(crate) trait SyntaxVisitor: Sized {
    fn visit_literal_integer(&mut self, _value: i32) {}
    fn visit_identifier(&mut self, _name: &str) {}
    fn visit_composite(&mut self, data: &CompositeData) {
        walk_composite(self, data)
    }
    fn visit_binary_operation(
        &mut self,
        _operation: &BinaryOperator,
        left_operand: &Node,
        right_operand: &Node,
    ) {
        walk_binary_operation(self, left_operand, right_operand)
    }
    fn visit_unary_operation(&mut self, _operation: &UnaryOperator, operand: &Node) {
        walk_unary_operation(self, operand)
    }
}

pub(crate) fn walk_composite(visitor: &mut impl SyntaxVisitor, data: &CompositeData) {
    data.right
        .iter()
        .chain(data.left.iter())
        .for_each(|node| node.accept(visitor));
}

pub(crate) fn walk_binary_operation(
    visitor: &mut impl SyntaxVisitor,
    left_operand: &Node,
    right_operand: &Node,
) {
    left_operand.accept(visitor);
    right_operand.accept(visitor);
}

pub(crate) fn walk_unary_operation(visitor: &mut impl SyntaxVisitor, operand: &Node) {
    operand.accept(visitor);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_complex_tree() -> Node {
        let a = Node::new_identifier("a".into());
        let b = Node::new_identifier("b".into());
        let c = Node::new_identifier("c".into());
        let d = Node::new_identifier("d".into());
        let second_plus = Node::new_binary_addition(b, c);
        let star = Node::new_binary_multiplication(second_plus, d);
        Node::new_binary_addition(a, star)
    }

    struct PrePostPrintVisitor {
        prints: Vec<String>,
    }

    impl SyntaxVisitor for PrePostPrintVisitor {
        fn visit_literal_integer(&mut self, value: i32) {
            self.prints.push(format!("{}", value))
        }
        fn visit_identifier(&mut self, name: &str) {
            self.prints.push(name.to_string())
        }
        fn visit_composite(&mut self, data: &CompositeData) {
            self.prints.push(data.node_name());
            walk_composite(self, data);
            self.prints.push(format!("exit {}", data.node_name()));
        }
        fn visit_binary_operation(
            &mut self,
            operation: &BinaryOperator,
            left_operand: &Node,
            right_operand: &Node,
        ) {
            self.prints.push(format!("{:?}", operation));
            walk_binary_operation(self, left_operand, right_operand);
            self.prints.push(format!("exit {:?}", operation));
        }
        fn visit_unary_operation(&mut self, operation: &UnaryOperator, operand: &Node) {
            self.prints.push(format!("{:?}", operation));
            walk_unary_operation(self, operand);
            self.prints.push(format!("exit {:?}", operation));
        }
    }

    #[test]
    fn walk_tree_prints_all_nodes_in_tree_in_pre_and_post_orders() {
        let root = create_complex_tree();
        println!("{}", root);
        let mut visitor = PrePostPrintVisitor { prints: vec![] };
        root.accept(&mut visitor);
        assert_eq!(
            visitor.prints,
            [
                "Summation",
                "a",
                "Fraction",
                "Summation",
                "b",
                "c",
                "exit Summation",
                "d",
                "exit Fraction",
                "exit Summation",
            ]
        )
    }
}
