use crate::interpreter::token::Token;
use anyhow::{anyhow, bail, Context, Result};
use std::rc::Rc;
use string_builder::Builder;

/// A part of an expression tree
#[derive(PartialEq)]
pub struct TokenNode {
    value: Option<Token>,
    left: Option<Rc<TokenNode>>,
    right: Option<Rc<TokenNode>>,
}

impl TokenNode {
    pub fn new() -> TokenNode {
        return TokenNode {
            value: None,
            left: None,
            right: None,
        };
    }

    pub fn from_token(token: Token) -> TokenNode {
        TokenNode {
            value: Some(token),
            left: None,
            right: None,
        }
    }

    pub fn set_left(&mut self, node: TokenNode) {
        self.left = Some(Rc::new(node));
    }

    pub fn set_right(&mut self, node: TokenNode) {
        self.right = Some(Rc::new(node));
    }

    pub fn print(&self) -> String {
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

pub fn create_expression_tree(postfix_tokens: Vec<Token>) -> Result<TokenNode> {
    let mut tokens = postfix_tokens.clone();
    tokens.reverse();
    let mut operands: Vec<TokenNode> = Vec::new();

    while let Some(token) = tokens.pop() {
        match token {
            Token::Operator(_) => {
                let second_operand = operands.pop().context("Expected a second operand")?;
                let first_operand = operands.pop().context("Expected a first operand")?;

                let mut operator_node = TokenNode::from_token(token);
                operator_node.set_left(first_operand);
                operator_node.set_right(second_operand);

                operands.push(operator_node);
            }
            Token::Literal(_) | Token::Identifier(_) => operands.push(TokenNode::from_token(token)),
            Token::OpenParenthesis | Token::CloseParenthesis => {
                bail!("There should not be any parenthesis present in the input")
            }
        }
    }

    operands.pop().ok_or(anyhow!("No tree root found"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_expression_returns_tree() {
        let tokens = [
            Token::Identifier("x".to_string()),
            Token::Identifier("y".to_string()),
            "+".parse().unwrap(),
        ]
        .to_vec();

        let tree_root = create_expression_tree(tokens).unwrap();

        assert_eq!(tree_root.print(), "+: [x,y]");
    }
}
