use std::rc::Rc;

use super::token::Token;

/// A part of an expression tree
pub struct TokenNode {
    left: Rc<TokenNode>,
    right: Rc<TokenNode>,
    value: Option<Token>,
}
