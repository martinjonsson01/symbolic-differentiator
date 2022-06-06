pub mod lexer;
pub mod parser;
mod token;

use anyhow::{Context, Result};

pub fn convert(expression: String) -> Result<String> {
    let value: Vec<token::Token> = lexer::tokenize(expression)
        .with_context(|| "could not parse expression".to_string())?;
    Ok(format!("{:?}", value))
}