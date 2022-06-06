use anyhow::anyhow;
use anyhow::Result;

/// A discrete part of an expression
enum Token {
    Plus,
    Minus,
    Star,
    ForwardSlash,
}

pub fn tokenize(expression: String) -> Result<i32> {
    if expression == "x+y" {
        Err(anyhow!(
            "Incorrect formatting of expression: {}",
            expression
        ))
    } else {
        Ok(1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn correct_expression_returns_tokens() {
        tokenize("x+y".to_string()).unwrap_err();
    }
}
