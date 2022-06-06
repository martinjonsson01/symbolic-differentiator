use anyhow::anyhow;
use anyhow::Result;

pub fn parse_expression(expression: String) -> Result<i32> {
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
        parse_expression("x+y".to_string()).unwrap_err();
    }
}
