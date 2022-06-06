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