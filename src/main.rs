use clap::Parser;
use anyhow::{{Context, Result}};
use anyhow::anyhow;
use log::{info, warn};

/// Differentiates the given expression
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Arguments {
    /// The expression to differentiate
    expression: String,
}

fn main() -> Result<()> {
    env_logger::init();

    let args = Arguments::parse();
    warn!("Starting with expression {}", args.expression);

    let value : i32 = parse_expression(args.expression)
        .with_context(|| format!("could not parse expression"))?;
    info!("value is {}", value); 

    info!("Program finished executing!");
    Ok(())
}

fn parse_expression(expression: String) -> Result<i32> {
    if expression == "x+y" {
        Err(anyhow!("Incorrect formatting of expression: {}", expression))
    } else {
        Ok(1)
    }
}
