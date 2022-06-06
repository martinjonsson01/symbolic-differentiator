use clap::Parser;
use anyhow::{{Context, Result}};
use anyhow::anyhow;

/// Differentiates the given expression
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Arguments {
    /// The expression to differentiate
    expression: String,
}

fn main() -> Result<()> {
    let args = Arguments::parse();
    println!("{}", args.expression);

    let value = parse_expression(args.expression)
        .with_context(|| format!("could not parse expression"))?;
    println!("value is {}", value); 

    Ok(())
}

fn parse_expression(expression: String) -> Result<i32> {
    if expression == "x+y" {
        Err(anyhow!("Incorrect formatting of expression: {}", expression))
    } else {
        Ok(1)
    }
}
