use clap::Parser;

/// Differentiates the given expression
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Arguments {
    /// The expression to differentiate
    expression: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Arguments::parse();
    println!("{}", args.expression);

    let value = parse_expression(args.expression)?;
    println!("value is {}", value); 

    Ok(())
}

fn parse_expression(expression: String) -> Result<i32, String> {
    if expression == "x+y" {
        Err("Whoops".to_string())
    } else {
        Ok(1)
    }
}
