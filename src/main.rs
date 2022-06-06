use clap::Parser;

/// Differentiates the given expression
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Arguments {
    /// The expression to differentiate
    expression: String,
}

fn main() {
    let args = Arguments::parse();
    println!("{0}", args.expression);
}
