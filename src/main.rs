use clap::Parser;

/// What and how to differentiate.
#[derive(Parser)]
struct Arguments {
    /// The expression to differentiate.
    expression: String,
}

fn main() {
    let args = Arguments::parse();
    println!("{0}", args.expression);
}
