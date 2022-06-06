use anyhow::{Context, Result};
use clap::Parser;
use log::{info, warn};

mod interpreter;

pub use crate::interpreter::lexer;

/// Differentiates the given expression with respect to the given variable
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Arguments {
    /// The expression to differentiate
    expression: String,
    /// The name of the variable to differentiate with respect to
    variable: String,
    /// Whether to log additional information
    #[clap(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
}

fn main() -> Result<()> {
    let args = Arguments::parse();
    env_logger::Builder::new()
        .filter_level(args.verbose.log_level_filter())
        .init();

    warn!("Starting with expression {}", args.expression);

    let value: i32 = lexer::tokenize(args.expression)
        .with_context(|| "could not parse expression".to_string())?;
    info!("value is {}", value);

    info!("Program finished executing!");
    Ok(())
}
