use anyhow::{Context, Result};
use clap::Parser;
use log::{info, warn};

mod interpreter;

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

    let result: String = interpreter::convert(args.expression)?;
    info!("value is {}", result);

    info!("Program finished executing!");
    Ok(())
}
