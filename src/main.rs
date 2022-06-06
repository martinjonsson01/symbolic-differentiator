use anyhow::{Context, Result};
use clap::Parser;
use log::{info, warn};

mod parser;

pub use crate::parser::text_parser;

/// Differentiates the given expression
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Arguments {
    /// The expression to differentiate
    expression: String,
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

    let value: i32 = text_parser::parse_expression(args.expression)
        .with_context(|| format!("could not parse expression"))?;
    info!("value is {}", value);

    info!("Program finished executing!");
    Ok(())
}
