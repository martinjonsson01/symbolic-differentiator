use crate::interpreter::convert;
use crate::interpreter::differentiator::find_derivative;
use crate::interpreter::token::Token;
use anyhow::Result;
use clap::Parser;
use log::{error, info, warn};
use std::io;
use std::str::FromStr;

mod interpreter;

/// Differentiates the given expression with respect to the given variable
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Arguments {
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

    // REPL (Read -> Eval -> Print -> Loop) interface
    loop {
        info!("Please input an expression to differentiate and press <ENTER>");

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        if input == "q" {
            break;
        }

        let expression = convert(input)?;

        info!(
            "Differentiating expression {} with respect to {}",
            expression, args.variable
        );

        let variable = match Token::from_str(&args.variable) {
            Ok(variable) => variable,
            Err(_) => {
                error!("Could not convert {} to a valid token", args.variable);
                continue;
            }
        };
        let derivative = find_derivative(expression, &variable)?;
        let derivative_expression = derivative.to_infix()?;
        let derivative_text = interpreter::tokens_to_string(derivative_expression)?;

        info!("Derivative is {}", derivative_text)
    }

    info!("Exiting...");
    Ok(())
}
