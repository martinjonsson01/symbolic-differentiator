#![feature(if_let_guard)]

use crate::interpreter::differentiate;
use crate::interpreter::differentiator::find_derivative;
use crate::interpreter::token::Token;
use anyhow::Result;
use clap::Parser;
use log::error;
use std::io;
use std::io::BufRead;

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
        println!("Please input an expression to differentiate and press <ENTER>");

        let line = io::stdin().lock().lines().next();
        let input = match line.and_then(|result| result.ok()) {
            None => {
                error!("Failed to read input, please input a correct string, or type 'q' to exit");
                continue;
            }
            Some(text) => text,
        };

        if input == "q" {
            break;
        }

        println!(
            "Differentiating expression {} with respect to {}",
            input, args.variable
        );

        let derivative = differentiate(input, args.variable.clone())?;
        println!("Derivative is {}", derivative)
    }

    println!("Exiting...");
    Ok(())
}
