use std::io::BufRead;

use clap::Parser;
use types::Regex;

mod matcher;
mod parser;
mod types;

/// A custom grep implementation
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The expression to parse
    #[arg(short = 'E', long)]
    extended_regexp: String,
}

fn main() {
    env_logger::init();

    let args = Args::parse();
    let regex = match Regex::try_from(args.extended_regexp) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("Error parsing regex: {e}");
            std::process::exit(1);
        }
    };

    let stdin = std::io::stdin();
    let mut matches = 0;

    for line in stdin.lock().lines() {
        let input_line = line.unwrap();

        // TODO: Do we care about output?
        if regex.matches(&input_line) {
            matches += 1;
            println!("{}", input_line);
        }
    }

    std::process::exit(if matches > 0 { 0 } else { 1 });
}
