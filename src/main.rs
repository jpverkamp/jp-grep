use std::io::BufRead;

use clap::Parser;
use types::Regex;

mod matcher;
mod parser;
mod types;

/// A custom grep implementation; always behaves as egrep
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// If we're operating in extended mode, this is ignored (always treated as true)
    #[clap(short = 'E', long)]
    extended_regexp: bool,
    /// Additional patterns, will return a line if any match
    #[clap(short='e', long="regexp", action=clap::ArgAction::Append, required=false)]
    additional_patterns: Vec<String>,
    /// The expression to parse
    pattern: Option<String>,
}

fn main() {
    env_logger::init();

    let args = Args::parse();

    let regexes = args
        .pattern
        .iter()
        .chain(args.additional_patterns.iter())
        .map(|p| match Regex::try_from(p.clone()) {
            Ok(r) => r,
            Err(e) => {
                eprintln!(
                    "Error parsing regex: {error}\n| {regex}\n| {spacing}^",
                    regex = p,
                    spacing = " ".repeat(e.position),
                    error = e.error
                );
                std::process::exit(1);
            }
        })
        .collect::<Vec<Regex>>();

    if regexes.is_empty() {
        eprintln!("No patterns provided");
        std::process::exit(1);
    }

    log::debug!("Parsed regexes: {:#?}", regexes);

    let stdin = std::io::stdin();
    let mut matches = 0;

    for line in stdin.lock().lines() {
        let input_line = line.unwrap();

        // TODO: Do we care about output?
        if regexes.iter().any(|regex| regex.matches(&input_line)) {
            matches += 1;
            println!("{}", input_line);
        }
    }

    std::process::exit(if matches > 0 { 0 } else { 1 });
}
