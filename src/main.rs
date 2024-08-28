use std::io::BufRead;

use clap::Parser;

/// A custom grep implementation
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The expression to parse
    #[arg(short = 'E', long)]
    extended_regexp: String,
}

fn match_pattern(input_line: &str, pattern: &str) -> bool {
    if pattern.chars().count() == 1 {
        return input_line.contains(pattern);
    } else {
        panic!("Unhandled pattern: {}", pattern)
    }
}

fn main() {
    let args = Args::parse();

    let stdin = std::io::stdin();
    let mut matches = 0;

    for line in stdin.lock().lines() {
        let input_line = line.unwrap();
        if match_pattern(&input_line, &args.extended_regexp) {
            matches += 1;
            println!("{}", input_line);
        }
    }

    if matches > 0 {
        std::process::exit(0);
    } else {
        std::process::exit(1);
    }
}
