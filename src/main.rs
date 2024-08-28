use std::io::BufRead;

use clap::Parser;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Regex {
    Sequence(Vec<Regex>),
    LiteralChar(char),
    AnyDigit,
}

impl From<String> for Regex {
    fn from(value: String) -> Self {
        let mut sequence = vec![];
        let mut chars = value.chars().peekable();

        while let Some(c) = chars.next() {
            let node = match c {
                '\\' => {
                    match chars.next() {
                        Some('d') => Regex::AnyDigit,
                        _ => unimplemented!()
                    }
                },
                c => Regex::LiteralChar(c),
            };

            sequence.push(node);
        }

        Regex::Sequence(sequence)
    }
}

impl Regex {
    pub fn matches(&self, input: &[char]) -> bool {
        // TODO: Do we care about remaining input at this level?
        let (matched, _) = self.match_recur(input);
        matched
    }

    fn match_recur<'a>(&self, input: &'a [char]) -> (bool, &'a [char]) {
        // TODO: Fix this when we have potentially empty groups
        if input.len() == 0 {
            return (false, input);
        }

        match self {
            // Single character matches
            Regex::LiteralChar(c) => {
                if input[0] == *c {
                    return (true, &input[1..]);
                }

                return self.match_recur(&input[1..]);
            },
            Regex::AnyDigit => {
                if input[0].is_digit(10) {
                    return (true, &input[1..]);
                }

                return self.match_recur(&input[1..]);
            },

            // A sequence of matches, all of which must match
            // If any fails, abort the entire sequence and advance to try again
            Regex::Sequence(seq) => {
                let mut remaining = input;
                for node in seq {
                    let (matched, new_remaining) = node.match_recur(remaining);
                    if !matched {
                        return self.match_recur(&input[1..]);
                    }

                    remaining = new_remaining;
                }

                return (true, remaining);
            },
        }
    }
}

/// A custom grep implementation
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The expression to parse
    #[arg(short = 'E', long)]
    extended_regexp: String,
}

fn main() {
    let args = Args::parse();
    let regex = Regex::from(args.extended_regexp);

    let stdin = std::io::stdin();
    let mut matches = 0;

    for line in stdin.lock().lines() {
        let input_line = line.unwrap();
        let input_chars = input_line.chars().collect::<Vec<_>>();
        
        // TODO: Do we care about output? 
        if regex.matches(&input_chars) {
            matches += 1;
            println!("{}", input_line);
        }
    }

    std::process::exit(if matches > 0 { 0 } else { 1 });
}
