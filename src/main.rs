use std::io::BufRead;

use clap::Parser;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Regex {
    Sequence(Vec<Regex>),
    LiteralChar(char),
    AnyDigit,
    AnyWord,
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
                        Some('w') => Regex::AnyWord,
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
    pub fn matches(&self, input: &str) -> bool {
        // TODO: Do we care about remaining input at this level?
        let chars = input.chars().collect::<Vec<_>>();
        let (matched, _) = self.match_recur(&chars);
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
            Regex::AnyWord => {
                if input[0].is_alphanumeric() {
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
        
        // TODO: Do we care about output? 
        if regex.matches(&input_line) {
            matches += 1;
            println!("{}", input_line);
        }
    }

    std::process::exit(if matches > 0 { 0 } else { 1 });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_literal_char() {
        let regex = Regex::from("a".to_string());
        assert!(regex.matches("a"));
        assert!(!regex.matches("b"));
        assert!(regex.matches("ab"));
        assert!(regex.matches("ba"));
    }

    #[test]
    fn test_any_digit() {
        let regex = Regex::from(r"\d".to_string());
        assert!(regex.matches("1"));
        assert!(regex.matches("2"));
        assert!(regex.matches("3"));
        assert!(!regex.matches("a"));
        assert!(!regex.matches("b"));
        assert!(!regex.matches("c"));
    }

    #[test]
    fn test_any_word() {
        let regex = Regex::from(r"\w".to_string());
        assert!(regex.matches("a"));
        assert!(regex.matches("b"));
        assert!(regex.matches("c"));
        assert!(regex.matches("1"));
        assert!(regex.matches("2"));
        assert!(regex.matches("3"));
        assert!(!regex.matches(" "));
        assert!(!regex.matches("\n"));
    }

    #[test]
    fn test_sequence() {
        let regex = Regex::from(r"\w\d".to_string());
        assert!(regex.matches("a1"));
        assert!(regex.matches("b2"));
        assert!(regex.matches("c3"));
        assert!(!regex.matches("a"));
        assert!(!regex.matches("1"));
        assert!(!regex.matches(" "));
        assert!(!regex.matches("\n"));
    }
}