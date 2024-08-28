use std::io::BufRead;

use clap::Parser;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Regex {
    Char(char),
    Range(char, char),
    Sequence(Vec<Regex>),
    Choice(Vec<Regex>),
    Not(Box<Regex>),
}

impl From<String> for Regex {
    fn from(value: String) -> Self {
        let mut sequence = vec![];
        let mut chars = value.chars().peekable();

        while let Some(c) = chars.next() {
            let node = match c {
                // Predefined character groups
                '\\' => {
                    match chars.next() {
                        Some('d') => Regex::Range('0', '9'),
                        Some('w') => Regex::Choice(vec![
                            Regex::Range('a', 'z'),
                            Regex::Range('A', 'Z'),
                            Regex::Range('0', '9'),
                            Regex::Char('_'),
                        ]),
                        _ => unimplemented!()
                    }
                },

                // Custom defined character groups
                // TODO: Implement ranges
                // TODO: Implement escaping in character groups
                '[' => {
                    // Handle negation
                    let negated = if let Some('^') = chars.peek() {
                        chars.next();
                        true
                    } else {
                        false
                    };

                    let mut choices = vec![];
                    while let Some(c) = chars.next() {
                        if c == ']' {
                            break;
                        }

                        choices.push(Regex::Char(c));
                    }

                    if negated {
                        Regex::Not(Box::new(Regex::Choice(choices)))
                    } else {
                        Regex::Choice(choices)
                    }
                },

                // Single characters
                c => Regex::Char(c),
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

    fn allow_none(&self) -> bool {
        // For each match type, determine if it can match an empty string
        match self {
            Regex::Char(_) => false,
            Regex::Range(_, _) => false,
            Regex::Sequence(seq) => seq.iter().all(|node| node.allow_none()),
            Regex::Choice(seq) => seq.iter().any(|node| node.allow_none()),
            Regex::Not(node) => node.allow_none(),
        }
    }

    fn match_recur<'a>(&self, input: &'a [char]) -> (bool, &'a [char]) {
        if input.len() == 0 {
            return (self.allow_none(), input);
        }

        match self {
            // Single character matches
            Regex::Char(c) => {
                if input[0] == *c {
                    return (true, &input[1..]);
                }

                return self.match_recur(&input[1..]);
            },
            Regex::Range(start, end) => {
                if input[0] >= *start && input[0] <= *end {
                    return (true, &input[1..]);
                }

                return self.match_recur(&input[1..]);
            },

            // A negative match of any other matcher
            Regex::Not(node) => {
                let (matched, new_remaining) = node.match_recur(input);
                if !matched {
                    return (true, new_remaining);
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

            // A choice of matches, any of which much match
            // If none match, abort the entire choice and advance to try again
            Regex::Choice(seq) => {
                for node in seq {
                    let (matched, new_remaining) = node.match_recur(input);
                    if matched {
                        return (true, new_remaining);
                    }
                }

                return self.match_recur(&input[1..]);
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

    macro_rules! test_regex {
        ($name:ident, $regex:expr, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let regex = Regex::from($regex.to_string());
                assert_eq!(regex.matches($input), $expected);
            }
        };
    }

    test_regex!(single_char, "a", "a", true);
    test_regex!(single_char_not, "a", "b", false);
    test_regex!(single_char_first, "a", "abb", true);
    test_regex!(single_char_middle, "a", "bab", true);
    test_regex!(single_char_last, "a", "bba", true);
    test_regex!(single_char_notmulti, "a", "bbb", false);
    test_regex!(single_char_empty, "a", "", false);

    test_regex!(any_digit, r"\d", "1", true);
    test_regex!(any_digit_not, r"\d", "a", false);
    test_regex!(any_digit_empty, r"\d", "", false);
    test_regex!(any_digit_multi, r"\d", "abc123", true);
    test_regex!(any_digit_multi_not, r"\d", "abc", false);

    test_regex!(any_word, r"\w", "a", true);
    test_regex!(any_word_not, r"\w", "!", false);
    test_regex!(any_word_empty, r"\w", "", false);
    test_regex!(any_word_multi, r"\w", "abc123", true);
    test_regex!(any_word_multi_not, r"\w", "!@#$", false);
    test_regex!(any_word_underscore, r"\w", "_", true);

    test_regex!(sequence, r"\w\d", "a1", true);
    test_regex!(sequence_not, r"\w\d", "a", false);
    test_regex!(sequence_not2, r"\w\d", "1", false);
    test_regex!(sequence_not3, r"\w\d", " ", false);
    test_regex!(sequence_not4, r"\w\d", "\n", false);

    test_regex!(choice, r"[abc]", "a", true);
    test_regex!(choice2, r"[abc]", "b", true);
    test_regex!(choice3, r"[abc]", "c", true);
    test_regex!(choice4, r"[abc]", "d", false);
    test_regex!(choice_multi, r"[abc]", "efagh", true);
    test_regex!(choice_multi_not, r"[abc]", "defgh", false);

    test_regex!(choice_negated, r"[^abc]", "d", true);
    test_regex!(choice_negated2, r"[^abc]", "a", false);
    test_regex!(choice_negated_long, r"[^abc]", "defaghi", true); // Any character can be not abc
    test_regex!(choice_negated_long2, r"[^abc]", "abcabc", false);
}