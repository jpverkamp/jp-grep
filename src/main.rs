use std::io::BufRead;

use clap::Parser;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Regex {
    Char(CharType),
    Sequence(Vec<Regex>),
    Choice(Vec<Regex>),
    CapturingGroup(Box<Regex>),
    Repeated(RepeatType, Box<Regex>),
    Not(Box<Regex>),
    Start,
    End,
    ChoicePlaceholder,
    Backref(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CharType {
    Any,
    Single(char),
    Range(char, char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RepeatType {
    OneOrMore,
    ZeroOrMore,
    ZeroOrOne,
}

impl From<String> for Regex {
    fn from(value: String) -> Self {
        // Read until the 'until' char, or end of string if None
        fn read_until<'a>(input: &'a [char], until: Option<char>) -> (Regex, &'a [char]) {
            let mut sequence = vec![];
            let mut input = input;

            // Read until end of input
            while let Some(&c) = input.first() {
                input = &input[1..];

                // Break if we have and hit the until character
                if until == Some(c) {
                    break;
                }

                let node = match c {
                    // Predefined character groups
                    '\\' => {
                        let group = match input.first() {
                            Some('d') => Regex::Char(CharType::Range('0', '9')),
                            Some('w') => Regex::Choice(vec![
                                Regex::Char(CharType::Range('a', 'z')),
                                Regex::Char(CharType::Range('A', 'Z')),
                                Regex::Char(CharType::Range('0', '9')),
                                Regex::Char(CharType::Single('_')),
                            ]),

                            // Backreference
                            Some(&c) if c.is_digit(10) => {
                                let index = c.to_digit(10).unwrap() as usize;
                                Regex::Backref(index)
                            },

                            // Escaped control characters
                            Some(&c) if "\\()[]|".contains(c) => Regex::Char(CharType::Single(c)),

                            // A group we don't know about
                            _ => unimplemented!()
                        };
                        input = &input[1..];
                        group
                    },

                    // Custom defined character groups
                    // TODO: Implement ranges
                    // TODO: Implement escaping in character groups
                    '[' => {
                        // Handle negation
                        let negated = if let Some('^') = input.first() {
                            input = &input[1..];
                            true
                        } else {
                            false
                        };

                        let mut choices = vec![];
                        while let Some(&c) = input.first() {
                            input = &input[1..];

                            if c == ']' {
                                break;
                            }

                            choices.push(Regex::Char(CharType::Single(c)));
                        }

                        if negated {
                            Regex::Not(Box::new(Regex::Choice(choices)))
                        } else {
                            Regex::Choice(choices)
                        }
                    },

                    // Capture groups
                    '(' => {
                        let (group, remaining) = read_until(input, Some(')'));
                        input = remaining;
                        Regex::CapturingGroup(Box::new(group))
                    },
                    ')' => {
                        // This should have been consumed by the parent group
                        unreachable!("Unmatched ')'");
                    },

                    // Anchors
                    '^' => Regex::Start,
                    '$' => Regex::End,

                    // Hit the any key
                    '.' => Regex::Char(CharType::Any),

                    // An alternate choice
                    // This will insert a placeholder we will deal with later
                    '|' => Regex::ChoicePlaceholder,

                    // Single characters
                    c => Regex::Char(CharType::Single(c)),
                };

                // Check for modifiers (+*)
                let node = match input.first() {
                    Some('+') => {
                        input = &input[1..];
                        Regex::Repeated(RepeatType::OneOrMore, Box::new(node))
                    },
                    Some('*') => {
                        input = &input[1..];
                        Regex::Repeated(RepeatType::ZeroOrMore, Box::new(node))
                    },
                    Some('?') => {
                        input = &input[1..];
                        Regex::Repeated(RepeatType::ZeroOrOne, Box::new(node))
                    },
                    _ => node,
                };

                sequence.push(node);
            }

            // If we have any choice placeholders, we need to split the sequence into choices
            if sequence.contains(&Regex::ChoicePlaceholder) {
                let mut choices = vec![];
                let mut current_choice = vec![];

                for node in sequence {
                    if node == Regex::ChoicePlaceholder {
                        choices.push(Regex::Sequence(current_choice));
                        current_choice = vec![];
                    } else {
                        current_choice.push(node);
                    }
                }

                choices.push(Regex::Sequence(current_choice));
                return (Regex::Choice(choices), input);
            }

            (Regex::Sequence(sequence), input)
        }

        let chars = value.chars().collect::<Vec<_>>();
        let (result, remaining_chars) = read_until(&chars, None);
        assert_eq!(remaining_chars.len(), 0, "Remaining input: {:?}", remaining_chars);
        result
    }
}

impl Regex {
    pub fn matches(&self, input: &str) -> bool {
        // Convert to char vec with \1 first and \2 last for start and end of text
        let chars = input.chars().collect::<Vec<_>>();

        // Pattern can apply at any starting point
        for i in 0..chars.len() {
            log::debug!("matches({:?}) against {:?}, start={}", chars[i..].iter().collect::<String>(), &self, i == 0);

            let mut groups = vec![];
            let (matched, _) = self.match_recur(&chars[i..], i == 0, &mut groups);

            if matched {
                return true;
            }
        }

        false
    }

    fn allow_none(&self) -> bool {
        // For each match type, determine if it can match an empty string
        match self {
            Regex::Char(_) => false,
            Regex::Sequence(seq) => seq.iter().all(|node| node.allow_none()),
            Regex::Choice(seq) => seq.iter().any(|node| node.allow_none()),
            Regex::CapturingGroup(node) => node.allow_none(),
            Regex::Not(node) => node.allow_none(),
            Regex::Start => true,
            Regex::End => true,
            Regex::Repeated(RepeatType::OneOrMore, node) => node.allow_none(),
            Regex::Repeated(RepeatType::ZeroOrMore, _) => true,
            Regex::Repeated(RepeatType::ZeroOrOne, _) => true,
            Regex::Backref(_) => true, // The capture group may be empty
            Regex::ChoicePlaceholder => unreachable!("ChoicePlaceholder should have been expanded"),
        }
    }

    fn match_recur<'a>(&self, input: &'a [char], at_start: bool, groups: &mut Vec<Option<&'a [char]>>) -> (bool, &'a [char]) {
        log::debug!("match_recur({self:?}, {}, {at_start})", input.iter().collect::<String>());

        if input.len() == 0 {
            return (self.allow_none(), input);
        }

        match self {
            // Hit the any key
            Regex::Char(CharType::Any) => {
                return (true, &input[1..]);
            },

            // Single character matches
            Regex::Char(CharType::Single(c)) => {
                if input[0] == *c {
                    return (true, &input[1..]);
                }
                return (false, input);
            },
            Regex::Char(CharType::Range(start, end)) => {
                if input[0] >= *start && input[0] <= *end {
                    return (true, &input[1..]);
                }
                return (false, input);
            },

            // A negative match of any other matcher
            Regex::Not(node) => {
                let (matched, new_remaining) = node.match_recur(input, at_start, groups);
                if !matched {
                    return (true, new_remaining);
                }
                return (false, input);
            },

            // Anchors
            Regex::Start => {
                return (at_start, input);
            },
            Regex::End => {
                return (input.len() == 0, input);
            },

            // Multi-match modifiers (+*)
            Regex::Repeated(mode, node) => {
                match mode {
                    RepeatType::OneOrMore => {
                        let mut remaining = input;
                        let mut matched = false;

                        while let (true, new_remaining) = node.match_recur(remaining, at_start, groups) {
                            matched = true;
                            remaining = new_remaining;
                        }

                        return (matched, remaining);
                    },

                    RepeatType::ZeroOrMore => {
                        let mut remaining = input;

                        while let (true, new_remaining) = node.match_recur(remaining, at_start, groups) {
                            remaining = new_remaining;
                        }

                        return (true, remaining);
                    },

                    RepeatType::ZeroOrOne => {
                        let (_, new_remaining) = node.match_recur(input, at_start, groups);
                        return (true, new_remaining);
                    },
                }
            },

            // A sequence of matches, all of which must match
            // If any fails, abort the entire sequence and advance to try again
            Regex::Sequence(seq) => {
                let mut remaining = input;
                let mut seq_at_start = at_start;

                for node in seq {
                    let (matched, new_remaining) = node.match_recur(remaining, seq_at_start, groups);
                    if !matched {
                        return (false, input);
                    }

                    remaining = new_remaining;
                    seq_at_start = false;
                }

                return (true, remaining);
            },

            // A choice of matches, any of which much match
            // If none match, abort the entire choice and advance to try again
            Regex::Choice(seq) => {
                for node in seq {
                    let (matched, new_remaining) = node.match_recur(input, at_start, groups);
                    if matched {
                        return (true, new_remaining);
                    }
                }

                return (false, input);
            },

            // Capturing groups wrap another node and then store what was captured
            Regex::CapturingGroup(node) => {
                // Add a placeholder to get order correct
                let index = groups.len();
                groups.push(None);

                let (matched, new_remaining) = node.match_recur(input, at_start, groups);
                if matched {
                    groups[index] = Some(&input[..(input.len() - new_remaining.len())]);
                    return (true, new_remaining);
                }

                groups.remove(index);
                return (false, input);
            },

            // Backreferences
            Regex::Backref(index) => {
                let index = index - 1; // 1-indexed

                // If we haven't captured that group, this is a problem
                if groups.len() <= index || groups[index].is_none() {
                    unimplemented!("Backreference to group {} that hasn't been captured", index);
                }

                let captured = groups[index].unwrap();
                if input.starts_with(captured) {
                    return (true, &input[captured.len()..]);
                }
                return (false, input);
            },

            // This should have been expanded by the time we get here
            Regex::ChoicePlaceholder => unreachable!("ChoicePlaceholder should have been expanded"),
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
    env_logger::init();

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
                // TODO: Test against system grep to verify expected is actually expected
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

    test_regex!(sequence_hole, r"ab", "acb", false);

    test_regex!(choice, r"[abc]", "a", true);
    test_regex!(choice2, r"[abc]", "b", true);
    test_regex!(choice3, r"[abc]", "c", true);
    test_regex!(choice4, r"[abc]", "d", false);
    test_regex!(choice_multi, r"[abc]", "efagh", true);
    test_regex!(choice_multi_not, r"[abc]", "defgh", false);

    test_regex!(choice_negated, r"[^abc]", "d", true);
    test_regex!(choice_negated2, r"[^abc]", "a", false);
    test_regex!(choice_negated_long, r"[^abc]", "defaghi", true); // Any character can be not abc
    test_regex!(choice_negated_long2, r"[^abc]", "abcabc", false); // All characters must be not abc

    test_regex!(escaped_backslash, r"\\", "\\", true);

    test_regex!(anchor_start, "^a", "a", true);
    test_regex!(anchor_start2, "^a", "ba", false);
    test_regex!(anchor_start3, "^a", "ab", true);
    test_regex!(anchor_end, "a$", "a", true);
    test_regex!(anchor_end2, "a$", "ba", true);
    test_regex!(anchor_end3, "a$", "ab", false);

    test_regex!(choice_only_negate, "[^anb]", "banana", false);

    test_regex!(one_or_more, "a+", "a", true);
    test_regex!(one_or_more2, "a+", "aa", true);
    test_regex!(one_or_more3, "a+", "bbb", false);

    test_regex!(zero_or_more, "a*", "a", true);
    test_regex!(zero_or_more2, "a*", "aa", true);
    test_regex!(zero_or_more3, "a*", "bbb", true);

    test_regex!(zero_or_one, "a?", "a", true);
    test_regex!(zero_or_one2, "a?", "aa", true);
    test_regex!(zero_or_one3, "a?", "bbb", true);

    test_regex!(any_key, "c.t", "cat", true);
    test_regex!(any_key2, "c.t", "cot", true);
    test_regex!(any_key3, "c.t", "dog", false);

    test_regex!(alternate, "a|b", "a", true);
    test_regex!(alternate2, "a|b", "b", true);
    test_regex!(alternate3, "a|b", "c", false);
    test_regex!(alternate4, "(cat|dog)", "cat", true);
    test_regex!(alternate5, "(cat|dog)", "frog", false);

    test_regex!(multiple_groups, "(a|b)(c|d)", "ac", true);
    test_regex!(multiple_groups2, "(a|b)(c|d)", "bd", true);
    test_regex!(multiple_groups3, "(a|b)(c|d)", "ca", false);

    test_regex!(backref, r"(a)\1", "aa", true);
    test_regex!(backref_not, r"(a)\1", "ab", false);
    test_regex!(backref_double, r"(a)\1\1", "aaa", true);
    test_regex!(backref_multiple, r"(a)(b)\2\1", "abba", true);
}