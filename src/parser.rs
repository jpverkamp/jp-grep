use crate::types::{CharType, Regex, RepeatType};

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
                            }

                            // Escaped control characters
                            Some(&c) if "\\()[]|".contains(c) => Regex::Char(CharType::Single(c)),

                            // A group we don't know about
                            _ => unimplemented!(),
                        };
                        input = &input[1..];
                        group
                    }

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
                        let mut previous = None;

                        while let Some(&c) = input.first() {
                            input = &input[1..];

                            match c {
                                // End of character group
                                ']' => break,
                                // Escaped characters, always treat as literal
                                '\\' => {
                                    let c = input.first().unwrap();
                                    input = &input[1..];

                                    choices.push(CharType::Single(*c));
                                    previous = Some(*c);
                                }
                                // Possible a range; if we are at the start or right after a group it is a literal
                                '-' if previous.is_some() => {
                                    // Match against the next character
                                    match input.first().copied() {
                                        // End of group, treat as a literal and exit the character group
                                        Some(']') => {
                                            choices.push(CharType::Single(c));
                                            break;
                                        }
                                        // Anything else tries to make a range
                                        Some(end) => {
                                            // Ranges must be start <= end (a-a is technically valid)
                                            let start = previous.unwrap();
                                            if start > end {
                                                panic!("Invalid range: {}-{}", start, end);
                                            }

                                            // Remove the previously pushed single character
                                            // TODOD: This *should* always exist
                                            choices.pop();

                                            choices.push(CharType::Range(start, c));
                                            previous = None;
                                        }
                                        // Rand out of characters while parsing a [...]
                                        None => {
                                            panic!("Unexpected end of input while parsing character group");
                                        }
                                    };
                                }
                                // Anything else is a literal character
                                _ => {
                                    choices.push(CharType::Single(c));
                                    previous = Some(c);
                                }
                            }
                        }

                        Regex::CharacterGroup(choices, negated)
                    }

                    // Capture groups
                    '(' => {
                        let (group, remaining) = read_until(input, Some(')'));
                        input = remaining;
                        Regex::CapturingGroup(Box::new(group))
                    }
                    ')' => {
                        // This should have been consumed by the parent group
                        unreachable!("Unmatched ')'");
                    }

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
                    }
                    Some('*') => {
                        input = &input[1..];
                        Regex::Repeated(RepeatType::ZeroOrMore, Box::new(node))
                    }
                    Some('?') => {
                        input = &input[1..];
                        Regex::Repeated(RepeatType::ZeroOrOne, Box::new(node))
                    }
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
        assert_eq!(
            remaining_chars.len(),
            0,
            "Remaining input: {:?}",
            remaining_chars
        );
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! test_parse {
        ($name:ident, $input:expr, $expected:expr) => {
            #[test]
            fn $name() {
                let regex = Regex::from($input.to_string());
                assert_eq!(regex, $expected);
            }
        };
    }

    test_parse!(
        parse_single_char,
        "a",
        Regex::Sequence(vec![Regex::Char(CharType::Single('a'))])
    );
    test_parse!(
        parse_single_char_any,
        ".",
        Regex::Sequence(vec![Regex::Char(CharType::Any)])
    );
    test_parse!(
        parse_char_group,
        "[a-z]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Range('a', 'z')],
            false
        )])
    );
    test_parse!(
        parse_char_group_multiple,
        "[a-z0-9]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Range('a', 'z'), CharType::Range('0', '9')],
            false
        )])
    );
    test_parse!(
        parse_char_group_literal_dash,
        "[-]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Single('-')],
            false
        )])
    );
    test_parse!(
        parse_char_group_literal_dash_after,
        "[a-]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Single('a'), CharType::Single('-')],
            false
        )])
    );
    test_parse!(
        parse_char_group_literal_dash_after_range,
        "[a-z-]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Range('a', 'z'), CharType::Single('-')],
            false
        )])
    );
}