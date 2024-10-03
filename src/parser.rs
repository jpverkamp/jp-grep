use crate::types::{AssertionType, CharType, Regex, RepeatType};

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
                        let escaped_c = input.first();
                        let group = match escaped_c {
                            Some('d') => {
                                Regex::CharacterGroup(vec![CharType::Range('0', '9')], false)
                            }
                            Some('D') => {
                                Regex::CharacterGroup(vec![CharType::Range('0', '9')], true)
                            }

                            Some('w') => Regex::CharacterGroup(
                                vec![
                                    CharType::Range('a', 'z'),
                                    CharType::Range('A', 'Z'),
                                    CharType::Range('0', '9'),
                                    CharType::Single('_'),
                                ],
                                false,
                            ),
                            Some('W') => Regex::CharacterGroup(
                                vec![
                                    CharType::Range('a', 'z'),
                                    CharType::Range('A', 'Z'),
                                    CharType::Range('0', '9'),
                                    CharType::Single('_'),
                                ],
                                true,
                            ),

                            // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_expressions/Character_classes
                            Some('s') => Regex::CharacterGroup(
                                vec![
                                    CharType::Single('\u{000c}'), // Form feed \f
                                    CharType::Single('\n'),
                                    CharType::Single('\r'),
                                    CharType::Single('\t'),
                                    CharType::Single('\u{000b}'), // Vertical tab \v
                                    CharType::Single('\u{0020}'),
                                    CharType::Single('\u{00a0}'),
                                    CharType::Single('\u{1680}'),
                                    CharType::Range('\u{2000}', '\u{200a}'),
                                    CharType::Single('\u{2028}'),
                                    CharType::Single('\u{2029}'),
                                    CharType::Single('\u{202f}'),
                                    CharType::Single('\u{205f}'),
                                    CharType::Single('\u{3000}'),
                                    CharType::Single('\u{feff}'),
                                ],
                                false,
                            ),
                            Some('S') => Regex::CharacterGroup(
                                vec![
                                    CharType::Single('\u{000c}'), // Form feed \f
                                    CharType::Single('\n'),
                                    CharType::Single('\r'),
                                    CharType::Single('\t'),
                                    CharType::Single('\u{000b}'), // Vertical tab \v
                                    CharType::Single('\u{0020}'),
                                    CharType::Single('\u{00a0}'),
                                    CharType::Single('\u{1680}'),
                                    CharType::Range('\u{2000}', '\u{200a}'),
                                    CharType::Single('\u{2028}'),
                                    CharType::Single('\u{2029}'),
                                    CharType::Single('\u{202f}'),
                                    CharType::Single('\u{205f}'),
                                    CharType::Single('\u{3000}'),
                                    CharType::Single('\u{feff}'),
                                ],
                                true,
                            ),

                            // Character literals
                            Some('n') => Regex::Char(CharType::Single('\n')),
                            Some('r') => Regex::Char(CharType::Single('\r')),
                            Some('t') => Regex::Char(CharType::Single('\t')),
                            Some('f') => Regex::Char(CharType::Single('\u{000c}')),
                            Some('v') => Regex::Char(CharType::Single('\u{000b}')),
                            Some('0') => Regex::Char(CharType::Single('\0')),

                            // Caret notation https://en.wikipedia.org/wiki/Caret_notation
                            Some('c') => {
                                input = &input[1..]; // Drop the c
                                let c = match input.first() {
                                    Some(&c) => c,
                                    None => {
                                        panic!("Unexpected end of input after \\c");
                                    }
                                };

                                assert!(c.is_ascii_uppercase(), "Invalid caret notation: \\c{}", c);

                                let code_point = (c as u8) - b'A' + 1;
                                Regex::Char(CharType::Single(code_point as char))
                            }

                            // Unicode characters
                            // TODO: Support \u{hhhh}
                            Some('h') | Some('u') => {
                                let length = match escaped_c {
                                    Some('h') => 2,
                                    Some('u') => 4,
                                    _ => unreachable!(),
                                };

                                let mut code_point = 0;
                                for i in 0..length {
                                    let c = match input.iter().nth(i + 1) {
                                        Some(&c) => c,
                                        None => {
                                            panic!(
                                                "Not enough characters in {}",
                                                escaped_c.unwrap()
                                            );
                                        }
                                    };

                                    code_point = code_point * 16
                                        + c.to_digit(16).expect(
                                            format!("Invalid hex digit '{c}' in \\u sequence")
                                                .as_str(),
                                        );
                                }

                                // Drop the characters we just read; the u/h will be removed at the end of this match
                                input = &input[length..];

                                Regex::Char(CharType::Single(char::from_u32(code_point).expect(
                                    format!("Invalid unicode code point: {code_point}").as_str(),
                                )))
                            }

                            // TODO: Unicode properties
                            Some('p') | Some('P') => {
                                unimplemented!("Unicode properties are not supported (yet!)");
                            }

                            // Backreference
                            Some(&c) if c.is_digit(10) => {
                                let index = c.to_digit(10).unwrap() as usize;
                                Regex::Backref(index)
                            }

                            // TODO: Named backreferences
                            // Format is \k<...>
                            Some('k') => {
                                unimplemented!("Named backreferences are not supported (yet!)");
                            }

                            // Escaped characters: anything else after a \ is a literal char
                            Some(&c) => Regex::Char(CharType::Single(c)),

                            // Pattern may not end with a single trailing \
                            None => {
                                panic!("Unexpected end of input after \\");
                            }
                        };
                        input = &input[1..];
                        group
                    }

                    // Custom defined character groups
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
                                // TODO: [\b] is a backspace character apparently
                                '\\' => {
                                    let c = match input.first() {
                                        Some(&c) => c,
                                        None => {
                                            panic!("Unexpected end of input while parsing an escape character in a character group");
                                        }
                                    };
                                    input = &input[1..];

                                    choices.push(CharType::Single(c));
                                    previous = Some(c);
                                }
                                // Possible a range; if we are at the start or right after a group it is a literal
                                '-' if previous.is_some() => {
                                    // Match against the next character
                                    match input.first().copied() {
                                        // End of group, treat as a literal and exit the character group
                                        Some(']') => {
                                            choices.push(CharType::Single(c));
                                            input = &input[1..];
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

                                            choices.push(CharType::Range(start, end));
                                            previous = None;
                                            input = &input[1..];
                                            continue;
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

                    // Capture groups and assertions
                    '(' => {
                        // TODO (?:x) for non-capturing groups
                        // TODO (?<name>x) for named groups
                        // TODO (?flags:x) for flags (i, m, and s)
                        // TODO (?=x) for positive lookahead
                        // TODO (?!x) for negative lookahead
                        // TODO (?<=x) for positive lookbehind
                        // TODO (?<!x) for negative lookbehind

                        enum Mode {
                            Default,
                            NonCapturing,
                            Flags(bool, bool, bool),
                            Assertion(AssertionType),
                        }

                        let mut mode = Mode::Default;

                        if let Some('?') = input.first() {
                            // Second character will determine the mode
                            // Note for all the input[N..], it's N-1 because we'll consume one later
                            match input.iter().nth(1) {
                                Some(':') => {
                                    mode = Mode::NonCapturing;
                                    input = &input[1..];
                                }
                                Some('=') => {
                                    mode = Mode::Assertion(AssertionType::PositiveLookahead);
                                    input = &input[1..];
                                }
                                Some('!') => {
                                    mode = Mode::Assertion(AssertionType::NegativeLookahead);
                                    input = &input[1..];
                                }
                                Some(c) if c.is_ascii_alphabetic() => {
                                    // Flags
                                    let mut i = false;
                                    let mut m = false;
                                    let mut s = false;
                                    let mut char_count = 0;

                                    while let Some(&c) = input.iter().nth(char_count + 1) {
                                        match c {
                                            'i' => i = true,
                                            'm' => m = true,
                                            's' => s = true,
                                            ':' => break,
                                            _ => {
                                                panic!("Invalid flags, expected :, got {}", c);
                                            }
                                        }
                                        char_count += 1;
                                    }

                                    mode = Mode::Flags(i, m, s);
                                    input = &input[char_count..];
                                }
                                Some('<') => {
                                    // Lookbehind
                                    if let Some('=') = input.iter().nth(2) {
                                        mode = Mode::Assertion(AssertionType::PositiveLookbehind);
                                        input = &input[2..];
                                    } else if let Some('!') = input.iter().nth(2) {
                                        mode = Mode::Assertion(AssertionType::NegativeLookbehind);
                                        input = &input[2..];
                                    } else {
                                        panic!("Invalid lookbehind assertion, expected only ?<= or ?<!");
                                    }
                                }
                                _ => {
                                    panic!("Invalid group assertion, there must be at least one character after the ?");
                                }
                            }

                            // Skip the ?
                            input = &input[1..];
                        }

                        let (group, remaining) = read_until(input, Some(')'));
                        input = remaining;

                        match mode {
                            Mode::Default => Regex::CapturingGroup(Box::new(group)),
                            Mode::NonCapturing => group,
                            Mode::Flags(_i, _m, _s) => {
                                unimplemented!("Flags are not supported (yet!)");
                            }
                            Mode::Assertion(assertion) => {
                                Regex::Assertion(assertion, Box::new(group))
                            }
                        }
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

    // Basic tests for single characters
    test_parse!(
        single_char,
        "a",
        Regex::Sequence(vec![Regex::Char(CharType::Single('a'))])
    );

    test_parse!(
        single_char_any,
        ".",
        Regex::Sequence(vec![Regex::Char(CharType::Any)])
    );

    // Test character classes
    test_parse!(
        char_class_digit,
        "\\d",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Range('0', '9')],
            false
        )])
    );

    test_parse!(
        char_class_not_digit,
        "\\D",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Range('0', '9')],
            true
        )])
    );

    // Test escape sequences
    test_parse!(
        escape_newline,
        "\\n",
        Regex::Sequence(vec![Regex::Char(CharType::Single('\n'))])
    );

    test_parse!(
        escape_carriage_return,
        "\\r",
        Regex::Sequence(vec![Regex::Char(CharType::Single('\r'))])
    );

    test_parse!(
        caret_notation,
        "\\cM\\cJ",
        Regex::Sequence(vec![
            Regex::Char(CharType::Single('\r')),
            Regex::Char(CharType::Single('\n')),
        ])
    );

    test_parse!(
        hex_escape,
        "\\h41",
        Regex::Sequence(vec![Regex::Char(CharType::Single('A'))])
    );

    test_parse!(
        unicode_escape,
        "\\u0041",
        Regex::Sequence(vec![Regex::Char(CharType::Single('A'))])
    );

    test_parse!(
        higher_unicode,
        "\\u2603",
        Regex::Sequence(vec![Regex::Char(CharType::Single('☃'))])
    );

    // Tests for character groups
    test_parse!(
        char_group,
        "[a-z]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Range('a', 'z')],
            false
        )])
    );

    test_parse!(
        char_group_multiple,
        "[a-z0-9]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Range('a', 'z'), CharType::Range('0', '9')],
            false
        )])
    );

    test_parse!(
        char_group_literal_dash,
        "[-]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Single('-')],
            false
        )])
    );

    test_parse!(
        char_group_literal_dash_after,
        "[a-]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Single('a'), CharType::Single('-')],
            false
        )])
    );

    test_parse!(
        char_group_literal_dash_after_range,
        "[a-z-]",
        Regex::Sequence(vec![Regex::CharacterGroup(
            vec![CharType::Range('a', 'z'), CharType::Single('-')],
            false
        )])
    );

    // Assertions
    test_parse!(
        positive_lookahead,
        "a(?=b)",
        Regex::Sequence(vec![
            Regex::Char(CharType::Single('a')),
            Regex::Assertion(
                AssertionType::PositiveLookahead,
                Box::new(
                    Regex::Sequence(vec![Regex::Char(CharType::Single('b'))])
                )
            ),
        ])
    );

    test_parse!(
        negative_lookahead,
        "a(?!bat)",
        Regex::Sequence(vec![
            Regex::Char(CharType::Single('a')),
            Regex::Assertion(
                AssertionType::NegativeLookahead,
                Box::new(
                    Regex::Sequence(vec![Regex::Char(CharType::Single('b')), Regex::Char(CharType::Single('a')), Regex::Char(CharType::Single('t'))])
                )
            ),
        ])
    );
}
