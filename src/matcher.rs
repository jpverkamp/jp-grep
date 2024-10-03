use crate::types::{CharType, Regex, RepeatType};

impl Regex {
    pub fn matches(&self, input: &str) -> bool {
        // Convert to char vec with \1 first and \2 last for start and end of text
        let chars = input.chars().collect::<Vec<_>>();

        // Pattern can apply at any starting point
        for i in 0..chars.len() {
            log::debug!(
                "matches({:?}) against {:?}, start={}",
                chars[i..].iter().collect::<String>(),
                &self,
                i == 0
            );

            let mut groups = vec![];
            let results = self.match_recur(&chars[i..], i == 0, &mut groups);

            if !results.is_empty() {
                return true;
            }
        }

        false
    }

    fn allow_none(&self) -> bool {
        // For each match type, determine if it can match an empty string
        match self {
            Regex::Char(_) => false,
            Regex::CharacterGroup(_, _) => false,
            Regex::Sequence(seq) => seq.iter().all(|node| node.allow_none()),
            Regex::Choice(seq) => seq.iter().any(|node| node.allow_none()),
            Regex::CapturingGroup(node) => node.allow_none(),
            Regex::Start => true,
            Regex::End => true,
            Regex::Repeated(RepeatType::OneOrMore, node) => node.allow_none(),
            Regex::Repeated(RepeatType::ZeroOrMore, _) => true,
            Regex::Repeated(RepeatType::ZeroOrOne, _) => true,
            Regex::Backref(_) => true, // The capture group may be empty
            Regex::ChoicePlaceholder => unreachable!("ChoicePlaceholder should have been expanded"),
        }
    }

    fn match_recur<'a>(
        &self,
        input: &'a [char],
        at_start: bool,
        groups: &mut Vec<Option<&'a [char]>>,
    ) -> Vec<&'a [char]> {
        log::debug!(
            "match_recur({self:?}, {}, {at_start})",
            input.iter().collect::<String>()
        );

        if input.len() == 0 {
            if self.allow_none() {
                return vec![input];
            } else {
                return vec![];
            }
        }

        match self {
            // Hit the any key
            Regex::Char(CharType::Any) => {
                return vec![&input[1..]];
            }

            // Single character matches
            Regex::Char(CharType::Single(c)) => {
                if input[0] == *c {
                    return vec![&input[1..]];
                }
                return vec![];
            }
            Regex::Char(CharType::Range(start, end)) => {
                if input[0] >= *start && input[0] <= *end {
                    return vec![&input[1..]];
                }
                return vec![];
            }

            // Character groups, match any of the characters (or none if negated)
            Regex::CharacterGroup(chars, negated) => {
                let matched = chars.iter().any(|c| match c {
                    CharType::Single(c) => input[0] == *c,
                    CharType::Range(start, end) => input[0] >= *start && input[0] <= *end,
                    CharType::Any => true,
                });

                if negated ^ matched {
                    return vec![&input[1..]];
                } else {
                    return vec![];
                }
            }

            // Anchors
            Regex::Start => {
                if at_start {
                    return vec![input];
                } else {
                    return vec![];
                }
            }
            Regex::End => {
                if input.len() == 0 {
                    return vec![input];
                } else {
                    return vec![];
                }
            }

            // Multi-match modifiers (?+*)
            // NOTE: These should match the longest group they can and still work
            Regex::Repeated(mode, node) => {
                match mode {
                    RepeatType::ZeroOrMore => {
                        // Return all possible matches at this level
                        // Base case: match nothing and return input as is
                        let mut results = vec![input];
                        let mut remaining = input;

                        loop {
                            let recur = node.match_recur(remaining, at_start, groups);
                            if recur.is_empty() {
                                break;
                            }

                            for new_remaining in recur {
                                results.push(new_remaining);
                                remaining = new_remaining;
                            }
                        }

                        results.reverse();
                        return results;
                    }

                    RepeatType::OneOrMore => {
                        // Return all possible matches at this level
                        // No base case: must match at least once
                        let mut results = vec![];
                        let mut remaining = input;

                        loop {
                            let recur = node.match_recur(remaining, at_start, groups);
                            if recur.is_empty() {
                                break;
                            }

                            for new_remaining in recur {
                                results.push(new_remaining);
                                remaining = new_remaining;
                            }
                        }

                        results.reverse();
                        return results;
                    }

                    RepeatType::ZeroOrOne => {
                        // If zero match
                        let mut results = vec![input];

                        // If one match
                        let mut recur = node.match_recur(input, at_start, groups);
                        results.append(&mut recur);

                        results.reverse();
                        return results;
                    }
                }
            }

            // A sequence of matches, all of which must match
            // If any fails, abort the entire sequence and advance to try again
            Regex::Sequence(seq) => {
                // Keep a list of the possible branching values
                // TODO: This is hugely memory intensive :)
                let mut remainings = vec![input];
                let mut seq_at_start = at_start;

                for node in seq {
                    remainings = remainings
                        .into_iter()
                        .flat_map(|input| node.match_recur(input, seq_at_start, groups))
                        .collect();
                    seq_at_start = false;
                }

                return remainings;
            }

            // A choice of matches, any of which much match
            // If none match, abort the entire choice and advance to try again
            Regex::Choice(seq) => {
                let mut results = vec![];

                for node in seq {
                    let mut recur = node.match_recur(input, at_start, groups);
                    if !recur.is_empty() {
                        results.append(&mut recur);
                    }
                }

                return results;
            }

            // Capturing groups wrap another node and then store what was captured
            Regex::CapturingGroup(node) => {
                // Add a placeholder to get order correct
                let index = groups.len();
                groups.push(None);

                let recur = node.match_recur(input, at_start, groups);
                if recur.is_empty() {
                    groups.remove(index);
                    return vec![];
                } else {
                    groups[index] = Some(&input[..(input.len() - recur[0].len())]);
                    return recur;
                }
            }

            // Backreferences
            Regex::Backref(index) => {
                let index = index - 1; // 1-indexed

                // If we haven't captured that group, this is a problem
                if groups.len() <= index || groups[index].is_none() {
                    unimplemented!("Backreference to group {} that hasn't been captured", index);
                }

                let captured = groups[index].unwrap();
                if input.starts_with(captured) {
                    return vec![&input[captured.len()..]];
                }
                return vec![];
            }

            // This should have been expanded by the time we get here
            Regex::ChoicePlaceholder => unreachable!("ChoicePlaceholder should have been expanded"),
        }
    }
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
    test_regex!(backref_nested, r"(a(b)\2)\1", "abbabb", true);

    test_regex!(negative_repeated_character_group2, r"[^abc]+", "xyz", true);

    test_regex!(repeated_longest, r"a+a", "aaa", true);
}
