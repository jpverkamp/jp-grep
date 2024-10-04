#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Regex {
    // Single characters
    Char(CharType),
    // A sequence of regexes that each must match in order
    Sequence(Vec<Regex>),
    // A character group, may be negated
    // True if inverted, (eg [^abc])
    CharacterGroup(Vec<CharType>, bool),
    // A capturing group used for backreferences
    CapturingGroup(Box<Regex>, Option<String>),
    Backref(usize),
    NamedBackref(String),
    // Assertions
    Assertion(AssertionType, Box<Regex>),
    // Repeat a pattern (e.g. +, *, ?)
    // Flag is if the match is greedy
    Repeated(RepeatType, bool, Box<Regex>),
    // Anchors for teh start and end of a line
    Start,
    End,
    // Used for parsing |, will be expanded into a Choice
    Choice(Vec<Regex>),
    ChoicePlaceholder,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CharType {
    Any,
    Single(char),
    Range(char, char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RepeatType {
    OneOrMore,
    ZeroOrMore,
    ZeroOrOne,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum AssertionType {
    PositiveLookahead,
    NegativeLookahead,
    PositiveLookbehind,
    NegativeLookbehind,
}
