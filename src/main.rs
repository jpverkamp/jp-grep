use std::{
    collections::VecDeque,
    fs::File,
    io::{BufRead, BufReader, StdinLock},
};

use clap::Parser;
use types::{Flags, Regex};

mod matcher;
mod parser;
mod types;

/// A custom grep implementation; always behaves as egrep
#[derive(Parser, Debug)]
#[command(version, about, long_about = None, disable_help_flag = true)]
struct Args {
    /// Lines of context to print after each match
    #[clap(short = 'A', long)]
    after_context: Option<usize>,
    /// Lines of context to print before each match
    #[clap(short = 'B', long)]
    before_context: Option<usize>,
    /// Lines to print both before and after
    #[clap(short = 'C', long)]
    context: Option<usize>,
    /// Only print the matching count
    #[clap(short = 'c', long)]
    count: bool,
    /// Extended regex mode (egrep); this option is ignored (always true)
    #[clap(short = 'E', long)]
    extended_regexp: bool,
    /// Additional patterns, will return a line if any match
    #[clap(short = 'e', long = "regexp", action = clap::ArgAction::Append, required = false)]
    additional_patterns: Vec<String>,
    /// Never print filenames
    #[clap(short = 'h', long)]
    no_filename: bool,
    /// Display this help message
    #[clap(long, action = clap::ArgAction::HelpLong, required = false)]
    help: Option<bool>,
    /// Default to case insensitive match
    #[clap(short = 'i', long)]
    ignore_case: bool,
    /// Print line numbers before matches and context
    #[clap(short = 'n', long)]
    line_number: bool,
    /// Invert the match; only print lines that don't match any pattern
    #[clap(short = 'v', long)]
    invert_match: bool,
    /// The expression to parse
    pattern: Option<String>,
    /// Paths to search for matches
    paths: Vec<String>,
}

fn main() {
    env_logger::init();

    let args = Args::parse();

    // Determine how much context we have
    if args.context.is_some() && (args.after_context.is_some() || args.before_context.is_some()) {
        eprintln!("Cannot specify -C/--context with -A/--after-context or -B/--before-context");
        std::process::exit(1);
    }

    // Collect and parse all regexes from pattern + -e flags; must end up with at least one
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

    // Let's actually run some regex!
    let mut matches = 0;

    // How many lines to print before and after + a flag if we're printing context at all
    // If we're printing context, use -- as a separator between findings
    let context_before = args.context.unwrap_or(args.before_context.unwrap_or(0));
    let context_after = args.context.unwrap_or(args.after_context.unwrap_or(0));
    let has_context = context_before > 0 || context_after > 0;

    // Countdown is how many lines to print until we meet the after
    let mut context_after_countdown = 0;

    // Holds 'before' lines in a rotating buffer in case we need to print them
    let mut context_buffer: VecDeque<String> = VecDeque::new();

    // Flag to determine if we need to print a --; only false for the first match
    let mut had_previous_match = false;

    // How many lines we've seen since the last match; used to determine if this match combines with the previous
    let mut context_before_countup = 0;

    // Set up flags
    let mut flags = Flags::default();
    if args.ignore_case {
        flags.case_insensitive = true;
    }

    // Collect the inputs we're going to loop over
    let inputs = collect_input(&args);
    let print_filenames = inputs.len() > 1 && !args.no_filename;

    // Finally... loop over the input
    for (name, mut readable) in inputs {
        for (line_number, line) in readable.lines().enumerate() {
            let input_line = line.unwrap();

            // Check if any of the regexes match; inverting matches if necessary
            let is_match = if args.invert_match {
                regexes
                    .iter()
                    .all(|regex| !regex.matches(&input_line, flags))
            } else {
                regexes
                    .iter()
                    .any(|regex| regex.matches(&input_line, flags))
            };
            if is_match {
                // Case 1: A regex matched
                matches += 1;

                // If we're in counting mode, do nothing of the rest
                // It will still buffer input, but never print it so :shrug:
                if args.count {
                    continue;
                }

                // We have finished printing the previous match
                if context_after_countdown == 0 {
                    // And there was at least one line in between (and we're printing context)
                    if has_context
                        && had_previous_match
                        && context_before_countup > context_buffer.len()
                    {
                        println!("--");
                    }

                    // Print out and clear the context buffer
                    for (offset, previous_line) in context_buffer.iter().enumerate() {
                        print_line(
                            &previous_line,
                            line_number - context_buffer.len() + offset + 1,
                            &name,
                            args.line_number,
                            print_filenames
                        );
                    }
                    context_buffer.clear();
                }

                // Then print out this line and print the next context_after lines no matter what
                print_line(
                    &input_line,
                    line_number + 1,
                    &name,
                    args.line_number,
                    print_filenames,
                );

                context_after_countdown = context_after;
                context_before_countup = 0;
                had_previous_match = true;
            } else if context_after_countdown > 0 {
                // Case 2: No match, but we're counting down 'after' context
                print_line(
                    &input_line,
                    line_number + 1,
                    &name,
                    args.line_number,
                    print_filenames,
                );

                context_after_countdown -= 1;
                context_before_countup = 0;
            } else {
                // Case 3: No match and we're not counting down any more
                // Buffer potential 'before' context, but only keep so many lines
                context_before_countup += 1;
                context_buffer.push_back(input_line);
                if context_buffer.len() > context_before {
                    context_buffer.pop_front();
                }
            }
        }
    }

    // Hopefully we didn't print anything else in this mode :smile:
    if args.count {
        println!("{}", matches);
    }

    std::process::exit(if matches > 0 { 0 } else { 1 });
}


// Collect the Lines iterators we're going to be working on
enum Readable<'a> {
    Stdin(BufReader<StdinLock<'a>>),
    File(BufReader<File>),
}

impl Readable<'_> {
    fn lines(&mut self) -> Box<dyn Iterator<Item = std::io::Result<String>> + '_> {
        match self {
            Readable::Stdin(r) => Box::new(r.lines().map(|line| line.map(|s| s.to_string()))),
            Readable::File(r) => Box::new(r.lines().map(|line| line.map(|s| s.to_string()))),
        }
    }
}

fn collect_input(args: &Args) -> Vec<(String, Readable<'_>)> {
    let mut files = vec![];

    if args.paths.is_empty() {
        files.push((
            "stdin".to_string(),
            Readable::Stdin(BufReader::new(std::io::stdin().lock())),
        ));
    } else {
        for path in args.paths.iter() {
            match File::open(&path) {
                Ok(f) => files.push((path.clone(), Readable::File(BufReader::new(f)))),
                Err(e) => {
                    eprintln!("Error opening file {path}: {error}", path = path, error = e);
                    std::process::exit(1);
                }
            }
        }
    }

    files
}

fn print_line(content: &str, line_number: usize, path: &str, print_line_numbers: bool, print_filename: bool) {
    if print_line_numbers {
        if print_filename {
            println!("{path}:{no}:{content}", path = path, no = line_number, content = content);
        } else {
            println!("{no}:{content}", no = line_number, content = content);
        }
    } else {
        if print_filename {
            println!("{path}:{content}", path = path, content = content);
        } else {
            println!("{content}", content = content);
        }
    }
}
