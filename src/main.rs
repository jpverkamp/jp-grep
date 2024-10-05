use std::{collections::VecDeque, io::BufRead};

use clap::Parser;
use types::Regex;

mod matcher;
mod parser;
mod types;

/// A custom grep implementation; always behaves as egrep
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Lines of context to print after each match
    #[clap(short='A', long)]
    after_context: Option<usize>,
    /// Lines of context to print before each match
    #[clap(short='B', long)]
    before_context: Option<usize>,
    /// Lines to print both before and after
    #[clap(short='C', long)]
    context: Option<usize>,
    /// If we're operating in extended mode, this is ignored (always treated as true)
    #[clap(short = 'E', long)]
    extended_regexp: bool,
    /// Additional patterns, will return a line if any match
    #[clap(short='e', long="regexp", action=clap::ArgAction::Append, required=false)]
    additional_patterns: Vec<String>,
    /// The expression to parse
    pattern: Option<String>,
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
    let stdin = std::io::stdin();
    let mut matches = 0;

    // How many lines to print before and after + a flag if we're printing context at all
    // If we're printing context, use -- as a separator between findings
    let context_before = args.context.unwrap_or(args.before_context.unwrap_or(0));
    let context_after = args.context.unwrap_or(args.after_context.unwrap_or(0));
    let has_context = context_before > 0 || context_after > 0;

    // Countdown is how many lines to print until we meet the after
    let mut context_after_countdown = 0;
    
    // Holds 'before' lines in a rotating buffer in case we need to print them
    let mut context_buffer = VecDeque::new();

    // Flag to determine if we need to print a --; only false for the first match
    let mut had_previous_match = false;

    // How many lines we've seen since the last match; used to determine if this match combines with the previous
    let mut context_before_countup = 0;

    // Finally... loop over the input
    for line in stdin.lock().lines() {
        let input_line = line.unwrap();

        if regexes.iter().any(|regex| regex.matches(&input_line)) {

            // Case 1: A regex matched
            matches += 1;

            // We have finished printing the previous match
            if context_after_countdown == 0 {
                // And there was at least one line in between (and we're printing context)
                if has_context && had_previous_match && context_before_countup > context_buffer.len() {
                    println!("--");
                }

                // Print out and clear the context buffer
                for line in context_buffer.iter() {
                    println!("{}", line);
                }
                context_buffer.clear();
            }

            // Then print out this line and print the next context_after lines no matter what
            println!("{}", input_line);
            context_after_countdown = context_after;
            context_before_countup = 0;
            had_previous_match = true;

        } else if context_after_countdown > 0 {

            // Case 2: No match, but we're counting down 'after' context
            println!("{}", input_line);
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

    std::process::exit(if matches > 0 { 0 } else { 1 });
}
