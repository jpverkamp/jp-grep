
This is my Rust solutions to the
["Build Your Own grep" Challenge](https://app.codecrafters.io/courses/grep/overview).

Supported:
  - Only the `-E` (extended) regex flag, reads from stdin
  - Single chars, ranges, dots
  - `\d` and `\w`
  - Character groups (without ranges), also negated
  - `^` and `$`
  - Repeated: `?` `*` and `+`
  - Choices with `|`, both in `(...)` and not
  - Capturing groups
  - Backreferences (including multiple and nested)

As the base `grep -E` does, this will print all matching lines and return exit code 0 if any lines matched and 1 otherwise.

To run:

```bash
$ cargo build --release
$ cat input_file | ./target/release/jp-grep -E "(some[regex]+)"
```

To install:

```bash
$ cargo install --path .