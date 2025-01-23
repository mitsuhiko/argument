# argument-parser

`argument-parser` is a crate that implements an argument parser for command
lines following POSIX conventions.  It's dead simple, dependency free and is
built so you can drive your own parsing.

The goal of this crate is that it's stable, excellently tested and requires
little updates and maintenance.  You can use it and it keeps working.

```rust
use argument_parser::{Error, Parser};

fn main() -> Result<(), Error> {
    let mut parser = Parser::from_env();

    while let Some(param) = parser.param()? {
        if param.is_short('n') || param.is_long("number") {
            println!("Got number {}", parser.value::<i32>()?);
        } else if param.is_arg() {
            println!("Got arg {}", parser.string_value()?);
        } else {
            return Err(param.into_unexpected_error());
        }
    }

    Ok(())
}
```

## Command Line Syntax

This crate primarily implements POSIX-compatible parsing conventions with a
focus on correctness.  It also explicitly makes certain tasks more challenging
to handle or strongly discourages them.

The following conventions are supported:

* Short options (`-q`)
* Long options (`--verbose`)
* End-of-options markers (`--`) (can be disabled with `Flag::HandleDoubleDash`)
* `=` to separate options from long values (`--option=value`)
* Optionally `=` to separate options from short values (`-o=value`)
  (can be enabled with `Flag::StripShortOptionEqualSign`)
* Spaces to separate options from values (`--option value`, `-o value`)
* Unseparated short options (`-ovalue`)
* Combined short options (`-abc` to mean `-a -b -c`)
* Options with optional arguments (like GNU sed's `-i`, which can be used
  standalone or as `-iSUFFIX`) (`Parser::optional_value()`)
* Options with one or more fixed numbers of arguments
* Options with a variable number of arguments (`Parser::looks_at_value()`)
* Allows disabling of numeric options (`-1` handled as an argument) (can be
  enabled with `Flag::DisableNumericOptions`)

## Why?

Why does this crate exist in the presence of already great crates for argument
parsing?  This comes from two angles.  One is that I'm primarily using
[clap](https://crates.io/crates/clap) but unfortunately clap compiles slow,
has a ton of dependencies, has an enormous amount of ecosystem churn and
ultimately provides not enough control.

On the other hand there is the excellent
[`lexopt`](https://github.com/blyxxyz/lexopt) crate.  This crate's API design
is significantly inspired by it and you will find some of the examples to be
almost verbatim copies from `lexopt`.  It has however some significant
differences:

* This crate operates directly off an iterator piece by piece and does not
  borrow from the parser which results in easier ergnomics when using nested
  parsing.
* It follows stronger getopt/POSIX semantics.  In particular short options
  do not take an equal sign to separate out the value.  (`-o=42` means the
  value is `=42` and not `42`.)
* Positional arguments are handled the same as named options in that they
  do not hold the value.  The value needs to be explicitly parsed from the
  parser.
* It has a different approach to handle multiple arguments to options or
  raw argument parsing.
* The parser has internal flags that can be changed to influence heuristics
  during parsing such as if `--` should be handled, if numeric arguments are
  valid, if options are disabled after the first positional argument and
  if equal sign in short options should be handled.
* This crate does not exist in a vacuum, it's part of `argument` which is
  a (slightly) higher level utility to make working with the lower level
  `argument-parser` crate simpler.

## Unicode and Broken Unicode

This library requires that option names be single characters for short options
or valid Unicode strings for long option names.  However, arguments to options
or command line arguments are always handled internally as `OsString`, allowing
you to create command line interfaces that function well even with broken
Unicode.

Unlike `lexopt`, this library is stricter with Unicode.  In the option name
portion of the command line, valid Unicode input is required, and an error is
raised if it is not.  This intentional design choice makes handling command
lines easier and more portable.  While `lexopt` would handle broken Unicode like
`foo -a�` by producing a parameter named `�`, `argument-parser` will issue a
Unicode error.  However, invalid Unicode is permissible in option arguments.

## Sponsor

If you like the project and find it useful you can [become a
sponsor](https://github.com/sponsors/mitsuhiko).

## License and Links

- License: [Apache-2.0](https://github.com/mitsuhiko/argument/blob/main/LICENSE)
