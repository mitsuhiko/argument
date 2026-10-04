# argument

A small library to build command line interfaces on top of
[argument-parser](https://github.com/mitsuhiko/argument/tree/main/argument-parser).
The interface is described with a static spec, parsing stays a plain loop that
`match`es on your own enum, and help pages, usage lines and errors are
generated from the spec.  There are no derives, no macros and no dependencies
besides `argument-parser`.

```rust
use std::path::PathBuf;

use argument::{Cli, Error, Opt, Parser, Pos, ValueHint};

#[derive(Copy, Clone)]
enum Arg {
    Verbose,
    Format,
    Input,
}

static CLI: Cli<Arg> = Cli::new("convert")
    .version("1.0.0")
    .about("Converts files between formats.")
    .opts(&[
        Opt::new(Arg::Verbose)
            .short('v')
            .long("verbose")
            .help("Print more output"),
        Opt::new(Arg::Format)
            .short('f')
            .long("format")
            .value("FORMAT")
            .possible_values(&["json", "yaml"])
            .help("The output format"),
    ])
    .args(&[Pos::new(Arg::Input, "INPUT")
        .optional()
        .multiple()
        .value_hint(ValueHint::FilePath)
        .help("The files to convert")]);

#[derive(Debug, Default)]
struct Args {
    verbose: bool,
    format: Option<String>,
    inputs: Vec<PathBuf>,
}

fn parse(mut p: Parser<Arg>) -> Result<Args, Error> {
    let mut args = Args::default();
    while let Some(arg) = p.param()? {
        match arg {
            Arg::Verbose => args.verbose = true,
            Arg::Format => args.format = Some(p.value()?),
            Arg::Input => args.inputs.push(p.raw_value()?.into()),
        }
    }
    Ok(args)
}

fn main() {
    // prints help, version and errors and exits if needed
    let args = CLI.run(parse);
    println!("{:?}", args);
}
```

```text
$ convert --help
Converts files between formats.

Usage: convert [OPTIONS] [INPUT]...

Arguments:
  [INPUT]...  The files to convert

Options:
  -v, --verbose          Print more output
  -f, --format <FORMAT>  The output format [possible values: json, yaml]
  -h, --help             Print help
  -V, --version          Print version

$ convert --format=jsno
error: invalid value 'jsno' for '--format <FORMAT>'

  tip: a similar value exists: 'json'

Usage: convert [OPTIONS] [INPUT]...

For more information, try 'convert --help'.
```

* **Exhaustive matching:** options, arguments and subcommands are values of
  your enum, so the compiler tells you if one is not handled.
* **Help pages:** short and long help, sections, default and possible
  values.  `--help` and `--version` are built in.
* **Errors:** unknown options, commands and values come with suggestions.
* **Subcommands:** each subcommand has its own spec and enum, the parser is
  handed over with `Parser::subcommand`.
* **Generators:** shell completions with
  [argument-completions](https://github.com/mitsuhiko/argument/tree/main/argument-completions)
  and man pages with
  [argument-mangen](https://github.com/mitsuhiko/argument/tree/main/argument-mangen).
