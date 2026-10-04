# argument-completions

Shell completion scripts for command line interfaces built with
[argument](https://github.com/mitsuhiko/argument).  Supports bash (3.2 and
later), elvish, fish, nushell, PowerShell and zsh and has no dependencies
besides `argument`.

```rust
use argument::{Cli, Opt, ValueHint};
use argument_completions::Shell;

#[derive(Copy, Clone)]
enum Arg {
    Output,
}

static CLI: Cli<Arg> = Cli::new("convert").opts(&[Opt::new(Arg::Output)
    .short('o')
    .long("output")
    .value("FILE")
    .value_hint(ValueHint::FilePath)
    .help("Where to write the output")]);

// at runtime, for instance from a `--generate-completion <SHELL>` option
// (use `Shell::NAMES` as possible values of that option)
let shell: Shell = "zsh".parse().unwrap();
let script = shell.generate(&CLI, "convert");
assert!(script.starts_with("#compdef convert"));

// or write the scripts of all shells, for instance from a build script
let dir = std::env::temp_dir().join("convert-completions");
std::fs::create_dir_all(&dir).unwrap();
for shell in Shell::ALL {
    shell.generate_to(&CLI, "convert", &dir).unwrap();
}
```

* Options, subcommands (also nested ones) and positional arguments are
  completed.
* Values are completed from the possible values of options and arguments
  (with their descriptions where the shell supports it) or from their value
  hints (files, directories, commands, users, hosts, ...).
