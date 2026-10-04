# argument-completions

`argument-completions` generates shell completion scripts for command line
interfaces that are described with [`argument`](../argument).  It has no
dependencies besides `argument`.

Supported shells: bash (3.2 and later), elvish, fish, nushell, PowerShell
and zsh.

```rust
use argument_completions::Shell;

// at runtime, eg: for a `--generate-completion SHELL` option
let shell: Shell = "zsh".parse().unwrap();
print!("{}", shell.generate(&CLI, "my-tool"));

// or from a build script
for shell in Shell::ALL {
    shell.generate_to(&CLI, "my-tool", &out_dir)?;
}
```

Values of options and arguments are completed from their possible values
(`Opt::possible_values`) or their value hints (`Opt::value_hint`).

## License and Links

- License: [Apache-2.0](https://github.com/mitsuhiko/argument/blob/main/LICENSE)
