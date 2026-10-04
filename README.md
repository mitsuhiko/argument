# argument

[![License](https://img.shields.io/github/license/mitsuhiko/argument)](https://github.com/mitsuhiko/argument/blob/main/LICENSE)

Argument is a collection of low-dependency crates to implement command line
argument parsing and handling.  The goal is that these crates achieve
"functional completeness" and turn into low-maintenance solutions to build
small and large command line applications.

## Crates

* [argument](https://github.com/mitsuhiko/argument/tree/main/argument):
  command line interfaces with help pages, usage and errors (the crate you
  depend on)
* [argument-parser](https://github.com/mitsuhiko/argument/tree/main/argument-parser):
  the low-level POSIX command line parser that `argument` is built on
* [argument-completions](https://github.com/mitsuhiko/argument/tree/main/argument-completions):
  shell completion scripts (bash, elvish, fish, nushell, PowerShell, zsh)
* [argument-mangen](https://github.com/mitsuhiko/argument/tree/main/argument-mangen):
  man pages

None of the crates have dependencies outside of this repository.  The
minimum supported Rust version is 1.78.

## Sponsor

If you like the project and find it useful you can [become a
sponsor](https://github.com/sponsors/mitsuhiko).

## License and Links

- [GitHub Repository](https://github.com/mitsuhiko/argument)
- [Issue Tracker](https://github.com/mitsuhiko/argument/issues)
- [Discussions](https://github.com/mitsuhiko/argument/discussions)
- License: [Apache-2.0](https://github.com/mitsuhiko/argument/blob/main/LICENSE)
