# argument-mangen

Man pages for command line interfaces built with
[argument](https://github.com/mitsuhiko/argument).  Renders roff from the
spec of the interface and has no dependencies besides `argument`.

```rust
use argument::{Cli, Opt};
use argument_mangen::Man;

#[derive(Copy, Clone)]
enum Arg {
    Verbose,
}

static CLI: Cli<Arg> = Cli::new("convert")
    .version("1.0.0")
    .about("Converts files between formats")
    .opts(&[Opt::new(Arg::Verbose)
        .short('v')
        .long("verbose")
        .help("Print more output")]);

// render a single page
let page = Man::new(&CLI).render();
assert!(page.contains(".SH NAME\nconvert \\- Converts files between formats"));

// or write the page and the pages of all subcommands (`convert-<name>.1`),
// for instance from a build script
let dir = std::env::temp_dir().join("convert-man");
std::fs::create_dir_all(&dir).unwrap();
Man::new(&CLI).date("2025-01-01").generate_to(&dir).unwrap();
```

* Sections of options become sections of the page.
* The long help texts, default values and possible values are included.
