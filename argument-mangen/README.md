# argument-mangen

`argument-mangen` renders man pages (roff) for command line interfaces that
are described with [`argument`](../argument).  It has no dependencies
besides `argument`.

```rust
use argument_mangen::Man;

// render a single page
let page = Man::new(&CLI).render();

// or write the page and the pages of all subcommands (eg: from build.rs)
Man::new(&CLI).generate_to(&out_dir)?;
```

## License and Links

- License: [Apache-2.0](https://github.com/mitsuhiko/argument/blob/main/LICENSE)
