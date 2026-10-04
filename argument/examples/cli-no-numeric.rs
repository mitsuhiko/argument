//! This example demonstrates how to turn off numeric options.
//!
//! This causes an argument like -1 to be handled as an argument
//! rather than an option.
use argument::{Cli, Error, Flag, Opt, Parser, Pos};

#[derive(Copy, Clone)]
enum A {
    Number,
    Arg,
}

static CLI: Cli<A> = Cli::new("no-numeric")
    .args(&[Pos::new(A::Arg, "ARG").optional().multiple()])
    .opts(&[Opt::new(A::Number)
        .short('n')
        .long("number")
        .value("NUMBER")
        .help("A number")]);

fn parse(mut p: Parser<A>) -> Result<(), Error> {
    p.set_flag(Flag::DisableNumericOptions, true);

    while let Some(arg) = p.param()? {
        match arg {
            A::Number => println!("Got number {}", p.value::<i32>()?),
            A::Arg => println!("Got arg {}", p.value::<String>()?),
        }
    }

    Ok(())
}

fn main() {
    CLI.run(parse)
}
