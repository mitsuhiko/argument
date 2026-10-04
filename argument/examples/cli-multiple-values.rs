//! This example shows how to accept multiple values for an option.
use argument::{Cli, Error, Opt, Parser, Pos};

#[derive(Copy, Clone)]
enum A {
    Message,
    Verbose,
    Extra,
}

static CLI: Cli<A> = Cli::new("multiple-values")
    .args(&[Pos::new(A::Extra, "EXTRA")
        .optional()
        .multiple()
        .help("Extra arguments")])
    .opts(&[
        Opt::new(A::Message)
            .short('m')
            .long("message")
            .value("MESSAGE...")
            .help("One or more messages"),
        Opt::new(A::Verbose)
            .short('v')
            .help("Increase verbosity (can be repeated)"),
    ]);

#[derive(Debug, Default)]
struct Args {
    messages: Vec<String>,
    extra: Vec<String>,
    verbosity: usize,
}

fn parse(mut p: Parser<A>) -> Result<Args, Error> {
    let mut args = Args::default();

    while let Some(arg) = p.param()? {
        match arg {
            A::Message => {
                while p.looks_at_value() {
                    args.messages.push(p.value()?);
                }
            }
            A::Verbose => args.verbosity += 1,
            A::Extra => args.extra.push(p.value()?),
        }
    }

    Ok(args)
}

fn main() {
    println!("{:#?}", CLI.run(parse));
}
