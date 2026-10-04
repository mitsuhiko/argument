//! This example demonstrates how to disable options after arguments.
use argument::{Cli, Error, Flag, Opt, Parser, Pos};

#[derive(Copy, Clone)]
enum A {
    Number,
    Shout,
    Arg,
}

static CLI: Cli<A> = Cli::new("posix-args")
    .args(&[Pos::new(A::Arg, "ARG").optional().multiple()])
    .opts(&[
        Opt::new(A::Number)
            .short('n')
            .long("number")
            .value("NUMBER")
            .help("A number"),
        Opt::new(A::Shout).long("shout").help("Shout"),
    ]);

fn parse(mut p: Parser<A>) -> Result<Vec<String>, Error> {
    p.set_flag(Flag::DisableOptionsAfterArgs, true);
    let mut args = Vec::new();

    while let Some(arg) = p.param()? {
        match arg {
            A::Number => println!("Got number {}", p.value::<i32>()?),
            A::Shout => println!("Got --shout"),
            A::Arg => args.push(p.value()?),
        }
    }

    Ok(args)
}

fn main() {
    println!("args: {:?}", CLI.run(parse));
}
