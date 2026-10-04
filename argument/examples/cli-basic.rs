//! This is a basic example with help page, usage and error printing.
use argument::{Cli, Error, Opt, Parser};

#[derive(Copy, Clone)]
enum A {
    Number,
    Shout,
}

static CLI: Cli<A> = Cli::new("basic")
    .about("A small example of argument")
    .opts(&[
        Opt::new(A::Number)
            .short('n')
            .long("number")
            .value("NUMBER")
            .help("Adds a number to sum"),
        Opt::new(A::Shout).long("shout").help("Shouts!"),
    ]);

fn parse(mut p: Parser<A>) -> Result<(Vec<i64>, bool), Error> {
    let mut numbers = Vec::new();
    let mut shout = false;

    while let Some(arg) = p.param()? {
        match arg {
            A::Number => numbers.push(p.value()?),
            A::Shout => shout = true,
        }
    }

    if numbers.is_empty() && !shout {
        return Err(p.help());
    }

    Ok((numbers, shout))
}

fn main() {
    let (numbers, shout) = CLI.run(parse);
    println!("Numbers: {:?}", numbers);
    println!("Sum: {}", numbers.into_iter().sum::<i64>());
    if shout {
        println!("I AM SHOUTING!");
    }
}
