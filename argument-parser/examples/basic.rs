//! This is a basic example with help page, usage and error printing.
use argument_parser::{Error, Parser};

const USAGE: &str = "error-printing [-n NUMBER]... [--shout] [--help]";
const HELP: &str = "error-printing
A small example of argument-parser

USAGE:
    !!USAGE!!

OPTIONS:
    -n, --number <NUMBER>   adds a number to sum
    --shouts                shouts!
    --help                  prints the help\
";

fn execute() -> Result<(), Error> {
    let mut parser = Parser::from_env();
    let mut numbers = Vec::<i64>::new();
    let mut shout = false;

    while let Some(param) = parser.param()? {
        if param.is_either('n', "number") {
            numbers.push(parser.value()?);
        } else if param.is_long("shout") {
            shout = true;
        } else if param.is_long("help") {
            println!("{}", HELP.replace("!!USAGE!!", USAGE));
            return Ok(());
        } else {
            return Err(parser.unexpected(param));
        }
    }

    if numbers.is_empty() && !shout {
        println!("{}", USAGE)
    } else {
        println!("Numbers: {:?}", &numbers);
        println!("Sum: {}", numbers.into_iter().sum::<i64>());
        if shout {
            println!("I AM SHOUTING!");
        }
    }

    Ok(())
}

fn main() {
    use std::error::Error;
    if let Err(err) = execute() {
        eprintln!("error: {}", err);
        if let Some(source) = err.source() {
            eprintln!("  cause: {}", source);
        }
        if let Some(value) = err.raw_value() {
            eprintln!("  value: {:?}", value.to_string_lossy());
        }
        std::process::exit(1);
    }
}
