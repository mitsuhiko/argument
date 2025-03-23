//! This example demonstrates how to disable options after arguments.
use argument_parser::{Error, Flag, Parser};

fn main() -> Result<(), Error> {
    let mut parser = Parser::from_env();
    parser.set_flag(Flag::DisableOptionsAfterArgs, true);
    let mut args = Vec::<String>::new();

    while let Some(param) = parser.param()? {
        if param.is_short('n') || param.is_long("number") {
            println!("Got number {}", parser.value::<i32>()?);
        } else if param.is_long("shout") {
            println!("Got --shout");
        } else if param.is_pos() {
            args.push(parser.value()?);
        } else {
            return Err(param.into_unexpected_error());
        }
    }

    println!("args: {:?}", args);

    Ok(())
}
