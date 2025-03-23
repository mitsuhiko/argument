//! This example demonstrates how to turn off numeric options.
//!
//! This causes an argument like -1 to be handled as an argument
//! rather than an option.
use argument_parser::{Error, Flag, Parser};

fn main() -> Result<(), Error> {
    let mut parser = Parser::from_env();
    parser.set_flag(Flag::DisableNumericOptions, true);

    while let Some(param) = parser.param()? {
        if param.is_short('n') || param.is_long("number") {
            println!("Got number {}", parser.value::<i32>()?);
        } else if param.is_pos() {
            println!("Got arg {}", parser.value::<String>()?);
        } else {
            return Err(param.into_unexpected_error());
        }
    }

    Ok(())
}
