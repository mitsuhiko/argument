//! This example shows how to accept multiple values for an option.
use argument_parser::{Error, Parser};

fn main() -> Result<(), Error> {
    let mut parser = Parser::from_env();
    let mut messages = Vec::new();
    let mut extra = Vec::new();
    let mut verbosity = 0;

    while let Some(param) = parser.param()? {
        if param.is_short('m') || param.is_long("message") {
            while parser.looks_at_value() {
                messages.push(parser.string_value()?);
            }
        } else if param.is_short('h') || param.is_long("help") {
            println!("usage: multiple-values [-v] [-h | --help] [-m | --message] <MESSAGE>...");
            return Ok(());
        } else if param.is_short('v') {
            verbosity += 1;
        } else if param.is_pos() {
            extra.push(parser.string_value()?);
        } else {
            return Err(param.into_unexpected_error());
        }
    }

    println!("messages: {:?}", messages);
    println!("extra: {:?}", extra);
    println!("verbosity: {}", verbosity);

    Ok(())
}
