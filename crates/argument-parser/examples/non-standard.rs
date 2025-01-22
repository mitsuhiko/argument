//! This example demonstrates weird stuff you can do
//! if you bypass the regular argument parsing.
use argument_parser::{Error, Parser};

fn parse_dashnum(parser: &mut Parser) -> Option<u64> {
    let arg = parser.peek_raw_arg()?.to_str()?;
    let num = arg.strip_prefix('-')?.parse().ok()?;
    parser.raw_arg(); // Consume the argument we just parsed
    Some(num)
}

fn main() -> Result<(), Error> {
    let mut parser = Parser::from_env();

    loop {
        // try custom parsing first
        if let Some(num) = parse_dashnum(&mut parser) {
            println!("Got number {}", num);

        // otherwise use parameter based parsing
        } else if let Some(param) = parser.param()? {
            if param.is_short('n') || param.is_long("number") {
                println!("Got number {}", parser.value::<i32>()?);
            } else if param.is_long("shout") {
                println!("Got --shout");
            } else {
                return Err(param.into_unexpected_error());
            }
        } else {
            break;
        }
    }

    Ok(())
}
