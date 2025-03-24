//! Shows raw argument parsing to implement chmod.
use std::ffi::OsStr;
use std::path::PathBuf;

use argument_parser::{Error, Parser};

const USAGE: &str = "chmod [-fLRhv] [--help] mode file ...";
const HELP: &str = "chmod: change file modes

USAGE:
    !!USAGE!!

OPTIONS:
    -f          Do not display a diagnostic message if chmod could not modify the mode for file
    -L          If the -R option is specified, all symbolic links are followed.
    -h          If the file is a symbolic link, change the mode of the link itself rather than
                the file that the link points to.
    -R          Change the modes of the file hierarchies rooted in the files, instead of just
                the files themselves.
    -v          Cause chmod to be verbose, showing filenames as the mode is modified.
    --help      Prints the help\
";

fn execute() -> Result<(), Error> {
    let mut parser = Parser::from_env();
    let mut new_mode = None;
    let mut diagnostic = true;
    let mut follow_symbolic = false;
    let mut recursive = false;
    let mut verbose = false;
    let mut change_symbolic = false;
    let mut files = Vec::new();

    loop {
        // if we don't have a mode yet, try to parse the mode first.  This requires
        // raw parsing since this does not follow any normal posix semantics
        if new_mode.is_none() {
            if let Some(m) = parser.peek_raw_arg().and_then(parse_mode) {
                new_mode = Some(m);
                let _ = parser.raw_arg();
                continue;
            }
        }

        // get a param or break from the loop
        let Some(param) = parser.param()? else { break };

        // regular parsing continues here
        if param.is_short('f') {
            diagnostic = false;
        } else if param.is_short('L') {
            follow_symbolic = true;
        } else if param.is_short('h') {
            change_symbolic = true;
        } else if param.is_short('R') {
            recursive = true;
        } else if param.is_short('v') {
            verbose = true;
        } else if param.is_long("help") {
            print_help();
            return Ok(());
        } else if param.is_pos() {
            files.push(PathBuf::from(parser.raw_value()?));
        } else {
            return Err(parser.unexpected());
        }
    }

    if files.is_empty() || new_mode.is_none() {
        print_help();
        return Ok(());
    }

    let mode = new_mode.unwrap();

    if verbose {
        println!("verbose: {:?}", verbose);
        println!("diagnostic: {:?}", diagnostic);
        println!("follow_symbolic: {:?}", follow_symbolic);
        println!("recursive: {:?}", recursive);
        println!("change_symbolic: {:?}", change_symbolic);
        match mode {
            Mode::Abs(abs) => println!("new absolute mode: {:o}", abs),
            Mode::Mask(mask) => println!("new mode mask: {:o}", mask),
        }
    }

    if verbose {
        println!();
        for file in files {
            println!("Update {}", file.display());
        }
    }

    Ok(())
}

fn print_help() {
    println!("{}", HELP.replace("!!USAGE!!", USAGE));
}

#[derive(Debug)]
enum Mode {
    Abs(u32),
    Mask(u32),
}

fn parse_mode(arg: &OsStr) -> Option<Mode> {
    let s = arg.to_str()?;
    if let Ok(abs) = u32::from_str_radix(s, 8) {
        return Some(Mode::Abs(abs));
    }

    if !(s.contains('+') || s.contains('-') || s.contains('=')) {
        return None;
    }

    let mut mask: u32 = 0o7777;
    for clause in s.split(',') {
        let mut who = 0;
        let mut op = '?';
        let mut perm = 0;

        let mut parsing_who = true;
        let mut parsing_perm = false;

        for c in clause.chars() {
            if parsing_who {
                match c {
                    'u' => who |= 0o700,
                    'g' => who |= 0o070,
                    'o' => who |= 0o007,
                    'a' => who |= 0o777,
                    '+' | '-' | '=' => {
                        if who == 0 {
                            who = 0o777;
                        }
                        op = c;
                        parsing_who = false;
                        parsing_perm = true;
                    }
                    _ => return None,
                }
            } else if parsing_perm {
                match c {
                    'r' => perm |= 0o444,
                    'w' => perm |= 0o222,
                    'x' => perm |= 0o111,
                    's' => perm |= 0o6000,
                    't' => perm |= 0o1000,
                    _ => return None,
                }
            }
        }

        match op {
            '+' => mask &= !(perm & who),
            '-' => mask |= perm & who,
            '=' => {
                mask &= !who;
                mask |= !(perm & who) & who;
            }
            _ => return None,
        }
    }

    Some(Mode::Mask(mask))
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
