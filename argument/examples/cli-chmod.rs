//! Implements chmod.
//!
//! The mode argument can look like an option (`chmod -w file`) so it is
//! declared with `allow_hyphen`: values starting with `-` that are not a known
//! option are handed to it.
use std::ffi::OsStr;
use std::path::PathBuf;

use argument::{Cli, Error, Opt, Parser, Pos};

#[derive(Copy, Clone)]
enum A {
    NoDiagnostic,
    FollowSymlinks,
    ChangeSymlink,
    Recursive,
    Verbose,
    Mode,
    File,
}

static CLI: Cli<A> = Cli::new("chmod")
    .about("chmod: change file modes")
    .usage("[-fLRhv] MODE FILE...")
    .args(&[
        Pos::new(A::Mode, "MODE")
            .allow_hyphen()
            .help("An octal mode or a symbolic mode (eg: go-w)"),
        Pos::new(A::File, "FILE")
            .multiple()
            .help("The files to change"),
    ])
    .opts(&[
        Opt::new(A::NoDiagnostic)
            .short('f')
            .help("Do not display a diagnostic message if chmod could not modify the mode for file"),
        Opt::new(A::FollowSymlinks)
            .short('L')
            .help("If the -R option is specified, all symbolic links are followed."),
        Opt::new(A::ChangeSymlink)
            .short('h')
            .help("If the file is a symbolic link, change the mode of the link itself rather than the file that the link points to."),
        Opt::new(A::Recursive)
            .short('R')
            .help("Change the modes of the file hierarchies rooted in the files, instead of just the files themselves."),
        Opt::new(A::Verbose)
            .short('v')
            .help("Cause chmod to be verbose, showing filenames as the mode is modified."),
    ]);

#[allow(dead_code)]
#[derive(Debug)]
struct Args {
    mode: Mode,
    diagnostic: bool,
    follow_symbolic: bool,
    change_symbolic: bool,
    recursive: bool,
    verbose: bool,
    files: Vec<PathBuf>,
}

fn parse(mut p: Parser<A>) -> Result<Args, Error> {
    let mut mode = None;
    let mut diagnostic = true;
    let mut follow_symbolic = false;
    let mut change_symbolic = false;
    let mut recursive = false;
    let mut verbose = false;
    let mut files = Vec::new();

    while let Some(arg) = p.param()? {
        match arg {
            A::NoDiagnostic => diagnostic = false,
            A::FollowSymlinks => follow_symbolic = true,
            A::ChangeSymlink => change_symbolic = true,
            A::Recursive => recursive = true,
            A::Verbose => verbose = true,
            A::Mode => {
                let raw = p.raw_value()?;
                mode = Some(parse_mode(&raw).ok_or_else(|| {
                    p.error(format!("invalid file mode: {}", raw.to_string_lossy()))
                })?);
            }
            A::File => files.push(p.raw_value()?.into()),
        }
    }

    Ok(Args {
        // required positional arguments are validated by the parser
        mode: mode.unwrap(),
        diagnostic,
        follow_symbolic,
        change_symbolic,
        recursive,
        verbose,
        files,
    })
}

#[allow(dead_code)]
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
    let args = CLI.run(parse);
    println!("{:#?}", args);
}
