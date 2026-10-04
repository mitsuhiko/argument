//! A minimal command line interface layer on top of [`argument_parser`].
//!
//! This crate keeps the imperative parsing loop of `argument-parser` but adds
//! a static description of the interface (a [`Cli`]).  From that description
//! it generates help pages and usage lines, handles `--help` and `--version`,
//! reports unknown options with suggestions and validates required positional
//! arguments.  There are no derives and no macros.
//!
//! Options, positional arguments and subcommands are identified by values of
//! a user defined enum.  The [`Parser`] hands those back, so the parsing loop
//! is an exhaustive `match`: if an option is declared but not handled, the
//! compiler complains.
//!
//! ```no_run
//! use std::path::PathBuf;
//! use argument::{Cli, Error, Opt, Parser, Pos};
//!
//! #[derive(Copy, Clone)]
//! enum Arg {
//!     Verbose,
//!     Output,
//!     Input,
//! }
//!
//! static CLI: Cli<Arg> = Cli::new("demo")
//!     .version("1.0")
//!     .about("Does demo things.")
//!     .opts(&[
//!         Opt::new(Arg::Verbose).short('v').long("verbose").help("Be verbose"),
//!         Opt::new(Arg::Output).short('o').long("output").value("FILE").help("Output file"),
//!     ])
//!     .args(&[Pos::new(Arg::Input, "INPUT").help("The input file")]);
//!
//! #[derive(Debug, Default)]
//! struct Args {
//!     verbose: bool,
//!     output: Option<PathBuf>,
//!     input: PathBuf,
//! }
//!
//! fn parse(mut p: Parser<Arg>) -> Result<Args, Error> {
//!     let mut args = Args::default();
//!     while let Some(arg) = p.param()? {
//!         match arg {
//!             Arg::Verbose => args.verbose = true,
//!             Arg::Output => args.output = Some(p.raw_value()?.into()),
//!             Arg::Input => args.input = p.raw_value()?.into(),
//!         }
//!     }
//!     Ok(args)
//! }
//!
//! fn main() {
//!     let args = CLI.run(parse);
//!     println!("{:?}", args);
//! }
//! ```
//!
//! # Subcommands
//!
//! Subcommands are declared with [`Cmd`] and point to another [`Cli`] (with
//! its own enum).  When the parser returns the identifier of a command, the
//! parser is handed over with [`Parser::subcommand`]:
//!
//! ```
//! use argument::{Cli, Cmd, Error, Opt, Parser, Pos};
//!
//! #[derive(Copy, Clone)]
//! enum Main { Verbose, Install }
//!
//! #[derive(Copy, Clone)]
//! enum Install { Crate }
//!
//! static MAIN: Cli<Main> = Cli::new("cargo")
//!     .opts(&[Opt::new(Main::Verbose).short('v').help("Be verbose")])
//!     .commands(&[Cmd::new(Main::Install, &INSTALL)]);
//!
//! static INSTALL: Cli<Install> = Cli::new("install")
//!     .about("Installs a crate")
//!     .args(&[Pos::new(Install::Crate, "CRATE")]);
//!
//! fn parse(mut p: Parser<Main>) -> Result<String, Error> {
//!     while let Some(arg) = p.param()? {
//!         match arg {
//!             Main::Verbose => {}
//!             Main::Install => return install(p.subcommand(&INSTALL)),
//!         }
//!     }
//!     Err(p.help())
//! }
//!
//! fn install(mut p: Parser<Install>) -> Result<String, Error> {
//!     let mut name = String::new();
//!     while let Some(arg) = p.param()? {
//!         match arg {
//!             Install::Crate => name = p.value()?,
//!         }
//!     }
//!     Ok(name)
//! }
//!
//! let rv = parse(MAIN.parser_from_args(["-v", "install", "foo"])).unwrap();
//! assert_eq!(rv, "foo");
//! ```
mod error;
mod help;
mod parser;
mod spec;

pub use argument_parser::{Flag, FromString};

pub use crate::error::{Error, ErrorKind};
pub use crate::parser::Parser;
pub use crate::spec::{Cli, Cmd, Opt, Pos};
