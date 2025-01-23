//! A very partial unfaithful implementation of cargo's command line.
//!
//! This showcases some hairier patterns, like subcommands and custom value parsing.
//!
//! This example is taken from the MIT licensed cargo example in lexopt
//!
//! Copyright 2022 Jan Verbeek <jan.verbeek@posteo.nl>

use std::path::PathBuf;
use std::str::FromStr;

use argument_parser::{Error, Parser};

const HELP: &str = "cargo [+toolchain] [OPTIONS] [SUBCOMMAND]";

fn main() {
    if let Err(err) = cli() {
        eprintln!("error: {:#}", err);
        std::process::exit(1);
    }
}

fn cli() -> Result<(), Error> {
    let mut settings = GlobalSettings {
        toolchain: "stable".to_owned(),
        color: Color::Auto,
        offline: false,
        quiet: false,
        verbose: false,
    };

    let mut parser = Parser::from_env();
    while let Some(param) = parser.param()? {
        if param.is_long("color") {
            settings.color = parser.value()?;
        } else if param.is_long("offline") {
            settings.offline = true;
        } else if param.is_long("quiet") {
            settings.quiet = true;
            settings.verbose = false;
        } else if param.is_long("verbose") {
            settings.verbose = true;
            settings.quiet = false;
        } else if param.is_either('h', "help") {
            break;
        } else if param.is_arg() {
            let value = parser.string_value()?;
            match value.as_str() {
                value if value.starts_with('+') => {
                    settings.toolchain = value[1..].to_owned();
                }
                "install" => {
                    return install(settings, parser);
                }
                value => {
                    return Err(format!("unknown subcommand '{}'", value).into());
                }
            }
        } else {
            return Err(param.into_unexpected_error());
        }
    }

    println!("{}", HELP);
    Ok(())
}

#[derive(Debug)]
struct GlobalSettings {
    toolchain: String,
    color: Color,
    offline: bool,
    quiet: bool,
    verbose: bool,
}

fn install(settings: GlobalSettings, mut parser: Parser) -> Result<(), Error> {
    let mut package: Option<String> = None;
    let mut root: Option<PathBuf> = None;
    let mut jobs: u16 = get_no_of_cpus();

    while let Some(param) = parser.param()? {
        if param.is_arg() && package.is_none() {
            package = Some(parser.string_value()?);
        } else if param.is_long("root") {
            root = Some(parser.value()?);
        } else if param.is_either('j', "jobs") {
            jobs = parser.value()?;
        } else if param.is_either('h', "help") {
            println!("cargo install [OPTIONS] CRATE");
            std::process::exit(0);
        } else {
            return Err(param.into_unexpected_error());
        }
    }

    println!("Settings: {:#?}", settings);
    println!(
        "Installing {} into {:?} with {} jobs",
        package.ok_or("missing CRATE argument")?,
        root,
        jobs
    );

    Ok(())
}

#[derive(Debug)]
enum Color {
    Auto,
    Always,
    Never,
}

impl FromStr for Color {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "auto" => Ok(Color::Auto),
            "always" => Ok(Color::Always),
            "never" => Ok(Color::Never),
            _ => Err("argument must be auto, always, or never"),
        }
    }
}

fn get_no_of_cpus() -> u16 {
    4
}
