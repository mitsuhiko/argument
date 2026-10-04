//! A very partial unfaithful implementation of cargo's command line.
//!
//! This showcases subcommands, options shared between commands, raw argument
//! peeking (for `+toolchain`) and custom value parsing.
use std::path::PathBuf;
use std::str::FromStr;

use argument::{Cli, Cmd, Error, Opt, Parser, Pos};

#[derive(Copy, Clone)]
enum Cargo {
    Color,
    Offline,
    Quiet,
    Verbose,
    Install,
    Uninstall,
}

#[derive(Copy, Clone)]
enum Install {
    Crate,
    Root,
    Jobs,
    Quiet,
    Verbose,
}

#[derive(Copy, Clone)]
enum Uninstall {
    Crate,
    Quiet,
    Verbose,
}

// Options shared between commands are just const fns that are generic over
// the identifier enum.
const fn quiet<A>(id: A) -> Opt<A> {
    Opt::new(id)
        .short('q')
        .long("quiet")
        .help("Do not print cargo log messages")
}

const fn verbose<A>(id: A) -> Opt<A> {
    Opt::new(id)
        .short('v')
        .long("verbose")
        .help("Use verbose output")
}

static CARGO: Cli<Cargo> = Cli::new("cargo")
    .version("1.0.0")
    .about("Rust's package manager")
    .usage("[+toolchain] [OPTIONS] [COMMAND]")
    .opts(&[
        Opt::new(Cargo::Color)
            .long("color")
            .value("WHEN")
            .help("Coloring: auto, always, never"),
        Opt::new(Cargo::Offline)
            .long("offline")
            .help("Run without accessing the network"),
        quiet(Cargo::Quiet),
        verbose(Cargo::Verbose),
    ])
    .commands(&[
        Cmd::new(Cargo::Install, &INSTALL),
        Cmd::new(Cargo::Uninstall, &UNINSTALL),
    ]);

static INSTALL: Cli<Install> = Cli::new("install")
    .about("Install a Rust binary")
    .args(&[Pos::new(Install::Crate, "CRATE").help("The crate to install")])
    .opts(&[
        Opt::new(Install::Root)
            .long("root")
            .value("DIR")
            .help("Directory to install packages into"),
        Opt::new(Install::Jobs)
            .short('j')
            .long("jobs")
            .value("N")
            .help("Number of parallel jobs, defaults to # of CPUs"),
        quiet(Install::Quiet),
        verbose(Install::Verbose),
    ]);

static UNINSTALL: Cli<Uninstall> = Cli::new("uninstall")
    .about("Remove a Rust binary")
    .args(&[Pos::new(Uninstall::Crate, "CRATE")
        .multiple()
        .help("The crates to uninstall")])
    .opts(&[quiet(Uninstall::Quiet), verbose(Uninstall::Verbose)]);

#[derive(Debug)]
struct GlobalSettings {
    toolchain: String,
    color: Color,
    offline: bool,
    quiet: bool,
    verbose: bool,
}

impl GlobalSettings {
    fn set_quiet(&mut self) {
        self.quiet = true;
        self.verbose = false;
    }

    fn set_verbose(&mut self) {
        self.verbose = true;
        self.quiet = false;
    }
}

#[allow(dead_code)]
#[derive(Debug)]
enum Command {
    Install {
        package: String,
        root: Option<PathBuf>,
        jobs: u16,
    },
    Uninstall {
        packages: Vec<String>,
    },
}

fn cli(mut p: Parser<Cargo>) -> Result<(GlobalSettings, Command), Error> {
    let mut settings = GlobalSettings {
        toolchain: "stable".to_owned(),
        color: Color::Auto,
        offline: false,
        quiet: false,
        verbose: false,
    };

    // `+toolchain` is only valid as first argument and does not follow any
    // normal conventions, so we peek at the raw argument.
    if let Some(toolchain) = p
        .peek_raw_arg()
        .and_then(|x| x.to_str())
        .and_then(|x| x.strip_prefix('+'))
    {
        settings.toolchain = toolchain.to_owned();
        p.raw_arg();
    }

    while let Some(arg) = p.param()? {
        match arg {
            Cargo::Color => settings.color = p.value()?,
            Cargo::Offline => settings.offline = true,
            Cargo::Quiet => settings.set_quiet(),
            Cargo::Verbose => settings.set_verbose(),
            Cargo::Install => return install(settings, p.subcommand(&INSTALL)),
            Cargo::Uninstall => return uninstall(settings, p.subcommand(&UNINSTALL)),
        }
    }

    Err(p.help())
}

fn install(
    mut settings: GlobalSettings,
    mut p: Parser<Install>,
) -> Result<(GlobalSettings, Command), Error> {
    let mut package = String::new();
    let mut root = None;
    let mut jobs = get_no_of_cpus();

    while let Some(arg) = p.param()? {
        match arg {
            Install::Crate => package = p.value()?,
            Install::Root => root = Some(p.raw_value()?.into()),
            Install::Jobs => jobs = p.value()?,
            Install::Quiet => settings.set_quiet(),
            Install::Verbose => settings.set_verbose(),
        }
    }

    Ok((
        settings,
        Command::Install {
            package,
            root,
            jobs,
        },
    ))
}

fn uninstall(
    mut settings: GlobalSettings,
    mut p: Parser<Uninstall>,
) -> Result<(GlobalSettings, Command), Error> {
    let mut packages = Vec::new();

    while let Some(arg) = p.param()? {
        match arg {
            Uninstall::Crate => packages.push(p.value()?),
            Uninstall::Quiet => settings.set_quiet(),
            Uninstall::Verbose => settings.set_verbose(),
        }
    }

    Ok((settings, Command::Uninstall { packages }))
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

fn main() {
    let (settings, command) = CARGO.run(cli);
    println!("{:#?}", settings);
    println!("{:#?}", command);
}
