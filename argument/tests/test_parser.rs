use argument::{Cli, Cmd, Error, ErrorKind, Opt, Parser, Pos};

#[derive(Copy, Clone, Debug, PartialEq)]
enum A {
    Verbose,
    Output,
    Level,
    Input,
    Rest,
    Run,
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum R {
    Fast,
    Target,
}

static RUN: Cli<R> = Cli::new("run")
    .about("Runs things")
    .args(&[Pos::new(R::Target, "TARGET")])
    .opts(&[Opt::new(R::Fast).long("fast")]);

static CLI: Cli<A> = Cli::new("demo")
    .version("1.0")
    .args(&[
        Pos::new(A::Input, "INPUT").optional(),
        Pos::new(A::Rest, "REST").optional().multiple(),
    ])
    .opts(&[
        Opt::new(A::Verbose).short('v').long("verbose"),
        Opt::new(A::Output).short('o').long("output").value("FILE"),
        Opt::new(A::Level)
            .short('l')
            .long("level")
            .optional_value("N"),
    ])
    .commands(&[Cmd::new(A::Run, &RUN)]);

#[derive(Debug, Default, PartialEq)]
struct Out {
    verbose: usize,
    output: Option<String>,
    level: Option<Option<u32>>,
    input: Option<String>,
    rest: Vec<String>,
    run: Option<(bool, String)>,
}

fn parse(mut p: Parser<A>) -> Result<Out, Error> {
    let mut out = Out::default();
    while let Some(arg) = p.param()? {
        match arg {
            A::Verbose => out.verbose += 1,
            A::Output => out.output = Some(p.value()?),
            A::Level => out.level = Some(p.optional_value()?),
            A::Input => out.input = Some(p.value()?),
            A::Rest => out.rest.push(p.value()?),
            A::Run => {
                let mut p = p.subcommand(&RUN);
                let mut fast = false;
                let mut target = String::new();
                while let Some(arg) = p.param()? {
                    match arg {
                        R::Fast => fast = true,
                        R::Target => target = p.value()?,
                    }
                }
                out.run = Some((fast, target));
                break;
            }
        }
    }
    Ok(out)
}

fn run(args: &[&str]) -> Result<Out, Error> {
    parse(CLI.parser_from_args(args.iter().copied()))
}

#[test]
fn test_basic() {
    let out = run(&["-vv", "--output=x", "in", "a", "-v", "b"]).unwrap();
    assert_eq!(
        out,
        Out {
            verbose: 3,
            output: Some("x".into()),
            input: Some("in".into()),
            rest: vec!["a".into(), "b".into()],
            ..Default::default()
        }
    );
}

#[test]
fn test_optional_value() {
    assert_eq!(run(&["-l"]).unwrap().level, Some(None));
    assert_eq!(run(&["-l3"]).unwrap().level, Some(Some(3)));
    assert_eq!(run(&["--level=4"]).unwrap().level, Some(Some(4)));
    // optional values are never taken from the next argument
    let out = run(&["--level", "5"]).unwrap();
    assert_eq!(out.level, Some(None));
    assert_eq!(out.input.as_deref(), Some("5"));
}

#[test]
fn test_subcommand() {
    let out = run(&["-v", "run", "--fast", "x"]).unwrap();
    assert_eq!(out.run, Some((true, "x".into())));
    assert_eq!(out.verbose, 1);

    let err = run(&["run"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::MissingArgument);
    assert_eq!(err.message(), "the argument '<TARGET>' is required");
    assert!(err.render().contains("Usage: demo run [OPTIONS] <TARGET>"));
    assert!(err
        .render()
        .contains("For more information, try 'demo run --help'."));

    let err = run(&["run", "--help"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::Help);
    assert_eq!(err.exit_code(), 0);
    assert!(err.message().starts_with("Runs things\n\nUsage: demo run"));
}

#[test]
fn test_help_and_version() {
    let err = run(&["--help"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::Help);
    assert!(err.message().contains("  run  Runs things"));

    let err = run(&["-V"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::Version);
    assert_eq!(err.message(), "demo 1.0");
}

#[test]
fn test_unknown_option() {
    let err = run(&["--verbos"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::UnknownOption);
    assert_eq!(err.message(), "unexpected argument '--verbos' found");
    assert_eq!(err.tip(), Some("a similar option exists: '--verbose'"));
    assert_eq!(err.exit_code(), 2);

    let err = run(&["-x"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::UnknownOption);
    assert_eq!(err.message(), "unexpected argument '-x' found");
}

#[test]
fn test_flag_with_value() {
    let err = run(&["--verbose=1"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::UnexpectedValue);
    assert_eq!(err.message(), "unexpected value '1' for '--verbose'");
}

#[test]
fn test_value_errors() {
    let err = run(&["-o"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::MissingValue);
    assert_eq!(
        err.message(),
        "a value is required for '-o <FILE>' but none was supplied"
    );

    let err = run(&["--level=x"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::InvalidValue);
    assert_eq!(
        err.message(),
        "invalid value 'x' for '--level[N]': invalid digit found in string"
    );
}

#[derive(Copy, Clone, Debug)]
enum S {
    Value,
}

static SIMPLE: Cli<S> = Cli::new("simple").opts(&[Opt::new(S::Value).short('x').value("V")]);

#[test]
fn test_unexpected_argument() {
    let err = SIMPLE.parser_from_args(["foo"]).param().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::UnexpectedArgument);
    assert_eq!(err.message(), "unexpected argument 'foo' found");
}

#[test]
fn test_unknown_command() {
    #[derive(Copy, Clone, Debug)]
    enum C {
        Run,
    }
    static C_CLI: Cli<C> = Cli::new("c").commands(&[Cmd::new(C::Run, &RUN)]);
    let err = C_CLI.parser_from_args(["rnu"]).param().unwrap_err();
    assert_eq!(err.kind(), ErrorKind::UnknownCommand);
    assert_eq!(err.tip(), Some("a similar command exists: 'run'"));
}

#[test]
fn test_allow_hyphen() {
    #[derive(Copy, Clone, Debug, PartialEq)]
    enum N {
        Verbose,
        Number,
    }
    static N_CLI: Cli<N> = Cli::new("n")
        .args(&[Pos::new(N::Number, "N").allow_hyphen()])
        .opts(&[Opt::new(N::Verbose).short('v')]);

    let mut p = N_CLI.parser_from_args(["-v", "-42"]);
    assert_eq!(p.param().unwrap(), Some(N::Verbose));
    assert_eq!(p.param().unwrap(), Some(N::Number));
    assert_eq!(p.value::<i32>().unwrap(), -42);
    assert_eq!(p.param().unwrap(), None);

    // known options are still options
    let mut p = N_CLI.parser_from_args(["-v"]);
    assert_eq!(p.param().unwrap(), Some(N::Verbose));
    assert_eq!(p.param().unwrap_err().kind(), ErrorKind::MissingArgument);
}

#[test]
fn test_missing() {
    let p = CLI.parser_from_args(Vec::<String>::new());
    assert_eq!(
        p.missing(A::Output).message(),
        "the option '--output <FILE>' is required"
    );
    assert_eq!(
        p.missing(A::Input).message(),
        "the argument '[INPUT]' is required"
    );
}

#[test]
fn test_custom_error_has_context() {
    let p = CLI.parser_from_args(Vec::<String>::new());
    let err = p.error("something is off");
    assert_eq!(
        err.render(),
        "error: something is off\n\nUsage: demo [OPTIONS] [INPUT] [REST]... [COMMAND]\n\nFor more information, try 'demo --help'."
    );
}

#[test]
#[cfg(debug_assertions)]
#[should_panic(expected = "the value for '-x <V>' was never consumed")]
fn test_unconsumed_value_panics() {
    let mut p = SIMPLE.parser_from_args(["-x", "1"]);
    p.param().unwrap();
    p.param().unwrap();
}

#[test]
#[cfg(debug_assertions)]
#[should_panic(expected = "a value was requested for '-v' which is declared as a flag")]
fn test_value_for_flag_panics() {
    let mut p = CLI.parser_from_args(["-v", "1"]);
    p.param().unwrap();
    let _ = p.value::<String>();
}

#[derive(Copy, Clone, Debug, PartialEq)]
enum V {
    Format,
    Mode,
    LongHelp,
}

static VALUES: Cli<V> = Cli::new("values")
    .about("Short about")
    .long_about("Long about")
    .args(&[Pos::new(V::Mode, "MODE")
        .optional()
        .possible_values(&["fast", "slow"])])
    .opts(&[
        Opt::new(V::Format)
            .short('f')
            .long("format")
            .value("FORMAT")
            .possible_values_with_help(&[("json", "JSON"), ("yaml", "YAML")]),
        Opt::new(V::LongHelp).long("long-help"),
    ]);

fn parse_values(args: &[&str]) -> Result<Vec<String>, Error> {
    let mut p = VALUES.parser_from_args(args.iter().copied());
    let mut rv = Vec::new();
    while let Some(arg) = p.param()? {
        match arg {
            V::Format => rv.push(p.value()?),
            V::Mode => rv.push(p.raw_value()?.to_string_lossy().into_owned()),
            V::LongHelp => return Err(p.long_help()),
        }
    }
    Ok(rv)
}

#[test]
fn test_possible_values() {
    assert_eq!(
        parse_values(&["-fjson", "slow"]).unwrap(),
        vec!["json", "slow"]
    );

    let err = parse_values(&["--format=jsno"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::InvalidValue);
    assert_eq!(
        err.message(),
        "invalid value 'jsno' for '--format <FORMAT>'"
    );
    assert_eq!(err.tip(), Some("a similar value exists: 'json'"));

    let err = parse_values(&["medium"]).unwrap_err();
    assert_eq!(err.message(), "invalid value 'medium' for '[MODE]'");
    assert_eq!(err.tip(), Some("possible values: fast, slow"));
}

#[test]
fn test_long_help() {
    let err = parse_values(&["--long-help"]).unwrap_err();
    assert_eq!(err.kind(), ErrorKind::Help);
    assert!(err.message().starts_with("Long about\n"));
    assert!(err.message().contains("- json: JSON"));

    let err = parse_values(&["--help"]).unwrap_err();
    assert!(err.message().starts_with("Short about\n"));
    assert!(err.message().contains("[possible values: json, yaml]"));
}
