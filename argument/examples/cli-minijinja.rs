//! Port of the minijinja-cli example from `argument-parser`.
use std::path::PathBuf;

use argument::{Cli, Error, Opt, Parser, Pos};

#[derive(Copy, Clone, PartialEq)]
enum A {
    TemplateFile,
    DataFile,
    ConfigFile,
    Format,
    Define,
    Template,
    Output,
    Select,
    PrintConfig,
    Autoescape,
    Strict,
    NoNewline,
    TrimBlocks,
    LstripBlocks,
    PyCompat,
    Syntax,
    Env,
    NoInclude,
    SafePath,
    Fuel,
    Expr,
    ExprOut,
    Dump,
    Repl,
}

static CLI: Cli<A> = Cli::new("minijinja-cli")
    .version("1.0.0")
    .about(
        "minijinja-cli is a command line tool to render or evaluate jinja2 templates.\n\
         \n\
         Pass a template and optionally a file with template variables to render it to stdout.",
    )
    .args(&[
        Pos::new(A::TemplateFile, "TEMPLATE_FILE")
            .optional()
            .help("Path to the input template [default: -]"),
        Pos::new(A::DataFile, "DATA_FILE")
            .optional()
            .help("Path to the data file"),
    ])
    .opts(&[
        Opt::new(A::ConfigFile)
            .long("config-file")
            .value("PATH")
            .help("Alternative path to the config file."),
        Opt::new(A::Format)
            .short('f')
            .long("format")
            .value("FORMAT")
            .help("The format of the input data [possible values: auto, cbor, ini, json, querystring, toml, yaml]"),
        Opt::new(A::Define)
            .short('D')
            .long("define")
            .value("EXPR")
            .help("Defines an input variable (key=value / key:=json_value)"),
        Opt::new(A::Template)
            .short('t')
            .long("template")
            .value("TEMPLATE_STRING")
            .help("Render a string template"),
        Opt::new(A::Output)
            .short('o')
            .long("output")
            .value("FILENAME")
            .help("Path to the output file [default: -]"),
        Opt::new(A::Select)
            .long("select")
            .value("SELECTOR")
            .help("Select a subset of the input data"),
        Opt::new(A::PrintConfig)
            .long("print-config")
            .help("Print out the loaded config"),
        Opt::section("Template Behavior"),
        Opt::new(A::Autoescape)
            .short('a')
            .long("autoescape")
            .value("MODE")
            .help("Reconfigures autoescape behavior [possible values: auto, html, json, none]"),
        Opt::new(A::Strict)
            .long("strict")
            .help("Disallow undefined variables in templates"),
        Opt::new(A::NoNewline)
            .short('n')
            .long("no-newline")
            .help("Do not output a trailing newline"),
        Opt::new(A::TrimBlocks)
            .long("trim-blocks")
            .help("Enable the trim-blocks flag"),
        Opt::new(A::LstripBlocks)
            .long("lstrip-blocks")
            .help("Enable the lstrip-blocks flag"),
        Opt::new(A::PyCompat)
            .long("py-compat")
            .help("Enables improved Python compatibility"),
        Opt::new(A::Syntax)
            .short('s')
            .long("syntax")
            .value("PAIR")
            .help("Changes a syntax feature (feature=value) [possible features: block-start, block-end, variable-start, variable-end, comment-start, comment-end, line-statement-prefix, line-statement-comment]"),
        Opt::new(A::Env)
            .long("env")
            .help("Pass environment variables as ENV to the template"),
        Opt::section("Security"),
        Opt::new(A::NoInclude)
            .long("no-include")
            .help("Disallow includes and extending"),
        Opt::new(A::SafePath)
            .long("safe-path")
            .value("PATH")
            .help("Only allow includes from this path"),
        Opt::new(A::Fuel)
            .long("fuel")
            .value("AMOUNT")
            .help("Configures the maximum fuel"),
        Opt::section("Advanced"),
        Opt::new(A::Expr)
            .short('E')
            .long("expr")
            .value("EXPR")
            .help("Evaluates an template expression"),
        Opt::new(A::ExprOut)
            .long("expr-out")
            .value("MODE")
            .help("The expression output mode [possible values: print, json, json-pretty, status]"),
        Opt::new(A::Dump)
            .long("dump")
            .value("KIND")
            .help("Dump internals of a template [possible values: instructions, ast, tokens]"),
        Opt::new(A::Repl)
            .long("repl")
            .help("Starts the repl with the given data"),
    ]);

#[derive(Debug)]
struct Args {
    template_file: PathBuf,
    data_file: Option<PathBuf>,
    config_file: Option<PathBuf>,
    format: String,
    defines: Vec<String>,
    template_str: Option<String>,
    output: PathBuf,
    select: Option<String>,
    print_config: bool,
    autoescape: String,
    strict: bool,
    no_newline: bool,
    trim_blocks: bool,
    lstrip_blocks: bool,
    py_compat: bool,
    syntax: Vec<String>,
    env: bool,
    no_include: bool,
    safe_path: Option<PathBuf>,
    fuel: Option<u64>,
    expr: Option<String>,
    expr_out: Option<String>,
    dump: Option<String>,
    repl: bool,
}

fn parse(mut p: Parser<A>) -> Result<Args, Error> {
    let mut args = Args {
        template_file: PathBuf::from("-"),
        data_file: None,
        config_file: None,
        format: "auto".into(),
        defines: Vec::new(),
        template_str: None,
        output: PathBuf::from("-"),
        select: None,
        print_config: false,
        autoescape: "auto".into(),
        strict: false,
        no_newline: false,
        trim_blocks: false,
        lstrip_blocks: false,
        py_compat: false,
        syntax: Vec::new(),
        env: false,
        no_include: false,
        safe_path: None,
        fuel: None,
        expr: None,
        expr_out: None,
        dump: None,
        repl: false,
    };

    while let Some(arg) = p.param()? {
        match arg {
            A::TemplateFile => args.template_file = p.raw_value()?.into(),
            A::DataFile => args.data_file = Some(p.raw_value()?.into()),
            A::ConfigFile => args.config_file = Some(p.raw_value()?.into()),
            A::Format => args.format = p.value()?,
            A::Define => args.defines.push(p.value()?),
            A::Template => args.template_str = Some(p.value()?),
            A::Output => args.output = p.raw_value()?.into(),
            A::Select => args.select = Some(p.value()?),
            A::PrintConfig => args.print_config = true,
            A::Autoescape => args.autoescape = p.value()?,
            A::Strict => args.strict = true,
            A::NoNewline => args.no_newline = true,
            A::TrimBlocks => args.trim_blocks = true,
            A::LstripBlocks => args.lstrip_blocks = true,
            A::PyCompat => args.py_compat = true,
            A::Syntax => args.syntax.push(p.value()?),
            A::Env => args.env = true,
            A::NoInclude => args.no_include = true,
            A::SafePath => args.safe_path = Some(p.raw_value()?.into()),
            A::Fuel => args.fuel = Some(p.value()?),
            A::Expr => args.expr = Some(p.value()?),
            A::ExprOut => args.expr_out = Some(p.value()?),
            A::Dump => args.dump = Some(p.value()?),
            A::Repl => args.repl = true,
        }
    }

    // validation happens after parsing in regular code
    if args.no_include && args.safe_path.is_some() {
        return Err(p.error("--no-include and --safe-path are mutually exclusive"));
    }
    let modes = [args.expr.is_some(), args.template_str.is_some(), args.repl];
    if modes.iter().filter(|x| **x).count() > 1 {
        return Err(p.error("--expr, --template and --repl are mutually exclusive"));
    }
    if args.expr_out.is_some() && args.expr.is_none() {
        return Err(p.error("--expr-out requires --expr"));
    }

    Ok(args)
}

fn main() {
    let args = CLI.run(parse);
    println!("{:#?}", args);
}
