//! This is an AI generated version of the minijinja-cli interface.
//!
//! It was generated by taking the --help output from minijinja-cli with some
//! options removed.  This has not been tested so there might be bugs.
//!
//! The AI came up with that weird matching thing and I have to say I
//! don't hate it.
use std::path::PathBuf;

use argument_parser::{Error, Parser};

const USAGE: &str = "minijinja-cli [OPTIONS] [TEMPLATE_FILE] [DATA_FILE]";
const HELP: &str = "\
minijinja-cli is a command line tool to render or evaluate jinja2 templates.

Pass a template and optionally a file with template variables to render it to
stdout.

Usage: !!USAGE!!

Arguments:
  [TEMPLATE_FILE]  Path to the input template [default: -]
  [DATA_FILE]      Path to the data file

Options:
      --config-file <PATH>
          Alternative path to the config file.
  -f, --format <FORMAT>
          The format of the input data [possible values: auto, cbor, ini, json,
          querystring, toml, yaml]
  -D, --define <EXPR>
          Defines an input variable (key=value / key:=json_value)
  -t, --template <TEMPLATE_STRING>
          Render a string template
  -o, --output <FILENAME>
          Path to the output file [default: -]
      --select <SELECTOR>
          Select a subset of the input data
      --print-config
          Print out the loaded config
  -h, --help
          Print short help (short texts)
  -V, --version
          Print version

Template Behavior:
  -a, --autoescape <MODE>  Reconfigures autoescape behavior [possible values: auto,
                           html, json, none]
      --strict             Disallow undefined variables in templates
  -n, --no-newline         Do not output a trailing newline
      --trim-blocks        Enable the trim-blocks flag
      --lstrip-blocks      Enable the lstrip-blocks flag
      --py-compat          Enables improved Python compatibility
  -s, --syntax <PAIR>      Changes a syntax feature (feature=value) [possible
                           features: block-start, block-end, variable-start,
                           variable-end, comment-start, comment-end,
                           line-statement-prefix, line-statement-comment]
      --env                Pass environment variables as ENV to the template

Security:
      --no-include        Disallow includes and extending
      --safe-path <PATH>  Only allow includes from this path
      --fuel <AMOUNT>     Configures the maximum fuel

Advanced:
  -E, --expr <EXPR>      Evaluates an template expression
      --expr-out <MODE>  The expression output mode [possible values: print, json,
                         json-pretty, status]
      --dump <KIND>      Dump internals of a template [possible values:
                         instructions, ast, tokens]
      --repl             Starts the repl with the given data

For a short help use --help.\
";

fn execute() -> Result<(), Error> {
    let mut parser = Parser::from_env();

    let mut template_file = None;
    let mut data_file = None;
    let mut output = PathBuf::from("-");
    let mut autoescape = String::from("auto");
    let mut strict = false;
    let mut no_newline = false;
    let mut trim_blocks = false;
    let mut lstrip_blocks = false;
    let mut py_compat = false;
    let mut syntax = Vec::<String>::new();
    let mut env = false;
    let mut no_include = false;
    let mut safe_path = None;
    let mut fuel = None::<u64>;
    let mut expr = None::<String>;
    let mut expr_out = String::from("print");
    let mut dump = None::<String>;
    let mut repl = false;
    let mut template_str = None::<String>;
    let mut defines = Vec::<String>::new();

    while let Some(param) = parser.param()? {
        match param {
            p if p.is_either('o', "output") => output = PathBuf::from(parser.raw_value()?),
            p if p.is_either('a', "autoescape") => autoescape = parser.value()?,
            p if p.is_long("strict") => strict = true,
            p if p.is_either('n', "no-newline") => no_newline = true,
            p if p.is_long("trim-blocks") => trim_blocks = true,
            p if p.is_long("lstrip-blocks") => lstrip_blocks = true,
            p if p.is_long("py-compat") => py_compat = true,
            p if p.is_either('s', "syntax") => syntax.push(parser.value()?),
            p if p.is_long("env") => env = true,
            p if p.is_long("no-include") => {
                check_conflict(safe_path.is_some(), &["--no-include", "--safe-path"])?;
                no_include = true;
            }
            p if p.is_long("safe-path") => {
                check_conflict(no_include, &["--safe-path", "--no-include"])?;
                safe_path = Some(PathBuf::from(parser.raw_value()?));
            }
            p if p.is_long("fuel") => fuel = Some(parser.value()?),
            p if p.is_either('E', "expr") => {
                check_conflict(
                    template_str.is_some() || repl,
                    &["--expr", "--template", "--repl"],
                )?;
                expr = Some(parser.value()?);
            }
            p if p.is_long("expr-out") => {
                check_conflict(expr.is_none(), &["--expr-out without --expr"])?;
                expr_out = parser.value()?;
            }
            p if p.is_long("dump") => dump = Some(parser.value()?),
            p if p.is_long("repl") => {
                check_conflict(
                    expr.is_some() || template_str.is_some(),
                    &["--repl", "--expr", "--template"],
                )?;
                repl = true;
            }
            p if p.is_either('t', "template") => {
                check_conflict(expr.is_some() || repl, &["--template", "--expr", "--repl"])?;
                template_str = Some(parser.value()?);
            }
            p if p.is_either('D', "define") => defines.push(parser.value()?),
            p if p.is_long("help") => {
                println!("{}", HELP.replace("!!USAGE!!", USAGE));
                return Ok(());
            }
            p if p.is_either('V', "version") => {
                println!("minijinja version 1.0.0");
                return Ok(());
            }
            p if p.is_pos() && template_file.is_none() => {
                template_file = Some(PathBuf::from(parser.raw_value()?));
            }
            p if p.is_pos() && data_file.is_none() => {
                data_file = Some(PathBuf::from(parser.raw_value()?));
            }
            p => return Err(p.into_unexpected_error()),
        }
    }

    println!("Template File: {:?}", template_file);
    println!("Data File: {:?}", data_file);
    println!("Output: {:?}", output);
    println!("Autoescape: {}", autoescape);
    println!("Strict: {}", strict);
    println!("No Newline: {}", no_newline);
    println!("Trim Blocks: {}", trim_blocks);
    println!("Lstrip Blocks: {}", lstrip_blocks);
    println!("Python Compatibility: {}", py_compat);
    println!("Syntax: {:?}", syntax);
    println!("Environment: {}", env);
    println!("No Include: {}", no_include);
    println!("Safe Path: {:?}", safe_path);
    println!("Fuel: {:?}", fuel);
    println!("Expression: {:?}", expr);
    println!("Expression Output: {}", expr_out);
    println!("Dump: {:?}", dump);
    println!("Repl: {}", repl);
    println!("Template String: {:?}", template_str);
    println!("Defines: {:?}", defines);

    Ok(())
}

fn check_conflict(cond: bool, conflicts: &[&str]) -> Result<(), Error> {
    if cond {
        Err(format!("{} are mutually exclusive", conflicts.join(" and ")).into())
    } else {
        Ok(())
    }
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
