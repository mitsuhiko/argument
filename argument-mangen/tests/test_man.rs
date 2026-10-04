use std::path::Path;
use std::process::Command;

use argument::{Cli, Cmd, Opt, Pos, ValueHint};
use argument_mangen::Man;

#[derive(Copy, Clone)]
enum Root {
    Verbose,
    Format,
    Strict,
    Input,
    Run,
}

#[derive(Copy, Clone)]
enum Run {
    Fast,
}

static RUN: Cli<Run> = Cli::new("run")
    .about("Runs the thing")
    .opts(&[Opt::new(Run::Fast).long("fast").help("Go fast")]);

static CLI: Cli<Root> = Cli::new("demo")
    .version("1.0")
    .before_help("demo is a tool.")
    .about("A demo tool")
    .long_about("A demo tool.\n\nIt has a long description.\n.with a dot\n'and a quote")
    .after_help("See also: the docs.")
    .args(&[Pos::new(Root::Input, "INPUT")
        .optional()
        .multiple()
        .default_value("-")
        .value_hint(ValueHint::FilePath)
        .help("The input file")])
    .opts(&[
        Opt::new(Root::Verbose)
            .short('v')
            .long("verbose")
            .help("Be verbose"),
        Opt::new(Root::Format)
            .short('f')
            .long("format")
            .value("FORMAT")
            .help("The format")
            .long_help("The format of the data.\n\nExamples:\n-f json\n-f yaml")
            .possible_values_with_help(&[("json", "JSON data"), ("yaml", "YAML data")]),
        Opt::section("Behavior"),
        Opt::new(Root::Strict)
            .long("strict")
            .help("Be strict \\ really"),
    ])
    .commands(&[Cmd::new(Root::Run, &RUN)]);

fn unused() {
    let _ = (
        Root::Verbose,
        Root::Format,
        Root::Strict,
        Root::Input,
        Root::Run,
        Run::Fast,
    );
}

#[test]
fn test_render() {
    unused();
    let page = Man::new(&CLI).date("2025-01-01").render();
    assert!(page.contains(".TH demo 1 2025\\-01\\-01 \"demo 1.0\" \"\""));
    assert!(page.contains(".SH NAME\ndemo \\- A demo tool\n"));
    assert!(page.contains(
        ".SH SYNOPSIS\n\\fBdemo\\fR [\\fB\\-v\\fR|\\fB\\-\\-verbose\\fR] [\\fB\\-f\\fR|\\fB\\-\\-format\\fR] [\\fB\\-h\\fR|\\fB\\-\\-help\\fR] [\\fB\\-V\\fR|\\fB\\-\\-version\\fR] [\\fB\\-\\-strict\\fR] [\\fIINPUT\\fR]... [\\fICOMMAND\\fR]\n"
    ));
    // lines starting with control characters are protected
    assert!(page.contains("\\&.with a dot\n"));
    assert!(page.contains("\\*(Aqand a quote\n"));
    assert!(page.contains(
        ".TP\n\\fB\\-f\\fR, \\fB\\-\\-format\\fR=\\fIFORMAT\\fR\nThe format of the data.\n.sp\nExamples:\n.br\n\\-f json\n.br\n\\-f yaml\n"
    ));
    assert!(page.contains(".SH BEHAVIOR\n.TP\n\\fB\\-\\-strict\\fR\nBe strict \\e really\n"));
    assert!(page
        .contains(".SH ARGUMENTS\n.TP\n[\\fIINPUT\\fR]...\nThe input file\n.sp\n[default: \\-]\n"));
    assert!(page.contains(".SH SUBCOMMANDS\n.TP\ndemo\\-run(1)\nRuns the thing\n"));
    assert!(page.contains(".SH EXTRA\nSee also: the docs.\n"));
    assert!(page.contains(".SH VERSION\nv1.0\n"));
}

#[test]
fn test_generate_to() {
    let dir = std::env::temp_dir().join(format!("argument-mangen-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let files = Man::new(&CLI).generate_to(&dir).unwrap();
    let names = files
        .iter()
        .map(|x| x.file_name().unwrap().to_str().unwrap())
        .collect::<Vec<_>>();
    assert_eq!(names, vec!["demo.1", "demo-run.1"]);

    let sub = std::fs::read_to_string(dir.join("demo-run.1")).unwrap();
    assert!(sub.contains(".TH demo\\-run 1 \"\" \"demo 1.0\" \"\""));
    assert!(sub.contains(".SH SYNOPSIS\n\\fBdemo run\\fR [\\fB\\-\\-fast\\fR]"));

    // validate with mandoc if available
    if Path::new("/usr/bin/mandoc").exists() || which("mandoc") {
        for file in &files {
            let output = Command::new("mandoc")
                .args(["-T", "lint", "-W", "warning"])
                .arg(file)
                .output()
                .unwrap();
            let stdout = String::from_utf8_lossy(&output.stdout);
            // missing dates and sections we do not render are fine
            let problems = stdout
                .lines()
                .filter(|x| !x.contains("missing date") && !x.contains("missing manual"))
                .filter(|x| !x.contains("sections out of conventional order"))
                .filter(|x| !x.contains("unknown manual section"))
                .collect::<Vec<_>>();
            assert!(problems.is_empty(), "{}", stdout);
        }
    }
}

fn which(cmd: &str) -> bool {
    std::env::var_os("PATH")
        .is_some_and(|paths| std::env::split_paths(&paths).any(|dir| dir.join(cmd).is_file()))
}
