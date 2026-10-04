use std::path::{Path, PathBuf};
use std::process::Command;

use argument::{Cli, Cmd, Opt, Pos, ValueHint};
use argument_completions::Shell;

#[derive(Copy, Clone)]
enum Root {
    Verbose,
    Format,
    Output,
    Dir,
    Level,
    Input,
    Run,
    Remote,
}

#[derive(Copy, Clone)]
enum Run {
    Fast,
    Mode,
}

#[derive(Copy, Clone)]
enum Remote {
    Add,
}

#[derive(Copy, Clone)]
enum Add {
    Force,
    Name,
}

static ADD: Cli<Add> = Cli::new("add")
    .about("Adds a remote")
    .args(&[Pos::new(Add::Name, "NAME").value_hint(ValueHint::Other)])
    .opts(&[Opt::new(Add::Force).long("force").help("Force it")]);

static REMOTE: Cli<Remote> = Cli::new("remote")
    .about("Manages remotes")
    .commands(&[Cmd::new(Remote::Add, &ADD)]);

static RUN: Cli<Run> = Cli::new("run")
    .about("Runs the thing\nwith a second line")
    .args(&[Pos::new(Run::Mode, "MODE")
        .optional()
        .possible_values(&["fast", "slow"])])
    .opts(&[Opt::new(Run::Fast)
        .long("fast")
        .help("Go fast, don't 'wait' [really]")]);

static CLI: Cli<Root> = Cli::new("demo")
    .version("1.0")
    .about("A demo tool")
    .args(&[Pos::new(Root::Input, "INPUT")
        .optional()
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
            .possible_values_with_help(&[("json", "JSON data"), ("yaml", "YAML: the 'other' one")])
            .help("The format"),
        Opt::new(Root::Output)
            .short('o')
            .long("output")
            .value("FILE")
            .value_hint(ValueHint::FilePath)
            .help("Output file"),
        Opt::new(Root::Dir)
            .long("dir")
            .value("DIR")
            .value_hint(ValueHint::DirPath)
            .help("A directory"),
        Opt::new(Root::Level)
            .long("level")
            .optional_value("N")
            .possible_values(&["1", "2"])
            .help("The level"),
    ])
    .commands(&[Cmd::new(Root::Run, &RUN), Cmd::new(Root::Remote, &REMOTE)]);

fn unused() {
    let _ = (
        Root::Verbose,
        Root::Format,
        Root::Output,
        Root::Dir,
        Root::Level,
        Root::Input,
        Root::Run,
        Root::Remote,
        Run::Fast,
        Run::Mode,
        Remote::Add,
        Add::Force,
        Add::Name,
    );
}

fn find_shell(candidates: &[&str]) -> Option<PathBuf> {
    for candidate in candidates {
        let path = Path::new(candidate);
        if path.is_absolute() {
            if path.is_file() {
                return Some(path.to_path_buf());
            }
        } else if let Some(paths) = std::env::var_os("PATH") {
            for dir in std::env::split_paths(&paths) {
                let path = dir.join(candidate);
                if path.is_file() {
                    return Some(path);
                }
            }
        }
    }
    None
}

fn write_script(shell: Shell, suffix: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "argument-completions-{}-{}",
        std::process::id(),
        suffix
    ));
    std::fs::create_dir_all(&dir).unwrap();
    shell.generate_to(&CLI, "demo", &dir).unwrap()
}

fn run(shell: &Path, args: &[&str]) -> (bool, String, String) {
    let output = Command::new(shell).args(args).output().unwrap();
    (
        output.status.success(),
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

#[test]
fn test_parse_shell() {
    unused();
    for shell in Shell::ALL {
        assert_eq!(shell.name().parse::<Shell>().unwrap(), *shell);
    }
    assert_eq!(Shell::NAMES.len(), Shell::ALL.len());
    assert!("cmd".parse::<Shell>().is_err());
}

#[test]
fn test_all_shells_generate() {
    for shell in Shell::ALL {
        let script = shell.generate(&CLI, "demo");
        assert!(script.contains("verbose"), "{}", shell);
        assert!(script.contains("remote"), "{}", shell);
    }
}

#[test]
fn test_zsh_syntax() {
    let Some(zsh) = find_shell(&["zsh"]) else {
        return;
    };
    let script = write_script(Shell::Zsh, "zsh");
    let (ok, _, stderr) = run(&zsh, &["-n", script.to_str().unwrap()]);
    assert!(ok, "{}", stderr);
    // loading it in an interactive-less shell with compinit must work
    let (ok, _, stderr) = run(
        &zsh,
        &[
            "-f",
            "-c",
            &format!(
                "autoload -U compinit && compinit -u -D && source {}",
                script.display()
            ),
        ],
    );
    assert!(ok, "{}", stderr);
}

fn zsh_complete(zsh_dir: &Path, lines: &[&str]) -> Vec<(String, String)> {
    let python = find_shell(&["python3"]).unwrap();
    let harness = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/zsh_complete.py");
    let mut args = vec![harness.to_str().unwrap(), zsh_dir.to_str().unwrap()];
    args.extend(lines);
    let (ok, stdout, stderr) = run(&python, &args);
    assert!(ok, "{}", stderr);
    stdout
        .lines()
        .filter_map(|x| x.split_once('\t'))
        .map(|(a, b)| (a.to_string(), b.to_string()))
        .collect()
}

#[test]
fn test_zsh_behavior() {
    if find_shell(&["zsh"]).is_none() || find_shell(&["python3"]).is_none() {
        return;
    }
    let script = write_script(Shell::Zsh, "zsh-behavior");
    let results = zsh_complete(
        script.parent().unwrap(),
        &[
            "demo --form",
            "demo --format ",
            "demo --format=y",
            "demo -fj",
            "demo r",
            "demo run --f",
            "demo run ",
            "demo -o foo remote ",
            "demo -f json remote add --",
            "demo --level=",
        ],
    );
    let get = |line: &str| {
        results
            .iter()
            .filter(|x| x.0 == line)
            .map(|x| x.1.as_str())
            .collect::<Vec<_>>()
    };
    assert_eq!(get("demo --form"), vec!["--format"]);
    assert_eq!(get("demo --format "), vec!["json", "yaml"]);
    assert_eq!(get("demo --format=y"), vec!["yaml"]);
    assert_eq!(get("demo -fj"), vec!["json"]);
    assert_eq!(get("demo r"), vec!["run", "remote"]);
    assert_eq!(get("demo run --f"), vec!["--fast"]);
    assert_eq!(get("demo run "), vec!["fast", "slow"]);
    assert_eq!(get("demo -o foo remote "), vec!["add"]);
    assert_eq!(get("demo -f json remote add --"), vec!["--force", "--help"]);
    assert_eq!(get("demo --level="), vec!["1", "2"]);
}

fn bash_complete(bash: &Path, script: &Path, words: &[&str]) -> Vec<String> {
    let quoted = words
        .iter()
        .map(|x| format!("'{}'", x.replace('\'', "'\\''")))
        .collect::<Vec<_>>()
        .join(" ");
    let code = format!(
        "source '{}'; COMP_WORDS=({}); COMP_CWORD={}; _demo; printf '%s\\n' \"${{COMPREPLY[@]}}\"",
        script.display(),
        quoted,
        words.len() - 1
    );
    let (ok, stdout, stderr) = run(bash, &["--norc", "--noprofile", "-c", &code]);
    assert!(ok, "{}", stderr);
    assert!(stderr.is_empty(), "{}", stderr);
    stdout
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| x.to_string())
        .collect()
}

#[test]
fn test_bash() {
    for bash in [
        find_shell(&["/opt/homebrew/bin/bash", "/usr/local/bin/bash"]),
        find_shell(&["/bin/bash"]),
    ]
    .into_iter()
    .flatten()
    {
        let script = write_script(Shell::Bash, "bash");
        let (ok, _, stderr) = run(&bash, &["-n", script.to_str().unwrap()]);
        assert!(ok, "{}", stderr);

        // options
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "--fo"]),
            vec!["--format"]
        );
        // commands
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "r"]),
            vec!["run", "remote"]
        );
        // option values in all forms
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "--format", ""]),
            vec!["json", "yaml"]
        );
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "-f", "y"]),
            vec!["yaml"]
        );
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "--format", "=", "j"]),
            vec!["json"]
        );
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "--format", "="]),
            vec!["json", "yaml"]
        );
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "--format=j"]),
            vec!["--format=json"]
        );
        // option values are skipped when finding the command
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "-f", "json", "run", "--"]),
            vec!["--fast", "--help"]
        );
        // positional values of subcommands
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "run", "s"]),
            vec!["slow"]
        );
        // nested subcommands
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "-v", "remote", ""]),
            vec!["add"]
        );
        assert_eq!(
            bash_complete(&bash, &script, &["demo", "remote", "add", "--f"]),
            vec!["--force"]
        );
        // file values fall back to the default completion
        assert!(bash_complete(&bash, &script, &["demo", "-o", ""]).is_empty());
    }
}

fn fish_complete(fish: &Path, script: &Path, line: &str) -> Vec<String> {
    let code = format!(
        "source '{}'; complete -C '{}'",
        script.display(),
        line.replace('\'', "\\'")
    );
    let (ok, stdout, stderr) = run(fish, &["--no-config", "-c", &code]);
    assert!(ok, "{}", stderr);
    assert!(stderr.is_empty(), "{}", stderr);
    stdout.lines().map(|x| x.to_string()).collect()
}

#[test]
fn test_fish() {
    let Some(fish) = find_shell(&["fish"]) else {
        return;
    };
    let script = write_script(Shell::Fish, "fish");
    let (ok, _, stderr) = run(&fish, &["--no-execute", script.to_str().unwrap()]);
    assert!(ok, "{}", stderr);

    assert_eq!(
        fish_complete(&fish, &script, "demo --form"),
        vec!["--format\tThe format"]
    );
    assert_eq!(
        fish_complete(&fish, &script, "demo --format "),
        vec!["json\tJSON data", "yaml\tYAML: the 'other' one"]
    );
    assert_eq!(
        fish_complete(&fish, &script, "demo ru"),
        vec!["run\tRuns the thing"]
    );
    assert_eq!(
        fish_complete(&fish, &script, "demo run --f"),
        vec!["--fast\tGo fast, don't 'wait' [really]"]
    );
    assert_eq!(fish_complete(&fish, &script, "demo run s"), vec!["slow"]);
    assert_eq!(
        fish_complete(&fish, &script, "demo remote "),
        vec!["add\tAdds a remote"]
    );
    assert_eq!(
        fish_complete(&fish, &script, "demo remote add --"),
        vec!["--force\tForce it", "--help\tPrint help"]
    );
}

#[test]
fn test_snapshots_are_stable() {
    // make sure generation is deterministic
    for shell in Shell::ALL {
        assert_eq!(shell.generate(&CLI, "demo"), shell.generate(&CLI, "demo"));
    }
}
