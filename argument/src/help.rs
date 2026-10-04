//! Help page and usage rendering.
use std::fmt::Write;

use crate::error::Context;
use crate::spec::{Builtin, Cli, Opt};

/// Left columns wider than this switch the table to the two-line layout.
const MAX_LEFT_COLUMN: usize = 30;

/// Indentation of help texts in the two-line layout.
const NEXT_LINE_INDENT: usize = 10;

pub(crate) fn terminal_width() -> usize {
    std::env::var("COLUMNS")
        .ok()
        .and_then(|x| x.parse::<usize>().ok())
        .unwrap_or(80)
        .clamp(40, 100)
}

pub(crate) fn context<A>(cli: &Cli<A>, prog: &str) -> Context {
    Context {
        prog: prog.to_string(),
        usage: usage(cli, prog),
        help_flag: help_flag(cli),
    }
}

fn help_flag<A>(cli: &Cli<A>) -> Option<&'static str> {
    if cli.builtin_long("help").is_some() {
        Some("--help")
    } else if cli.builtin_short('h').is_some() {
        Some("-h")
    } else {
        None
    }
}

pub(crate) fn usage<A>(cli: &Cli<A>, prog: &str) -> String {
    if let Some(usage) = cli.usage {
        return format!("{} {}", prog, usage);
    }
    let mut rv = prog.to_string();
    if cli.opts.iter().any(|x| x.id.is_some() && !x.hidden) || help_flag(cli).is_some() {
        rv.push_str(" [OPTIONS]");
    }
    for arg in cli.args {
        rv.push(' ');
        rv.push_str(&arg.display());
    }
    if !cli.cmds.is_empty() {
        rv.push_str(" [COMMAND]");
    }
    rv
}

/// A row in the options table.
struct OptRow<'a> {
    short: Option<char>,
    long: Option<&'a str>,
    value: Option<&'a str>,
    optional_value: bool,
    help: &'a str,
}

impl<'a> OptRow<'a> {
    fn from_opt<A>(opt: &'a Opt<A>) -> OptRow<'a> {
        OptRow {
            short: opt.short,
            long: opt.long,
            value: opt.value,
            optional_value: opt.optional_value,
            help: opt.help,
        }
    }

    fn left(&self, pad_long: bool) -> String {
        let mut rv = String::new();
        match (self.short, self.long) {
            (Some(s), Some(l)) => write!(rv, "-{}, --{}", s, l).unwrap(),
            (Some(s), None) => write!(rv, "-{}", s).unwrap(),
            (None, Some(l)) if pad_long => write!(rv, "    --{}", l).unwrap(),
            (None, Some(l)) => write!(rv, "--{}", l).unwrap(),
            (None, None) => {}
        }
        if let Some(value) = self.value {
            match (self.optional_value, self.long.is_some()) {
                (true, true) => write!(rv, "[={}]", value).unwrap(),
                (true, false) => write!(rv, "[{}]", value).unwrap(),
                (false, _) => write!(rv, " <{}>", value).unwrap(),
            }
        }
        rv
    }
}

pub(crate) fn help_text<A>(cli: &Cli<A>, prog: &str, width: usize) -> String {
    let mut out = String::new();

    if let Some(about) = cli.about {
        for line in wrap(about, width) {
            out.push_str(&line);
            out.push('\n');
        }
        out.push('\n');
    }

    writeln!(out, "Usage: {}", usage(cli, prog)).unwrap();

    if !cli.cmds.is_empty() {
        let rows = cli
            .cmds
            .iter()
            .map(|cmd| {
                let about = cmd.info.about().unwrap_or("");
                (
                    cmd.info.name().to_string(),
                    about.lines().next().unwrap_or(""),
                )
            })
            .collect::<Vec<_>>();
        write_table(&mut out, "Commands", &rows, width);
    }

    if !cli.args.is_empty() {
        let rows = cli
            .args
            .iter()
            .map(|arg| (arg.display(), arg.help))
            .collect::<Vec<_>>();
        write_table(&mut out, "Arguments", &rows, width);
    }

    let mut sections: Vec<(&str, Vec<OptRow<'_>>)> = vec![("Options", Vec::new())];
    for opt in cli.opts {
        if opt.id.is_none() {
            sections.push((opt.help, Vec::new()));
        } else if !opt.hidden {
            sections.last_mut().unwrap().1.push(OptRow::from_opt(opt));
        }
    }

    let builtins = [
        (Builtin::Help, 'h', "help", "Print help"),
        (Builtin::Version, 'V', "version", "Print version"),
    ];
    for (builtin, short, long, help) in builtins {
        let short = cli.builtin_short(short).filter(|x| *x == builtin);
        let long = cli.builtin_long(long).filter(|x| *x == builtin);
        if short.is_none() && long.is_none() {
            continue;
        }
        sections[0].1.push(OptRow {
            short: short.map(|_| long_to_short(builtin)),
            long: long.map(|_| builtin_long_name(builtin)),
            value: None,
            optional_value: false,
            help,
        });
    }

    // long options are indented to line up with short options if any
    // option in any section has a short name.
    let pad_long = sections
        .iter()
        .flat_map(|x| x.1.iter())
        .any(|x| x.short.is_some());
    for (title, rows) in sections {
        if rows.is_empty() {
            continue;
        }
        let rows = rows
            .iter()
            .map(|row| (row.left(pad_long), row.help))
            .collect::<Vec<_>>();
        write_table(&mut out, title, &rows, width);
    }

    if let Some(after_help) = cli.after_help {
        out.push('\n');
        for line in wrap(after_help, width) {
            out.push_str(&line);
            out.push('\n');
        }
    }

    out.truncate(out.trim_end().len());
    out
}

fn long_to_short(builtin: Builtin) -> char {
    match builtin {
        Builtin::Help => 'h',
        Builtin::Version => 'V',
    }
}

fn builtin_long_name(builtin: Builtin) -> &'static str {
    match builtin {
        Builtin::Help => "help",
        Builtin::Version => "version",
    }
}

fn write_table(out: &mut String, title: &str, rows: &[(String, &str)], width: usize) {
    writeln!(out, "\n{}:", title).unwrap();
    let left_width = rows.iter().map(|x| x.0.chars().count()).max().unwrap_or(0);
    let col = 2 + left_width + 2;

    if left_width <= MAX_LEFT_COLUMN && width >= col + 20 {
        for (left, help) in rows {
            let lines = wrap(help, width - col);
            let mut line = format!("  {}", left);
            if let Some(first) = lines.first() {
                let pad = col - 2 - left.chars().count();
                line.push_str(&" ".repeat(pad));
                line.push_str(first);
            }
            out.push_str(line.trim_end());
            out.push('\n');
            for line in lines.iter().skip(1) {
                if !line.is_empty() {
                    out.push_str(&" ".repeat(col));
                    out.push_str(line);
                }
                out.push('\n');
            }
        }
    } else {
        for (idx, (left, help)) in rows.iter().enumerate() {
            if idx > 0 {
                out.push('\n');
            }
            writeln!(out, "  {}", left).unwrap();
            for line in wrap(help, width.saturating_sub(NEXT_LINE_INDENT).max(20)) {
                if !line.is_empty() {
                    out.push_str(&" ".repeat(NEXT_LINE_INDENT));
                    out.push_str(&line);
                }
                out.push('\n');
            }
        }
    }
}

/// Wraps text to the given width.  Newlines and leading indentation of
/// lines are preserved.
pub(crate) fn wrap(text: &str, width: usize) -> Vec<String> {
    let mut rv = Vec::new();
    if text.is_empty() {
        return rv;
    }
    for line in text.split('\n') {
        let trimmed = line.trim_start();
        if trimmed.is_empty() {
            rv.push(String::new());
            continue;
        }
        let indent = &line[..line.len() - trimmed.len()];
        let mut current = indent.to_string();
        let mut current_width = indent.chars().count();
        let mut has_word = false;
        for word in trimmed.split_whitespace() {
            let word_width = word.chars().count();
            if has_word && current_width + 1 + word_width > width {
                rv.push(std::mem::replace(&mut current, indent.to_string()));
                current_width = indent.chars().count();
                has_word = false;
            }
            if has_word {
                current.push(' ');
                current_width += 1;
            }
            current.push_str(word);
            current_width += word_width;
            has_word = true;
        }
        rv.push(current);
    }
    rv
}

/// Finds the most similar candidate for a misspelled input.
pub(crate) fn suggest<'a, I>(input: &str, candidates: I) -> Option<&'a str>
where
    I: IntoIterator<Item = &'a str>,
{
    let threshold = (input.chars().count() / 3).max(1);
    candidates
        .into_iter()
        .filter_map(|candidate| {
            let distance = edit_distance(input, candidate);
            if distance <= threshold || (input.len() >= 3 && candidate.starts_with(input)) {
                Some((distance, candidate))
            } else {
                None
            }
        })
        .min_by_key(|x| x.0)
        .map(|x| x.1)
}

/// Optimal string alignment distance (levenshtein with transpositions).
fn edit_distance(a: &str, b: &str) -> usize {
    let a = a.chars().collect::<Vec<_>>();
    let b = b.chars().collect::<Vec<_>>();
    let mut d = vec![vec![0; b.len() + 1]; a.len() + 1];
    for (i, row) in d.iter_mut().enumerate() {
        row[0] = i;
    }
    for (j, cell) in d[0].iter_mut().enumerate() {
        *cell = j;
    }
    for i in 1..=a.len() {
        for j in 1..=b.len() {
            let cost = usize::from(a[i - 1] != b[j - 1]);
            d[i][j] = (d[i - 1][j] + 1)
                .min(d[i][j - 1] + 1)
                .min(d[i - 1][j - 1] + cost);
            if i > 1 && j > 1 && a[i - 1] == b[j - 2] && a[i - 2] == b[j - 1] {
                d[i][j] = d[i][j].min(d[i - 2][j - 2] + 1);
            }
        }
    }
    d[a.len()][b.len()]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::spec::{Cmd, Pos};

    #[derive(Copy, Clone)]
    enum A {
        Output,
        Strict,
        Syntax,
        Input,
        Run,
    }

    #[derive(Copy, Clone)]
    enum R {
        Fast,
    }

    static RUN: Cli<R> = Cli::new("run")
        .about("Runs the thing\nwith more details")
        .opts(&[Opt::new(R::Fast).long("fast").help("Go fast")]);

    static CLI: Cli<A> = Cli::new("demo")
        .version("1.0")
        .about("A demo tool.\n\nIt does demo things.")
        .args(&[Pos::new(A::Input, "INPUT")
            .optional()
            .help("The input file [default: -]")])
        .opts(&[
            Opt::new(A::Output)
                .short('o')
                .long("output")
                .value("FILE")
                .help("Path to the output file"),
            Opt::section("Template Behavior"),
            Opt::new(A::Strict)
                .long("strict")
                .help("Disallow undefined variables in templates"),
            Opt::new(A::Syntax)
                .short('s')
                .long("syntax")
                .value("PAIR")
                .help("Changes a syntax feature (feature=value) [possible features: block-start, block-end, variable-start]"),
        ])
        .commands(&[Cmd::new(A::Run, &RUN)]);

    #[test]
    fn test_help() {
        let _ = (A::Output, A::Strict, A::Syntax, A::Input, A::Run, R::Fast);
        assert_eq!(
            help_text(&CLI, "demo", 60),
            "\
A demo tool.

It does demo things.

Usage: demo [OPTIONS] [INPUT] [COMMAND]

Commands:
  run  Runs the thing

Arguments:
  [INPUT]  The input file [default: -]

Options:
  -o, --output <FILE>  Path to the output file
  -h, --help           Print help
  -V, --version        Print version

Template Behavior:
      --strict         Disallow undefined variables in
                       templates
  -s, --syntax <PAIR>  Changes a syntax feature
                       (feature=value) [possible features:
                       block-start, block-end,
                       variable-start]"
        );
        assert_eq!(
            help_text(&RUN, "demo run", 60),
            "\
Runs the thing
with more details

Usage: demo run [OPTIONS]

Options:
      --fast  Go fast
  -h, --help  Print help"
        );
    }

    #[test]
    fn test_wrap() {
        assert_eq!(wrap("a b c d", 3), vec!["a b", "c d"]);
        assert_eq!(wrap("  a b c", 5), vec!["  a b", "  c"]);
        assert_eq!(wrap("a\n\nb", 5), vec!["a", "", "b"]);
    }

    #[test]
    fn test_suggest() {
        assert_eq!(suggest("strct", ["strict", "syntax"]), Some("strict"));
        assert_eq!(suggest("verb", ["verbose", "quiet"]), Some("verbose"));
        assert_eq!(suggest("xyz", ["verbose", "quiet"]), None);
        assert_eq!(suggest("jbos", ["jobs", "root"]), Some("jobs"));
    }
}
