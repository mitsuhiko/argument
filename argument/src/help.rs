//! Help page and usage rendering.
use std::fmt::Write;

use crate::error::Context;
use crate::info::{CommandInfo, OptionInfo, PossibleValue};

/// Indentation of help texts in the two-line layout.
const NEXT_LINE_INDENT: usize = 10;

/// The default and maximum width if not configured otherwise.
const DEFAULT_WIDTH: usize = 100;

/// The width used for rendering help pages.
///
/// An explicit `COLUMNS` environment variable wins over the detected terminal
/// width.  The result is capped by the configured maximum width.
pub(crate) fn render_width(cmd: &dyn CommandInfo) -> usize {
    let detected = std::env::var("COLUMNS")
        .ok()
        .and_then(|x| x.parse::<usize>().ok())
        .filter(|&x| x > 0)
        .or_else(crate::term::terminal_width)
        .unwrap_or(DEFAULT_WIDTH);
    detected
        .min(cmd.max_width().unwrap_or(DEFAULT_WIDTH))
        .max(40)
}

pub(crate) fn context(cmd: &dyn CommandInfo, prog: &str) -> Context {
    Context {
        prog: prog.to_string(),
        usage: usage(cmd, prog),
        help_flag: help_flag(cmd),
    }
}

/// Finds the flag that shows the help (`--help` or `-h`).
fn help_flag(cmd: &dyn CommandInfo) -> Option<&'static str> {
    let opts = cmd.options();
    if opts.iter().any(|x| x.long() == Some("help")) {
        Some("--help")
    } else if opts.iter().any(|x| x.short() == Some('h')) {
        Some("-h")
    } else {
        None
    }
}

pub(crate) fn usage(cmd: &dyn CommandInfo, prog: &str) -> String {
    if let Some(usage) = cmd.usage_override() {
        return format!("{} {}", prog, usage);
    }
    let mut rv = prog.to_string();
    if cmd.options().iter().any(|x| !x.is_hidden()) {
        rv.push_str(" [OPTIONS]");
    }
    for arg in cmd.arguments() {
        rv.push(' ');
        rv.push_str(&arg.display());
    }
    if !cmd.commands().is_empty() {
        rv.push_str(" [COMMAND]");
    }
    rv
}

/// Renders the left column of an option.
pub(crate) fn option_display(opt: &OptionInfo, pad_long: bool) -> String {
    let mut rv = String::new();
    match (opt.short(), opt.long()) {
        (Some(s), Some(l)) => write!(rv, "-{}, --{}", s, l).unwrap(),
        (Some(s), None) => write!(rv, "-{}", s).unwrap(),
        (None, Some(l)) if pad_long => write!(rv, "    --{}", l).unwrap(),
        (None, Some(l)) => write!(rv, "--{}", l).unwrap(),
        (None, None) => {}
    }
    if let Some(value) = opt.value_name() {
        match (opt.has_optional_value(), opt.long().is_some()) {
            (true, true) => write!(rv, "[={}]", value).unwrap(),
            (true, false) => write!(rv, "[{}]", value).unwrap(),
            (false, _) => write!(rv, " <{}>", value).unwrap(),
        }
    }
    rv
}

/// Assembles the help text of an option or argument.
fn item_help(
    help: &str,
    long_help: Option<&str>,
    default_value: Option<&str>,
    values: &[PossibleValue],
    long: bool,
) -> String {
    if !long {
        let mut rv = help.to_string();
        let mut add = |piece: String| {
            if !rv.is_empty() {
                rv.push(' ');
            }
            rv.push_str(&piece);
        };
        if let Some(default) = default_value {
            add(format!("[default: {}]", default));
        }
        if !values.is_empty() {
            let names = values.iter().map(|x| x.name()).collect::<Vec<_>>();
            add(format!("[possible values: {}]", names.join(", ")));
        }
        return rv;
    }

    let mut rv = long_help.unwrap_or(help).trim_end().to_string();
    let mut add = |piece: String| {
        if !rv.is_empty() {
            rv.push_str("\n\n");
        }
        rv.push_str(&piece);
    };
    if let Some(default) = default_value {
        add(format!("[default: {}]", default));
    }
    if values.iter().any(|x| x.help().is_some()) {
        let mut piece = "Possible values:".to_string();
        for value in values {
            match value.help() {
                Some(help) => write!(piece, "\n- {}: {}", value.name(), help).unwrap(),
                None => write!(piece, "\n- {}", value.name()).unwrap(),
            }
        }
        add(piece);
    } else if !values.is_empty() {
        let names = values.iter().map(|x| x.name()).collect::<Vec<_>>();
        add(format!("[possible values: {}]", names.join(", ")));
    }
    rv
}

/// Renders the help page.  `long` selects the long help.
pub(crate) fn help_text(cmd: &dyn CommandInfo, prog: &str, width: usize, long: bool) -> String {
    let mut out = String::new();

    let about = if long {
        cmd.long_about().or(cmd.about())
    } else {
        cmd.about()
    };
    for text in [cmd.before_help(), about].into_iter().flatten() {
        for line in wrap(text.trim_end(), width) {
            out.push_str(&line);
            out.push('\n');
        }
        out.push('\n');
    }

    writeln!(out, "Usage: {}", usage(cmd, prog)).unwrap();

    let commands = cmd.commands();
    if !commands.is_empty() {
        let rows = commands
            .iter()
            .map(|cmd| {
                let about = cmd.about().unwrap_or("");
                (
                    cmd.name().to_string(),
                    about.lines().next().unwrap_or("").to_string(),
                )
            })
            .collect::<Vec<_>>();
        write_table(&mut out, "Commands", &rows, width, false);
    }

    let args = cmd.arguments();
    if !args.is_empty() {
        let rows = args
            .iter()
            .map(|arg| {
                let help = item_help(
                    arg.help(),
                    arg.long_help(),
                    arg.default_value(),
                    &arg.possible_values(),
                    long,
                );
                (arg.display(), help)
            })
            .collect::<Vec<_>>();
        write_table(&mut out, "Arguments", &rows, width, long);
    }

    let opts = cmd
        .options()
        .into_iter()
        .filter(|x| !x.is_hidden())
        .collect::<Vec<_>>();
    // long options are indented to line up with short options if any
    // option has a short name.
    let pad_long = opts.iter().any(|x| x.short().is_some());
    type Rows = Vec<(String, String)>;
    let mut sections: Vec<(Option<&str>, Rows)> = Vec::new();
    for opt in &opts {
        let row = (
            option_display(opt, pad_long),
            item_help(
                opt.help(),
                opt.long_help(),
                opt.default_value(),
                &opt.possible_values(),
                long,
            ),
        );
        match sections.iter_mut().find(|x| x.0 == opt.section()) {
            Some(section) => section.1.push(row),
            None => sections.push((opt.section(), vec![row])),
        }
    }
    for (title, rows) in sections {
        write_table(&mut out, title.unwrap_or("Options"), &rows, width, long);
    }

    if let Some(after_help) = cmd.after_help() {
        out.push('\n');
        for line in wrap(after_help.trim_end(), width) {
            out.push_str(&line);
            out.push('\n');
        }
    }

    out.truncate(out.trim_end().len());
    out
}

fn text_width(s: &str) -> usize {
    s.chars().count()
}

/// Decides if a table should put the help text on the next line.
///
/// This uses the same heuristic as clap: the help goes on the next line if
/// the left column takes up more than 40% of the width and a help text does
/// not fit next to it.
fn use_next_line(rows: &[(String, String)], width: usize) -> bool {
    let longest = rows.iter().map(|x| text_width(&x.0)).max().unwrap_or(0);
    let taken = longest + 4;
    rows.iter().any(|(_, help)| {
        width >= taken && (taken as f32 / width as f32) > 0.40 && text_width(help) > width - taken
    })
}

fn write_table(
    out: &mut String,
    title: &str,
    rows: &[(String, String)],
    width: usize,
    next_line: bool,
) {
    if rows.is_empty() {
        return;
    }
    writeln!(out, "\n{}:", title).unwrap();

    if !next_line && !use_next_line(rows, width) {
        let longest = rows.iter().map(|x| text_width(&x.0)).max().unwrap_or(0);
        let col = 2 + longest + 2;
        for (left, help) in rows {
            let lines = wrap(help, width.saturating_sub(col).max(20));
            let mut line = format!("  {}", left);
            if let Some(first) = lines.first() {
                line.push_str(&" ".repeat(col - 2 - text_width(left)));
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

/// Wraps text to the given width.  Newlines, leading indentation of lines
/// and whitespace between words are preserved (whitespace at line breaks is
/// dropped).
pub(crate) fn wrap(text: &str, width: usize) -> Vec<String> {
    let mut rv = Vec::new();
    if text.is_empty() {
        return rv;
    }
    for line in text.split('\n') {
        let line = line.trim_end();
        let trimmed = line.trim_start();
        if trimmed.is_empty() {
            rv.push(String::new());
            continue;
        }
        let indent = &line[..line.len() - trimmed.len()];
        let mut current = indent.to_string();
        let mut current_width = text_width(indent);
        let mut has_word = false;
        let mut rest = trimmed;
        while !rest.is_empty() {
            let space_len = rest.len() - rest.trim_start().len();
            let space = &rest[..space_len];
            rest = &rest[space_len..];
            let word_len = rest.find(char::is_whitespace).unwrap_or(rest.len());
            let word = &rest[..word_len];
            rest = &rest[word_len..];
            let word_width = text_width(word);
            if has_word && current_width + text_width(space) + word_width > width {
                rv.push(std::mem::replace(&mut current, indent.to_string()));
                current_width = text_width(indent);
                has_word = false;
            }
            if has_word {
                current.push_str(space);
                current_width += text_width(space);
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
    use crate::info::ValueHint;
    use crate::spec::{Cli, Cmd, Opt, Pos};

    #[derive(Copy, Clone)]
    enum A {
        Output,
        Strict,
        Syntax,
        Format,
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
        .before_help("demo is a demo tool.")
        .about("It does demo things.")
        .long_about("It does demo things.\n\nIn a long way.")
        .args(&[Pos::new(A::Input, "INPUT")
            .optional()
            .default_value("-")
            .value_hint(ValueHint::FilePath)
            .help("The input file")])
        .opts(&[
            Opt::new(A::Output)
                .short('o')
                .long("output")
                .value("FILE")
                .help("Path to the output file")
                .long_help("Path to the output file.\n\nAtomically written."),
            Opt::new(A::Format)
                .long("format")
                .value("FORMAT")
                .help("The format")
                .possible_values_with_help(&[("json", "JSON"), ("yaml", "YAML")]),
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
            help_text(&CLI, "demo", 60, false),
            "\
demo is a demo tool.

It does demo things.

Usage: demo [OPTIONS] [INPUT] [COMMAND]

Commands:
  run  Runs the thing

Arguments:
  [INPUT]  The input file [default: -]

Options:
  -o, --output <FILE>
          Path to the output file

      --format <FORMAT>
          The format [possible values: json, yaml]

  -h, --help
          Print help

  -V, --version
          Print version

Template Behavior:
      --strict         Disallow undefined variables in
                       templates
  -s, --syntax <PAIR>  Changes a syntax feature
                       (feature=value) [possible features:
                       block-start, block-end,
                       variable-start]"
        );
        assert_eq!(
            help_text(&RUN, "demo run", 60, false),
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
    fn test_long_help() {
        assert_eq!(
            help_text(&CLI, "demo", 60, true),
            "\
demo is a demo tool.

It does demo things.

In a long way.

Usage: demo [OPTIONS] [INPUT] [COMMAND]

Commands:
  run  Runs the thing

Arguments:
  [INPUT]
          The input file

          [default: -]

Options:
  -o, --output <FILE>
          Path to the output file.

          Atomically written.

      --format <FORMAT>
          The format

          Possible values:
          - json: JSON
          - yaml: YAML

  -h, --help
          Print help

  -V, --version
          Print version

Template Behavior:
      --strict
          Disallow undefined variables in templates

  -s, --syntax <PAIR>
          Changes a syntax feature (feature=value) [possible
          features: block-start, block-end, variable-start]"
        );
    }

    #[test]
    fn test_wrap() {
        assert_eq!(wrap("a b c d", 3), vec!["a b", "c d"]);
        assert_eq!(wrap("  a b c", 5), vec!["  a b", "  c"]);
        assert_eq!(wrap("a\n\nb", 5), vec!["a", "", "b"]);
        assert_eq!(wrap("a.  b   c", 20), vec!["a.  b   c"]);
        assert_eq!(wrap("a.  b   c", 5), vec!["a.  b", "c"]);
    }

    #[test]
    fn test_suggest() {
        assert_eq!(suggest("strct", ["strict", "syntax"]), Some("strict"));
        assert_eq!(suggest("verb", ["verbose", "quiet"]), Some("verbose"));
        assert_eq!(suggest("xyz", ["verbose", "quiet"]), None);
        assert_eq!(suggest("jbos", ["jobs", "root"]), Some("jobs"));
    }
}
