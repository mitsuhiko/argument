//! Fish completions.
use std::fmt::Write;

use argument::ValueHint;

use crate::{first_line, Node, ValueCompletion};

/// Escapes a string for use in single quotes.
fn quote(s: &str) -> String {
    format!(
        "'{}'",
        s.replace('\\', "\\\\")
            .replace('\'', "\\'")
            .replace('\n', " ")
    )
}

/// Renders the arguments for `-a` (as one single quoted string).
///
/// The string is evaluated by fish again, so values and descriptions are
/// quoted individually.
fn values_arg(completion: &ValueCompletion) -> Option<String> {
    match completion {
        ValueCompletion::Values(values) => {
            let items = values
                .iter()
                .map(|x| match x.help() {
                    Some(help) => format!(
                        "{}\\t{}",
                        inner_quote(x.name()),
                        inner_quote(first_line(help))
                    ),
                    None => inner_quote(x.name()),
                })
                .collect::<Vec<_>>();
            Some(quote(&items.join(" ")))
        }
        ValueCompletion::Hint(ValueHint::DirPath) => Some(quote("(__fish_complete_directories)")),
        ValueCompletion::Hint(ValueHint::CommandName) => Some(quote("(__fish_complete_command)")),
        ValueCompletion::Hint(ValueHint::Username) => Some(quote("(__fish_complete_users)")),
        ValueCompletion::Hint(ValueHint::Hostname) => Some(quote("(__fish_print_hostnames)")),
        ValueCompletion::Hint(_) => None,
    }
}

/// Quotes a string that is placed inside the `-a` argument.
fn inner_quote(s: &str) -> String {
    format!(
        "\"{}\"",
        s.replace('\\', "\\\\")
            .replace('"', "\\\"")
            .replace('$', "\\$")
    )
}

/// The condition for completions of a command.
fn condition(node: &Node) -> Option<String> {
    let mut parts = Vec::new();
    for name in node.path.iter().skip(1) {
        parts.push(format!("__fish_seen_subcommand_from {}", name));
    }
    if !node.children.is_empty() {
        let children = node
            .children
            .iter()
            .map(|x| x.name.as_str())
            .collect::<Vec<_>>();
        parts.push(format!(
            "not __fish_seen_subcommand_from {}",
            children.join(" ")
        ));
    }
    if parts.is_empty() {
        None
    } else {
        Some(quote(&parts.join("; and ")))
    }
}

pub(crate) fn generate(root: &Node) -> String {
    let mut out = String::new();
    let bin = quote(root.bin_name());

    for node in root.walk() {
        let base = match condition(node) {
            Some(cond) => format!("complete -c {} -n {}", bin, cond),
            None => format!("complete -c {}", bin),
        };

        for opt in &node.options {
            let mut line = base.clone();
            if let Some(short) = opt.short() {
                write!(line, " -s {}", quote(&short.to_string())).unwrap();
            }
            if let Some(long) = opt.long() {
                write!(line, " -l {}", quote(long)).unwrap();
            }
            let help = first_line(opt.help());
            if !help.is_empty() {
                write!(line, " -d {}", quote(help)).unwrap();
            }
            if opt.takes_value() {
                let completion = ValueCompletion::for_option(opt);
                if !opt.has_optional_value() {
                    line.push_str(" -r");
                }
                if completion.completes_files() {
                    line.push_str(" -F");
                } else {
                    line.push_str(" -f");
                    if let Some(values) = values_arg(&completion) {
                        write!(line, " -a {}", values).unwrap();
                    }
                }
            }
            out.push_str(&line);
            out.push('\n');
        }

        // positional arguments: fish does not track positions, so the
        // completions of all positionals are offered.
        let mut completes_files = false;
        for arg in &node.arguments {
            let completion = ValueCompletion::for_argument(arg);
            if completion.completes_files() {
                completes_files = true;
            } else if let Some(values) = values_arg(&completion) {
                writeln!(out, "{} -f -a {}", base, values).unwrap();
            }
        }

        for child in &node.children {
            let mut line = format!("{} -f -a {}", base, quote(&child.name));
            if let Some(about) = child.about {
                write!(line, " -d {}", quote(first_line(about))).unwrap();
            }
            out.push_str(&line);
            out.push('\n');
        }

        if !completes_files {
            writeln!(out, "{} -f", base).unwrap();
        }
    }
    out
}
