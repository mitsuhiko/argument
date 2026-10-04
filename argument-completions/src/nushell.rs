//! Nushell completions (as `extern` declarations).
use std::fmt::Write;

use argument::ValueHint;

use crate::{first_line, ident, Node, ValueCompletion};

fn quote(s: &str) -> String {
    format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
}

/// The nushell type for a value.
fn value_type(completion: &ValueCompletion) -> &'static str {
    match completion {
        ValueCompletion::Hint(
            ValueHint::AnyPath
            | ValueHint::FilePath
            | ValueHint::DirPath
            | ValueHint::ExecutablePath,
        ) => "path",
        _ => "string",
    }
}

/// The name of the custom completer for values.
fn completer_name(node: &Node, name: &str) -> String {
    format!("nu-complete {} {}", node.path.join(" "), name)
}

fn write_completer(out: &mut String, node: &Node, name: &str, completion: &ValueCompletion) {
    if let ValueCompletion::Values(values) = completion {
        writeln!(out, "  def {} [] {{", quote(&completer_name(node, name))).unwrap();
        let values = values
            .iter()
            .map(|x| quote(x.name()))
            .collect::<Vec<_>>()
            .join(" ");
        writeln!(out, "    [ {} ]\n  }}\n", values).unwrap();
    }
}

/// Appends the type annotation, completer and help comment.
fn finish_line(
    out: &mut String,
    mut line: String,
    node: &Node,
    name: &str,
    completion: Option<&ValueCompletion>,
    help: &str,
) {
    if let Some(completion) = completion {
        write!(line, ": {}", value_type(completion)).unwrap();
        if let ValueCompletion::Values(_) = completion {
            write!(line, "@{}", quote(&completer_name(node, name))).unwrap();
        }
    }
    let help = first_line(help);
    if !help.is_empty() {
        let pad = 30usize.saturating_sub(line.len()).max(1);
        write!(line, "{}# {}", " ".repeat(pad), help).unwrap();
    }
    out.push_str(&line);
    out.push('\n');
}

pub(crate) fn generate(root: &Node) -> String {
    let mut out = String::from("module completions {\n\n");

    for node in root.walk() {
        let opts = node
            .options
            .iter()
            .map(|opt| {
                let name = opt
                    .long()
                    .map(str::to_string)
                    .or_else(|| opt.short().map(|x| x.to_string()))
                    .unwrap_or_default();
                let completion = opt.takes_value().then(|| ValueCompletion::for_option(opt));
                (opt, name, completion)
            })
            .collect::<Vec<_>>();
        let args = node
            .arguments
            .iter()
            .map(|arg| {
                (
                    arg,
                    ident(&arg.name().to_lowercase()),
                    ValueCompletion::for_argument(arg),
                )
            })
            .collect::<Vec<_>>();

        for (_, name, completion) in &opts {
            if let Some(completion) = completion {
                write_completer(&mut out, node, name, completion);
            }
        }
        for (_, name, completion) in &args {
            write_completer(&mut out, node, name, completion);
        }

        if let Some(about) = node.about {
            writeln!(out, "  # {}", first_line(about)).unwrap();
        }
        writeln!(out, "  export extern {} [", quote(&node.path.join(" "))).unwrap();
        for (opt, name, completion) in &opts {
            let line = match (opt.short(), opt.long()) {
                (Some(short), Some(long)) => format!("    --{}(-{})", long, short),
                (Some(short), None) => format!("    -{}", short),
                (None, Some(long)) => format!("    --{}", long),
                (None, None) => continue,
            };
            finish_line(&mut out, line, node, name, completion.as_ref(), opt.help());
        }
        for (arg, name, completion) in &args {
            let line = if arg.is_multiple() {
                format!("    ...{}", name)
            } else if arg.is_required() {
                format!("    {}", name)
            } else {
                format!("    {}?", name)
            };
            finish_line(&mut out, line, node, name, Some(completion), arg.help());
        }
        out.push_str("  ]\n\n");
    }

    out.push_str("}\n\nexport use completions *\n");
    out
}
