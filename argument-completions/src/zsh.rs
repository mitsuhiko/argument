//! Zsh completions.
use std::fmt::Write;

use argument::ValueHint;

use crate::{first_line, ident, Node, ValueCompletion};

/// Escapes help text for use in `[...]` within single quotes.
fn escape_help(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('\'', "'\\''")
        .replace('[', "\\[")
        .replace(']', "\\]")
        .replace(':', "\\:")
        .replace('$', "\\$")
        .replace('`', "\\`")
        .replace('\n', " ")
}

/// Escapes values for use in `(...)` within single quotes.
fn escape_value(s: &str) -> String {
    escape_help(s)
        .replace('(', "\\(")
        .replace(')', "\\)")
        .replace(' ', "\\ ")
}

fn action(completion: &ValueCompletion) -> String {
    match completion {
        ValueCompletion::Values(values) => {
            if values.iter().any(|x| x.help().is_some()) {
                let values = values
                    .iter()
                    .map(|x| {
                        format!(
                            "{}\\:\"{}\"",
                            escape_value(x.name()),
                            escape_help(x.help().unwrap_or("")).replace('"', "\\\"")
                        )
                    })
                    .collect::<Vec<_>>();
                format!("(({}))", values.join(" "))
            } else {
                let values = values
                    .iter()
                    .map(|x| escape_value(x.name()))
                    .collect::<Vec<_>>();
                format!("({})", values.join(" "))
            }
        }
        ValueCompletion::Hint(hint) => match hint {
            ValueHint::Other => "",
            ValueHint::AnyPath | ValueHint::FilePath => "_files",
            ValueHint::DirPath => "_files -/",
            ValueHint::ExecutablePath => "_absolute_command_paths",
            ValueHint::CommandName => "_command_names -e",
            ValueHint::Username => "_users",
            ValueHint::Hostname => "_hosts",
            ValueHint::Url => "_urls",
            ValueHint::EmailAddress => "_email_addresses",
            _ => "_default",
        }
        .to_string(),
    }
}

pub(crate) fn generate(root: &Node) -> String {
    let bin = root.bin_name();
    let fn_name = format!("_{}", ident(bin));
    let mut out = String::new();
    writeln!(
        out,
        r#"#compdef {bin}

autoload -U is-at-least

{fn_name}() {{
    typeset -A opt_args
    typeset -a _arguments_options
    local ret=1

    if is-at-least 5.2; then
        _arguments_options=(-s -S -C)
    else
        _arguments_options=(-s -C)
    fi

    local context curcontext="$curcontext" state line"#,
        bin = bin,
        fn_name = fn_name
    )
    .unwrap();
    write_command(&mut out, root, 1);
    out.push_str("    return ret\n}\n\n");

    for node in root.walk() {
        if node.children.is_empty() {
            continue;
        }
        let commands_fn = format!("_{}_commands", node.ident());
        writeln!(
            out,
            "(( $+functions[{f}] )) ||\n{f}() {{\n    local commands; commands=(",
            f = commands_fn
        )
        .unwrap();
        for child in &node.children {
            let about = child.about.map(first_line).unwrap_or("");
            writeln!(
                out,
                "        '{}:{}'",
                escape_value(&child.name),
                escape_help(about)
            )
            .unwrap();
        }
        writeln!(
            out,
            "    )\n    _describe -t commands '{} commands' commands \"$@\"\n}}\n",
            escape_help(&node.path.join(" "))
        )
        .unwrap();
    }

    writeln!(
        out,
        r#"if [ "$funcstack[1]" = "{fn_name}" ]; then
    {fn_name} "$@"
else
    compdef {fn_name} {bin}
fi"#,
        fn_name = fn_name,
        bin = bin
    )
    .unwrap();
    out
}

fn write_command(out: &mut String, node: &Node, depth: usize) {
    let indent = "    ".repeat(depth);
    writeln!(
        out,
        "{}_arguments \"${{_arguments_options[@]}}\" : \\",
        indent
    )
    .unwrap();

    for opt in &node.options {
        let help = escape_help(first_line(opt.help()));
        let value = match opt.value_name() {
            Some(name) => {
                let colons = if opt.has_optional_value() { "::" } else { ":" };
                format!(
                    "{}{}:{}",
                    colons,
                    escape_value(name),
                    action(&ValueCompletion::for_option(opt))
                )
            }
            None => String::new(),
        };
        // options may be repeated as we do not know if they can be given once
        if let Some(short) = opt.short() {
            let suffix = match (opt.takes_value(), opt.has_optional_value()) {
                (true, true) => "-",
                (true, false) => "+",
                (false, _) => "",
            };
            writeln!(
                out,
                "{}'*-{}{}[{}]{}' \\",
                indent, short, suffix, help, value
            )
            .unwrap();
        }
        if let Some(long) = opt.long() {
            let suffix = match (opt.takes_value(), opt.has_optional_value()) {
                (true, true) => "=-",
                (true, false) => "=",
                (false, _) => "",
            };
            writeln!(
                out,
                "{}'*--{}{}[{}]{}' \\",
                indent, long, suffix, help, value
            )
            .unwrap();
        }
    }

    // the parser matches commands before positional arguments, so if there
    // are subcommands, they take the positional slots.
    let arguments = if node.children.is_empty() {
        &node.arguments[..]
    } else {
        &[]
    };
    for arg in arguments {
        let cardinality = if arg.is_multiple() {
            if arg.is_required() {
                "*:"
            } else {
                "*::"
            }
        } else if arg.is_required() {
            ":"
        } else {
            "::"
        };
        let help = match first_line(arg.help()) {
            "" => String::new(),
            help => format!(" -- {}", escape_help(help)),
        };
        writeln!(
            out,
            "{}'{}{}{}:{}' \\",
            indent,
            cardinality,
            escape_value(arg.name()),
            help,
            action(&ValueCompletion::for_argument(arg))
        )
        .unwrap();
    }

    if !node.children.is_empty() {
        writeln!(out, "{}\":: :_{}_commands\" \\", indent, node.ident()).unwrap();
        writeln!(out, "{}\"*::: :->{}\" \\", indent, node.ident()).unwrap();
    }
    writeln!(out, "{}&& ret=0", indent).unwrap();

    if node.children.is_empty() {
        return;
    }

    writeln!(
        out,
        r#"{indent}case $state in
{indent}({state})
{indent}    words=($line[1] "${{words[@]}}")
{indent}    (( CURRENT += 1 ))
{indent}    curcontext="${{curcontext%:*:*}}:{hyphen}-command-$line[1]:"
{indent}    case $line[1] in"#,
        indent = indent,
        state = node.ident(),
        hyphen = node.path.join("-"),
    )
    .unwrap();
    for child in &node.children {
        writeln!(out, "{}        ({})", indent, escape_value(&child.name)).unwrap();
        write_command(out, child, depth + 3);
        writeln!(out, "{}            ;;", indent).unwrap();
    }
    writeln!(
        out,
        "{indent}    esac\n{indent}    ;;\n{indent}esac",
        indent = indent
    )
    .unwrap();
}
