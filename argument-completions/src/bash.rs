//! Bash completions (compatible with bash 3.2).
use std::fmt::Write;

use argument::ValueHint;

use crate::{ident, Node, ValueCompletion};

/// Quotes a string for use in single quotes.
fn quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', "'\\''"))
}

fn words<'a, I: IntoIterator<Item = &'a str>>(iter: I) -> String {
    quote(&iter.into_iter().collect::<Vec<_>>().join(" "))
}

/// Renders the statements that complete a value into `COMPREPLY`.
fn value_action(completion: &ValueCompletion) -> String {
    match completion {
        ValueCompletion::Values(values) => format!(
            "COMPREPLY+=($(compgen -W {} -- \"$cur\"))",
            words(values.iter().map(|x| x.name()))
        ),
        ValueCompletion::Hint(ValueHint::DirPath) => {
            "while IFS= read -r i; do COMPREPLY+=(\"$i\"); done < <(compgen -d -- \"$cur\")"
                .to_string()
        }
        ValueCompletion::Hint(ValueHint::CommandName) => {
            "COMPREPLY+=($(compgen -c -- \"$cur\"))".to_string()
        }
        ValueCompletion::Hint(ValueHint::Username) => {
            "COMPREPLY+=($(compgen -u -- \"$cur\"))".to_string()
        }
        ValueCompletion::Hint(ValueHint::Hostname) => {
            "COMPREPLY+=($(compgen -A hostname -- \"$cur\"))".to_string()
        }
        // an empty reply falls back to the default (file name) completion
        ValueCompletion::Hint(_) => ":".to_string(),
    }
}

pub(crate) fn generate(root: &Node) -> String {
    let fn_name = format!("_{}", ident(root.bin_name()));
    let nodes = root.walk();
    let mut out = String::new();

    writeln!(out, "{}() {{", fn_name).unwrap();
    out.push_str(
        r#"    local i w cur cmd value_for opts_done pos prefix
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    value_for=""
    opts_done=0
    pos=0
    prefix=""
"#,
    );
    writeln!(out, "    cmd={}", quote(&root.ident())).unwrap();

    // walk the words before the cursor to find the current command, the
    // positional index and if the cursor is at the value of an option.
    out.push_str(
        r#"
    for (( i=1; i<COMP_CWORD; i++ )); do
        w="${COMP_WORDS[i]}"
        if [[ -n "$value_for" ]]; then
            [[ "$w" == "=" ]] && continue
            value_for=""
            continue
        fi
        if (( ! opts_done )); then
            if [[ "$w" == "--" ]]; then
                opts_done=1
                continue
            fi
            if [[ "$w" == -?* ]]; then
                case "$cmd:$w" in
"#,
    );
    for node in &nodes {
        let patterns = value_patterns(node);
        if !patterns.is_empty() {
            writeln!(out, "                    {}) value_for=\"$w\" ;;", patterns).unwrap();
        }
    }
    out.push_str(
        r#"                    *) ;;
                esac
                continue
            fi
        fi
        case "$cmd:$w" in
"#,
    );
    for node in &nodes {
        for child in &node.children {
            writeln!(
                out,
                "            {}) cmd={}; pos=0; continue ;;",
                quote(&format!("{}:{}", node.ident(), child.name)),
                quote(&child.ident())
            )
            .unwrap();
        }
    }
    out.push_str(
        r#"            *) ;;
        esac
        pos=$((pos + 1))
    done

    if [[ -z "$value_for" && $opts_done -eq 0 && "$cur" == --*=* ]]; then
        value_for="${cur%%=*}"
        prefix="${value_for}="
        cur="${cur#*=}"
    elif [[ -n "$value_for" && "$cur" == "=" ]]; then
        cur=""
    fi

    if [[ -n "$value_for" ]]; then
        case "$cmd:$value_for" in
"#,
    );
    for node in &nodes {
        for opt in node.options.iter().filter(|x| x.takes_value()) {
            writeln!(
                out,
                "            {}) {} ;;",
                option_patterns(node, opt),
                value_action(&ValueCompletion::for_option(opt))
            )
            .unwrap();
        }
    }
    out.push_str(
        r#"            *) ;;
        esac
        if [[ -n "$prefix" ]]; then
            COMPREPLY=("${COMPREPLY[@]/#/$prefix}")
        fi
        return 0
    fi

    if (( ! opts_done )) && [[ "$cur" == -* ]]; then
        case "$cmd" in
"#,
    );
    for node in &nodes {
        let mut names = Vec::new();
        for opt in &node.options {
            if let Some(short) = opt.short() {
                names.push(format!("-{}", short));
            }
            if let Some(long) = opt.long() {
                names.push(format!("--{}", long));
            }
        }
        writeln!(
            out,
            "            {}) COMPREPLY=($(compgen -W {} -- \"$cur\")) ;;",
            quote(&node.ident()),
            words(names.iter().map(|x| x.as_str()))
        )
        .unwrap();
    }
    out.push_str(
        r#"        esac
        return 0
    fi

    case "$cmd" in
"#,
    );
    for node in &nodes {
        writeln!(out, "        {})", quote(&node.ident())).unwrap();
        if !node.children.is_empty() {
            writeln!(
                out,
                "            COMPREPLY=($(compgen -W {} -- \"$cur\"))",
                words(node.children.iter().map(|x| x.name.as_str()))
            )
            .unwrap();
        }
        if !node.arguments.is_empty() {
            out.push_str("            case \"$pos\" in\n");
            for (idx, arg) in node.arguments.iter().enumerate() {
                let action = value_action(&ValueCompletion::for_argument(arg));
                if arg.is_multiple() && idx + 1 == node.arguments.len() {
                    writeln!(out, "                *) {} ;;", action).unwrap();
                } else {
                    writeln!(out, "                {}) {} ;;", idx, action).unwrap();
                }
            }
            out.push_str("            esac\n");
        }
        out.push_str("            ;;\n");
    }
    out.push_str("    esac\n    return 0\n}\n\n");

    let bin = quote(root.bin_name());
    writeln!(
        out,
        r#"if [[ "${{BASH_VERSINFO[0]}}" -eq 4 && "${{BASH_VERSINFO[1]}}" -ge 4 || "${{BASH_VERSINFO[0]}}" -gt 4 ]]; then
    complete -F {fn_name} -o nosort -o bashdefault -o default {bin}
else
    complete -F {fn_name} -o bashdefault -o default {bin}
fi"#,
        fn_name = fn_name,
        bin = bin
    )
    .unwrap();
    out
}

/// Case patterns for all options of a node that take a separate value.
fn value_patterns(node: &Node) -> String {
    node.options
        .iter()
        .filter(|x| x.takes_value() && !x.has_optional_value())
        .map(|opt| option_patterns(node, opt))
        .collect::<Vec<_>>()
        .join("|")
}

fn option_patterns(node: &Node, opt: &argument::OptionInfo) -> String {
    let mut rv = Vec::new();
    if let Some(short) = opt.short() {
        rv.push(quote(&format!("{}:-{}", node.ident(), short)));
    }
    if let Some(long) = opt.long() {
        rv.push(quote(&format!("{}:--{}", node.ident(), long)));
    }
    rv.join("|")
}
