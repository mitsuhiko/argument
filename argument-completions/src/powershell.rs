//! PowerShell completions.
use std::fmt::Write;

use crate::{first_line, Node};

/// Quotes a string for single quotes.
///
/// PowerShell also treats typographic quotes as quote characters.
fn quote(s: &str) -> String {
    let mut rv = String::from("'");
    for c in s.chars() {
        match c {
            '\'' | '\u{2018}' | '\u{2019}' | '\u{201a}' | '\u{201b}' => {
                rv.push(c);
                rv.push(c);
            }
            '\n' => rv.push(' '),
            c => rv.push(c),
        }
    }
    rv.push('\'');
    rv
}

fn result(text: &str, list_text: &str, kind: &str, tooltip: &str) -> String {
    // the tooltip must not be empty
    let tooltip = if tooltip.is_empty() { text } else { tooltip };
    format!(
        "[CompletionResult]::new({}, {}, [CompletionResultType]::{}, {})",
        quote(text),
        quote(list_text),
        kind,
        quote(tooltip)
    )
}

pub(crate) fn generate(root: &Node) -> String {
    let bin = root.bin_name();
    let nodes = root.walk();
    let mut out = String::new();
    writeln!(
        out,
        r#"using namespace System.Management.Automation
using namespace System.Management.Automation.Language

Register-ArgumentCompleter -Native -CommandName {bin} -ScriptBlock {{
    param($wordToComplete, $commandAst, $cursorPosition)

    $commands = @{{"#,
        bin = quote(bin)
    )
    .unwrap();
    for node in &nodes {
        writeln!(out, "        {} = $true", quote(&node.path.join(";"))).unwrap();
    }
    writeln!(
        out,
        r#"    }}

    $commandElements = $commandAst.CommandElements
    $command = {bin}
    for ($i = 1; $i -lt $commandElements.Count; $i++) {{
        $element = $commandElements[$i]
        if ($element -isnot [StringConstantExpressionAst] -or
            $element.StringConstantType -ne [StringConstantType]::BareWord -or
            $element.Value -eq $wordToComplete) {{
            break
        }}
        if ($element.Value.StartsWith('-')) {{
            continue
        }}
        $next = $command + ';' + $element.Value
        if ($commands.ContainsKey($next)) {{
            $command = $next
        }}
    }}

    $completions = @(switch ($command) {{"#,
        bin = quote(bin)
    )
    .unwrap();

    for node in &nodes {
        writeln!(out, "        {} {{", quote(&node.path.join(";"))).unwrap();
        for opt in &node.options {
            let help = first_line(opt.help());
            if let Some(short) = opt.short() {
                let text = format!("-{}", short);
                // list item texts are case insensitive and must be unique
                let list_text = if short.is_uppercase() {
                    format!("{} ", text)
                } else {
                    text.clone()
                };
                writeln!(
                    out,
                    "            {}",
                    result(&text, &list_text, "ParameterName", help)
                )
                .unwrap();
            }
            if let Some(long) = opt.long() {
                let text = format!("--{}", long);
                writeln!(
                    out,
                    "            {}",
                    result(&text, &text, "ParameterName", help)
                )
                .unwrap();
            }
        }
        for child in &node.children {
            let about = child.about.map(first_line).unwrap_or("");
            writeln!(
                out,
                "            {}",
                result(&child.name, &child.name, "ParameterValue", about)
            )
            .unwrap();
        }
        out.push_str("            break\n        }\n");
    }

    out.push_str(
        r#"    })

    $completions.Where{ $_.CompletionText -like "$wordToComplete*" } |
        Sort-Object -Property ListItemText
}
"#,
    );
    out
}
