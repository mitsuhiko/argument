//! Elvish completions.
use std::fmt::Write;

use crate::{first_line, Node};

/// Quotes a string for single quotes.
fn quote(s: &str) -> String {
    format!("'{}'", s.replace('\'', "''").replace('\n', " "))
}

pub(crate) fn generate(root: &Node) -> String {
    let bin = root.bin_name();
    let mut out = String::new();
    writeln!(
        out,
        r#"use builtin;
use str;

set edit:completion:arg-completer[{bin}] = {{|@words|
    fn spaces {{|n|
        builtin:repeat $n ' ' | str:join ''
    }}
    fn cand {{|text desc|
        edit:complex-candidate $text &display=$text' '(spaces (- 14 (wcswidth $text)))$desc
    }}
    var completions = ["#,
        bin = quote(bin)
    )
    .unwrap();

    for node in root.walk() {
        writeln!(out, "        &{}= {{", quote(&node.path.join(";"))).unwrap();
        for opt in &node.options {
            let help = match first_line(opt.help()) {
                "" => opt.long().unwrap_or(""),
                help => help,
            };
            if let Some(short) = opt.short() {
                writeln!(
                    out,
                    "            cand {} {}",
                    quote(&format!("-{}", short)),
                    quote(help)
                )
                .unwrap();
            }
            if let Some(long) = opt.long() {
                writeln!(
                    out,
                    "            cand {} {}",
                    quote(&format!("--{}", long)),
                    quote(help)
                )
                .unwrap();
            }
        }
        for child in &node.children {
            let about = child.about.map(first_line).unwrap_or(&child.name);
            writeln!(
                out,
                "            cand {} {}",
                quote(&child.name),
                quote(about)
            )
            .unwrap();
        }
        out.push_str("        }\n");
    }

    writeln!(
        out,
        r#"    ]
    var command = {bin}
    for word $words[1..-1] {{
        if (str:has-prefix $word '-') {{
            continue
        }}
        if (has-key $completions $command';'$word) {{
            set command = $command';'$word
        }}
    }}
    $completions[$command]
}}"#,
        bin = quote(bin)
    )
    .unwrap();
    out
}
