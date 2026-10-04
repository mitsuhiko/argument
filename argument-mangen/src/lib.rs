//! Man page generation for [`argument`] based command line interfaces.
//!
//! This renders a man page in roff format from a [`Cli`](argument::Cli)
//! description.  Subcommands get their own pages (eg: `cargo-install.1`).
//!
//! ```
//! use argument::{Cli, Opt};
//! use argument_mangen::Man;
//!
//! #[derive(Copy, Clone)]
//! enum Arg {
//!     Verbose,
//! }
//!
//! static CLI: Cli<Arg> = Cli::new("demo")
//!     .version("1.0")
//!     .about("A demo tool")
//!     .opts(&[Opt::new(Arg::Verbose).short('v').long("verbose").help("Be verbose")]);
//!
//! let page = Man::new(&CLI).render();
//! assert!(page.contains(".SH NAME\ndemo \\- A demo tool"));
//! ```
use std::fmt::Write;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use argument::{ArgumentInfo, CommandInfo, OptionInfo, PossibleValue};

/// A man page for a command.
pub struct Man<'a> {
    cmd: &'a dyn CommandInfo,
    name: String,
    invocation: String,
    section: String,
    date: Option<String>,
    source: Option<String>,
    manual: Option<String>,
}

impl<'a> Man<'a> {
    /// Creates a man page for the given command.
    pub fn new(cmd: &'a dyn CommandInfo) -> Man<'a> {
        Man {
            cmd,
            name: cmd.name().to_string(),
            invocation: cmd.name().to_string(),
            section: "1".into(),
            date: None,
            source: None,
            manual: None,
        }
    }

    /// Overrides the name of the command (defaults to the name of the [`Cli`](argument::Cli)).
    pub fn name(mut self, name: impl Into<String>) -> Man<'a> {
        self.name = name.into();
        self.invocation = self.name.clone();
        self
    }

    /// Sets the manual section (defaults to `1`).
    pub fn section(mut self, section: impl Into<String>) -> Man<'a> {
        self.section = section.into();
        self
    }

    /// Sets the date shown in the footer.
    pub fn date(mut self, date: impl Into<String>) -> Man<'a> {
        self.date = Some(date.into());
        self
    }

    /// Sets the source shown in the footer (defaults to name and version).
    pub fn source(mut self, source: impl Into<String>) -> Man<'a> {
        self.source = Some(source.into());
        self
    }

    /// Sets the title of the manual shown in the header.
    pub fn manual(mut self, manual: impl Into<String>) -> Man<'a> {
        self.manual = Some(manual.into());
        self
    }

    /// The file name of the man page (eg: `demo.1`).
    pub fn file_name(&self) -> String {
        format!("{}.{}", self.name, self.section)
    }

    /// Renders the man page.
    pub fn render(&self) -> String {
        let mut out = Roff::default();
        let source = match (&self.source, self.cmd.version()) {
            (Some(source), _) => source.clone(),
            (None, Some(version)) => format!("{} {}", self.name, version),
            (None, None) => self.name.clone(),
        };
        out.control_raw(".ie \\n(.g .ds Aq \\(aq\n.el .ds Aq '");
        out.control(
            "TH",
            &[
                &self.name,
                &self.section,
                self.date.as_deref().unwrap_or(""),
                &source,
                self.manual.as_deref().unwrap_or(""),
            ],
        );

        out.control("SH", &["NAME"]);
        match self.cmd.about() {
            Some(about) => out.text(&format!("{} - {}", self.name, first_line(about))),
            None => out.text(&self.name),
        }

        out.control("SH", &["SYNOPSIS"]);
        out.line(&self.synopsis());

        let description = self.cmd.long_about().or(self.cmd.about());
        if self.cmd.before_help().is_some() || description.is_some() {
            out.control("SH", &["DESCRIPTION"]);
            let mut first = true;
            for text in [self.cmd.before_help(), description].into_iter().flatten() {
                if !first {
                    out.control("PP", &[]);
                }
                first = false;
                out.paragraphs(text, "PP");
            }
        }

        let options = self
            .cmd
            .options()
            .into_iter()
            .filter(|x| !x.is_hidden())
            .collect::<Vec<_>>();
        let mut sections: Vec<(Option<&str>, Vec<&OptionInfo>)> = Vec::new();
        for opt in &options {
            match sections.iter_mut().find(|x| x.0 == opt.section()) {
                Some(section) => section.1.push(opt),
                None => sections.push((opt.section(), vec![opt])),
            }
        }
        for (title, opts) in sections {
            let title = title.unwrap_or("Options").to_uppercase();
            out.control("SH", &[&title]);
            for opt in opts {
                out.control("TP", &[]);
                out.line(&option_term(opt));
                item_body(
                    &mut out,
                    opt.long_help().unwrap_or(opt.help()),
                    opt.default_value(),
                    &opt.possible_values(),
                );
            }
        }

        let args = self.cmd.arguments();
        if !args.is_empty() {
            out.control("SH", &["ARGUMENTS"]);
            for arg in &args {
                out.control("TP", &[]);
                out.line(&argument_term(arg));
                item_body(
                    &mut out,
                    arg.long_help().unwrap_or(arg.help()),
                    arg.default_value(),
                    &arg.possible_values(),
                );
            }
        }

        let commands = self.cmd.commands();
        if !commands.is_empty() {
            out.control("SH", &["SUBCOMMANDS"]);
            for cmd in &commands {
                out.control("TP", &[]);
                out.line(&format!(
                    "{}({})",
                    escape(&format!("{}-{}", self.name, cmd.name())),
                    escape(&self.section)
                ));
                if let Some(about) = cmd.about() {
                    out.paragraphs(about, "sp");
                }
            }
        }

        if let Some(after_help) = self.cmd.after_help() {
            out.control("SH", &["EXTRA"]);
            out.paragraphs(after_help, "PP");
        }

        if let Some(version) = self.cmd.version() {
            out.control("SH", &["VERSION"]);
            out.text(&format!("v{}", version));
        }

        out.0
    }

    /// Writes the man page and the pages of all subcommands into a directory.
    ///
    /// Returns the paths of all written files.
    pub fn generate_to(&self, dir: &Path) -> io::Result<Vec<PathBuf>> {
        let mut rv = Vec::new();
        let path = dir.join(self.file_name());
        fs::write(&path, self.render())?;
        rv.push(path);
        for cmd in self.cmd.commands() {
            let mut sub = Man::new(cmd)
                .name(format!("{}-{}", self.name, cmd.name()))
                .section(self.section.clone());
            sub.invocation = format!("{} {}", self.invocation, cmd.name());
            sub.date = self.date.clone();
            sub.source = self.source.clone();
            sub.manual = self.manual.clone();
            if sub.source.is_none() {
                if let Some(version) = self.cmd.version() {
                    sub.source = Some(format!("{} {}", self.name, version));
                }
            }
            rv.extend(sub.generate_to(dir)?);
        }
        Ok(rv)
    }

    fn synopsis(&self) -> String {
        let mut rv = bold(&self.invocation);
        if let Some(usage) = self.cmd.usage_override() {
            rv.push(' ');
            rv.push_str(&escape(usage));
            return rv;
        }
        for opt in self.cmd.options().iter().filter(|x| !x.is_hidden()) {
            let names = match (opt.short(), opt.long()) {
                (Some(s), Some(l)) => {
                    format!("{}|{}", bold(&format!("-{}", s)), bold(&format!("--{}", l)))
                }
                (Some(s), None) => bold(&format!("-{}", s)),
                (None, Some(l)) => bold(&format!("--{}", l)),
                (None, None) => continue,
            };
            write!(rv, " [{}]", names).unwrap();
        }
        for arg in self.cmd.arguments() {
            let name = italic(arg.name());
            let mut piece = if arg.is_required() {
                format!("<{}>", name)
            } else {
                format!("[{}]", name)
            };
            if arg.is_multiple() {
                piece.push_str("...");
            }
            rv.push(' ');
            rv.push_str(&piece);
        }
        if !self.cmd.commands().is_empty() {
            write!(rv, " [{}]", italic("COMMAND")).unwrap();
        }
        rv
    }
}

fn option_term(opt: &OptionInfo) -> String {
    let mut rv = match (opt.short(), opt.long()) {
        (Some(s), Some(l)) => format!(
            "{}, {}",
            bold(&format!("-{}", s)),
            bold(&format!("--{}", l))
        ),
        (Some(s), None) => bold(&format!("-{}", s)),
        (None, Some(l)) => bold(&format!("--{}", l)),
        (None, None) => String::new(),
    };
    if let Some(value) = opt.value_name() {
        if opt.has_optional_value() {
            write!(rv, "[={}]", italic(value)).unwrap();
        } else {
            write!(rv, "={}", italic(value)).unwrap();
        }
    }
    rv
}

fn argument_term(arg: &ArgumentInfo) -> String {
    let name = italic(arg.name());
    let mut rv = if arg.is_required() {
        format!("<{}>", name)
    } else {
        format!("[{}]", name)
    };
    if arg.is_multiple() {
        rv.push_str("...");
    }
    rv
}

fn item_body(out: &mut Roff, help: &str, default: Option<&str>, values: &[PossibleValue]) {
    out.paragraphs(help, "sp");
    if let Some(default) = default {
        out.control("sp", &[]);
        out.text(&format!("[default: {}]", default));
    }
    if values.iter().any(|x| x.help().is_some()) {
        out.control("sp", &[]);
        out.text("Possible values:");
        out.control("RS", &[]);
        out.control("PD", &["0"]);
        for value in values {
            out.control_raw(".IP \\(bu 2");
            match value.help() {
                Some(help) => out.text(&format!("{}: {}", value.name(), help)),
                None => out.text(value.name()),
            }
        }
        out.control("PD", &[]);
        out.control("RE", &[]);
    } else if !values.is_empty() {
        let names = values.iter().map(|x| x.name()).collect::<Vec<_>>();
        out.control("sp", &[]);
        out.text(&format!("[possible values: {}]", names.join(", ")));
    }
}

fn first_line(s: &str) -> &str {
    s.lines().next().unwrap_or("").trim()
}

/// Escapes text for roff.
fn escape(s: &str) -> String {
    let mut rv = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => rv.push_str("\\e"),
            '-' => rv.push_str("\\-"),
            '\'' => rv.push_str("\\*(Aq"),
            c => rv.push(c),
        }
    }
    rv
}

fn bold(s: &str) -> String {
    format!("\\fB{}\\fR", escape(s))
}

fn italic(s: &str) -> String {
    format!("\\fI{}\\fR", escape(s))
}

/// A minimal roff writer.
#[derive(Default)]
struct Roff(String);

impl Roff {
    fn control_raw(&mut self, s: &str) {
        self.0.push_str(s);
        self.0.push('\n');
    }

    fn control(&mut self, name: &str, args: &[&str]) {
        self.0.push('.');
        self.0.push_str(name);
        for arg in args {
            self.0.push(' ');
            if arg.is_empty() || arg.contains(' ') {
                write!(self.0, "\"{}\"", escape(arg).replace('"', "\\(dq")).unwrap();
            } else {
                self.0.push_str(&escape(arg));
            }
        }
        self.0.push('\n');
    }

    /// Writes an already escaped line.
    fn line(&mut self, s: &str) {
        if s.starts_with('.') || s.starts_with('\'') {
            self.0.push_str("\\&");
        }
        self.0.push_str(s);
        self.0.push('\n');
    }

    /// Writes a line of text.
    fn text(&mut self, s: &str) {
        self.line(&escape(s));
    }

    /// Writes text with paragraphs.  Line breaks are preserved and empty
    /// lines are separated with the given request (eg: `PP` or `sp`).
    fn paragraphs(&mut self, text: &str, separator: &str) {
        let lines = text.trim_end().lines().collect::<Vec<_>>();
        for (idx, line) in lines.iter().enumerate() {
            if line.trim().is_empty() {
                self.control(separator, &[]);
                continue;
            }
            self.text(line);
            if lines.get(idx + 1).is_some_and(|x| !x.trim().is_empty()) {
                self.control("br", &[]);
            }
        }
    }
}
