//! Shell completion generation for [`argument`] based command line interfaces.
//!
//! This crate generates static completion scripts from a [`Cli`](argument::Cli)
//! description.  The following shells are supported: bash, elvish, fish,
//! nushell, PowerShell and zsh.
//!
//! ```
//! use argument::{Cli, Opt, ValueHint};
//! use argument_completions::Shell;
//!
//! #[derive(Copy, Clone)]
//! enum Arg {
//!     Output,
//! }
//!
//! static CLI: Cli<Arg> = Cli::new("demo").opts(&[Opt::new(Arg::Output)
//!     .short('o')
//!     .long("output")
//!     .value("FILE")
//!     .value_hint(ValueHint::FilePath)
//!     .help("Output file")]);
//!
//! let script = Shell::Bash.generate(&CLI, "demo");
//! assert!(script.contains("complete -F _demo"));
//! ```
//!
//! To offer completions at runtime (eg: via `--generate-completion SHELL`),
//! use [`Shell::NAMES`] as possible values and parse the shell with
//! [`str::parse`].
use std::fmt;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use argument::{ArgumentInfo, CommandInfo, OptionInfo, PossibleValue, ValueHint};

mod bash;
mod elvish;
mod fish;
mod nushell;
mod powershell;
mod zsh;

/// The supported shells.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Shell {
    /// GNU bash (3.2 and later)
    Bash,
    /// Elvish
    Elvish,
    /// fish
    Fish,
    /// Nushell
    Nushell,
    /// PowerShell
    PowerShell,
    /// Z shell
    Zsh,
}

impl Shell {
    /// All supported shells.
    pub const ALL: &'static [Shell] = &[
        Shell::Bash,
        Shell::Elvish,
        Shell::Fish,
        Shell::Nushell,
        Shell::PowerShell,
        Shell::Zsh,
    ];

    /// The names of all supported shells.
    ///
    /// This can be used with [`Opt::possible_values`](argument::Opt::possible_values).
    pub const NAMES: &'static [&'static str] =
        &["bash", "elvish", "fish", "nushell", "powershell", "zsh"];

    /// The name of the shell.
    pub fn name(self) -> &'static str {
        match self {
            Shell::Bash => "bash",
            Shell::Elvish => "elvish",
            Shell::Fish => "fish",
            Shell::Nushell => "nushell",
            Shell::PowerShell => "powershell",
            Shell::Zsh => "zsh",
        }
    }

    /// The conventional file name of the completion script.
    pub fn file_name(self, bin_name: &str) -> String {
        match self {
            Shell::Bash => format!("{}.bash", bin_name),
            Shell::Elvish => format!("{}.elv", bin_name),
            Shell::Fish => format!("{}.fish", bin_name),
            Shell::Nushell => format!("{}.nu", bin_name),
            Shell::PowerShell => format!("_{}.ps1", bin_name),
            Shell::Zsh => format!("_{}", bin_name),
        }
    }

    /// Generates the completion script for the given command.
    ///
    /// `bin_name` is the name of the executable that is completed.
    pub fn generate(self, cmd: &dyn CommandInfo, bin_name: &str) -> String {
        let root = Node::new(cmd, bin_name, Vec::new());
        match self {
            Shell::Bash => bash::generate(&root),
            Shell::Elvish => elvish::generate(&root),
            Shell::Fish => fish::generate(&root),
            Shell::Nushell => nushell::generate(&root),
            Shell::PowerShell => powershell::generate(&root),
            Shell::Zsh => zsh::generate(&root),
        }
    }

    /// Generates the completion script into the given directory.
    ///
    /// The file is named after [`file_name`](Self::file_name) and the path
    /// to the written file is returned.
    pub fn generate_to(
        self,
        cmd: &dyn CommandInfo,
        bin_name: &str,
        dir: &Path,
    ) -> io::Result<PathBuf> {
        let path = dir.join(self.file_name(bin_name));
        fs::write(&path, self.generate(cmd, bin_name))?;
        Ok(path)
    }
}

impl fmt::Display for Shell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// Error returned when parsing an unknown shell name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnknownShellError(String);

impl fmt::Display for UnknownShellError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "unknown shell '{}' (supported: {})",
            self.0,
            Shell::NAMES.join(", ")
        )
    }
}

impl std::error::Error for UnknownShellError {}

impl FromStr for Shell {
    type Err = UnknownShellError;

    fn from_str(s: &str) -> Result<Shell, UnknownShellError> {
        Shell::ALL
            .iter()
            .copied()
            .find(|x| x.name() == s)
            .ok_or_else(|| UnknownShellError(s.to_string()))
    }
}

/// A normalized command tree that the generators work with.
pub(crate) struct Node {
    /// The name of the command (the binary name for the root).
    pub name: String,
    /// The names of the commands from the binary to this command.
    pub path: Vec<String>,
    pub about: Option<&'static str>,
    pub options: Vec<OptionInfo>,
    pub arguments: Vec<ArgumentInfo>,
    pub children: Vec<Node>,
}

impl Node {
    fn new(cmd: &dyn CommandInfo, name: &str, parent_path: Vec<String>) -> Node {
        let mut path = parent_path;
        path.push(name.to_string());
        Node {
            name: name.to_string(),
            about: cmd.about(),
            options: cmd
                .options()
                .into_iter()
                .filter(|x| !x.is_hidden())
                .collect(),
            arguments: cmd.arguments(),
            children: cmd
                .commands()
                .into_iter()
                .map(|sub| Node::new(sub, sub.name(), path.clone()))
                .collect(),
            path,
        }
    }

    /// The binary name.
    pub fn bin_name(&self) -> &str {
        &self.path[0]
    }

    /// An identifier for this command that is safe to use in function names.
    pub fn ident(&self) -> String {
        self.path
            .iter()
            .map(|x| ident(x))
            .collect::<Vec<_>>()
            .join("__")
    }

    /// Iterates over this node and all descendants (depth first).
    pub fn walk(&self) -> Vec<&Node> {
        let mut rv = vec![self];
        for child in &self.children {
            rv.extend(child.walk());
        }
        rv
    }
}

/// Converts a name into something that is safe for function names.
pub(crate) fn ident(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

/// Returns the first line of a help text.
pub(crate) fn first_line(s: &str) -> &str {
    s.lines().next().unwrap_or("").trim()
}

/// The completion of a value.
pub(crate) enum ValueCompletion {
    /// One of the given values.
    Values(Vec<PossibleValue>),
    /// A hint.
    Hint(ValueHint),
}

impl ValueCompletion {
    pub fn for_option(opt: &OptionInfo) -> ValueCompletion {
        let values = opt.possible_values();
        if values.is_empty() {
            ValueCompletion::Hint(opt.value_hint())
        } else {
            ValueCompletion::Values(values)
        }
    }

    pub fn for_argument(arg: &ArgumentInfo) -> ValueCompletion {
        let values = arg.possible_values();
        if values.is_empty() {
            ValueCompletion::Hint(arg.value_hint())
        } else {
            ValueCompletion::Values(values)
        }
    }

    /// Returns `true` if the shell should fall back to completing files.
    pub fn completes_files(&self) -> bool {
        matches!(
            self,
            ValueCompletion::Hint(
                ValueHint::Unknown
                    | ValueHint::AnyPath
                    | ValueHint::FilePath
                    | ValueHint::ExecutablePath
            )
        )
    }
}
