//! The static description of a command line interface.
use crate::info::{CommandInfo, ValueHint};

/// Describes a command line interface (or one of its subcommands).
///
/// A `Cli` is meant to be declared as a `static` with the `const` builder
/// methods.  The type parameter `A` is a user defined (typically fieldless)
/// enum that identifies options, positional arguments and subcommands.  The
/// [`Parser`](crate::Parser) hands back those identifiers so the user code
/// can `match` on them exhaustively.
///
/// ```
/// use argument::{Cli, Opt, Pos};
///
/// #[derive(Copy, Clone)]
/// enum Arg {
///     Verbose,
///     Output,
///     Input,
/// }
///
/// static CLI: Cli<Arg> = Cli::new("demo")
///     .about("Demonstrates the spec")
///     .opts(&[
///         Opt::new(Arg::Verbose).short('v').long("verbose").help("Be verbose"),
///         Opt::new(Arg::Output).short('o').long("output").value("FILE").help("Output file"),
///     ])
///     .args(&[Pos::new(Arg::Input, "INPUT").help("The input file")]);
/// ```
pub struct Cli<A: 'static> {
    pub(crate) name: &'static str,
    pub(crate) version: Option<&'static str>,
    pub(crate) about: Option<&'static str>,
    pub(crate) long_about: Option<&'static str>,
    pub(crate) before_help: Option<&'static str>,
    pub(crate) usage: Option<&'static str>,
    pub(crate) after_help: Option<&'static str>,
    pub(crate) max_width: Option<usize>,
    pub(crate) opts: &'static [Opt<A>],
    pub(crate) args: &'static [Pos<A>],
    pub(crate) cmds: &'static [Cmd<A>],
}

impl<A: 'static> Cli<A> {
    /// Creates a new command line interface description.
    ///
    /// The name is used in the usage line and help output.  For subcommands
    /// this is also the name of the command on the command line.
    pub const fn new(name: &'static str) -> Cli<A> {
        Cli {
            name,
            version: None,
            about: None,
            long_about: None,
            before_help: None,
            usage: None,
            after_help: None,
            max_width: None,
            opts: &[],
            args: &[],
            cmds: &[],
        }
    }

    /// Sets the version.  This enables the `-V` / `--version` builtin.
    pub const fn version(mut self, version: &'static str) -> Cli<A> {
        self.version = Some(version);
        self
    }

    /// Sets the description shown at the top of the help page.
    ///
    /// For subcommands the first line is also used in the command listing of
    /// the parent.
    pub const fn about(mut self, about: &'static str) -> Cli<A> {
        self.about = Some(about);
        self
    }

    /// Sets the description shown at the top of the long help page.
    ///
    /// If not set, [`about`](Self::about) is used for the long help as well.
    pub const fn long_about(mut self, long_about: &'static str) -> Cli<A> {
        self.long_about = Some(long_about);
        self
    }

    /// Text that is shown before the description on the help page.
    pub const fn before_help(mut self, text: &'static str) -> Cli<A> {
        self.before_help = Some(text);
        self
    }

    /// Sets the maximum width for rendering help (defaults to `100`).
    pub const fn max_width(mut self, width: usize) -> Cli<A> {
        self.max_width = Some(width);
        self
    }

    /// Overrides the generated usage line.
    ///
    /// The program name is automatically prepended.
    pub const fn usage(mut self, usage: &'static str) -> Cli<A> {
        self.usage = Some(usage);
        self
    }

    /// Text that is shown at the end of the help page.
    pub const fn after_help(mut self, text: &'static str) -> Cli<A> {
        self.after_help = Some(text);
        self
    }

    /// Sets the options.
    ///
    /// Use [`Opt::section`] entries to group them into sections in the help.
    pub const fn opts(mut self, opts: &'static [Opt<A>]) -> Cli<A> {
        self.opts = opts;
        self
    }

    /// Sets the positional arguments.
    ///
    /// Positional arguments are handed out in order.  Only the last one can
    /// be marked as [`multiple`](Pos::multiple).
    pub const fn args(mut self, args: &'static [Pos<A>]) -> Cli<A> {
        self.args = args;
        self
    }

    /// Sets the subcommands.
    pub const fn commands(mut self, cmds: &'static [Cmd<A>]) -> Cli<A> {
        self.cmds = cmds;
        self
    }

    pub(crate) fn find_short(&self, c: char) -> Option<&Opt<A>> {
        self.opts
            .iter()
            .find(|o| o.id.is_some() && o.short == Some(c))
    }

    pub(crate) fn find_long(&self, name: &str) -> Option<&Opt<A>> {
        self.opts
            .iter()
            .find(|o| o.id.is_some() && o.long == Some(name))
    }

    pub(crate) fn builtin_short(&self, c: char) -> Option<Builtin> {
        if self.find_short(c).is_some() {
            None
        } else if c == 'h' {
            Some(Builtin::Help)
        } else if c == 'V' && self.version.is_some() {
            Some(Builtin::Version)
        } else {
            None
        }
    }

    pub(crate) fn builtin_long(&self, name: &str) -> Option<Builtin> {
        if self.find_long(name).is_some() {
            None
        } else if name == "help" {
            Some(Builtin::Help)
        } else if name == "version" && self.version.is_some() {
            Some(Builtin::Version)
        } else {
            None
        }
    }

    /// Checks if the raw argument would be handled as a known option.
    pub(crate) fn is_known_option(&self, arg: &str) -> bool {
        if let Some(long) = arg.strip_prefix("--") {
            let name = long.split('=').next().unwrap_or("");
            self.find_long(name).is_some() || self.builtin_long(name).is_some()
        } else if let Some(c) = arg.strip_prefix('-').and_then(|x| x.chars().next()) {
            self.find_short(c).is_some() || self.builtin_short(c).is_some()
        } else {
            false
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum Builtin {
    Help,
    Version,
}

/// Describes an option (`-o`, `--output`) or a section header.
pub struct Opt<A: 'static> {
    pub(crate) id: Option<A>,
    pub(crate) short: Option<char>,
    pub(crate) long: Option<&'static str>,
    pub(crate) value: Option<&'static str>,
    pub(crate) optional_value: bool,
    pub(crate) help: &'static str,
    pub(crate) long_help: Option<&'static str>,
    pub(crate) default_value: Option<&'static str>,
    pub(crate) values: Values,
    pub(crate) value_hint: ValueHint,
    pub(crate) hidden: bool,
}

impl<A: 'static> Opt<A> {
    /// Creates a new option with the given identifier.
    ///
    /// Without a call to [`value`](Self::value) the option is a flag.
    pub const fn new(id: A) -> Opt<A> {
        Opt {
            id: Some(id),
            short: None,
            long: None,
            value: None,
            optional_value: false,
            help: "",
            long_help: None,
            default_value: None,
            values: Values::None,
            value_hint: ValueHint::Unknown,
            hidden: false,
        }
    }

    /// Creates a section header.
    ///
    /// All options following this entry are listed in the help page under
    /// the given title.
    pub const fn section(title: &'static str) -> Opt<A> {
        Opt {
            id: None,
            short: None,
            long: None,
            value: None,
            optional_value: false,
            help: title,
            long_help: None,
            default_value: None,
            values: Values::None,
            value_hint: ValueHint::Unknown,
            hidden: false,
        }
    }

    /// Sets the short name (`-o`).
    pub const fn short(mut self, c: char) -> Opt<A> {
        self.short = Some(c);
        self
    }

    /// Sets the long name (`--output`).
    pub const fn long(mut self, name: &'static str) -> Opt<A> {
        self.long = Some(name);
        self
    }

    /// Declares that this option takes a value with the given display name.
    pub const fn value(mut self, name: &'static str) -> Opt<A> {
        self.value = Some(name);
        self.optional_value = false;
        self
    }

    /// Declares that this option takes an optional attached value.
    ///
    /// Such a value can only be passed as `--opt=value` or `-ovalue`.  Use
    /// [`Parser::optional_value`](crate::Parser::optional_value) to read it.
    pub const fn optional_value(mut self, name: &'static str) -> Opt<A> {
        self.value = Some(name);
        self.optional_value = true;
        self
    }

    /// Sets the help text.
    pub const fn help(mut self, text: &'static str) -> Opt<A> {
        self.help = text;
        self
    }

    /// Sets the long help text (shown in the long help page).
    pub const fn long_help(mut self, text: &'static str) -> Opt<A> {
        self.long_help = Some(text);
        self
    }

    /// Sets the default value for display purposes.
    ///
    /// This does not change parsing, it only shows `[default: ...]` in the
    /// help page.
    pub const fn default_value(mut self, value: &'static str) -> Opt<A> {
        self.default_value = Some(value);
        self
    }

    /// Restricts the value to the given possible values.
    ///
    /// The values are validated when the value is read, shown in the help
    /// page and used for shell completions.
    pub const fn possible_values(mut self, values: &'static [&'static str]) -> Opt<A> {
        self.values = Values::Plain(values);
        self
    }

    /// Like [`possible_values`](Self::possible_values) but with a description
    /// for each value.
    pub const fn possible_values_with_help(
        mut self,
        values: &'static [(&'static str, &'static str)],
    ) -> Opt<A> {
        self.values = Values::Described(values);
        self
    }

    /// Sets a hint for completing the value.
    pub const fn value_hint(mut self, hint: ValueHint) -> Opt<A> {
        self.value_hint = hint;
        self
    }

    /// Hides the option from the help page.
    pub const fn hidden(mut self) -> Opt<A> {
        self.hidden = true;
        self
    }

    pub(crate) fn takes_value(&self) -> bool {
        self.value.is_some()
    }
}

/// Describes a positional argument.
pub struct Pos<A: 'static> {
    pub(crate) id: A,
    pub(crate) name: &'static str,
    pub(crate) help: &'static str,
    pub(crate) long_help: Option<&'static str>,
    pub(crate) default_value: Option<&'static str>,
    pub(crate) values: Values,
    pub(crate) value_hint: ValueHint,
    pub(crate) required: bool,
    pub(crate) multiple: bool,
    pub(crate) allow_hyphen: bool,
}

impl<A: 'static> Pos<A> {
    /// Creates a new positional argument.  It's required by default.
    pub const fn new(id: A, name: &'static str) -> Pos<A> {
        Pos {
            id,
            name,
            help: "",
            long_help: None,
            default_value: None,
            values: Values::None,
            value_hint: ValueHint::Unknown,
            required: true,
            multiple: false,
            allow_hyphen: false,
        }
    }

    /// Sets the help text.
    pub const fn help(mut self, text: &'static str) -> Pos<A> {
        self.help = text;
        self
    }

    /// Sets the long help text (shown in the long help page).
    pub const fn long_help(mut self, text: &'static str) -> Pos<A> {
        self.long_help = Some(text);
        self
    }

    /// Sets the default value for display purposes.
    pub const fn default_value(mut self, value: &'static str) -> Pos<A> {
        self.default_value = Some(value);
        self
    }

    /// Restricts the value to the given possible values.
    pub const fn possible_values(mut self, values: &'static [&'static str]) -> Pos<A> {
        self.values = Values::Plain(values);
        self
    }

    /// Like [`possible_values`](Self::possible_values) but with a description
    /// for each value.
    pub const fn possible_values_with_help(
        mut self,
        values: &'static [(&'static str, &'static str)],
    ) -> Pos<A> {
        self.values = Values::Described(values);
        self
    }

    /// Sets a hint for completing the value.
    pub const fn value_hint(mut self, hint: ValueHint) -> Pos<A> {
        self.value_hint = hint;
        self
    }

    /// Marks the argument as optional.
    pub const fn optional(mut self) -> Pos<A> {
        self.required = false;
        self
    }

    /// Allows the argument to be given multiple times.
    ///
    /// This only makes sense for the last positional argument.
    pub const fn multiple(mut self) -> Pos<A> {
        self.multiple = true;
        self
    }

    /// Accepts values starting with `-` unless they are a known option.
    ///
    /// This is useful for things like negative numbers or `chmod -w`.
    pub const fn allow_hyphen(mut self) -> Pos<A> {
        self.allow_hyphen = true;
        self
    }

    pub(crate) fn display(&self) -> String {
        let mut rv = if self.required {
            format!("<{}>", self.name)
        } else {
            format!("[{}]", self.name)
        };
        if self.multiple {
            rv.push_str("...");
        }
        rv
    }
}

/// Possible values of an option or argument.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Values {
    None,
    Plain(&'static [&'static str]),
    Described(&'static [(&'static str, &'static str)]),
}

impl Values {
    pub(crate) fn is_empty(self) -> bool {
        match self {
            Values::None => true,
            Values::Plain(values) => values.is_empty(),
            Values::Described(values) => values.is_empty(),
        }
    }

    pub(crate) fn contains(self, value: &str) -> bool {
        match self {
            Values::None => false,
            Values::Plain(values) => values.contains(&value),
            Values::Described(values) => values.iter().any(|x| x.0 == value),
        }
    }

    pub(crate) fn names(self) -> Vec<&'static str> {
        match self {
            Values::None => Vec::new(),
            Values::Plain(values) => values.to_vec(),
            Values::Described(values) => values.iter().map(|x| x.0).collect(),
        }
    }
}

/// Describes a subcommand.
///
/// The subcommand points to another [`Cli`] (which typically has its own
/// identifier enum).  When the parser encounters the command name, the
/// command's identifier is returned and the parser can be handed over with
/// [`Parser::subcommand`](crate::Parser::subcommand).
pub struct Cmd<A: 'static> {
    pub(crate) id: A,
    pub(crate) info: &'static (dyn CommandInfo + Sync),
}

impl<A: 'static> Cmd<A> {
    /// Creates a new subcommand that is described by the given [`Cli`].
    pub const fn new<B: Sync + 'static>(id: A, cli: &'static Cli<B>) -> Cmd<A> {
        Cmd { id, info: cli }
    }
}
