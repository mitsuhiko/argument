//! Read-only, type erased access to a command line interface description.
//!
//! This is used by the help renderer and by external generators such as
//! `argument-completions` and `argument-mangen`.  The identifier type of a
//! [`Cli`] is erased so that subcommands (which typically use different
//! identifier enums) can be walked uniformly.

use crate::spec::{Builtin, Cli, Opt, Pos, Values};

/// Describes how the value of an option or argument should be completed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
#[non_exhaustive]
pub enum ValueHint {
    /// No hint.  Shells typically fall back to completing file names.
    #[default]
    Unknown,
    /// Free form text that cannot be completed.
    Other,
    /// Any existing path.
    AnyPath,
    /// A path to a file.
    FilePath,
    /// A path to a directory.
    DirPath,
    /// A path to an executable.
    ExecutablePath,
    /// The name of a command (as found on the `PATH`).
    CommandName,
    /// The name of a user.
    Username,
    /// A host name.
    Hostname,
    /// A URL.
    Url,
    /// An email address.
    EmailAddress,
}

/// A possible value of an option or argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PossibleValue {
    pub(crate) name: &'static str,
    pub(crate) help: Option<&'static str>,
}

impl PossibleValue {
    /// The value.
    pub fn name(&self) -> &'static str {
        self.name
    }

    /// An optional description of the value.
    pub fn help(&self) -> Option<&'static str> {
        self.help
    }
}

impl Values {
    pub(crate) fn to_vec(self) -> Vec<PossibleValue> {
        match self {
            Values::None => Vec::new(),
            Values::Plain(values) => values
                .iter()
                .map(|&name| PossibleValue { name, help: None })
                .collect(),
            Values::Described(values) => values
                .iter()
                .map(|&(name, help)| PossibleValue {
                    name,
                    help: Some(help),
                })
                .collect(),
        }
    }
}

/// Information about an option.
#[derive(Debug, Clone, Copy)]
pub struct OptionInfo {
    pub(crate) short: Option<char>,
    pub(crate) long: Option<&'static str>,
    pub(crate) value_name: Option<&'static str>,
    pub(crate) optional_value: bool,
    pub(crate) help: &'static str,
    pub(crate) long_help: Option<&'static str>,
    pub(crate) default_value: Option<&'static str>,
    pub(crate) values: Values,
    pub(crate) value_hint: ValueHint,
    pub(crate) section: Option<&'static str>,
    pub(crate) hidden: bool,
}

impl OptionInfo {
    fn from_opt<A>(opt: &Opt<A>, section: Option<&'static str>) -> OptionInfo {
        OptionInfo {
            short: opt.short,
            long: opt.long,
            value_name: opt.value,
            optional_value: opt.optional_value,
            help: opt.help,
            long_help: opt.long_help,
            default_value: opt.default_value,
            values: opt.values,
            value_hint: opt.value_hint,
            section,
            hidden: opt.hidden,
        }
    }

    fn builtin(short: Option<char>, long: Option<&'static str>, help: &'static str) -> OptionInfo {
        OptionInfo {
            short,
            long,
            value_name: None,
            optional_value: false,
            help,
            long_help: None,
            default_value: None,
            values: Values::None,
            value_hint: ValueHint::Unknown,
            section: None,
            hidden: false,
        }
    }

    /// The short name (`-o`) without the dash.
    pub fn short(&self) -> Option<char> {
        self.short
    }

    /// The long name (`--output`) without the dashes.
    pub fn long(&self) -> Option<&'static str> {
        self.long
    }

    /// The display name of the value if the option takes one.
    pub fn value_name(&self) -> Option<&'static str> {
        self.value_name
    }

    /// Returns `true` if the option takes a value (required or optional).
    pub fn takes_value(&self) -> bool {
        self.value_name.is_some()
    }

    /// Returns `true` if the value is optional and has to be attached
    /// (`--opt=value` or `-ovalue`).
    pub fn has_optional_value(&self) -> bool {
        self.optional_value
    }

    /// The short help text.
    pub fn help(&self) -> &'static str {
        self.help
    }

    /// The long help text if one was set.
    pub fn long_help(&self) -> Option<&'static str> {
        self.long_help
    }

    /// The default value for display purposes.
    pub fn default_value(&self) -> Option<&'static str> {
        self.default_value
    }

    /// The possible values.
    pub fn possible_values(&self) -> Vec<PossibleValue> {
        self.values.to_vec()
    }

    /// The value hint.
    pub fn value_hint(&self) -> ValueHint {
        self.value_hint
    }

    /// The title of the help section (`None` for the default section).
    pub fn section(&self) -> Option<&'static str> {
        self.section
    }

    /// Returns `true` if the option should not be shown.
    pub fn is_hidden(&self) -> bool {
        self.hidden
    }
}

/// Information about a positional argument.
#[derive(Debug, Clone, Copy)]
pub struct ArgumentInfo {
    pub(crate) name: &'static str,
    pub(crate) help: &'static str,
    pub(crate) long_help: Option<&'static str>,
    pub(crate) default_value: Option<&'static str>,
    pub(crate) values: Values,
    pub(crate) value_hint: ValueHint,
    pub(crate) required: bool,
    pub(crate) multiple: bool,
}

impl ArgumentInfo {
    fn from_pos<A>(pos: &Pos<A>) -> ArgumentInfo {
        ArgumentInfo {
            name: pos.name,
            help: pos.help,
            long_help: pos.long_help,
            default_value: pos.default_value,
            values: pos.values,
            value_hint: pos.value_hint,
            required: pos.required,
            multiple: pos.multiple,
        }
    }

    /// The display name.
    pub fn name(&self) -> &'static str {
        self.name
    }

    /// The short help text.
    pub fn help(&self) -> &'static str {
        self.help
    }

    /// The long help text if one was set.
    pub fn long_help(&self) -> Option<&'static str> {
        self.long_help
    }

    /// The default value for display purposes.
    pub fn default_value(&self) -> Option<&'static str> {
        self.default_value
    }

    /// The possible values.
    pub fn possible_values(&self) -> Vec<PossibleValue> {
        self.values.to_vec()
    }

    /// The value hint.
    pub fn value_hint(&self) -> ValueHint {
        self.value_hint
    }

    /// Returns `true` if the argument is required.
    pub fn is_required(&self) -> bool {
        self.required
    }

    /// Returns `true` if the argument can be given multiple times.
    pub fn is_multiple(&self) -> bool {
        self.multiple
    }

    /// Renders the argument for usage lines (eg: `<FILE>...` or `[FILE]`).
    pub fn display(&self) -> String {
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

mod private {
    pub trait Sealed {}
}

impl<A: 'static> private::Sealed for Cli<A> {}

/// Type erased read-only access to a [`Cli`].
///
/// This trait is sealed and only implemented for [`Cli`].
pub trait CommandInfo: private::Sealed {
    /// The name of the command.
    fn name(&self) -> &'static str;

    /// The version if set.
    fn version(&self) -> Option<&'static str>;

    /// The short description.
    fn about(&self) -> Option<&'static str>;

    /// The long description.
    fn long_about(&self) -> Option<&'static str>;

    /// Text shown before the description.
    fn before_help(&self) -> Option<&'static str>;

    /// Text shown at the end of the help page.
    fn after_help(&self) -> Option<&'static str>;

    /// The usage override (without the program name).
    fn usage_override(&self) -> Option<&'static str>;

    /// The maximum width for rendering help.
    fn max_width(&self) -> Option<usize>;

    /// All options including the builtin `--help` and `--version` options.
    fn options(&self) -> Vec<OptionInfo>;

    /// All positional arguments.
    fn arguments(&self) -> Vec<ArgumentInfo>;

    /// All subcommands.
    fn commands(&self) -> Vec<&dyn CommandInfo>;
}

impl<A: 'static> CommandInfo for Cli<A> {
    fn name(&self) -> &'static str {
        self.name
    }

    fn version(&self) -> Option<&'static str> {
        self.version
    }

    fn about(&self) -> Option<&'static str> {
        self.about
    }

    fn long_about(&self) -> Option<&'static str> {
        self.long_about
    }

    fn before_help(&self) -> Option<&'static str> {
        self.before_help
    }

    fn after_help(&self) -> Option<&'static str> {
        self.after_help
    }

    fn usage_override(&self) -> Option<&'static str> {
        self.usage
    }

    fn max_width(&self) -> Option<usize> {
        self.max_width
    }

    fn options(&self) -> Vec<OptionInfo> {
        let mut rv = Vec::new();
        let mut section = None;
        let mut builtins_added = false;
        for opt in self.opts {
            if opt.id.is_some() {
                rv.push(OptionInfo::from_opt(opt, section));
            } else {
                // builtins go to the end of the default section
                if !builtins_added {
                    push_builtins(self, &mut rv);
                    builtins_added = true;
                }
                section = Some(opt.help);
            }
        }
        if !builtins_added {
            push_builtins(self, &mut rv);
        }
        rv
    }

    fn arguments(&self) -> Vec<ArgumentInfo> {
        self.args.iter().map(ArgumentInfo::from_pos).collect()
    }

    fn commands(&self) -> Vec<&dyn CommandInfo> {
        self.cmds
            .iter()
            .map(|x| x.info as &dyn CommandInfo)
            .collect()
    }
}

fn push_builtins<A>(cli: &Cli<A>, rv: &mut Vec<OptionInfo>) {
    let builtins = [
        (Builtin::Help, 'h', "help", "Print help"),
        (Builtin::Version, 'V', "version", "Print version"),
    ];
    for (builtin, short, long, help) in builtins {
        let short = Some(short).filter(|&c| cli.builtin_short(c) == Some(builtin));
        let long = Some(long).filter(|&l| cli.builtin_long(l) == Some(builtin));
        if short.is_some() || long.is_some() {
            rv.push(OptionInfo::builtin(short, long, help));
        }
    }
}
