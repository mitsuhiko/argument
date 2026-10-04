use std::cell::RefCell;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::rc::Rc;

use argument_parser::{Flag, FromString, Param};

use crate::error::{Context, Error, ErrorKind};
use crate::help;
use crate::spec::{Builtin, Cli, Opt, Pos, Values};

/// What the parser handed out last.
enum Last<'a, A: 'static> {
    Nothing,
    Opt(&'a Opt<A>, bool),
    Pos(&'a Pos<A>),
    Cmd,
}

impl<A: 'static> Last<'_, A> {
    fn describe(&self) -> Option<String> {
        match *self {
            Last::Opt(opt, short) => Some(opt_display(opt, short)),
            Last::Pos(pos) => Some(pos.display()),
            Last::Nothing | Last::Cmd => None,
        }
    }
}

fn opt_display<A>(opt: &Opt<A>, short: bool) -> String {
    let mut rv = match (short, opt.short, opt.long) {
        (true, Some(c), _) | (false, Some(c), None) => format!("-{}", c),
        (_, _, Some(l)) => format!("--{}", l),
        (_, None, None) => String::new(),
    };
    if let Some(value) = opt.value {
        if opt.optional_value {
            rv.push_str(&format!("[{}]", value));
        } else {
            rv.push_str(&format!(" <{}>", value));
        }
    }
    rv
}

/// Parses a command line based on a [`Cli`] description.
///
/// This wraps an [`argument_parser::Parser`].  Instead of handing out raw
/// parameters it hands out the identifiers declared in the [`Cli`].  Values
/// are pulled explicitly with [`value`](Self::value) and friends, exactly like
/// with the low-level parser.
///
/// The parser handles `-h` / `--help` and `-V` / `--version` (unless the
/// spec declares options with those names), reports unknown options with
/// suggestions and checks that required positional arguments were provided.
///
/// In debug builds the parser panics if the parsing code disagrees with the
/// spec: when a value of an option declared with [`Opt::value`] is not consumed
/// or when a value is requested for an option declared as flag.
pub struct Parser<'a, A: 'static> {
    inner: argument_parser::Parser<'a>,
    cli: &'a Cli<A>,
    prog: String,
    pos_index: usize,
    pos_filled: usize,
    last: Last<'a, A>,
    pending: bool,
    context: Rc<RefCell<Context>>,
}

impl<A: 'static> fmt::Debug for Parser<'_, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("prog", &self.prog)
            .field("inner", &self.inner)
            .finish()
    }
}

impl<A: Copy + 'static> Cli<A> {
    /// Creates a parser for the arguments of the current process.
    pub fn parser_from_env(&self) -> Parser<'_, A> {
        self.parser(argument_parser::Parser::from_env())
    }

    /// Creates a parser from the given arguments (without program name).
    pub fn parser_from_args<'a, I, S>(&'a self, args: I) -> Parser<'a, A>
    where
        I: IntoIterator<Item = S> + 'a,
        S: Into<OsString> + 'a,
    {
        self.parser(argument_parser::Parser::from_args(args))
    }

    /// Creates a parser from an existing low-level parser.
    ///
    /// The low-level parser can be configured with [`Flag`]s before.
    pub fn parser<'a>(&'a self, inner: argument_parser::Parser<'a>) -> Parser<'a, A> {
        let prog = self.name.to_string();
        let context = Rc::new(RefCell::new(help::context(self, &prog)));
        Parser {
            inner,
            cli: self,
            prog,
            pos_index: 0,
            pos_filled: 0,
            last: Last::Nothing,
            pending: false,
            context,
        }
    }

    /// Parses the process arguments with the given function.
    ///
    /// If the function fails, the error is printed and the process exits.
    /// Help and version requests print to stdout and exit with `0`, all
    /// other errors print to stderr and exit with `2`.
    ///
    /// ```no_run
    /// # use argument::{Cli, Opt, Parser, Error};
    /// # #[derive(Copy, Clone)] enum A { Verbose }
    /// # static CLI: Cli<A> = Cli::new("demo").opts(&[Opt::new(A::Verbose).short('v')]);
    /// fn parse(mut p: Parser<A>) -> Result<bool, Error> {
    ///     let mut verbose = false;
    ///     while let Some(arg) = p.param()? {
    ///         match arg {
    ///             A::Verbose => verbose = true,
    ///         }
    ///     }
    ///     Ok(verbose)
    /// }
    ///
    /// let verbose = CLI.run(parse);
    /// ```
    pub fn run<'a, T, F>(&'a self, f: F) -> T
    where
        F: FnOnce(Parser<'a, A>) -> Result<T, Error>,
    {
        let parser = self.parser_from_env();
        let context = parser.context.clone();
        match f(parser) {
            Ok(rv) => rv,
            Err(err) => err.with_context(&context.borrow()).exit(),
        }
    }
}

impl<'a, A: Copy + 'static> Parser<'a, A> {
    /// Parses the next parameter and returns its identifier.
    ///
    /// Returns `Ok(None)` when the end of the command line was reached.  At
    /// that point missing required positional arguments are reported.
    ///
    /// When an option that takes a value or a positional argument is
    /// returned, the value has to be consumed with [`value`](Self::value) or
    /// one of the other value methods.  When a subcommand is returned, the
    /// name was already consumed and the parser can be handed over with
    /// [`subcommand`](Self::subcommand).
    pub fn param(&mut self) -> Result<Option<A>, Error> {
        let was_pending = std::mem::replace(&mut self.pending, false);
        if was_pending && cfg!(debug_assertions) {
            panic!(
                "argument: the value for '{}' was never consumed (call value() or raw_value())",
                self.last.describe().unwrap_or_default()
            );
        }
        self.last = Last::Nothing;

        if !was_pending && self.at_hyphen_value() {
            self.inner.set_flag(Flag::OptionsEnabled, false);
            let rv = self.inner.param();
            if !self.inner.get_flag(Flag::DisableOptionsAfterArgs) {
                self.inner.set_flag(Flag::OptionsEnabled, true);
            }
            rv.map_err(|err| self.convert_error(err))?;
            return self.handle_pos();
        }

        match self.inner.param() {
            Ok(None) => self.finish().map(|()| None),
            Ok(Some(Param::Short(c))) => self.handle_short(c),
            Ok(Some(Param::Long(name))) => self.handle_long(name),
            Ok(Some(Param::Pos)) => self.handle_pos(),
            Err(err) => Err(self.convert_error(err)),
        }
    }

    /// Parses the value of the current option or positional argument.
    ///
    /// If the spec declares possible values, the value is validated against
    /// them.  See [`argument_parser::Parser::value`].
    pub fn value<V: FromString>(&mut self) -> Result<V, Error> {
        self.consume_value();
        if self.possible_values().is_empty() {
            return self.inner.value().map_err(|err| self.convert_error(err));
        }
        let value = self.raw_value_unchecked()?;
        self.parse_checked(value)
    }

    /// Returns the raw value of the current option or positional argument.
    ///
    /// If the spec declares possible values, the value is validated against
    /// them.  See [`argument_parser::Parser::raw_value`].
    pub fn raw_value(&mut self) -> Result<OsString, Error> {
        self.consume_value();
        let value = self.raw_value_unchecked()?;
        self.check_possible_value(&value)?;
        Ok(value)
    }

    /// Parses an optional attached value (`--opt=value` or `-ovalue`).
    ///
    /// See [`argument_parser::Parser::optional_value`].
    pub fn optional_value<V: FromString>(&mut self) -> Result<Option<V>, Error> {
        self.consume_value();
        if self.possible_values().is_empty() {
            return self
                .inner
                .optional_value()
                .map_err(|err| self.convert_error(err));
        }
        match self.inner.optional_raw_value() {
            Some(value) => self.parse_checked(value).map(Some),
            None => Ok(None),
        }
    }

    /// Returns an optional attached raw value.
    ///
    /// See [`argument_parser::Parser::optional_raw_value`].
    pub fn optional_raw_value(&mut self) -> Result<Option<OsString>, Error> {
        self.consume_value();
        match self.inner.optional_raw_value() {
            Some(value) => {
                self.check_possible_value(&value)?;
                Ok(Some(value))
            }
            None => Ok(None),
        }
    }

    /// Checks if the parser looks at something that is not an option.
    ///
    /// This is useful for options that take a variable number of values.
    /// Calling this counts as handling the value of the current option.
    ///
    /// See [`argument_parser::Parser::looks_at_value`].
    pub fn looks_at_value(&mut self) -> bool {
        self.pending = false;
        self.inner.looks_at_value()
    }

    /// Peeks at the current raw argument.
    ///
    /// See [`argument_parser::Parser::peek_raw_arg`].
    pub fn peek_raw_arg(&self) -> Option<&OsStr> {
        self.inner.peek_raw_arg()
    }

    /// Consumes the current raw argument.
    ///
    /// Raw arguments bypass the spec entirely: they are not counted towards
    /// positional arguments.  See [`argument_parser::Parser::raw_arg`].
    pub fn raw_arg(&mut self) -> Option<OsString> {
        self.pending = false;
        self.inner.raw_arg()
    }

    /// Returns `true` if the end of the command line was reached.
    pub fn finished(&self) -> bool {
        self.inner.finished()
    }

    /// Checks a flag on the low-level parser.
    pub fn get_flag(&self, flag: Flag) -> bool {
        self.inner.get_flag(flag)
    }

    /// Sets a flag on the low-level parser.
    pub fn set_flag(&mut self, flag: Flag, yes: bool) {
        self.inner.set_flag(flag, yes);
    }

    /// The program name including the subcommand path (eg: `cargo install`).
    pub fn prog(&self) -> &str {
        &self.prog
    }

    /// Returns the usage line.
    pub fn usage(&self) -> String {
        help::usage(self.cli, &self.prog)
    }

    /// Renders the (short) help page.
    pub fn help_text(&self) -> String {
        help::help_text(self.cli, &self.prog, help::render_width(self.cli), false)
    }

    /// Renders the long help page.
    ///
    /// The long help uses [`Cli::long_about`] and the long help texts of
    /// options and arguments where available.
    pub fn long_help_text(&self) -> String {
        help::help_text(self.cli, &self.prog, help::render_width(self.cli), true)
    }

    /// Creates a help "error".
    ///
    /// Returning this prints the help page and exits successfully when used
    /// with [`Cli::run`] or [`Error::exit`].
    pub fn help(&self) -> Error {
        Error::with_kind(ErrorKind::Help, self.help_text())
    }

    /// Creates a long help "error".
    ///
    /// Like [`help`](Self::help) but renders the long help page.
    pub fn long_help(&self) -> Error {
        Error::with_kind(ErrorKind::Help, self.long_help_text())
    }

    /// Creates a custom error that includes usage information.
    pub fn error(&self, message: impl Into<String>) -> Error {
        self.with_context(Error::new(message))
    }

    /// Creates an error for a missing option or positional argument.
    pub fn missing(&self, id: A) -> Error
    where
        A: PartialEq,
    {
        if let Some(opt) = self.cli.opts.iter().find(|x| x.id == Some(id)) {
            let display = opt_display(opt, opt.long.is_none());
            self.with_context(Error::with_kind(
                ErrorKind::MissingArgument,
                format!("the option '{}' is required", display),
            ))
        } else if let Some(pos) = self.cli.args.iter().find(|x| x.id == id) {
            self.missing_pos(pos)
        } else {
            self.with_context(Error::with_kind(
                ErrorKind::MissingArgument,
                "a required argument is missing",
            ))
        }
    }

    /// Hands the parser over to a subcommand.
    ///
    /// This is typically called after [`param`](Self::param) returned the
    /// identifier of a [`Cmd`](crate::Cmd).
    pub fn subcommand<B: Copy + 'static>(self, cli: &'a Cli<B>) -> Parser<'a, B> {
        let prog = format!("{} {}", self.prog, cli.name);
        *self.context.borrow_mut() = help::context(cli, &prog);
        Parser {
            inner: self.inner,
            cli,
            prog,
            pos_index: 0,
            pos_filled: 0,
            last: Last::Nothing,
            pending: false,
            context: self.context,
        }
    }

    /// Gives access to the low-level parser.
    pub fn inner_mut(&mut self) -> &mut argument_parser::Parser<'a> {
        &mut self.inner
    }

    fn handle_short(&mut self, c: char) -> Result<Option<A>, Error> {
        if let Some(opt) = self.cli.find_short(c) {
            return self.found_opt(opt, true);
        }
        match self.cli.builtin_short(c) {
            Some(builtin) => Err(self.builtin(builtin)),
            None => Err(self.with_context(Error::with_kind(
                ErrorKind::UnknownOption,
                format!("unexpected argument '-{}' found", c),
            ))),
        }
    }

    fn handle_long(&mut self, name: String) -> Result<Option<A>, Error> {
        if let Some(opt) = self.cli.find_long(&name) {
            return self.found_opt(opt, false);
        }
        if let Some(builtin) = self.cli.builtin_long(&name) {
            return Err(self.builtin(builtin));
        }
        let builtins = ["help", "version"]
            .into_iter()
            .filter(|x| self.cli.builtin_long(x).is_some());
        let candidates = self
            .cli
            .opts
            .iter()
            .filter(|x| x.id.is_some() && !x.hidden)
            .filter_map(|x| x.long)
            .chain(builtins);
        let tip =
            help::suggest(&name, candidates).map(|x| format!("a similar option exists: '--{}'", x));
        Err(self.with_context(
            Error::with_kind(
                ErrorKind::UnknownOption,
                format!("unexpected argument '--{}' found", name),
            )
            .with_tip(tip),
        ))
    }

    fn found_opt(&mut self, opt: &'a Opt<A>, short: bool) -> Result<Option<A>, Error> {
        // a flag with an attached value (--flag=value)
        if !opt.takes_value()
            && !short
            && self.inner.peek_raw_arg().is_none()
            && !self.inner.finished()
        {
            let value = self.inner.optional_raw_value().unwrap_or_default();
            return Err(self.with_context(Error::with_kind(
                ErrorKind::UnexpectedValue,
                format!(
                    "unexpected value '{}' for '{}'",
                    value.to_string_lossy(),
                    opt_display(opt, false)
                ),
            )));
        }
        self.pending = opt.takes_value() && !opt.optional_value;
        self.last = Last::Opt(opt, short);
        // the id is always set for options found by name
        Ok(opt.id)
    }

    fn handle_pos(&mut self) -> Result<Option<A>, Error> {
        let cli = self.cli;
        if !cli.cmds.is_empty() {
            let cmd = self
                .inner
                .peek_raw_arg()
                .and_then(|x| x.to_str())
                .and_then(|name| cli.cmds.iter().find(|cmd| cmd.info.name() == name));
            if let Some(cmd) = cmd {
                self.inner.raw_arg();
                self.last = Last::Cmd;
                return Ok(Some(cmd.id));
            }
        }

        if let Some(pos) = cli.args.get(self.pos_index) {
            self.pos_filled = self.pos_index + 1;
            if !pos.multiple {
                self.pos_index += 1;
            }
            self.pending = true;
            self.last = Last::Pos(pos);
            return Ok(Some(pos.id));
        }

        let value = self.inner.raw_arg().unwrap_or_default();
        let value = value.to_string_lossy();
        if cli.cmds.is_empty() {
            Err(self.with_context(Error::with_kind(
                ErrorKind::UnexpectedArgument,
                format!("unexpected argument '{}' found", value),
            )))
        } else {
            let tip = help::suggest(&value, cli.cmds.iter().map(|x| x.info.name()))
                .map(|x| format!("a similar command exists: '{}'", x));
            Err(self.with_context(
                Error::with_kind(
                    ErrorKind::UnknownCommand,
                    format!("unrecognized command '{}'", value),
                )
                .with_tip(tip),
            ))
        }
    }

    /// Checks if the next argument should be forced into a positional
    /// argument that allows hyphen values.
    fn at_hyphen_value(&self) -> bool {
        let Some(pos) = self.cli.args.get(self.pos_index) else {
            return false;
        };
        if !pos.allow_hyphen || !self.inner.get_flag(Flag::OptionsEnabled) {
            return false;
        }
        match self.inner.peek_raw_arg().map(|x| x.to_string_lossy()) {
            Some(arg) => {
                arg.len() > 1
                    && arg.starts_with('-')
                    && arg != "--"
                    && !self.cli.is_known_option(&arg)
            }
            None => false,
        }
    }

    fn finish(&mut self) -> Result<(), Error> {
        let rest = self.cli.args.get(self.pos_filled..).unwrap_or_default();
        match rest.iter().find(|x| x.required) {
            Some(pos) => Err(self.missing_pos(pos)),
            None => Ok(()),
        }
    }

    fn consume_value(&mut self) {
        self.pending = false;
        if cfg!(debug_assertions) {
            if let Last::Opt(opt, short) = self.last {
                if !opt.takes_value() {
                    panic!(
                        "argument: a value was requested for '{}' which is declared as a flag (add .value() to the spec)",
                        opt_display(opt, short)
                    );
                }
            }
        }
    }

    fn possible_values(&self) -> Values {
        match self.last {
            Last::Opt(opt, _) => opt.values,
            Last::Pos(pos) => pos.values,
            Last::Nothing | Last::Cmd => Values::None,
        }
    }

    fn raw_value_unchecked(&mut self) -> Result<OsString, Error> {
        self.inner
            .raw_value()
            .map_err(|err| self.convert_error(err))
    }

    fn check_possible_value(&self, value: &OsStr) -> Result<(), Error> {
        let values = self.possible_values();
        if values.is_empty() || value.to_str().is_some_and(|x| values.contains(x)) {
            return Ok(());
        }
        let value = value.to_string_lossy();
        let names = values.names();
        let tip = match help::suggest(&value, names.iter().copied()) {
            Some(similar) => format!("a similar value exists: '{}'", similar),
            None => format!("possible values: {}", names.join(", ")),
        };
        Err(self.with_context(
            Error::with_kind(
                ErrorKind::InvalidValue,
                format!(
                    "invalid value '{}' for '{}'",
                    value,
                    self.last.describe().unwrap_or_default()
                ),
            )
            .with_tip(Some(tip)),
        ))
    }

    fn parse_checked<V: FromString>(&self, value: OsString) -> Result<V, Error> {
        self.check_possible_value(&value)?;
        // possible values are valid unicode, so this cannot fail
        let value = value.into_string().unwrap_or_default();
        V::from_string(value).map_err(|err| self.convert_error(err))
    }

    fn builtin(&self, builtin: Builtin) -> Error {
        match builtin {
            Builtin::Help => self.help(),
            Builtin::Version => Error::with_kind(
                ErrorKind::Version,
                format!("{} {}", self.prog, self.cli.version.unwrap_or_default()),
            ),
        }
    }

    fn missing_pos(&self, pos: &Pos<A>) -> Error {
        self.with_context(Error::with_kind(
            ErrorKind::MissingArgument,
            format!("the argument '{}' is required", pos.display()),
        ))
    }

    fn with_context(&self, err: Error) -> Error {
        err.with_context(&self.context.borrow())
    }

    fn convert_error(&self, err: argument_parser::Error) -> Error {
        use argument_parser::ErrorKind as K;
        let what = match self.last.describe() {
            Some(what) => format!("'{}'", what),
            None => match err.param() {
                Some(Param::Short(c)) => format!("'-{}'", c),
                Some(Param::Long(l)) => format!("'--{}'", l),
                _ => "argument".to_string(),
            },
        };
        let (kind, message) = match err.kind() {
            K::MissingValue => (
                ErrorKind::MissingValue,
                format!("a value is required for {} but none was supplied", what),
            ),
            K::InvalidUnicode => (
                ErrorKind::InvalidValue,
                format!("invalid unicode in value for {}", what),
            ),
            K::InvalidValue => {
                let mut message = match err.value() {
                    Some(value) => format!("invalid value '{}' for {}", value, what),
                    None => format!("invalid value for {}", what),
                };
                if let Some(source) = std::error::Error::source(&err) {
                    message.push_str(&format!(": {}", source));
                }
                (ErrorKind::InvalidValue, message)
            }
            // custom errors come from user FromString implementations
            K::Custom => (
                ErrorKind::InvalidValue,
                format!("invalid value for {}: {}", what, err),
            ),
            _ => (ErrorKind::Custom, format!("{:#}", err)),
        };
        self.with_context(Error::with_kind(kind, message).with_source(Box::new(err)))
    }
}
