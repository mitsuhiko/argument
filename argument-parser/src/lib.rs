//! This crate implements an argument parser for command lines following POSIX
//! conventions.  It's dead simple, dependency free and is built so you can
//! drive your own parsing.
//!
//! The goal of this crate is that it's stable, excellently tested and requires
//! little updates and maintenance.  The goal is that you can use it and it keeps
//! working.
//!
//! # Example
//!
//! Parsing happens via the [`Parser`] type:
//!
//! ```
//! use argument_parser::{Error, Parser};
//!
//! fn main() -> Result<(), Error> {
//!     let mut parser = Parser::from_env();
//!
//!     while let Some(param) = parser.param()? {
//!         if param.is_short('n') || param.is_long("number") {
//!             println!("Got number {}", parser.value::<i32>()?);
//!         } else if param.is_arg() {
//!             println!("Got arg {}", parser.string_value()?);
//!         } else {
//!             return Err(param.into_unexpected_error());
//!         }
//!     }
//!
//!     Ok(())
//! }
//! ```
//!
//! Here is what's happening:
//!
//! * [`Parser::from_env`] initializes the parser with the command line
//!   from the current environment.
//! * [`Parser::param`] pulls parameters from the command line one after another.  If
//!   the end of the command line is reached, `Ok(None)` is returned.  Reading
//!   parameters only reads the name (or presence) of the parameter, not the
//!   value.  To get the value, use [`Parser::value`] (parses via [`FromStr`]),
//!   [`Parser::string_value`] (converts into a [`String`]), or
//!   [`Parser::raw_value`] (just reads the raw [`OsString`]).
//! * [`Param`] can be deconstructed (it's an enum) and it provides utilities:
//!   * [`Param::is_short`] checks if a parameter is a specific short option.
//!   * [`Param::is_long`] checks if a parameter is a specific long option.
//!   * [`Param::is_arg`] checks if a parameter is a positional argument.
//! * [`Parser::value`] pulls a single value from the parser and parses it with [`FromStr`].
//! * [`Parser::string_value`] pulls a single string value from the parser.
//! * [`Param::into_unexpected_error`] propagates an "unexpected argument" error.
//!
//! # Behavior
//!
//! This crate follows POSIX behavior with getopt-style handling as much as
//! possible.  In particular, short arguments do not take `=` to separate from
//! the value (e.g., `-x42` represents a value of `42` and `-x=42` a value of
//! `=42`).  If you don't want that, you can change it with
//! [`Flag::StripShortOptionEqualSign`], though this is strongly discouraged.
//!
//! The special `--` argument is handled automatically for you, but the behavior
//! can be disabled by unsetting [`Flag::HandleDoubleDash`].
//!
//! By default, numeric options are valid options, but if you don't expect to have
//! any, you can disable them by setting [`Flag::DisableNumericOptions`], in which
//! case they are handled like normal arguments.
//!
//! By default, options and arguments can be freely mixed, but this can be
//! changed by setting [`Flag::DisableOptionsAfterArgs`].  With this flag set,
//! the first time a positional argument is encountered, the options are disabled.
//!
//! # General Parsing Rules
//!
//! * You can at any point parse a [`value`](Parser::value).  It will fail
//!   if there are no more arguments pending (you can use
//!   [`Parser::finished`] to check if you reached the end).
//! * When parsing a [`param`](Parser::param), it only parses the name and
//!   pauses before the value.  Consequently, when parsing a positional
//!   argument, it pauses just before the actual value is parsed.
//! * When entering short options (e.g., `-f`), the parser will prevent
//!   raw parameter access until the rest of the argument is parsed.
//! * When `--` is encountered while parsing parameters, it swallows that
//!   argument and flips the [`Flag::OptionsEnabled`] flag to `true`.  If you
//!   don't want that, you can unset the [`Flag::HandleDoubleDash`] flag.
//! * For complex parsing, you can try to parse raw arguments with
//!   [`peek_raw_arg`](Parser::peek_raw_arg) and
//!   [`raw_arg`](Parser::raw_arg) before falling back to the regular
//!   parsing methods.
//! * Option names must be valid unicode.
//! * All the flags and options of the parser can be changed at any point
//!   as parsing takes place.  For instance, you can turn on and off the handling
//!   of numeric options as you keep parsing.
//!
//! # Error Handling
//!
//! When the [`Parser`] encounters an error, it's not recoverable and the parser is
//! left in an undefined state (e.g., an argument might be lost, future calls
//! might fail).  For instance, it's not safe to try to call `value::<i32>()` and
//! fall back to `string_value()` if parsing fails.
//!
//! This crate makes a best effort at error reporting, but higher level abstractions
//! should be used to improve the user experience.  Error messages might indicate
//! the wrong parameters if raw argument parsing is used.
use std::error::Error as StdError;
use std::ffi::{OsStr, OsString};
use std::fmt;
use std::iter::once;
use std::mem::replace;
use std::path::Path;
use std::str::{from_utf8, from_utf8_unchecked, FromStr};

/// Represents a parsing error.
pub struct Error(Box<ErrorRepr>);

impl Error {
    fn new(kind: ErrorKind) -> Error {
        Error(Box::new(ErrorRepr {
            kind,
            param: None,
            value: None,
            source: None,
        }))
    }

    /// The type of error that ocurred.
    pub fn kind(&self) -> ErrorKind {
        self.0.kind
    }

    /// If available, a reference to the parameter that caused the error.
    pub fn param(&self) -> Option<&Param> {
        self.0.param.as_ref()
    }

    /// The value of the argument as string if available.
    ///
    /// If the parameter wasn't valid unicode, you might find the value
    /// instead in [`raw_value`](Self::raw_value).
    pub fn value(&self) -> Option<&str> {
        match (self.0.kind, self.0.value.as_ref()?) {
            (ErrorKind::Custom, _) => None,
            (_, Ok(s)) => Some(s),
            (_, Err(s)) => s.to_str(),
        }
    }

    /// The value of the argument as raw [`OsStr`] if available.
    pub fn raw_value(&self) -> Option<&OsStr> {
        match (self.0.kind, self.0.value.as_ref()?) {
            (ErrorKind::Custom, _) => None,
            (_, Ok(s)) => Some(OsStr::new(s)),
            (_, Err(s)) => Some(s),
        }
    }

    fn with_param(mut self, param: Param) -> Error {
        self.0.param = Some(param);
        self
    }

    fn with_string(mut self, value: String) -> Error {
        self.0.value = Some(Ok(value));
        self
    }

    fn with_os_string(mut self, value: OsString) -> Error {
        self.0.value = Some(Err(value));
        self
    }

    fn with_source(mut self, source: Box<dyn StdError + Send + Sync + 'static>) -> Error {
        self.0.source = Some(source);
        self
    }
}

impl<'s> From<&'s str> for Error {
    fn from(message: &'s str) -> Error {
        Error::from(message.to_string())
    }
}

impl From<String> for Error {
    fn from(message: String) -> Error {
        Error::new(ErrorKind::Custom).with_string(message)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        let option_name = match self.param() {
            Some(Param::Short(x)) => Some(format!("'-{}'", x)),
            Some(Param::Long(x)) => Some(format!("'--{}'", x)),
            _ => None,
        };
        match (self.kind(), option_name) {
            (MissingValue, Some(x)) => write!(f, "missing argument for {}", x),
            (MissingValue, None) => write!(f, "missing argument"),
            (InvalidUnicode, Some(x)) => write!(f, "argument for {} contains invalid unicode", x),
            (InvalidUnicode, None) => write!(f, "argument contains invalid unicode"),
            (InvalidValue, Some(x)) => write!(f, "argument for {} is invalid", x),
            (InvalidValue, None) => write!(f, "argument is invalid"),
            (UnexpectedParameter, Some(x)) => write!(f, "unexpected argument {}", x),
            (UnexpectedParameter, None) => write!(f, "unexpected argument"),
            (Custom, _) => write!(f, "{}", self.0.value.as_ref().unwrap().as_ref().unwrap()),
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field("message", &self.to_string())
            .field("kind", &self.kind())
            .field("param", &self.param())
            .field("value", &self.raw_value())
            .field("source", &self.source())
            .finish()
    }
}

impl StdError for Error {
    fn source(&self) -> Option<&(dyn StdError + 'static)> {
        match self.0.source {
            Some(ref source) => Some(&**source),
            None => None,
        }
    }
}

struct ErrorRepr {
    kind: ErrorKind,
    param: Option<Param>,
    value: Option<Result<String, OsString>>,
    source: Option<Box<dyn StdError + Send + Sync + 'static>>,
}

/// Represents the type of parsing error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[non_exhaustive]
pub enum ErrorKind {
    /// Emitted when a value was expected but none was there.
    MissingValue,
    /// Happens when parsing a parameter as string and the string is invalid unicode.
    InvalidUnicode,
    /// Happens when parsing a parameter into another type and parsing failed.
    InvalidValue,
    /// Created by [`Param::into_unexpected_error`].
    UnexpectedParameter,
    /// A custom error message
    Custom,
}

/// Represents a parsed parameter.
///
/// A parameter is either a short or long option, or a positional argument.
/// In all of those cases it does not hold on to the value passed.  In
/// case of options however it contains the name of the option.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Param {
    /// This parameter is a single character option
    Short(char),
    /// This parameter is a long option
    Long(String),
    /// This parameter is a positional argument
    Arg,
}

impl Param {
    /// Is this a specific short option?
    pub fn is_short(&self, c: char) -> bool {
        match self {
            Param::Short(r) => c == *r,
            Param::Long(_) | Param::Arg => false,
        }
    }

    /// Is this a specific long option?
    pub fn is_long(&self, s: &str) -> bool {
        match self {
            Param::Long(r) => r == s,
            Param::Short(_) | Param::Arg => false,
        }
    }

    /// Does it match the short or long option?
    ///
    /// This is a shortcut for checking both short and long option.
    pub fn is_either(&self, short: char, long: &str) -> bool {
        self.is_short(short) || self.is_long(long)
    }

    /// Is this a positional argument?
    pub fn is_arg(&self) -> bool {
        matches!(self, Param::Arg)
    }

    /// Consumes the parameter and creates an unexpected error.
    pub fn into_unexpected_error(self) -> Error {
        Error::new(ErrorKind::UnexpectedParameter).with_param(self)
    }
}

/// Parser behavior flags.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Flag {
    /// When set the parser still accept options.
    ///
    /// **Default:** flag is set
    OptionsEnabled,
    /// When set, the parser disbles option parsing after `--`.
    ///
    /// **Default:** flag is set
    HandleDoubleDash,
    /// When set the aprser will treat numeric options as arguments instead.
    ///
    /// **Default:** flag is unset
    DisableNumericOptions,
    /// When set the parser will stop accepting options after positional arguments.
    ///
    /// **Default:** flag is unset
    DisableOptionsAfterArgs,
    /// When set, one leading equal sign from short option values is removed.
    ///
    /// **Default:** flag is unset
    StripShortOptionEqualSign,
}

impl Flag {
    fn as_u8(self) -> u8 {
        match self {
            Flag::OptionsEnabled => 1,
            Flag::HandleDoubleDash => 2,
            Flag::DisableNumericOptions => 4,
            Flag::DisableOptionsAfterArgs => 8,
            Flag::StripShortOptionEqualSign => 16,
        }
    }
}

/// An internal state indicator for the parser
enum State {
    Default,
    ExplicitOptionValue,
    ShortOptChain(usize),
    ArgPause,
}

/// A low-level command line parser for POSIX command lines.
///
/// This parser steps through an iterator of command line arguments
/// parsing them one after another.  It maintains internal state to
/// avoid accidental mis-parsing if the user invokes parsing methods in the
/// wrong order.  For basic instructions consult the crate documentation.
pub struct Parser<'it> {
    args: Box<dyn Iterator<Item = OsString> + 'it>,
    current_arg: Option<OsString>,
    prog: Option<OsString>,
    last_param: Option<Param>,
    state: State,
    flags: u8,
}

impl fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parser")
            .field("prog", &self.prog)
            .field("finished", &self.finished())
            .finish()
    }
}

impl<'it> Parser<'it> {
    /// Creates a parser from the environment.
    pub fn from_env() -> Parser<'static> {
        Parser::from_cmdline(std::env::args_os())
    }

    /// Creates a parser from the given split command line.
    ///
    /// The first argument must be the name of the program.
    pub fn from_cmdline<I, S>(args: I) -> Parser<'it>
    where
        I: Iterator<Item = S> + 'it,
        S: Into<OsString> + 'it,
    {
        let mut args = args.map(Into::into);
        Parser {
            prog: args.next(),
            current_arg: args.next(),
            args: Box::new(args),
            state: State::Default,
            last_param: None,
            flags: Flag::HandleDoubleDash.as_u8() | Flag::OptionsEnabled.as_u8(),
        }
    }

    /// Creates a parser from just the arguments.
    ///
    /// In this case the program name is empty.
    pub fn from_args<I, S>(args: I) -> Parser<'it>
    where
        I: Iterator<Item = S> + 'it,
        S: Into<OsString> + 'it,
    {
        Parser::from_cmdline(once(OsString::new()).chain(args.map(Into::into)))
    }

    /// Returns the normalized program name (first argument).
    ///
    /// This will only have the file name portion of the first
    /// argument if it was passed as full path.  If you want the full,
    /// unprocessed first argument use [`raw_prog`](Self::raw_prog) instead.
    pub fn prog(&self) -> &str {
        self.raw_prog()
            .map(Path::new)
            .and_then(|x| x.file_name())
            .and_then(|x| x.to_str())
            .unwrap_or_default()
    }

    /// Returns the raw first argument.
    ///
    /// This will be the full path name if it's there.
    pub fn raw_prog(&self) -> Option<&OsStr> {
        self.prog.as_deref()
    }

    /// Parse the current argument as parameter.
    ///
    /// A parameter can be a short named option (`-o`), a long named option
    /// `(--option)`, or a positional argument.  Note that parameters always
    /// come without values, that includes positional arguments!  To get the
    /// value to a parameter you need to use one of the [`value`](Self::value)
    /// parsing methods.
    ///
    /// You can think of this as a special parsing step that always pauses just
    /// before the value is consumed.  If you were to call [`param`](Self::param)
    /// again on a positional argument without consuming the value, the value is
    /// lost.
    ///
    /// For simplicity reasons, parameter names are always valid unicode strings.
    ///
    /// While parsing for parameters, `--` is automatically handled unless disabled.
    pub fn param(&mut self) -> Result<Option<Param>, Error> {
        let param = self.next_param().map_err(|err| self.augment_error(err))?;
        self.last_param = param.clone();
        Ok(param)
    }

    fn next_param(&mut self) -> Result<Option<Param>, Error> {
        loop {
            let arg = match self.state {
                State::Default => match self.current_arg.as_deref() {
                    Some(arg) => arg,
                    None => return Ok(None),
                },
                // Not consuming values of positional arguments or started option
                // values leads to the discarding of values.
                State::ArgPause | State::ExplicitOptionValue => {
                    self.next_arg_and_reset_state();
                    continue;
                }
                State::ShortOptChain(ref mut pos) => {
                    let arg = self.current_arg.as_deref().unwrap();
                    return match os_str_char_at(arg, *pos)? {
                        None => {
                            self.next_arg_and_reset_state();
                            continue;
                        }
                        Some(ch) => {
                            *pos += ch.len_utf8();
                            if *pos >= arg.len() {
                                self.next_arg_and_reset_state();
                            }
                            Ok(Some(Param::Short(ch)))
                        }
                    };
                }
            };

            let arg_bytes = arg.as_encoded_bytes();
            if !self.get_flag(Flag::OptionsEnabled) {
                return Ok(Some(self.pause_for_arg()));
            } else if arg_bytes == b"--" {
                if !self.get_flag(Flag::HandleDoubleDash) {
                    return Ok(Some(self.pause_for_arg()));
                }
                self.set_flag(Flag::OptionsEnabled, false);
                self.next_arg_and_reset_state();
                continue;
            } else if arg_bytes.starts_with(b"--") {
                let name = if let Some(ind) = arg_bytes.iter().position(|&b| b == b'=') {
                    let (name, arg_value) = os_str_split_utf8_prefix(arg, ind + 1)?;
                    let name = name[2..name.len() - 1].to_string();
                    self.current_arg = Some(arg_value.to_owned());
                    self.state = State::ExplicitOptionValue;
                    name
                } else {
                    let mut name = os_string_into_string(self.next_arg_and_reset_state().unwrap())?;
                    name.drain(..2);
                    name
                };
                return Ok(Some(Param::Long(name)));
            } else if self.considered_opt(arg) {
                self.state = State::ShortOptChain(1);
                continue;
            } else {
                return Ok(Some(self.pause_for_arg()));
            }
        }
    }

    /// Parse the current argument as value.
    ///
    /// This gets a value for both options and positional arguments and tries to
    /// parse it with [`FromStr`].
    ///
    /// When you should use which method?
    ///
    /// * [`string_value`](Self::string_value) for when you know that you need a string.
    /// * [`value`](Self::value) for if you need something that can be parsed from a string.
    /// * [`raw_value`](Self::raw_value) for when you need to accept a file system path,
    ///   environment variable value or similar.
    ///
    /// Additionally for some CLIs where you might want to represent optional
    /// values to parameters, you can also use [`optional_value`](Self::optional_value)
    /// but the usage of that method is discouraged.
    ///
    /// Note that this method will fail if there are no more arguments.  If you want
    /// to parse the rest of the values until you are done you can either use
    /// [`param`](Self::param) and [`value`](Self::value) alternating, or you can
    /// check with [`finished`](Self::finished) if there are more arguments.
    ///
    /// Lastly calling this method will always parse a value, even if it looks
    /// like an option or end of options marker.  If you don't want that, you can use
    /// [`looks_at_value`](Self::looks_at_value).  This is a basic heuristic
    /// that will return `false` if what it's looking at currently looks like an
    /// option or the end of the command line was reached.
    pub fn value<V>(&mut self) -> Result<V, Error>
    where
        V: FromStr,
        V::Err: Into<Box<dyn StdError + Send + Sync + 'static>>,
    {
        parse_string(self.string_value()?).map_err(|err| self.augment_error(err))
    }

    /// Parse the current argument as value ensuring it's a valid unicode string.
    ///
    /// This behaves like [`value::<String>`](Self::value) but avoids an extra copy.
    pub fn string_value(&mut self) -> Result<String, Error> {
        os_string_into_string(self.raw_value()?).map_err(|err| self.augment_error(err))
    }

    /// Parse the current argument as a raw value ([`OsString`]).
    ///
    /// On unix command lines inputs do not have to be valid unicode.  This is
    /// typically unlikely to happen but there are situations where you might want
    /// to accept invalid unicode.  For instance you might have a user working on a
    /// file system with a misconfigured encoding.  As such it's recommended to use
    /// this method to accept file names and then convert it into a `PathBuf` or
    /// similar.
    pub fn raw_value(&mut self) -> Result<OsString, Error> {
        self.optional_raw_value()
            .or_else(|| self.next_arg_and_reset_state())
            .ok_or_else(|| Error::new(ErrorKind::MissingValue))
            .map_err(|err| self.augment_error(err))
    }

    /// Parse the current argument as an optional value.
    ///
    /// Optional values are values directly attached to options in the form
    /// ``-ovalue`` or ``--option=value``, but not ``-o value`` or ``--option
    /// value``.
    ///
    /// Creating command line interface with these types of options is generally
    /// not a good idea and the use of it is strongly discouraged.
    pub fn optional_value<V>(&mut self) -> Result<Option<V>, Error>
    where
        V: FromStr,
        V::Err: Into<Box<dyn StdError + Send + Sync + 'static>>,
    {
        self.optional_string_value()?
            .map(parse_string)
            .transpose()
            .map_err(|err| self.augment_error(err))
    }

    /// Parse the current argument as an optional value ensuring it's a valid unicode string.
    ///
    /// This is like [`optional_value`](Self::optional_value) but avoids an
    /// extra copy.
    pub fn optional_string_value(&mut self) -> Result<Option<String>, Error> {
        match self.optional_raw_value() {
            Some(value) => os_string_into_string(value)
                .and_then(parse_string)
                .map_err(|err| self.augment_error(err))
                .map(Some),
            None => Ok(None),
        }
    }

    /// Parse the current argument as a raw optional value ([`OsString`]).
    ///
    /// See [`optional_value`](Self::optional_value) and
    /// [`raw_value`](Self::raw_value) for more details.
    pub fn optional_raw_value(&mut self) -> Option<OsString> {
        match self.state {
            State::Default | State::ArgPause => None,
            State::ExplicitOptionValue => self.next_arg_and_reset_state(),
            State::ShortOptChain(pos) => {
                let arg = self.next_arg_and_reset_state()?;
                // reading short codes will read only by valid characters, so
                // the perfix is known to be good.
                let (_, mut rest) = os_str_split_utf8_prefix(&arg, pos).unwrap();
                if self.get_flag(Flag::StripShortOptionEqualSign) {
                    rest = os_str_strip_prefix(rest, "=");
                }
                Some(rest.to_owned())
            }
        }
    }

    /// Peeks at the current, unparsed raw argument.
    ///
    /// This lets one implement custom argument parsing bypassing whatever
    /// internal state exists.  If the parser is currently in a state where
    /// raw argument access is not possible, this will return `None`
    pub fn peek_raw_arg(&self) -> Option<&OsStr> {
        match self.state {
            State::Default | State::ArgPause => self.current_arg.as_deref(),
            State::ExplicitOptionValue | State::ShortOptChain(_) => None,
        }
    }

    /// This returns the current raw argument and goes one argument forward.
    ///
    /// This is similar to [`peek_raw_arg`](Self::peek_raw_arg) but extra
    /// care needs to be taken about the return value.  If `None` is returned
    /// it can also mean that the end of the argument line was reached.  You
    /// should generally combine this with peaking first to disambiugate.
    ///
    /// Unlike [`raw_value`](Self::raw_value) this will not accept values from
    /// combined arguments (eg: `bar` in `--foo=bar`) and it will not perform
    /// any state updates.  For instnace if you consume `--` as a raw arg,
    /// it will not flip the [`Flag::OptionsEnabled`] flag.
    pub fn raw_arg(&mut self) -> Option<OsString> {
        match self.state {
            State::Default | State::ArgPause => self.next_arg_and_reset_state(),
            State::ExplicitOptionValue | State::ShortOptChain(_) => None,
        }
    }

    /// This checks if the parser currently looks at what looks like a normal argument.
    ///
    /// This will return false if the parser reached the end of the command line
    /// or if the parser looks at something that looks like an argument.  The parser
    /// itself never has such a heuristic but for multi-value parsing to options
    /// this can come in handy.
    ///
    /// It's generally strongly recommended not to create command line tools that
    /// depend on such a behavior.
    pub fn looks_at_value(&self) -> bool {
        let current = match self.current_arg.as_ref() {
            None => return false,
            Some(arg) => arg,
        };
        if !self.get_flag(Flag::OptionsEnabled) {
            true
        } else {
            match self.state {
                State::ExplicitOptionValue | State::ShortOptChain(_) => true,
                State::Default | State::ArgPause => {
                    if current == "--" {
                        !self.get_flag(Flag::HandleDoubleDash)
                    } else {
                        !self.considered_opt(current)
                    }
                }
            }
        }
    }

    /// Returns `true` if the parser reached the end.
    ///
    /// At that point [`param`](Self::param) will always return `None`
    /// and [`value`](Self::value) will fail with an error.
    #[inline]
    pub fn finished(&self) -> bool {
        self.current_arg.is_none()
    }

    /// Check if a parsing [`Flag`] is currently set.
    #[inline]
    pub fn get_flag(&self, flag: Flag) -> bool {
        self.flags & flag.as_u8() != 0
    }

    /// Sets or unsets a parsing [`Flag`].
    #[inline]
    pub fn set_flag(&mut self, flag: Flag, yes: bool) {
        if yes {
            self.flags |= flag.as_u8();
        } else {
            self.flags &= !flag.as_u8();
        }
    }

    /// Moves ahead one argument and resets the internal state.
    fn next_arg_and_reset_state(&mut self) -> Option<OsString> {
        self.state = State::Default;
        replace(&mut self.current_arg, self.args.next())
    }

    /// Sets the state for arg pause and returns an arg param.
    ///
    /// This also flips options_allowed if necessary.
    fn pause_for_arg(&mut self) -> Param {
        self.state = State::ArgPause;
        if self.get_flag(Flag::DisableOptionsAfterArgs) {
            self.set_flag(Flag::OptionsEnabled, false);
        }
        Param::Arg
    }

    /// Should we consider the given argument as an option?
    fn considered_opt(&self, s: &OsStr) -> bool {
        let arg_bytes = s.as_encoded_bytes();
        arg_bytes.len() > 1
            && arg_bytes.first() == Some(&b'-')
            && (!self.get_flag(Flag::DisableNumericOptions)
                || arg_bytes.get(1).map_or(true, |x| !x.is_ascii_digit()))
    }

    /// Attach the name of the current parameter to the error if needed.
    fn augment_error(&self, err: Error) -> Error {
        if err.param().is_none() {
            if let Some(ref param) = self.last_param {
                return err.with_param(param.clone());
            }
        }
        err
    }
}

fn parse_string<V>(value: String) -> Result<V, Error>
where
    V: FromStr,
    V::Err: Into<Box<dyn StdError + Send + Sync + 'static>>,
{
    V::from_str(&value).map_err(|err| {
        Error::new(ErrorKind::InvalidValue)
            .with_string(value)
            .with_source(err.into())
    })
}

/// Gets a single unicode character at an offset in the OsStr.
fn os_str_char_at(s: &OsStr, idx: usize) -> Result<Option<char>, Error> {
    let prefix = match s.as_encoded_bytes().get(idx..) {
        Some(b) => b.get(..4).unwrap_or(b),
        None => return Ok(None),
    };
    // SAFETY: up to the given byte, we know the utf-8 is valid
    let prefix = match from_utf8(prefix) {
        Ok(prefix) => prefix,
        Err(err) => match err.valid_up_to() {
            0 => return Err(Error::new(ErrorKind::InvalidUnicode).with_os_string(s.into())),
            n => unsafe { from_utf8_unchecked(&prefix[..n]) },
        },
    };
    Ok(prefix.chars().next())
}

/// Splits a OsStr at a point into a prefix that is utf-8, and the rest.
fn os_str_split_utf8_prefix(s: &OsStr, point: usize) -> Result<(&str, &OsStr), Error> {
    let b = s.as_encoded_bytes();
    let s1 = from_utf8(&b[..point])
        .map_err(|_| Error::new(ErrorKind::InvalidUnicode).with_os_string(s.into()))?;
    let s2 = &b[point..];
    // SAFETY: because s1 is valid utf-8 as checked per from_utf8, we
    // can safely restore the rest of the OsStr as OsStr.
    Ok((s1, unsafe { OsStr::from_encoded_bytes_unchecked(s2) }))
}

/// Strip a prefix from an OsStr
fn os_str_strip_prefix<'s>(s: &'s OsStr, prefix: &str) -> &'s OsStr {
    let b = s.as_encoded_bytes();
    // SAFETY: stripping an utf-8 prefix leaves a well formed OsStr behind
    unsafe { OsStr::from_encoded_bytes_unchecked(b.strip_prefix(prefix.as_bytes()).unwrap_or(b)) }
}

fn os_string_into_string(s: OsString) -> Result<String, Error> {
    s.into_string()
        .map_err(|s| Error::new(ErrorKind::InvalidUnicode).with_os_string(s))
}
