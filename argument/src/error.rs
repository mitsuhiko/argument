use std::fmt;

type BoxedStdError = Box<dyn std::error::Error + Send + Sync + 'static>;

/// The kind of an [`Error`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum ErrorKind {
    /// Help was requested.  The message is the help page.
    Help,
    /// The version was requested.  The message is the version string.
    Version,
    /// An option that is not known was passed.
    UnknownOption,
    /// A command that is not known was passed.
    UnknownCommand,
    /// A positional argument was passed that is not expected.
    UnexpectedArgument,
    /// A value was attached to a flag (`--flag=value`).
    UnexpectedValue,
    /// An option did not get its value.
    MissingValue,
    /// A required argument or option is missing.
    MissingArgument,
    /// A value could not be parsed.
    InvalidValue,
    /// A custom error.
    Custom,
}

/// Context needed to render an error with usage information.
#[derive(Debug, Clone)]
pub(crate) struct Context {
    pub(crate) prog: String,
    pub(crate) usage: String,
    pub(crate) help_flag: Option<&'static str>,
}

struct Repr {
    kind: ErrorKind,
    message: String,
    tip: Option<String>,
    context: Option<Context>,
    source: Option<BoxedStdError>,
}

/// An error produced while parsing the command line.
///
/// Requests for help and version are also represented as errors so that
/// they can be propagated with `?`.  Use [`exit`](Self::exit) to print the
/// error (or help page) and exit with the right status code.
pub struct Error(Box<Repr>);

impl Error {
    /// Creates a custom error.
    ///
    /// Prefer [`Parser::error`](crate::Parser::error) which attaches usage
    /// information.
    pub fn new(message: impl Into<String>) -> Error {
        Error::with_kind(ErrorKind::Custom, message)
    }

    pub(crate) fn with_kind(kind: ErrorKind, message: impl Into<String>) -> Error {
        Error(Box::new(Repr {
            kind,
            message: message.into(),
            tip: None,
            context: None,
            source: None,
        }))
    }

    pub(crate) fn with_tip(mut self, tip: Option<String>) -> Error {
        self.0.tip = tip;
        self
    }

    pub(crate) fn with_source(mut self, source: BoxedStdError) -> Error {
        self.0.source = Some(source);
        self
    }

    pub(crate) fn with_context(mut self, context: &Context) -> Error {
        if self.0.context.is_none() {
            self.0.context = Some(context.clone());
        }
        self
    }

    /// The kind of error.
    pub fn kind(&self) -> ErrorKind {
        self.0.kind
    }

    /// The error message (or help page / version for those kinds).
    pub fn message(&self) -> &str {
        &self.0.message
    }

    /// An optional tip (eg: a suggestion for a misspelled option).
    pub fn tip(&self) -> Option<&str> {
        self.0.tip.as_deref()
    }

    /// The exit code that should be used for this error.
    ///
    /// This is `0` for help and version and `2` for everything else.
    pub fn exit_code(&self) -> i32 {
        match self.kind() {
            ErrorKind::Help | ErrorKind::Version => 0,
            _ => 2,
        }
    }

    /// Renders the full error message as it would be printed.
    pub fn render(&self) -> String {
        if let ErrorKind::Help | ErrorKind::Version = self.kind() {
            return self.0.message.clone();
        }
        let mut rv = format!("error: {}\n", self.0.message);
        if let Some(ref tip) = self.0.tip {
            rv.push_str(&format!("\n  tip: {}\n", tip));
        }
        if let Some(ref ctx) = self.0.context {
            rv.push_str(&format!("\nUsage: {}\n", ctx.usage));
            if let Some(flag) = ctx.help_flag {
                rv.push_str(&format!(
                    "\nFor more information, try '{} {}'.\n",
                    ctx.prog, flag
                ));
            }
        }
        rv.truncate(rv.trim_end().len());
        rv
    }

    /// Prints the error to stderr (or the help page / version to stdout).
    pub fn print(&self) {
        match self.kind() {
            ErrorKind::Help | ErrorKind::Version => println!("{}", self.render()),
            _ => eprintln!("{}", self.render()),
        }
    }

    /// Prints the error and exits the process with [`exit_code`](Self::exit_code).
    pub fn exit(&self) -> ! {
        self.print();
        std::process::exit(self.exit_code());
    }
}

impl From<&str> for Error {
    fn from(message: &str) -> Error {
        Error::new(message)
    }
}

impl From<String> for Error {
    fn from(message: String) -> Error {
        Error::new(message)
    }
}

impl From<argument_parser::Error> for Error {
    fn from(err: argument_parser::Error) -> Error {
        let kind = match err.kind() {
            argument_parser::ErrorKind::MissingValue => ErrorKind::MissingValue,
            argument_parser::ErrorKind::InvalidValue
            | argument_parser::ErrorKind::InvalidUnicode => ErrorKind::InvalidValue,
            argument_parser::ErrorKind::UnexpectedParam => ErrorKind::UnexpectedArgument,
            _ => ErrorKind::Custom,
        };
        Error::with_kind(kind, format!("{:#}", err)).with_source(Box::new(err))
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0.message)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error")
            .field("kind", &self.0.kind)
            .field("message", &self.0.message)
            .field("tip", &self.0.tip)
            .field("source", &self.0.source)
            .finish()
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self.0.source {
            Some(ref source) => Some(&**source),
            None => None,
        }
    }
}
