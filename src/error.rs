use std::fmt;

use crate::tokenizer::TokenSpan;

pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Error returned by the configuration parser.
///
/// The [`std::fmt::Display`] implementation prints a diagnostic including filename,
/// line number and column.
#[derive(Clone, Debug)]
pub struct Error {
    pub(crate) span: Option<TokenSpan>,
    pub(crate) msg: String,
}

impl Error {
    pub(crate) fn new(msg: impl Into<String>, span: TokenSpan) -> Error {
        Error {
            span: Some(span),
            msg: msg.into(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(span) = self.span.as_ref() {
            write!(
                f,
                "{}:{}:{}: {}",
                span.source.filename, span.line, span.column, self.msg
            )
        } else {
            write!(f, "{}", self.msg)
        }
    }
}

impl std::error::Error for Error {}
