use serde::{de, ser};
use std::fmt;

use crate::tokenizer::TokenSpan;

pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Generic error returned by the configuration parser.
#[derive(Clone, Debug)]
pub struct Error {
    pub span: Option<TokenSpan>,
    pub msg: String,
}

impl Error {
    pub fn new(msg: impl Into<String>, span: TokenSpan) -> Error {
        Error {
            span: Some(span),
            msg: msg.into(),
        }
    }
}

impl ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error { msg: msg.to_string(), span: None }
    }
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error { msg: msg.to_string(), span: None }
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
