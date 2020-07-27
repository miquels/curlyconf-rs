use serde::{de, ser};
use std::fmt;

use crate::tokenizer::TokenPos;

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Clone, Debug)]
pub struct Error {
    pub(crate) pos: TokenPos,
    pub(crate) msg: String,
}

impl Error {
    pub fn new(msg: impl Into<String>, pos: &TokenPos) -> Error {
        Error {
            pos: pos.clone(),
            msg: msg.into(),
        }
    }
}

impl ser::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::new(msg.to_string(), &TokenPos::none())
    }
}

impl de::Error for Error {
    fn custom<T: fmt::Display>(msg: T) -> Self {
        Error::new(msg.to_string(), &TokenPos::none())
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.pos.line == 0 {
            write!(f, "{}", self.msg)
        } else {
            write!(f, "{}:{}: {}", self.pos.line, self.pos.column, self.msg)
        }
    }
}

impl std::error::Error for Error {}
