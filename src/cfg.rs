use std::collections::{HashMap, HashSet};
use std::io::{self, Error as IoError, ErrorKind as Kind};

use serde::de;
use serde::Deserialize;

use crate::de::{Deserializer, SECTION_CTX};
use crate::error::Result;
use crate::tokenizer::Mode;

/// Read configuration from a string.
///
/// This uses the defaults (e.g. `Mode::Semicolon`). If you want to
/// configure the configuration parser, use a [`Builder`] struct.
///
/// [`Builder`]: struct.Builder.html
pub fn from_str<T>(s: &str) -> Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    Builder::new().from_str(s)
}

/// Read configuration from a file.
///
/// This uses the defaults (e.g. `Mode::Semicolon`). If you want to
/// configure the configuration parser, use a [`Builder`] struct.
///
/// [`Builder`]: struct.Builder.html
pub fn from_file<T>(name: impl Into<String>) -> io::Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    Builder::new().from_file(name)
}

/// Configuration builder.
pub struct Builder {
    mode: Mode,
    aliases: HashMap<String, String>,
    ignored: HashSet<String>,
}

impl Builder {
    /// Return a new configuration builder.
    pub fn new() -> Builder {
        Builder {
            mode: Mode::Semicolon,
            aliases: HashMap::new(),
            ignored: HashSet::new(),
        }
    }

    /// Set the mode of this configuration file parser:
    /// - `Mode::Semicolon`: values must end in `;`.
    /// - `Mode::Newline`: values end with a newline.
    pub fn mode(mut self, mode: Mode) -> Builder {
        self.mode = mode;
        self
    }

    /// Add an alias for a section name or value name.
    ///
    /// This is much like the `#[serde(alias = "foo")]` attribute, but since we
    /// need to know what aliases exist, and serde does not provide that
    /// information, we need to implement the aliasing ourself.
    pub fn alias<T>(mut self, alias: &str, section_name: &str) -> Builder {
        let type_name = std::any::type_name::<T>().split(":").last().unwrap();
        let type_dot_alias = format!("{}.{}", type_name, alias);
        self.aliases
            .insert(type_dot_alias, section_name.to_string());
        self
    }

    /// Ignore a value.
    pub fn ignore<T>(mut self, key: &str) -> Builder {
        let type_name = std::any::type_name::<T>().split(":").last().unwrap();
        let type_dot_alias = format!("{}.{}", type_name, key);
        self.ignored.insert(type_dot_alias);
        self
    }

    /// This concludes the building phase and reads the configuration from a string.
    pub fn from_str<T>(self, text: &str) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let mut deserializer =
            Deserializer::from_str(text, self.mode, self.aliases, self.ignored);
        T::deserialize(&mut deserializer)
    }

    /// This concludes the building phase and reads the configuration from a file.
    pub fn from_file<T>(self, name: impl Into<String>) -> io::Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let mut deserializer =
            Deserializer::from_file(name, self.mode, self.aliases, self.ignored)?;
        T::deserialize(&mut deserializer)
            .map_err(|e| IoError::new(Kind::Other, e))
    }
}

/// Make it possible for `Deserialize` impls to access the state of the parser.
///
/// Automatically implemented on every `Deserializer` implementation.
pub trait ParserAccess {
    /// Get the name of the value that's currently being parsed.
    ///
    /// Useful in `Deserialize` implementations. You can have one field
    /// with several aliases (see `Builder::alias`) and differentiate
    /// the action in `Deserialize::deserialize` based on which alias it is.
    ///
    /// For example, a `groups: Vec<String>` value, with `addgroup` and
    /// `delgroup` aliases.
    fn value_name(&self) -> String {
        SECTION_CTX.with(|ctx| ctx.borrow().subsection_name().to_string())
    }

    /// Get a direct reference to the parser instead of via the Deserializer handle
    /// so that it can be stored somewhere (usually in a visitor struct).
    fn parser(&self) -> Parser {
        Parser
    }
}

impl<'de, T> ParserAccess for T where T: de::Deserializer<'de> {}

/// Reference to the parser for `Deserialize` impls.
///
/// Implements [`ParserAccess`].
///
/// [`ParserAccess`]: trait.ParserAccess.html
pub struct Parser;
impl ParserAccess for Parser {}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::Deserialize;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_struct() {
        init();

        #[derive(Deserialize, PartialEq, Debug)]
        struct Main {
            test: Test,
        }

        #[derive(Deserialize, PartialEq, Debug)]
        #[serde(rename = "test")]
        struct Test {
            __label__: String,
            int: u32,
            seq: Vec<String>,
        }

        let j = r#"test foo {
            int 1;
            seq a,"b";
        }"#;
        let expected = Main {
            test: Test {
                __label__: "foo".to_owned(),
                int: 1,
                seq: vec!["a".to_owned(), "b".to_owned()],
            },
        };
        assert_eq!(expected, from_str(j).unwrap());
    }
}
