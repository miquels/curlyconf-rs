use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{self, Error as IoError, ErrorKind as Kind};

use serde::de;
use serde::Deserialize;

use crate::de::{Deserializer, SECTION_CTX};
use crate::error::Result;
use crate::tokenizer::Mode;

/// Read configuration from a string.
pub fn from_str<T>(s: &str) -> Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    Builder::new().from_str(s)
}

/// Read configuration from a file.
pub fn from_file<T>(name: impl Into<String>) -> io::Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    Builder::new().from_file(name)
}

pub struct Builder {
    mode: Mode,
    aliases: HashMap<String, String>,
    sections: HashSet<String>,
}

impl Builder {
    pub fn new() -> Builder {
        Builder {
            mode: Mode::Semicolon,
            aliases: HashMap::new(),
            sections: HashSet::new(),
        }
    }

    pub fn mode(mut self, mode: Mode) -> Builder {
        self.mode = mode;
        self
    }

    pub fn alias<T>(mut self, alias: &str, section_name: &str) -> Builder {
        let type_name = std::any::type_name::<T>().split(":").last().unwrap();
        let type_dot_alias = format!("{}.{}", type_name, alias);
        self.aliases.insert(type_dot_alias, section_name.to_string());
        self
    }

    pub fn section(mut self, section_name: impl Into<String>) -> Builder {
        self.sections.insert(section_name.into());
        self
    }

    pub fn from_str<T>(self, text: &str) -> Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let mut deserializer = Deserializer::from_str(text, self.mode, self.aliases, self.sections);
        T::deserialize(&mut deserializer)
    }

    pub fn from_file<T>(self, name: impl Into<String>) -> io::Result<T>
    where
        T: for<'de> Deserialize<'de>,
    {
        let name = name.into();
        let data =
            fs::read(&name).map_err(|e| IoError::new(e.kind(), format!("{}: {}", name, e)))?;
        let text = String::from_utf8(data)
            .map_err(|_| IoError::new(Kind::Other, format!("{}: utf-8 error", name)))?;
        let mut deserializer = Deserializer::from_str(text, self.mode, self.aliases, self.sections);
        T::deserialize(&mut deserializer)
            .map_err(|mut e| {
                e.file_name = name;
                e
            })
            .map_err(|e| IoError::new(Kind::Other, e))
    }
}

/// Helper trait to get at the current section name.
///
/// Automatically implemented for everything that implements `Deserialize`.
pub trait SectionName<'de>: de::Deserializer<'de> {
    /// Get the current section name.
    fn section_name(&self) -> String;
}

impl<'a, 'de, T> SectionName<'de> for T
where
    T: de::Deserializer<'de>,
{
    fn section_name(&self) -> String {
        SECTION_CTX.with(|ctx| {
            ctx.borrow().subsection_name().to_string()
        })
    }
}

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
        };"#;
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
