use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fs;
use std::io::{self, Error as IoError, ErrorKind as Kind};

use serde::de::{self, Visitor};
use serde::Deserialize;

use crate::de::{Deserializer, MAGIC_SECTION_NAME};
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

    pub fn alias(mut self, alias: impl Into<String>, section_name: impl Into<String>) -> Builder {
        self.aliases.insert(alias.into(), section_name.into());
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

// We need a "backdoor" into our Deserializer. It would be great if
// there was a "custom" method we could call, but alas. So we abuse
// the deserialize_unit_struct method. If it gets passed a magic `name`,
// then it calls visitor.visit_str(cur_name).
struct MyVisitor;

impl<'de> Visitor<'de> for MyVisitor {
    type Value = Option<String>;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("nothing")
    }

    // Only used to pass a string back.
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        Ok(if v != "" { Some(v.to_string()) } else { None })
    }
}

/// Helper trait to get at the current section name.
///
/// Automatically implemented for everything that implements `Deserialize`.
pub trait SectionName<'de>: de::Deserializer<'de> {
    /// Get the current section name.
    fn section_name(&self) -> Option<String>;
}

impl<'a, 'de, T> SectionName<'de> for T
where
    T: de::Deserializer<'de>,
{
    fn section_name(&self) -> Option<String> {
        //
        // `self` is already a reference, see crate::de, where the impl is
        // `impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer`.
        // So &self is a reference to _that_, and we need to unref one
        // level. If anyone knows a _safe_ solution - let me know!
        //
        let this = unsafe { std::ptr::read(self as *const Self) };
        this.deserialize_unit_struct(MAGIC_SECTION_NAME, MyVisitor)
            .unwrap()
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
