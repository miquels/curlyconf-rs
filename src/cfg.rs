use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::io::{self, Error as IoError, ErrorKind as Kind};

use serde::de::{self, Visitor};
use serde::Deserialize;

use crate::de::{Deserializer, MAGIC_SECTION_NAME};
use crate::error::Result;

/// Config file parser variant.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[non_exhaustive]
pub enum Mode {
    /// variable settings end in a newline.
    Newline,
    /// variable settings must be terminated with a ';'
    Semicolon,
    #[doc(hidden)]
    Diablo,
}

/// Read configuration from a string.
pub fn from_str<T>(s: &str, mode: Mode) -> Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    let mut deserializer = Deserializer::from_str(s, mode);
    T::deserialize(&mut deserializer)
}

/// Read configuration from a file.
pub fn from_file<T>(name: impl Into<String>, mode: Mode) -> io::Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    let name = name.into();
    let data = fs::read(&name)
        .map_err(|e| IoError::new(e.kind(), format!("{}: {}", name, e)))?;
    let text = String::from_utf8(data)
        .map_err(|_| IoError::new(Kind::Other, format!("{}: utf-8 error", name)))?;
    let mut deserializer = Deserializer::from_str(text, mode);
    T::deserialize(&mut deserializer)
        .map_err(|mut e| { e.file_name = name; e })
        .map_err(|e| IoError::new(Kind::Other, e))
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

//impl<'a, 'de, T> SectionName<'de> for &'a mut T where &'a mut T: de::Deserializer<'de> {
//impl<'a, 'de, T> SectionName<'de> for &'a T where &'a T: de::Deserializer<'de> + Copy {
impl<'a, 'de, T> SectionName<'de> for T where T: de::Deserializer<'de> {
    fn section_name(&self) -> Option<String> {
        let this = unsafe { std::ptr::read(self as *const Self) };
        this.deserialize_unit_struct(MAGIC_SECTION_NAME, MyVisitor).unwrap()
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
