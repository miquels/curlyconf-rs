use std::str::FromStr;

use serde::Deserialize;
use serde::de::{
    self, DeserializeSeed, MapAccess, SeqAccess, Visitor,
};

use crate::error::{Error, Result};
use crate::parser::{self, Parser};
use crate::tokenizer::{Token, TokenType, Tokenizer};

pub struct Deserializer {
    parser: Parser,
    eov:    TokenType,
}

impl Deserializer {
    pub fn from_str(s: &str) -> Self {
        let t = Tokenizer::from_string(s);
        let parser = Parser::new(t);
        Deserializer {
            parser,
            eov: TokenType::Comma,
        }
    }
}

pub fn from_str<T>(s: &str) -> Result<T>
where
    T: for <'de> Deserialize<'de>,
{
    let mut deserializer = Deserializer::from_str(s);
    let t = T::deserialize(&mut deserializer)?;
    deserializer.parser.expect_eof()?;
    Ok(t)
}

impl Deserializer {
    fn parse_expr<T>(&mut self, name: &str) -> Result<T> where T: FromStr {
        let word = self.parser.expect(TokenType::Word)?;
        let value: T = FromStr::from_str(&word.value)
            .map_err(|_| Error::new(format!("expected {} value", name), &word.pos))?;
        Ok(value)
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer {
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let word = self.parser.expect(TokenType::Word)?;
        Err(Error::new("BUG: serde called deserialize_any", &word.pos))
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_bool(self.parse_expr("boolean")?)
    }

    // The `parse_expr` function is generic over the type `T` so here
    // it is invoked with `T=i8`. The next 8 methods are similar.
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i8(self.parse_expr("i8 integer")?)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i16(self.parse_expr("i16 integer")?)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i32(self.parse_expr("i32 integer")?)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_i64(self.parse_expr("i64 integer")?)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u8(self.parse_expr("u8 integer")?)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u16(self.parse_expr("u16 integer")?)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u32(self.parse_expr("u32 integer")?)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_u64(self.parse_expr("u64 integer")?)
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f32(self.parse_expr("f32 integer")?)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_f64(self.parse_expr("f64 integer")?)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Parse a string, check that it is one character, call `visit_char`.
        visitor.visit_char(self.parse_expr("single character")?)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let s: String = self.parse_expr("string")?;
        visitor.visit_str(&s)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_string(self.parse_expr("string")?)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    // We always parse any value as Some(value). Optional values must
    // have the '#[serde(default)]' field attribute, so that a value
    // that's not present gets set to `None'.
    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_some(self)
    }

    // In Serde, unit means an anonymous value containing no data.
    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    // Unit struct means a named value containing no data.
    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    // newtype struct. use defaults.
    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    // Deserialization of compound types like sequences and maps happens by
    // passing the visitor an "Access" object that gives it the ability to
    // iterate through the data contained in the sequence.
    fn deserialize_seq<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
            // Give the visitor access to each element of the sequence.
            let value = visitor.visit_seq(Separated::new(&mut self, TokenType::Comma))?;

            // Must end with ';' or '\n'.
            // XXX FIXME not for:
            // - sequences of sections
            // - map values that are sequences (since we already check they end with ';')
            // - are there any left?
            // self.parser.expect(self.eov)?;

            Ok(value)
    }

    // Tuples look just like sequences.
    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    // Tuple structs look just like sequences.
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    // Much like `deserialize_seq` but calls the visitors `visit_map` method
    // with a `MapAccess` implementation, rather than the visitor's `visit_seq`
    // method with a `SeqAccess` implementation.
    fn deserialize_map<V>(mut self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // if it starts with a name, remember it.
        let name = match self.parser.peek()? {
            Some(token) if token.ttype == TokenType::Word => Some(token),
            _ => None,
        };

        // then an opening brace.
        self.parser.expect(TokenType::LcBrace)?;

        // push "__name__ value;" if the section has a name.
        if let Some(name) = name {
            self.parser.push_token(Token::new(TokenType::Word, "__name__", self.parser.t.pos));
            self.parser.push_token(name);
            self.parser.push_token(Token::new(self.eov, ";", self.parser.t.pos));
        }

        // Give the visitor access to each entry of the map.
        let eov = self.eov;
        let value = visitor.visit_map(Separated::new(&mut self, eov))?;

        // Parse the closing brace of the map.
        self.parser.expect(TokenType::RcBrace)?;

        Ok(value)
    }

    // A struct is like a map. The difference is the __name__ field. If it's
    // not present in the struct, you cannot have a name before the opening brace.
    // Also if it _is_ present in the struct, we must have a name.
    fn deserialize_struct<V>(
        mut self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // see if it starts with a name.
        let name = match self.parser.peek()? {
            Some(token) if token.ttype == TokenType::Word => {
                // yes. so "fields" must contain "__name__".
                if !fields.contains(&"__name__") {
                    return Err(Error::new(format!("expected '{{'"), &self.parser.t.pos));
                }
                Some(token)
            },
            _ => {
                // no, so if there's a "__name__" field return an error.
                if fields.contains(&"__name__") {
                    return Err(Error::new(format!("expected section name"), &self.parser.t.pos));
                }
                None
            },
        };

        // then an opening brace.
        self.parser.expect(TokenType::LcBrace)?;

        // push "__name__ value;" if the section has a name.
        if let Some(name) = name {
            self.parser.push_token(Token::new(TokenType::Word, "__name__", self.parser.t.pos));
            self.parser.push_token(name);
            self.parser.push_token(Token::new(self.eov, ";", self.parser.t.pos));
        }

        // Give the visitor access to each entry of the map.
        let eov = self.eov;
        let value = visitor.visit_map(Separated::new(&mut self, eov))?;

        // Parse the closing brace of the map.
        self.parser.expect(TokenType::RcBrace)?;

        Ok(value)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    // An identifier in Serde is the type that identifies a field of a struct or the
    // variant of an enum. Struct fields and enum variants are represented as strings.
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    // We don't support this.
    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let word = self.parser.expect(TokenType::Word)?;
        Err(Error::new("BUG: serde called deserialize_ignored_any", &word.pos))
    }
}

// This handles a separated list.
struct Separated<'a> {
    de: &'a mut Deserializer,
    sep: TokenType,
    first: bool,
}

impl<'a> Separated<'a> {
    fn new(de: &'a mut Deserializer, separator: TokenType) -> Self {
        Separated {
            de,
            sep: separator,
            first: true,
        }
    }
}

// `SeqAccess` iterates through elements of the sequence.
// It checks that every element is preceded by the separator, except the first one.
impl<'de, 'a> SeqAccess<'de> for Separated<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        if !self.first {
            // After the first element there must be a separator. If not, simply return.
            self.de.parser.start_try();
            if self.de.parser.try_expect(self.sep)?.is_none() {
                return Ok(None);
            }
            self.de.parser.end_try()?;
        }
        self.first = false;

        // Deserialize an array element.
        seed.deserialize(&mut *self.de).map(Some)
    }
}

// `MapAccess` iterates through elements of the sequence.
// It checks that every key/value is followed by a separator.
impl<'de, 'a> MapAccess<'de> for Separated<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        // Separator is required after every entry.
        if !self.first {
            self.de.parser.expect(self.sep)?;
        }
        self.first = false;

        // If it's not an ident we're at the end of the section.
        match self.de.parser.peek() {
            Ok(Some(mut token)) => {
                if !parser::is_ident(&mut token) {
                    return Ok(None);
                }
            },
            _ => return Ok(None),
        }

        // Deserialize a map key.
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        // Deserialize a map value.
        seed.deserialize(&mut *self.de)
    }
}

#[test]
fn test_struct() {
    #[derive(Deserialize, PartialEq, Debug)]
    #[serde(rename = "test")]
    struct Test {
        __name__: String,
        int: u32,
        seq: Vec<String>,
    }

    let j = r#"test foo {
        int 1;
        seq a, "b";
    }"#;
    let expected = Test {
        __name__: "foo".to_owned(),
        int: 1,
        seq: vec!["a".to_owned(), "b".to_owned()],
    };
    assert_eq!(expected, from_str(j).unwrap());
}

