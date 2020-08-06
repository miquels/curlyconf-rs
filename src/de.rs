use std::fs;
use std::io::{self, Error as IoError, ErrorKind as Kind};
use std::str::FromStr;

use serde::de::{self, DeserializeSeed, IntoDeserializer, MapAccess, SeqAccess, Visitor};
use serde::Deserialize;

use crate::error::{Error, Result};
use crate::parser::Parser;
use crate::tokenizer::{Tokenizer, Token, TokenType, TokenPos};

pub struct Deserializer {
    parser: Parser,
    eov: TokenType,
    level: u32,
    cur_name: Option<Token>,
    __label__: Option<Token>,
    #[allow(dead_code)] // TODO
    mode: Mode,
}

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

impl Deserializer {
    fn from_str(s: impl Into<String>, mode: Mode) -> Self {
        let tokenizer = Tokenizer::from_string(s, mode != Mode::Semicolon);
        let parser = Parser::new(tokenizer);
        let eov = if mode == Mode::Semicolon { TokenType::Semi } else { TokenType::Nl };
        Deserializer {
            parser,
            eov,
            level: 0,
            cur_name: None,
            __label__: None,
            mode,
        }
    }
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

impl Deserializer {
    fn parse_expr<T>(&mut self, name: &str) -> Result<(TokenPos, T)>
    where
        T: FromStr,
    {
        let word = self.parser.expect(TokenType::Word)?;
        let value: T = FromStr::from_str(&word.value)
            .map_err(|_| Error::new(format!("expected {} value", name), word.pos))?;
        Ok((word.pos, value))
    }
}

fn update_pos(mut e: Error, pos: TokenPos) -> Error {
    //println!("update_pos {:?} {:?}", e, pos);
    if e.pos.offset == 0 {
        e.pos = pos;
    }
    e
}

fn update_tpos(mut e: Error, token: &Option<Token>) -> Error {
    //println!("update_tpos {:?} {:?}", e, token);
    if e.pos.offset == 0 {
        e.pos = token.as_ref().map(|t| t.pos).unwrap_or(TokenPos::none());
    }
    e
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer {
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let value = self.parser.expect(TokenType::Word)?;
        Err(Error::new("BUG: serde called deserialize_any", value.pos))
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("boolean")?;
        visitor.visit_bool(value).map_err(|e| update_pos(e, pos))
    }

    // The `parse_expr` function is generic over the type `T` so here
    // it is invoked with `T=i8`. The next 8 methods are similar.
    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("i8 integer")?;
        visitor.visit_i8(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("i16 integer")?;
        visitor.visit_i16(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("i32 integer")?;
        visitor.visit_i32(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("i64 integer")?;
        visitor.visit_i64(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("u8 integer")?;
        visitor.visit_u8(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("u16 integer")?;
        visitor.visit_u16(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("u32 integer")?;
        visitor.visit_u32(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("u64 integer")?;
        visitor.visit_u64(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("f32 float")?;
        visitor.visit_f32(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr("f64 float")?;
        visitor.visit_f64(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // Parse a string, check that it is one character, call `visit_char`.
        let (pos, value) = self.parse_expr("single character")?;
        visitor.visit_char(value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr::<String>("string")?;
        visitor.visit_str(&value).map_err(|e| update_pos(e, pos))
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (pos, value) = self.parse_expr::<String>("string")?;
        visitor.visit_str(&value).map_err(|e| update_pos(e, pos))
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
    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        unimplemented!()
    }

    // newtype struct. use defaults.
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
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
        // Peek ahead to see if this is a value or a section.
        let mut lookahead = self.parser.lookahead();
        let is_section =
            lookahead.peek(TokenType::LcBrace)?.is_some() ||
            lookahead.peek2(TokenType::Expr, TokenType::LcBrace)?.is_some();
        let pos = self.parser.save_pos();

        // Give the visitor access to each element of the sequence.
        if is_section {
            visitor.visit_seq(SectionAccess::new(&mut self, None, None))
        } else {
            visitor.visit_seq(ListAccess::new(&mut self))
        }.map_err(|e| update_pos(e, pos))
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
        log::debug!("deserialize_map");

        // see if it starts with a label.
        let label = match self.parser.peek()? {
            Some(token) if token.ttype == TokenType::Word => {
                Some(self.parser.expect(TokenType::Word)?)
            },
            _ => None,
        };

        // then an opening brace.
        self.parser.expect(TokenType::LcBrace)?;

        // Give the visitor access to each entry of the map.
        let pos = self.cur_name.as_ref().map(|n| n.pos).unwrap_or(TokenPos::none());
        let sa = SectionAccess::new(&mut self, label.map(|t| t.value), None);
        let value = visitor.visit_map(sa).map_err(|e| update_pos(e, pos))?;

        // Parse the closing brace of the map, but don't consume it.
        let mut lookahead = self.parser.lookahead();
        lookahead.peek(TokenType::RcBrace)?;
        lookahead.end()?;

        Ok(value)
    }

    // A struct is like a map. The difference is the __label__ field. If it's
    // not present in the struct, you cannot have a name before the opening brace.
    // Also if it _is_ present in the struct, we must have a label.
    fn deserialize_struct<V>(
        mut self,
        _name: &'static str,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        log::debug!("deserialize_struct({})", _name);

        let mut label = None;
        if self.level > 0 {
            // see if it starts with a label.
            let mut lookahead = self.parser.lookahead();
            let token = lookahead.peek(TokenType::Expr)?;
            log::debug!("deserialize_struct({}): peeked: {:?}", _name, token);
            if let Some(lbl) = token {
                // yes. so "fields" must contain "__label__".
                lookahead.advance();
                if !fields.contains(&"__label__") {
                    log::debug!("deserialize_struct({}): fields has no __label__", _name);
                    return Err(Error::new(format!("expected '{{'"), lbl.pos));
                }
                log::debug!("fields is OK");
                label = Some(lbl.value);
            } else {
                // no, so if there's a "__label__" field return an error.
                if fields.contains(&"__label__") {
                    // this will generate "expected expression"
                    lookahead.end()?;
                }
            }

            // then an opening brace.
            self.parser.expect(TokenType::LcBrace)?;
        }

        // Give the visitor access to each entry of the map.
        let saved_cur_name = self.cur_name.clone();
        self.level += 1;
        let res = visitor.visit_map(SectionAccess::new(&mut self, label, Some(fields)));
        self.level -= 1;
        self.cur_name = saved_cur_name;
        log::debug!("XXX reset cur_name to {:?}", self.cur_name);
        let value = res.map_err(|e| update_tpos(e, &self.cur_name))?;

        log::debug!("XX level is {}", self.level);
        if self.level != 0 {
            // Parse the closing brace of the map.
            let mut lookahead = self.parser.lookahead();
            lookahead.peek(TokenType::RcBrace)?;
            lookahead.end()?;
        }

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
        Err(Error::new(
            "BUG: serde called deserialize_ignored_any",
            word.pos,
        ))
    }
}

// This handles a comma-separated list.
struct ListAccess<'a> {
    de: &'a mut Deserializer,
    first: bool,
}

impl<'a> ListAccess<'a> {
    fn new(
        de: &'a mut Deserializer,
    ) -> Self {
        ListAccess {
            de,
            first: true,
        }
    }
}

// `SeqAccess` iterates through elements of the sequence.
// It checks that every element is preceded by the separator, except the first one.
impl<'de, 'a> SeqAccess<'de> for ListAccess<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        if !self.first {
            let mut lookahead = self.de.parser.lookaheadnl();

            // end-of-value? (eg ';' or '\n')
            if let Some(_) = lookahead.peek(self.de.eov)? {
                return Ok(None);
            }

            // comma must be next.
            if let Some(_) = lookahead.peek(TokenType::Comma)? {
                lookahead.advance();
            }
            lookahead.end()?;
        }
        self.first = false;

        // Deserialize an array element.
        let token = self.de.parser.peek()?;
        seed.deserialize(&mut *self.de).map(Some).map_err(|e| update_tpos(e, &token))
    }
}

struct SectionAccess<'a> {
    de: &'a mut Deserializer,
    label: Option<String>,
    first: bool,
    name: Option<Token>,
    fields: Option<&'static [&'static str]>,
}

impl<'a> SectionAccess<'a> {
    fn new(
        de: &'a mut Deserializer,
        label: Option<String>,
        fields: Option<&'static [&'static str]>,
    ) -> Self {
        log::debug!("SectionAccess::new");
        SectionAccess {
            name: de.cur_name.clone(),
            label,
            de,
            first: true,
            fields,
        }
    }
}

// `SeqAccess` iterates through elements of the sequence of sections.
impl<'de, 'a> SeqAccess<'de> for SectionAccess<'a> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        log::debug!("SectionAccess::SeqAccess::next_element_seed {:?}", self.name);

        if !self.first {
            // eat the closing-brace of the previous struct.
            let saved_pos = self.de.parser.save_pos();
            self.de.parser.expect(TokenType::RcBrace)?;

            // See if the next section is a continuation.
            if let Some(ref name) = self.name {
                let mut continuation = false;
                let mut lookahead = self.de.parser.lookahead();
                if let Some(token) = lookahead.peek(TokenType::Ident)? {
                    if &token.value == &name.value {
                        continuation = true;
                        lookahead.advance();
                    }
                }
                if !continuation {
                    // Undo the eating of the closing brace.
                    // This is so that both a section{} and a list of section{}s
                    // end in a closing brace, which is a terminating token, like ';' is.
                    self.de.parser.restore_pos(saved_pos);
                    log::debug!("SectionAccess::SeqAccess: end of section {:?}", name);
                    return Ok(None);
                }
            }
        }
        self.first = false;

        // Deserialize an array element.
        let token = self.de.parser.peek()?;
        seed.deserialize(&mut *self.de).map(Some).map_err(|e| update_tpos(e, &token))
    }
}

// This handles the ';' or '\n' separated key/value fields of a section.
impl<'de, 'a> MapAccess<'de> for SectionAccess<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        log::debug!("SectionAccess::MapAccess::next_key_seed");

        // if the struct has a __label__ field, insert field name (once!)
        if self.first && self.label.is_some() {
            let de = "__label__".into_deserializer();
            return seed.deserialize(de).map(Some);
        }

        // Separator is required between key/value entries (';' or '\n')
        if !self.first {
            // if this was a section, it ends in '}' instead of ';'
            let mut expect = self.de.eov;
            if let Some(token) = self.de.parser.peeknl()? {
                if token.ttype == TokenType::RcBrace {
                    expect = TokenType::RcBrace;
                }
            }
            self.de.parser.expect(expect)?;
        }
        self.first = false;

        // if we're at root-level, check for end-of-file.
        log::debug!("check for EOF");
        if self.de.level == 1 && self.de.parser.is_eof() {
            return Ok(None);
        }
        log::debug!("check for EOF done");

        let mut lookahead = self.de.parser.lookahead();

        // Expect an ident.
        let token = lookahead.peek(TokenType::Ident)?;
        if let Some(ref token) = token {

            // yes, we have an identifier. must be one of the struct fieldnames.
            let fields = self.fields.as_ref().unwrap();
            if !fields.contains(&(token.value.as_str()))
            {
                // advance parser, then generate error.
                lookahead.advance();
                println!("XXX unknown key {:?} -- {:?}", token, fields);
                return Err(Error::new(
                    format!("unknown key: `{}'", token.value),
                    token.pos,
                ));
            }
            log::debug!("XXX set cur_name to {}", token.value);
            self.de.cur_name = Some(token.clone());
        }
        // Or a closing brace.
        if lookahead.peek(TokenType::RcBrace)?.is_some() {
            return Ok(None);
        }
        lookahead.end()?;

        // Deserialize a map key.
        seed.deserialize(&mut *self.de).map(Some).map_err(|e| update_tpos(e, &token))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        log::debug!("SectionAccess::MapAccess::next_value_seed {}", std::any::type_name::<V::Value>());

        // if the struct has a __label__ field, insert label value (once!)
        if let Some(label) = self.label.take() {
            let de = label.into_deserializer();
            return seed.deserialize(de)
        }

        // Deserialize a map value.
        let token = self.de.parser.peek()?;
        seed.deserialize(&mut *self.de).map_err(|e| update_tpos(e, &token))
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
