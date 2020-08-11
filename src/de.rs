use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::str::FromStr;

use serde::de::{
    self, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess, VariantAccess,
    Visitor,
};

use crate::error::{Error, Result};
use crate::parser::Parser;
use crate::tokenizer::Mode;
use crate::tokenizer::{Token, TokenPos, TokenType, Tokenizer};

// We always save a current copy of the section context as a thread local value.
thread_local!(pub(crate) static SECTION_CTX: RefCell<SectionCtx> = RefCell::new(SectionCtx::empty()));

// Info about the section we're processing.
#[derive(Debug, Clone)]
pub(crate) struct SectionCtx {
    token: Token,
    typename: &'static str,
    level: u32,
}

impl SectionCtx {
    // Initial value.
    fn empty() -> SectionCtx {
        SectionCtx {
            token: Token::new(TokenType::Unknown, "", TokenPos::none()),
            typename: "",
            level: 0,
        }
    }

    // Enter a new section. returns the old section.
    fn new<T>(&mut self) -> SectionCtx {
        let this = self.clone();
        self.token = Token::new(TokenType::Unknown, "", TokenPos::none());
        self.typename = std::any::type_name::<T>().split(":").last().unwrap();
        self.level += 1;
        SECTION_CTX.with(|ctx| *ctx.borrow_mut() = self.clone());
        this
    }

    // Restore old section.
    fn restore(&mut self, scx: SectionCtx) {
        *self = scx;
        SECTION_CTX.with(|ctx| *ctx.borrow_mut() = self.clone());
    }

    // Update the section token (name and position).
    fn update_section_token(&mut self, section_token: Token) {
        self.token = section_token;
        SECTION_CTX.with(|ctx| *ctx.borrow_mut() = self.clone());
    }

    // Return the type name of the current section.
    pub fn section_type(&self) -> &'static str {
        self.typename
    }

    // Return the subsection name (aka struct field) we're processing right now.
    pub fn subsection_name(&self) -> &str {
        self.token.value.as_str()
    }

    // Position.
    fn subsection_pos(&self) -> TokenPos {
        self.token.pos
    }
}

pub(crate) struct Deserializer {
    parser: Parser,
    eov: TokenType,
    ctx: SectionCtx,
    is_closed: bool,
    #[allow(dead_code)] // TODO
    mode: Mode,
    aliases: HashMap<String, String>,
    ignored: HashSet<String>,
    sections: HashSet<String>,
}

impl Deserializer {
    pub fn from_str(
        s: impl Into<String>,
        mode: Mode,
        aliases: HashMap<String, String>,
        ignored: HashSet<String>,
        sections: HashSet<String>,
    ) -> Self {
        let tokenizer = Tokenizer::from_string(s, mode);
        let parser = Parser::new(tokenizer);
        let eov = if mode == Mode::Semicolon {
            TokenType::Semi
        } else {
            TokenType::Nl
        };
        Deserializer {
            parser,
            eov,
            ctx: SectionCtx::empty(),
            is_closed: false,
            mode,
            aliases,
            ignored,
            sections,
        }
    }
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

    fn current_section(&self, t1: &Token) -> bool {
        let v1 = format!("{}.{}", self.ctx.section_type(), t1.value);
        let v2 = format!("{}.{}", self.ctx.section_type(), self.ctx.subsection_name());
        let t1 = self.aliases.get(&v1).unwrap_or(&t1.value);
        let t2 = self
            .aliases
            .get(&v2)
            .map(|s| s.as_str())
            .unwrap_or(self.ctx.subsection_name());
        t1 == t2
    }
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
        let mut lookahead = self.parser.lookaheadnl(1);

        // No argument means "true".
        // So "enable-warp;" is the same as "enable-warp yes;".
        if let Some(token) = lookahead.peek(self.eov)? {
            return visitor.visit_bool(true).map_err(|e| update_pos(e, token.pos));
        }
        if let Some(token) = lookahead.peek(TokenType::Expr)? {
            lookahead.advance(&mut self.parser);
            let v = match token.value.as_str() {
                "y"|"yes"|"t"|"true"|"on"|"1" => true,
                "n"|"no"|"f"|"false"|"off"|"0" => false,
                _ => return Err(Error::new(format!("expected boolean"), token.pos)),
            };
            return visitor.visit_bool(v).map_err(|e| update_pos(e, token.pos));
        }
        lookahead.error()
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
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_unit()
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
        let mut lookahead = self.parser.lookahead(3);
        let is_section = self.sections.contains(self.ctx.subsection_name())
            || lookahead.peek(TokenType::LcBrace)?.is_some()
            || lookahead
                .peek2(TokenType::Expr, TokenType::LcBrace)?
                .is_some()
            || lookahead
                .peek3(TokenType::Ident, TokenType::Expr, TokenType::LcBrace)?
                .is_some();
        let pos = self.parser.save_pos();

        // Give the visitor access to each element of the sequence.
        let res = if is_section {
            self.is_closed = true;
            visitor.visit_seq(SectionAccess::new(&mut self, None, None))
        } else {
            self.is_closed = false;
            visitor.visit_seq(ListAccess::new(&mut self))
        }
        .map_err(|e| update_pos(e, pos));

        res
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
        debug!("deserialize_map");

        // Give the visitor access to each entry of the map.
        let hma = HashMapAccess::new(&mut self);
        let value = visitor
            .visit_map(hma)
            .map_err(|e| update_pos(e, self.ctx.subsection_pos()))?;

        // We don't expect a ';' or '\n' after this.
        self.is_closed = true;

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
        debug!("deserialize_struct({})", _name);

        let mut label = None;
        if self.ctx.level > 0 {
            // see if it starts with a label.
            let mut lookahead = self.parser.lookahead(1);
            let token = lookahead.peek(TokenType::Expr)?;
            debug!("deserialize_struct({}): peeked: {:?}", _name, token);
            if let Some(lbl) = token {
                // yes. so "fields" must contain "__label__".
                lookahead.advance(&mut self.parser);
                if !fields.contains(&"__label__") {
                    debug!("deserialize_struct({}): fields has no __label__", _name);
                    return Err(Error::new(format!("expected '{{'"), lbl.pos));
                }
                debug!("fields is OK");
                label = Some(lbl.value);
            } else {
                // no, so if there's a "__label__" field return an error.
                if fields.contains(&"__label__") {
                    // this will generate "expected expression"
                    lookahead.end()?;
                }
            }

            // then an opening brace.
            if self.mode == Mode::Diablo {
                self.parser.expect(TokenType::Nl)?;
            } else {
                self.parser.expect(TokenType::LcBrace)?;
            }
        }

        // Give the visitor access to each entry of the map.
        let saved_context = self.ctx.new::<V::Value>();
        let res = visitor.visit_map(SectionAccess::new(&mut self, label, Some(fields)));
        self.ctx.restore(saved_context);
        let value = res.map_err(|e| update_pos(e, self.ctx.subsection_pos()))?;

        debug!("XX level is {}", self.ctx.level);
        if self.ctx.level != 0 {
            // Parse the closing brace of the map.
            if self.mode == Mode::Diablo {
                self.parser.expect(TokenType::End)?;
            } else {
                self.parser.expect(TokenType::RcBrace)?;
            }
        }

        // We don't expect a ';' or '\n' after this.
        self.is_closed = true;

        Ok(value)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let pos = self.parser.save_pos();
        let ident = self.parser.expect(TokenType::Ident)?;

        let mut lookahead = self.parser.lookahead(2);

        if let Some(_token) = lookahead.peek(TokenType::LcBrace)? {
            // Visit a struct variant.
            self.parser.restore_pos(pos);
            return visitor.visit_enum(Enum::new(self));
        }

        if let Some(_token) = lookahead.peek2(TokenType::Ident, TokenType::LcBrace)? {
            // Visit a struct variant with label.
            self.parser.restore_pos(pos);
            return visitor.visit_enum(Enum::new(self));
        }

        // Unit variant.
        visitor.visit_enum(ident.value.into_deserializer())
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
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        loop {
            let mut lookahead = self.parser.lookaheadnl(1);
            if lookahead.peek(self.eov)?.is_some() {
                break;
            }
            lookahead.advance(&mut self.parser);
        }
        self.deserialize_unit(visitor)
    }
}

// This handles a comma-separated list.
struct ListAccess<'a> {
    de: &'a mut Deserializer,
    first: bool,
}

impl<'a> ListAccess<'a> {
    fn new(de: &'a mut Deserializer) -> Self {
        ListAccess { de, first: true }
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
            let mut lookahead = self.de.parser.lookaheadnl(2);

            // Is this a comma?
            if let Some(_) = lookahead.peek(TokenType::Comma)? {
                lookahead.advance(&mut self.de.parser);
            }

            // or end-of-value? (eg ';' or '\n')
            if let Some(_) = lookahead.peek(self.de.eov)? {
                // See if the next section is a continuation.
                let mut continuation = false;

                let mut lookahead2 = self.de.parser.lookaheadnl(2);

                if let Some(ident) = lookahead2.peek2(self.de.eov, TokenType::Ident)? {
                    if self.de.current_section(&ident) {
                        // It is a continuation. The value name might be an
                        // alias, so update the current name. This is _only_
                        // useful for the SectionName trait.
                        self.de.ctx.update_section_token(ident);
                        lookahead2.advance(&mut self.de.parser);
                        continuation = true;
                    }
                }
                if !continuation {
                    return Ok(None);
                }
            }
            lookahead.end()?;
        }
        self.first = false;

        // Deserialize an array element.
        let token = self.de.parser.peek()?;
        seed.deserialize(&mut *self.de)
            .map(Some)
            .map_err(|e| update_tpos(e, &token))
    }
}

struct SectionAccess<'a> {
    de: &'a mut Deserializer,
    label: Option<String>,
    first: bool,
    fields: Option<&'static [&'static str]>,
}

impl<'a> SectionAccess<'a> {
    fn new(
        de: &'a mut Deserializer,
        label: Option<String>,
        fields: Option<&'static [&'static str]>,
    ) -> Self {
        debug!("SectionAccess::new");
        SectionAccess {
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
        debug!(
            "SectionAccess::SeqAccess::next_element_seed {:?}",
            self.de.ctx.token,
        );

        if !self.first {
            // See if the next section is a continuation.
            let mut continuation = false;
            let mut lookahead = self.de.parser.lookahead(1);

            if let Some(token) = lookahead.peek(TokenType::Ident)? {
                if self.de.current_section(&token) {
                    // It is a continuation. The value name might be an
                    // alias, so update the current name. This is _only_
                    // useful for the SectionName trait.
                    self.de.ctx.update_section_token(token);
                    continuation = true;
                    lookahead.advance(&mut self.de.parser);
                }
            }
            if !continuation {
                //debug!("SectionAccess::SeqAccess: end of section {:?}", name);
                return Ok(None);
            }
        }
        self.first = false;

        // Deserialize an array element.
        let token = self.de.parser.peek()?;
        seed.deserialize(&mut *self.de)
            .map(Some)
            .map_err(|e| update_tpos(e, &token))
    }
}

// This handles the ';' or '\n' separated key/value fields of a section.
impl<'de, 'a> MapAccess<'de> for SectionAccess<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        debug!("SectionAccess::MapAccess::next_key_seed");

        // if the struct has a __label__ field, insert field name (once!)
        if self.first && self.label.is_some() {
            debug!("SectionAccess::MapAccess::next_key_seed: insert label {:?}", self.label);
            let de = "__label__".into_deserializer();
            return seed.deserialize(de).map(Some);
        }

        if !self.first {
            if !self.de.is_closed {
                // end-of-value is required after a value.
                self.de.parser.expect(self.de.eov)?;
            }
        }
        self.first = false;

        // if we're at root-level, check for end-of-file.
        debug!("check for EOF");
        if self.de.ctx.level == 1 && self.de.parser.is_eof() {
            return Ok(None);
        }
        debug!("check for EOF done");

        // Check for a continuation.
        let mut continuation = false;
        let mut lookahead = self.de.parser.lookaheadnl(2);
        if let Some(ident) = lookahead.peek2(self.de.eov, TokenType::Ident)? {
            if self.de.current_section(&ident) {
                lookahead.advance(&mut self.de.parser);
                continuation = true;
            }
        }

        let mut lookahead = self.de.parser.lookahead(1);

        if !continuation {
            // Check if it's the end of the section.
            let closed = if self.de.mode == Mode::Diablo {
                lookahead.peek(TokenType::End)?.is_some()
            } else {
                lookahead.peek(TokenType::RcBrace)?.is_some()
            };
            if closed {
                return Ok(None);
            }
        }

        // No, so expect an ident.
        if let Some(token) = lookahead.peek(TokenType::Ident)? {
            debug!("next_key_seed: key {:?}", token);
            lookahead.advance(&mut self.de.parser);

            // First resolve this field name - might be an alias.
            let type_dot_field = format!("{}.{}", self.de.ctx.section_type(), token.value);
            let name = self
                .de
                .aliases
                .get(&type_dot_field)
                .map(|s| s.as_str())
                .unwrap_or(token.value.as_str());

            // now see if it matches one of the struct's field names.
            let fields = self.fields.as_ref().unwrap();
            if !fields.contains(&name) && !self.de.ignored.contains(&type_dot_field) {
                return Err(Error::new(
                    format!("unknown field: `{}'", token.value),
                    token.pos,
                ));
            }
            debug!("XXX set section_name_token to {}", token.value);
            self.de.ctx.update_section_token(token.clone());

            // and deserialize the resolved (unaliases) map key.
            return seed.deserialize(name.into_deserializer()).map(Some);
        }

        lookahead.error()
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        debug!(
            "SectionAccess::MapAccess::next_value_seed {}",
            std::any::type_name::<V::Value>()
        );

        // if the struct has a __label__ field, insert label value (once!)
        if let Some(label) = self.label.take() {
            let de = label.into_deserializer();
            return seed.deserialize(de);
        }

        // Deserialize a map value.
        self.de.is_closed = false;
        let token = self.de.parser.peek()?;
        seed.deserialize(&mut *self.de)
            .map_err(|e| update_tpos(e, &token))
    }
}

struct HashMapAccess<'a> {
    de: &'a mut Deserializer,
    first: bool,
}

impl<'a> HashMapAccess<'a> {
    fn new(
        de: &'a mut Deserializer,
    ) -> Self {
        debug!("HashMapAccess::new");
        HashMapAccess {
            de,
            first: true,
        }
    }
}

// This handles the ';' or '\n' separated key/value fields of a hashmap.
impl<'de, 'a> MapAccess<'de> for HashMapAccess<'a> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        debug!("HashMapAccess::MapAccess::next_key_seed");

        if !self.first {
            // Check for a continuation.
            let mut continuation = false;

            let mut lookahead = self.de.parser.lookaheadnl(2);
            if self.de.is_closed {
                if let Some(ident) = lookahead.peek(TokenType::Ident)? {
                    if self.de.current_section(&ident) {
                        lookahead.advance(&mut self.de.parser);
                        continuation = true;
                    }
                }
            } else {
                if let Some(ident) = lookahead.peek2(self.de.eov, TokenType::Ident)? {
                    if self.de.current_section(&ident) {
                        lookahead.advance(&mut self.de.parser);
                        continuation = true;
                    }
                }
            }

            if !continuation {
                if !self.de.is_closed {
                    // end-of-value is required after a value.
                    self.de.parser.expect(self.de.eov)?;
                }
                return Ok(None);
            }
        }
        self.first = false;

        let mut lookahead = self.de.parser.lookaheadnl(2);
        let ttype = if self.de.mode == Mode::Diablo { TokenType::Nl } else { TokenType::LcBrace };
        if let Some(label) = lookahead.peek2(TokenType::Expr, ttype)? {
            // it's a section. do not eat the label.
            return seed.deserialize(label.value.into_deserializer()).map(Some);
        }

        seed.deserialize(&mut *self.de).map(Some)
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        debug!(
            "HashMapAccess::MapAccess::next_value_seed {}",
            std::any::type_name::<V::Value>()
        );

        // Deserialize a map value.
        self.de.is_closed = false;
        let token = self.de.parser.peek()?;
        seed.deserialize(&mut *self.de)
            .map_err(|e| update_tpos(e, &token))
    }
}

struct Enum<'a> {
    de: &'a mut Deserializer,
}

impl<'a> Enum<'a> {
    fn new(de: &'a mut Deserializer) -> Self {
        Enum { de }
    }
}

// `EnumAccess` is provided to the `Visitor` to give it the ability to determine
// which variant of the enum is supposed to be deserialized.
impl<'de, 'a> EnumAccess<'de> for Enum<'a> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: DeserializeSeed<'de>,
    {
        // Get the ident, which indicates the variant.
        let val = seed.deserialize(&mut *self.de)?;

        // Then continue parsing.
        Ok((val, self))
    }
}

// `VariantAccess` is provided to the `Visitor` to give it the ability to see
// the content of the single variant that it decided to deserialize.
impl<'de, 'a> VariantAccess<'de> for Enum<'a> {
    type Error = Error;

    // If the `Visitor` expected this variant to be a unit variant, the input
    // should have been the simple ident case handled in `deserialize_enum`.
    fn unit_variant(self) -> Result<()> {
        let pos = self.de.parser.save_pos();
        Err(Error::new("expected identifier", pos))
    }

    // Newtype variants.
    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(self.de)
    }

    // Tuple variants.
    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.de, visitor)
    }

    // Struct variants.
    fn struct_variant<V>(self, fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_struct(self.de, "", fields, visitor)
    }
}

// helper
fn update_pos(mut e: Error, pos: TokenPos) -> Error {
    if e.pos.offset == 0 {
        e.pos = pos;
    }
    e
}

// helper
fn update_tpos(mut e: Error, token: &Option<Token>) -> Error {
    if e.pos.offset == 0 {
        e.pos = token.as_ref().map(|t| t.pos).unwrap_or(TokenPos::none());
    }
    e
}
