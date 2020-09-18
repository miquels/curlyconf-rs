use std::fmt;
use std::fs;
use std::io::{self, ErrorKind};
use std::sync::Arc;

type Range = std::ops::Range<usize>;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum TokenType {
    Nl,
    LcBrace,
    RcBrace,
    LBrace,
    RBrace,
    Comma,
    Semi,
    Equal,
    Word,
    UnterminatedString,
    Unknown,
    End,
    Ident,
    Expr,
    Eof,
}
use TokenType::*;

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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Cursor(usize);

impl TokenType {
    pub fn as_str(&self) -> &'static str {
        match *self {
            Nl => "'newline'",
            LcBrace => "'{'",
            RcBrace => "'}'",
            LBrace => "'('",
            RBrace => "')'",
            Comma => "','",
            Semi => "';'",
            Equal => "'='",
            Word => "word",
            UnterminatedString => "[unterminated string]",
            Unknown => "[unknown]",
            End => "'end'",
            Ident => "identifier", // alias for Word
            Expr => "expression",  // alias for Word
            Eof => "end-of-data",
        }
    }
}

pub struct TokenSource {
    pub filename:   String,
    text:       String,
}

impl fmt::Debug for TokenSource {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt.debug_struct("TokenSource")
            .field("filename", &self.filename)
            .field("text", &"[]")
            .finish()
    }
}

#[derive(Clone)]
pub struct TokenSpan {
    pub line: u32,
    pub column: u32,
    pub range: Range,
    pub source: Arc<TokenSource>,
}

impl fmt::Debug for TokenSpan {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let s = &self.source.text[self.range.clone()];
        fmt.debug_struct("TokenSpan")
            .field("file", &self.source.filename)
            .field("line", &self.line)
            .field("column", &self.column)
            .field("str", &s)
            .finish()
    }
}

#[derive(Debug, Default, Clone, Copy)]
struct Position {
    line:   u32,
    column: u32,
    offset: usize,
}

#[derive(Debug)]
pub struct Tokenizer {
    tokens: Vec<Token>,
    source: Arc<TokenSource>,
    pos: Position,
    cursor: usize,
    pub mode: Mode,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub span: TokenSpan,
    pub value: Option<String>,
}

impl Token {
    pub fn value(&self) -> &str {
        if let Some(value) = self.value.as_ref() {
            value.as_str()
        } else {
            &self.span.source.text[self.span.range.clone()]
        }
    }
    pub fn span(&self) -> TokenSpan {
        self.span.clone()
    }
}

impl Tokenizer {
    pub fn from_file(filename: impl Into<String>, mode: Mode) -> io::Result<Tokenizer> {
        let filename = filename.into();
        let data =
            fs::read(&filename).map_err(|e| io::Error::new(e.kind(), format!("{:?}: {}", filename, e)))?;
        let text = String::from_utf8(data)
            .map_err(|_| io::Error::new(ErrorKind::Other, format!("{:?}: utf-8 error", filename)))?;
        Ok(Tokenizer::from_string(text, filename, mode))
    }

    pub fn from_string(text: impl Into<String>, filename: impl Into<String>, mode: Mode) -> Tokenizer {
        let filename = filename.into();
        let text = text.into();
        let mut t = Tokenizer {
            tokens: Vec::new(),
            source: Arc::new(TokenSource{ filename, text }),
            mode,
            cursor: 0,
            pos: Position { line: 1, column: 1, offset: 0 },
        };
        let mut end = false;
        while !end {
            let token = t.get_token();
            if token.ttype == TokenType::Eof {
                end = true;
            }
            t.tokens.push(token);
        }
        t
    }

    pub fn update_pos(&mut self, n: usize) {
        let s = &self.source.text[self.pos.offset..self.pos.offset + n];
        for c in s.chars() {
            self.pos.offset += c.len_utf8();
            if c == '\n' {
                if self.pos.offset < self.source.text.len() {
                    self.pos.line += 1;
                    self.pos.column = 1;
                }
            } else {
                self.pos.column += c.len_utf8() as u32;
            }
        }
    }

    // Skip whitespace.
    pub fn skip_space(&self, skip_nl: bool) -> usize {
        let mut n = 0;
        let mut comment = false;
        let mut s = self.source.text[self.pos.offset..].chars();
        while let Some(c) = s.next() {
            if comment {
                if c == '\n' {
                    comment = false;
                } else {
                    n += c.len_utf8();
                    continue;
                }
            }
            if c == '#' {
                comment = true;
                n += 1;
                continue;
            }
            if !c.is_whitespace() || (!skip_nl && c == '\n') {
                break;
            }
            n += c.len_utf8();
        }
        n
    }

    pub fn last_span(&self) -> TokenSpan {
        let c = if self.cursor > 0 { self.cursor - 1 } else { 0 };
        self.tokens[c].span()
    }

    fn parse_sqstring(&mut self) -> Token {
        let offset = self.pos.offset;
        let s = &self.source.text[offset + 1..];
        if let Some(x) = s.find('\'') {
            let pos = self.pos;
            let value = s[..x].to_string();
            self.update_pos(x + 2);
            self.build_token(TokenType::Word, pos, x + 2, Some(value))
        } else {
            let len = self.source.text.len() - offset;
            let pos = self.pos;
            self.update_pos(len);
            self.build_token(TokenType::UnterminatedString, pos, len, None)
        }
    }

    fn parse_dqstring(&mut self) -> Token {
        let offset = self.pos.offset;
        let s = &self.source.text[offset + 1..];
        let mut esc = false;
        let mut n = 0;
        let mut end = false;
        for c in s.chars() {
            n += c.len_utf8();
            if esc == true {
                esc = false;
                continue;
            }
            if c == '\\' {
                esc = true;
                continue;
            }
            if c == '"' {
                n -= 1;
                end = true;
                break;
            }
        }

        if end {
            let pos = self.pos;
            let value = expand_string(&s[..n]).unwrap_or(String::from(&s[..n]));
            self.update_pos(n + 2);
            self.build_token(TokenType::Word, pos, n + 2, Some(value))
        } else {
            let len = s.len() + 1;
            let pos = self.pos;
            self.update_pos(len);
            self.build_token(TokenType::UnterminatedString, pos, len, None)
        }
    }

    pub fn parse_word(&mut self) -> Token {
        // Words.
        let mut n = 0;
        let mut i = 0;
        let offset = self.pos.offset;
        let s = &self.source.text[offset..];
        for c in s.chars() {
            match c {
                '@' | '$' | '%' | '^' | '&' | '*' | '-' | '_' | '+' | '[' | ']' | ':' | '|'
                | '~' | '.' | '?' | '/' | '!' => {}
                '0'..='9' => {}
                'a'..='z' => {}
                'A'..='Z' => {}
                _ => break,
            }
            i += 1;
            n += c.len_utf8();
        }

        let pos = self.pos;

        if i == 0 {
            self.update_pos(1);
            return self.build_token(TokenType::Unknown, pos, 1, None);
        }

        self.update_pos(n);
        let mut ttype = TokenType::Word;

        // Yes, this is kind of ugly.
        if self.mode == Mode::Diablo && &self.source.text[offset .. offset + n] == "end" {
            ttype = TokenType::End;
        }

        self.build_token(ttype, pos, n, None)
    }

    pub fn parse_token(&mut self, t: TokenType) -> Token {
        let pos = self.pos;
        self.update_pos(1);
        self.build_token(t, pos, 1, None)
    }

    fn build_token(&self, ttype: TokenType, pos: Position, len: usize, value: Option<String>) -> Token {
        Token {
            ttype,
            value,
            span: TokenSpan{
                line: pos.line,
                column: pos.column,
                range: Range{ start: pos.offset, end: pos.offset + len },
                source: self.source.clone(),
            },
        }
    }

    fn get_token(&mut self) -> Token {
        let n = self.skip_space(self.mode == Mode::Semicolon);
        self.update_pos(n);
        if self.pos.offset == self.source.text.len() {
            return self.build_token(TokenType::Eof, self.pos, 0, None);
        }

        let s = &self.source.text[self.pos.offset..];

        // We can figure out tokentype based on the first char.
        let first = s.chars().next().unwrap();
        let token = match first {
            '\n' => self.parse_token(TokenType::Nl),
            '{' => self.parse_token(TokenType::LcBrace),
            '}' => self.parse_token(TokenType::RcBrace),
            '(' => self.parse_token(TokenType::LBrace),
            ')' => self.parse_token(TokenType::RBrace),
            ',' => self.parse_token(TokenType::Comma),
            ';' => self.parse_token(TokenType::Semi),
            '=' => self.parse_token(TokenType::Equal),
            '"' => self.parse_dqstring(),
            '\'' => self.parse_sqstring(),
            _ => self.parse_word(),
        };

        // if we got a Newline token, skip all following newlines.
        if token.ttype == TokenType::Nl {
            let n = self.skip_space(true);
            self.update_pos(n);
        }

        token
    }

    pub fn next_token(&mut self) -> Token {
        let token = self.tokens[self.cursor].clone();
        if token.ttype != TokenType::Eof {
            self.cursor += 1;
        }
        token
    }

    pub fn save_cursor(&self) -> Cursor {
        Cursor(self.cursor)
    }

    pub fn restore_cursor(&mut self, cursor: Cursor) {
        self.cursor = cursor.0;
    }

    pub fn insert(&mut self, other: Tokenizer) {
        let mut tokens = other.tokens;
        if !tokens.is_empty() && tokens[tokens.len()-1].ttype == TokenType::Eof {
            tokens.pop();
        }
        let tail = self.tokens.split_off(self.cursor);
        self.tokens.extend(tokens);
        self.tokens.extend(tail);
    }
}

fn expand_string(s: &str) -> Option<String> {
    if !s.contains("\\") {
        return None;
    }
    let mut r = String::with_capacity(s.len());
    let mut escaped = false;
    for c in s.chars() {
        if !escaped {
            if c == '\\' {
                escaped = true;
            } else {
                r.push(c);
            }
            continue;
        }
        let x = match c {
            '\\' => '\\',
            '0' => 0 as char,
            'a' => 7 as char,
            'b' => 8 as char,
            'e' => 27 as char,
            'f' => 12 as char,
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => 11 as char,
            x => x,
        };
        escaped = false;
        r.push(x);
    }
    Some(r)
}

/*
static WORD_RE: Lazy<Regex> = Lazy::new(|| RegexSet::new(&[
        r"^[A-Za-z_][A-Za-z0-9_]*$",                            // ident
        r"^-([0-9]+|[0-9]*\.[0-9]+)$",                          // number,
        r"^(/?[-_+=:.0-9A-Za-z]+)+$",                           // filename,
        r"^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|\*$",             // IPv4Addr,
        r"^(:|[0-9A-Fa-f]{1,4})+|[(:|[0-9A-Fa-f]{1,4})+]$",     // IPv6Addr,
        r"^(\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|\*):\d+$",       // SockAddr4,
        r"^[(:|[0-9A-Fa-f]{1,4})+]:\d+$",                       // SockAddr6,
        r"^:\d+$",                                              // SockAddr,
        r"^[@!]?[0-9a-z+_*]+(\.[0-9a-z+_*]+)*",                 // NgMatch
        r"^[-@%&*_+~.0-9a-zA-Z]$",                              // Word.
]));

def_tokens! {
    [ 0, Nl, "\n" ],
    [ 1, LcBrace, "{" ],
    [ 2, RcBrace, "}" ],
    [ 3, LBrace, "(" ],
    [ 4, RBrace, ")" ],
    [ 5, Comma, "," ],
    [ 6, Semi, ";" ],
    [ 7, Equal, "=" ],
    [ 8, SqString, "'" ],
    [ 9, DqString, "\"" ],
    [ 10, Ident, "[A-Za-z_][A-Za-z0-9_]*" ],
    [ 11, Number, "-([0-9]+|[0-9]*\.[0-9]+)" ],
    [ 12, Word, "[-@%&*_+~./0-9a-zA-Z_]" ],
}
*/
