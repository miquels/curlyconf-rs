use std::collections::VecDeque;
use std::fs;
use std::io::{self, Error as IoError, ErrorKind as Kind};

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
    Ident,
    Expr,
}
use TokenType::*;

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
            Ident => "identifier", // alias for Word
            Expr => "expression",  // alias for Word
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TokenPos {
    pub line: u32,
    pub column: u32,
    pub offset: usize,
}

impl TokenPos {
    pub(crate) fn new() -> TokenPos {
        TokenPos {
            line: 1,
            column: 1,
            offset: 0,
        }
    }

    pub(crate) fn none() -> TokenPos {
        TokenPos {
            line: 0,
            column: 0,
            offset: 0,
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer {
    file: String,
    data: String,
    buf: VecDeque<Token>,
    pub(crate) pos: TokenPos,
    pub(crate) nl_token: bool,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenType,
    pub pos: TokenPos,
    pub value: String,
}

impl Token {
    pub fn new(ttype: TokenType, value: impl Into<String>, pos: TokenPos) -> Token {
        let value = value.into();
        Token { ttype, value, pos }
    }
}

impl Tokenizer {
    pub fn from_string(s: impl Into<String>) -> Tokenizer {
        Tokenizer {
            file: "[data]".to_string(),
            data: s.into(),
            pos: TokenPos::new(),
            nl_token: false,
            buf: VecDeque::new(),
        }
    }

    pub fn from_file(name: impl Into<String>) -> io::Result<Tokenizer> {
        let name = name.into();
        let data = fs::read(&name)?;
        let text = String::from_utf8(data).map_err(|_| IoError::new(Kind::Other, "utf-8 error"))?;

        Ok(Tokenizer {
            file: name,
            data: text,
            pos: TokenPos::new(),
            nl_token: false,
            buf: VecDeque::new(),
        })
    }

    pub fn update_pos(&mut self, n: usize) {
        let s = &self.data[self.pos.offset..self.pos.offset + n];
        for c in s.chars() {
            if c == '\n' {
                self.pos.line += 1;
                self.pos.column = 1;
            } else {
                self.pos.column += c.len_utf8() as u32;
            }
        }
        self.pos.offset += s.len();
    }

    pub fn skip_space(&self) -> Option<usize> {
        let mut n = 0;
        let mut comment = false;
        let mut s = self.data[self.pos.offset..].chars();
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
            if !c.is_whitespace() || (self.nl_token && c == '\n') {
                break;
            }
            n += c.len_utf8();
        }
        if self.pos.offset + n == self.data.len() {
            None
        } else {
            Some(n)
        }
    }

    fn parse_sqstring(&mut self) -> (TokenType, Range) {
        let offset = self.pos.offset;
        let s = &self.data[offset + 1..];
        if let Some(x) = s.find('\'') {
            self.update_pos(x + 2);
            (
                TokenType::Word,
                Range {
                    start: offset + 1,
                    end: offset + 1 + x,
                },
            )
        } else {
            let x = s.len() + 1;
            self.update_pos(x);
            (
                TokenType::UnterminatedString,
                Range {
                    start: offset + 1,
                    end: offset + x,
                },
            )
        }
    }

    fn parse_dqstring(&mut self) -> (TokenType, Range) {
        let offset = self.pos.offset;
        let s = &self.data[offset + 1..];
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
                end = true;
                break;
            }
        }

        if end {
            self.update_pos(n + 1);
            (
                TokenType::Word,
                Range {
                    start: offset + 1,
                    end: offset + n,
                },
            )
        } else {
            let x = s.len() + 1;
            self.update_pos(x);
            (
                TokenType::UnterminatedString,
                Range {
                    start: offset + 1,
                    end: offset + x,
                },
            )
        }
    }

    pub fn parse_word(&mut self) -> (TokenType, Range) {
        // Words.
        let mut n = 0;
        let mut i = 0;
        let offset = self.pos.offset;
        let s = &self.data[offset..];
        for c in s.chars() {
            match c {
                '@' | '$' | '%' | '^' | '&' | '*' | '-' | '_' | '+' | '[' | ']' | ':' | '|'
                | '~' | '.' | '?' | '/' => {}
                '0'..='9' => {}
                'a'..='z' => {}
                'A'..='Z' => {}
                _ => break,
            }
            i += 1;
            n += c.len_utf8();
        }
        if i == 0 {
            self.update_pos(1);
            (
                TokenType::Unknown,
                Range {
                    start: offset,
                    end: offset + 1,
                },
            )
        } else {
            self.update_pos(n);
            (
                TokenType::Word,
                Range {
                    start: offset,
                    end: offset + n,
                },
            )
        }
    }

    pub fn parse_token(&mut self, t: TokenType) -> (TokenType, Range) {
        let offset = self.pos.offset;
        self.update_pos(1);
        (
            t,
            Range {
                start: offset,
                end: offset + 1,
            },
        )
    }

    pub fn push_token(&mut self, token: Token) {
        self.buf.push_front(token);
    }

    fn do_next_token(&mut self, peek: bool) -> Option<Token> {
        if let Some(token) = self.buf.pop_back() {
            if peek {
                self.buf.push_back(token.clone());
            }
            return Some(token);
        }
        let oldpos = self.pos;
        let n = self.skip_space()?;
        self.update_pos(n);
        let s = &self.data[self.pos.offset..];
        let pos = self.pos;

        // We can figure out tokentype based on the first char.
        let mut chars = s.chars();
        let (t, range) = match chars.next().unwrap() {
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

        if peek {
            self.pos = oldpos;
        }

        Some(Token {
            ttype: t,
            pos,
            value: self.data[range].to_string(),
        })
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.do_next_token(false)
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.do_next_token(true)
    }
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
