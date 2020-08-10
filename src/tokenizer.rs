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
    pub(crate) pos: TokenPos,
    pub(crate) mode: Mode,
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
    pub fn from_string(s: impl Into<String>, mode: Mode) -> Tokenizer {
        Tokenizer {
            file: "cfg-data".to_string(),
            data: s.into(),
            pos: TokenPos::new(),
            mode,
        }
    }

    pub fn update_pos(&mut self, n: usize) {
        let s = &self.data[self.pos.offset..self.pos.offset + n];
        for c in s.chars() {
            self.pos.offset += c.len_utf8();
            if c == '\n' {
                if self.pos.offset < self.data.len() {
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
            if !c.is_whitespace() || (!skip_nl && c == '\n') {
                break;
            }
            n += c.len_utf8();
        }
        n
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
                | '~' | '.' | '?' | '/' | '!'  => {}
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
            return (
                TokenType::Unknown,
                Range {
                    start: offset,
                    end: offset + 1,
                },
            );
        }

        self.update_pos(n);
        let range = Range {
            start: offset,
            end: offset + n,
        };

        // Yes, this is kind of ugly.
        if self.mode == Mode::Diablo && &self.data[range.clone()] == "end" {
            return (TokenType::End, range);
        }

        (TokenType::Word, range)
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

    pub fn next_token(&mut self) -> Token {
        let n = self.skip_space(self.mode == Mode::Semicolon);
        self.update_pos(n);
        if self.pos.offset == self.data.len() {
            return Token {
                ttype: TokenType::Eof,
                pos: self.pos,
                value: "".to_string(),
            };
        }

        let s = &self.data[self.pos.offset..];
        let pos = self.pos;
        let mut dq = false;

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
            '"' => {
                dq = true;
                self.parse_dqstring()
            }
            '\'' => self.parse_sqstring(),
            _ => self.parse_word(),
        };

        // if we got a Newline token, skip all following newlines.
        if t == TokenType::Nl {
            let n = self.skip_space(true);
            self.update_pos(n);
        }

        Token {
            ttype: t,
            pos,
            value: expand_string(dq, &self.data[range]),
        }
    }

    pub fn save_pos(&self) -> TokenPos {
        self.pos
    }

    pub fn restore_pos(&mut self, pos: TokenPos) {
        self.pos = pos;
    }
}

fn expand_string(dq: bool, s: &str) -> String {
    if !dq || !s.contains("\\") {
        return s.to_string();
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
    r
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
