use std::borrow::Cow;
use std::io::{self, ErrorKind};
use std::path::Path;

use once_cell::sync::Lazy;
use regex::Regex;

use crate::error::{Error, Result};
use crate::tokenizer::*;

pub struct Parser {
    tokenizer: Tokenizer,
    mode: Mode,
}

impl Parser {
    pub fn from_file(name: impl Into<String>, mode: Mode) -> io::Result<Parser> {
        let tokenizer = Tokenizer::from_file(name, mode)?;
        Ok(Parser { tokenizer, mode })
    }

    pub fn from_string(name: impl Into<String>, mode: Mode) -> Parser {
        let tokenizer = Tokenizer::from_string(name, "config-text", mode);
        Parser { tokenizer, mode }
    }

    pub fn include(&mut self, name: impl Into<String>, curfile: &str) -> io::Result<()> {
        let mut name = name.into();

        let path = Path::new(&name);
        if path.is_relative() {
            // Take the parent of the current file, and build a new path.
            if let Some(parent) = Path::new(curfile).parent() {
                let mut pathbuf = parent.to_path_buf();
                pathbuf.push(&path);
                name = pathbuf.to_string_lossy().to_string();
            }
        }

        // It might be a glob.
        let globresult = glob::glob(&name).map_err(|e| {
            io::Error::new(ErrorKind::InvalidData, format!("{}: {}", name, e.msg))
        })?;
        let files = globresult.collect::<Result<Vec<_>, _>>().map_err(|e| {
            io::Error::new(e.error().kind(), format!("{}: {}", e.path().to_string_lossy(), e.error()))
        })?;
        if files.is_empty() {
            return Err(io::Error::new(ErrorKind::NotFound, format!("{}: file not found", name)));
        }

        // Now parse and insert these files one by one.
        for file in files.into_iter().rev().map(|p| p.to_string_lossy().to_string()) {
            let tokenizer = Tokenizer::from_file(file, self.mode)?;
            self.tokenizer.insert(tokenizer);
        }

        Ok(())
    }

    pub fn last_span(&mut self) -> TokenSpan {
        self.tokenizer.last_span()
    }

    // Read the next token, and translate unexpected tokens to errors.
    fn do_token(&mut self, eof_ok: bool) -> Result<Token> {

        let token = self.tokenizer.next_token();
        match token.ttype {
            TokenType::Eof => {
                if eof_ok {
                    return Ok(token.clone());
                }
                return Err(Error::new("unexpected end-of-file", token.span()));
            }
            TokenType::UnterminatedString => {
                return Err(Error::new("unterminated string literal", token.span()));
            }
            TokenType::Unknown => {
                return Err(Error::new("unexpected token", token.span()));
            }
            _ => {}
        }

        Ok(token)
    }

    // Get the next token.
    pub fn next_token(&mut self) -> Result<Token> {
        self.do_token(false)
    }

    // Check if we are at EOF (that is, self.next_token() will hit EOF).
    pub fn is_eof(&mut self) -> bool {
        let pos = self.save_pos();
        let mut is_eof = false;
        loop {
            match self.do_token(true) {
                Ok(token) => match token.ttype {
                    TokenType::Nl => {}
                    TokenType::Eof => {
                        is_eof = true;
                        break;
                    }
                    _ => break,
                },
                Err(_) => break,
            }
        }
        self.restore_pos(pos);
        is_eof
    }

    // Expect a certain token-type.
    pub fn expect(&mut self, want: TokenType) -> Result<Token> {
        debug!("expect {:?}", want);
        // skip newlines, _unless_ that is what we are looking for.
        let mut token = loop {
            let t = self.next_token()?;
            if t.ttype == want || t.ttype != TokenType::Nl {
                break t;
            }
        };

        if token.ttype == TokenType::Word {
            if want == TokenType::Ident && is_ident(&mut token) {
                debug!("+-- found {:?}", token);
                return Ok(token);
            }
            if want == TokenType::Expr && is_expr(&mut token) {
                debug!("+-- found {:?}", token);
                return Ok(token);
            }
        }

        if want == token.ttype {
            debug!("+-- found {:?}", token);
            return Ok(token);
        }

        debug!("+-- failed, got {:?}", token);
        Err(Error::new(format!("expected {}", want.as_str()), token.span()))
    }

    // Peek at the next token. Returns Ok(None) on EOF instead of an error.
    pub fn peek(&mut self) -> Result<Option<Token>> {
        let pos = self.save_pos();
        let token = loop {
            let token = self.do_token(true)?;
            if token.ttype != TokenType::Nl {
                break token;
            }
        };
        self.restore_pos(pos);
        if token.ttype == TokenType::Eof {
            Ok(None)
        } else {
            Ok(Some(token))
        }
    }

    // Peek at the next token. Returns Ok(None) on EOF instead of an error.
    #[allow(dead_code)]
    pub fn peeknl(&mut self) -> Result<Option<Token>> {
        let pos = self.save_pos();
        let token = self.do_token(true)?;
        self.restore_pos(pos);
        if token.ttype == TokenType::Eof {
            Ok(None)
        } else {
            Ok(Some(token))
        }
    }

    pub fn lookahead(&mut self, how_far: usize) -> Lookahead {
        Lookahead::new(self, how_far)
    }

    pub fn lookaheadnl(&mut self, how_far: usize) -> Lookahead {
        if self.tokenizer.mode == Mode::Semicolon {
            Lookahead::new(self, how_far)
        } else {
            Lookahead::newnl(self, how_far)
        }
    }

    pub fn save_pos(&mut self) -> Cursor {
        self.tokenizer.save_cursor()
    }

    pub fn restore_pos(&mut self, pos: Cursor) {
        self.tokenizer.restore_cursor(pos);
    }
}

pub struct Lookahead {
    token: Vec<Result<Token>>,
    span: Option<TokenSpan>,
    next_pos: Vec<Cursor>,
    expected: Vec<Cow<'static, str>>,
    found: bool,
}

impl Lookahead {
    fn do_new(parser: &mut Parser, depth: usize, skipnl: bool) -> Lookahead {
        let pos = parser.save_pos();
        let mut token = Vec::new();
        let mut span = None;
        let mut next_pos = Vec::new();
        for _ in 0..depth {
            let t = loop {
                match parser.do_token(true) {
                    Ok(token) if token.ttype != TokenType::Nl || !skipnl => {
                        if span.is_none() {
                            span = Some(token.span());
                        }
                        break Ok(token);
                    },
                    Err(e) => break Err(e),
                    _ => {}
                }
            };
            token.push(t);
            next_pos.push(parser.save_pos());
        }
        parser.restore_pos(pos);

        Lookahead {
            token,
            span,
            next_pos,
            expected: Vec::new(),
            found: false,
        }
    }

    pub(crate) fn new(parser: &mut Parser, how_far: usize) -> Lookahead {
        Lookahead::do_new(parser, how_far, true)
    }

    pub(crate) fn newnl(parser: &mut Parser, how_far: usize) -> Lookahead {
        Lookahead::do_new(parser, how_far, false)
    }

    pub fn peek(&mut self, want: TokenType) -> Result<Option<Token>> {
        self.peekx(&[want])
    }

    pub fn peek2(&mut self, want1: TokenType, want2: TokenType) -> Result<Option<Token>> {
        self.peekx(&[want1, want2])
    }

    #[allow(dead_code)]
    pub fn peek3(
        &mut self,
        want1: TokenType,
        want2: TokenType,
        want3: TokenType,
    ) -> Result<Option<Token>> {
        self.peekx(&[want1, want2, want3])
    }

    fn peekx(&mut self, want: &[TokenType]) -> Result<Option<Token>> {
        if self.found {
            return Ok(None);
        }
        let mut found = true;
        let mut tokens = std::collections::VecDeque::new();

        for i in 0..want.len() {
            match self.token[i] {
                Ok(ref t) => {
                    let t = t.clone();
                    if let Some(t) = self.compare(t, &want[i]) {
                        tokens.push_back(t);
                    } else {
                        found = false;
                        break;
                    }
                }
                Err(ref e) => {
                    if i == 0 {
                        return Err(e.clone());
                    }
                    found = false;
                    break;
                }
            }
        }

        if found {
            self.found = true;
            self.next_pos[0] = self.next_pos[tokens.len() - 1];
            loop {
                let token = tokens.pop_front().unwrap();
                let t = token.ttype;
                if tokens.len() == 0
                    || t == TokenType::Word
                    || t == TokenType::Ident
                    || t == TokenType::Expr
                {
                    return Ok(Some(token));
                }
            }
        }

        let want_str = want
            .iter()
            .map(|t| t.as_str())
            .collect::<Vec<_>>()
            .join(" ");
        self.expected.push(Cow::Owned(want_str));
        Ok(None)
    }

    fn compare(&mut self, mut token: Token, want: &TokenType) -> Option<Token> {
        if token.ttype == TokenType::Word {
            if want == &TokenType::Ident && is_ident(&mut token) {
                debug!("+-- found {:?}", token);
                return Some(token);
            }
            if want == &TokenType::Expr && is_expr(&mut token) {
                debug!("+-- found {:?}", token);
                return Some(token);
            }
        }

        if want == &token.ttype {
            debug!("+-- found {:?}", token);
            return Some(token);
        }

        debug!("+-- failed, got {:?}", token);

        None
    }

    pub fn advance(&mut self, parser: &mut Parser) {
        debug!("lookahead::advance: to {:?}", self.next_pos);
        parser.restore_pos(self.next_pos[0]);
    }

    pub fn end(&mut self) -> Result<()> {
        if self.found {
            return Ok(());
        }
        let last = if self.expected.len() > 1 {
            self.expected.pop()
        } else {
            None
        };
        let mut msg = "expected ".to_string() + &(self.expected.join(", "));
        if let Some(last) = last {
            msg += &format!(" or {}", last);
        }
        Err(Error::new(msg, self.span.take().unwrap()))
    }

    pub fn error<T>(&mut self) -> Result<T> {
        Err(self.end().unwrap_err())
    }
}

pub(crate) fn is_ident(token: &mut Token) -> bool {
    static RE_IDENT: Lazy<Regex> = Lazy::new(|| {
        let re = r"^[_A-Za-z][-_0-9A-Za-z]*";
        Regex::new(re).expect("could not compile RE_IDENT regexp")
    });
    if token.ttype != TokenType::Word && token.ttype != TokenType::Ident {
        return false;
    }
    if RE_IDENT.is_match(token.value()) {
        token.ttype = TokenType::Ident;
        true
    } else {
        false
    }
}

fn is_expr(token: &mut Token) -> bool {
    if token.ttype == TokenType::Word {
        token.ttype = TokenType::Expr;
    }
    token.ttype == TokenType::Expr
}
