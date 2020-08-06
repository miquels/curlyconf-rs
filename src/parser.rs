use std::borrow::Cow;

use once_cell::sync::Lazy;
use regex::Regex;

use crate::error::{Error, Result};
use crate::tokenizer::*;

pub struct Parser {
    tokenizer: Tokenizer,
}

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Parser {
        Parser {
            tokenizer
        }
    }

    // Read the next token, and translate unexpected tokens to errors.
    fn do_token(&mut self, eof_ok: bool) -> Result<Token> {

        let token = self.tokenizer.next_token();

        match token.ttype {
            TokenType::Eof => {
                if eof_ok {
                    return Ok(token.clone());
                }
                return Err(Error::new("unexpected end-of-file", token.pos));
            },
            TokenType::UnterminatedString => {
                return Err(Error::new("unterminated string literal", token.pos));
            }
            TokenType::Unknown => {
                return Err(Error::new("unexpected token", token.pos));
            }
            _ => {}
        }

        Ok(token.clone())
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
                Ok(token) => {
                    match token.ttype {
                        TokenType::Nl => {},
                        TokenType::Eof => {
                            is_eof = true;
                            break;
                        }
                        _ => break,
                    }
                },
                Err(_) => break,
            }
        }
        self.restore_pos(pos);
        is_eof
    }

    // Expect a certain token-type.
    pub fn expect(&mut self, want: TokenType) -> Result<Token> {
        log::debug!("expect {:?}", want);
        // skip newlines, _unless_ that is what we are looking for.
        let mut token = loop {
            let t = self.next_token()?;
            if t.ttype == want || t.ttype != TokenType::Nl {
                break t;
            }
        };

        if token.ttype == TokenType::Word {
            if want == TokenType::Ident && is_ident(&mut token) {
                log::debug!("+-- found {:?}", token);
                return Ok(token);
            }
            if want == TokenType::Expr && is_expr(&mut token) {
                log::debug!("+-- found {:?}", token);
                return Ok(token);
            }
        }

        if want == token.ttype {
            log::debug!("+-- found {:?}", token);
            return Ok(token);
        }

        log::debug!("+-- failed, got {:?}", token);
        Err(Error::new(format!("expected {}", want.as_str()), token.pos))
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

    pub fn lookahead(&mut self) -> Lookahead {
        Lookahead::new(self)
    }

    pub fn lookaheadnl(&mut self) -> Lookahead {
        Lookahead::newnl(self)
    }

    pub fn save_pos(&self) -> TokenPos {
        self.tokenizer.save_pos()
    }

    pub fn restore_pos(&mut self, pos: TokenPos) {
        self.tokenizer.restore_pos(pos);
    }
}

pub struct Lookahead<'a> {
    token: Result<Token>,
    token2: Result<Token>,
    this_pos: TokenPos,
    next_pos: TokenPos,
    next_pos2: TokenPos,
    parser: &'a mut Parser,
    expected: Vec<Cow<'static, str>>,
    found: bool,
}

impl<'a> Lookahead<'a> {
    pub fn do_new(parser: &'a mut Parser, skipnl: bool) -> Lookahead<'a> {
        let pos = parser.save_pos();
        let this_pos = pos;
        let token = loop {
            match parser.do_token(true) {
                Ok(token) if token.ttype != TokenType::Nl || !skipnl => break Ok(token),
                Err(e) => break Err(e),
                _ => {},
            }
        };
        let next_pos = parser.save_pos();
        let token2 = loop {
            match parser.do_token(true) {
                Ok(token) if token.ttype != TokenType::Nl || !skipnl => break Ok(token),
                Err(e) => break Err(e),
                _ => {},
            }
        };
        let next_pos2 = parser.save_pos();
        parser.restore_pos(pos);

        Lookahead {
            token,
            token2,
            this_pos,
            next_pos,
            next_pos2,
            expected: Vec::new(),
            found: false,
            parser,
        }
    }

    pub fn new(parser: &'a mut Parser) -> Lookahead<'a> {
        Lookahead::do_new(parser, true)
    }

    pub fn newnl(parser: &'a mut Parser) -> Lookahead<'a> {
        Lookahead::do_new(parser, false)
    }

    pub fn peek(&mut self, want: TokenType) -> Result<Option<Token>> {

        if self.found {
            return Ok(None);
        }

        let token = self.token.clone()?;
        let t = self.do_peek(token, want);
        if t.is_none() {
            self.expected.push(Cow::Borrowed(want.as_str()));
        }
        Ok(t)
    }

    pub fn peek2(&mut self, want1: TokenType, want2: TokenType) -> Result<Option<Token>> {

        if self.found {
            return Ok(None);
        }

        let token1 = self.token.clone()?;
        if let Some(t1) = self.do_peek(token1, want1) {

            if let Ok(token2) = self.token2.clone() {
                if let Some(t2) = self.do_peek(token2, want2) {
                    // We have a match.
                    self.next_pos = self.next_pos2;
                    // If the first token is a word/ident/expr, return that,
                    // otherwise return the second token.
                    match t1.ttype {
                        TokenType::Word|TokenType::Ident|TokenType::Expr => {
                            return Ok(Some(t1));
                        }
                        _ => return Ok(Some(t2)),
                    }
                }
            }
        }

        let want_str = format!("{} {}", want1.as_str(), want2.as_str());
        self.expected.push(Cow::Owned(want_str));
        Ok(None)
    }

    fn do_peek(&mut self, mut token: Token, want: TokenType) -> Option<Token> {

        if token.ttype == TokenType::Word {
            if want == TokenType::Ident && is_ident(&mut token) {
                self.found = true;
                log::debug!("+-- found {:?}", token);
                return Some(token);
            }
            if want == TokenType::Expr && is_expr(&mut token) {
                self.found = true;
                log::debug!("+-- found {:?}", token);
                return Some(token);
            }
        }

        if want == token.ttype {
            self.found = true;
            log::debug!("+-- found {:?}", token);
            return Some(token);
        }

        log::debug!("+-- failed, got {:?}", token);

        None
    }

    pub fn advance(&mut self) {
        log::debug!("lookahead::advance: to {:?}", self.next_pos);
        self.parser.restore_pos(self.next_pos);
        self.expected.clear();
        self.found = false;
    }

    pub fn end(&mut self) -> Result<()> {
        if self.expected.len() == 0 {
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
        Err(Error::new(msg, self.this_pos))
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
    if RE_IDENT.is_match(&token.value) {
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
