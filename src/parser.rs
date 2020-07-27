use once_cell::sync::Lazy;
use regex::Regex;

use crate::error::{Error, Result};
use crate::tokenizer::*;

pub(crate) struct Parser {
    pub(crate) t: Tokenizer,
    expected: Vec<&'static str>,
    try_ok: bool,
}

impl Parser {
    pub fn new(t: Tokenizer) -> Parser {
        Parser {
            t,
            try_ok: false,
            expected: Vec::new(),
        }
    }

    fn next_token(&mut self) -> Result<Token> {
        let token = match self.t.next_token() {
            None => {
                return Err(Error {
                    pos: self.t.pos,
                    msg: "unexpected end-of-file".to_string(),
                })
            }
            Some(token) => token,
        };
        match token.ttype {
            TokenType::UnterminatedString => {
                return Err(Error {
                    pos: token.pos,
                    msg: "unterminated string literal".to_string(),
                });
            }
            TokenType::Unknown => {
                return Err(Error {
                    pos: token.pos,
                    msg: "unexpected token".to_string(),
                });
            }
            _ => {}
        }

        Ok(token)
    }

    pub fn push_token(&mut self, token: Token) {
        self.t.push_token(token);
    }

    pub fn start_try(&mut self) {
        self.expected.clear();
        self.try_ok = false;
    }

    pub fn end_try(&mut self) -> Result<()> {
        if self.try_ok || self.expected.len() == 0 {
            return Ok(());
        }
        let last = if self.expected.len() > 1 {
            self.expected.pop()
        } else {
            None
        };
        let mut msg = "expected ".to_string() + &(self.expected.join(", "));
        if let Some(last) = last {
            msg += &format!("or {}", last);
        }
        Err(Error {
            pos: self.t.pos,
            msg,
        })
    }

    /*
    pub fn try_eof(&mut self) -> bool {
        self.t.peek().is_none()
    }
    */

    pub fn expect_eof(&mut self) -> Result<()> {
        if self.t.peek().is_some() {
            Err(Error {
                pos: self.t.pos,
                msg: "expected end-of-file".to_string(),
            })
        } else {
            Ok(())
        }
    }

    pub fn try_expect(&mut self, want: TokenType) -> Result<Option<Token>> {
        log::debug!("try_expect {:?}", want);
        if self.try_ok {
            log::debug!("+-- ignored");
            return Ok(None);
        }

        let pos = self.t.pos;
        let mut token = self.next_token()?;

        if token.ttype == TokenType::Word {
            if want == TokenType::Ident && is_ident(&mut token) {
                self.try_ok = true;
                log::debug!("+-- found {:?}", token);
                return Ok(Some(token));
            }
            if want == TokenType::Expr && is_expr(&mut token) {
                self.try_ok = true;
                log::debug!("+-- found {:?}", token);
                return Ok(Some(token));
            }
        }

        if want == token.ttype {
            self.try_ok = true;
            log::debug!("+-- found {:?}", token);
            return Ok(Some(token));
        }

        log::debug!("+-- failed, got {:?}", token);

        self.expected.push(want.as_str());
        self.t.pos = pos;
        Ok(None)
    }

    pub fn expect(&mut self, ttype: TokenType) -> Result<Token> {
        self.start_try();
        if let Some(token) = self.try_expect(ttype)? {
            return Ok(token);
        }
        self.end_try()?;
        unreachable!()
    }

    pub fn peek(&mut self) -> Result<Option<Token>> {
        Ok(self.t.peek())
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
