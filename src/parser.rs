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
        Parser { tokenizer }
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
            }
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

    pub fn save_pos(&self) -> TokenPos {
        self.tokenizer.save_pos()
    }

    pub fn restore_pos(&mut self, pos: TokenPos) {
        self.tokenizer.restore_pos(pos);
    }
}

pub struct Lookahead {
    token: Vec<Result<Token>>,
    this_pos: TokenPos,
    next_pos: Vec<TokenPos>,
    expected: Vec<Cow<'static, str>>,
    found: bool,
}

impl Lookahead {
    fn do_new(parser: &mut Parser, depth: usize, skipnl: bool) -> Lookahead {
        let pos = parser.save_pos();
        let this_pos = pos;
        let mut token = Vec::new();
        let mut next_pos = Vec::new();
        for _ in 0..depth {
            let t = loop {
                match parser.do_token(true) {
                    Ok(token) if token.ttype != TokenType::Nl || !skipnl => break Ok(token),
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
            this_pos,
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
        Err(Error::new(msg, self.this_pos))
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
