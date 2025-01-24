use crate::parse::error::{Expected, ParseError};
use crate::parse::lexer::Lexer;
use crate::parse::token::Token;
use std::ops::Range;

pub struct Parser<'src> {
    pub(crate) lexer: Lexer<'src>,
    pub(crate) peeked: Peeked,
}

type IndexedToken = (Token, Range<usize>);

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            lexer: Lexer::new(src),
            peeked: Peeked::new(),
        }
    }

    pub(crate) fn peek_first(&mut self) -> Result<IndexedToken, ParseError> {
        if self.peeked.one.is_none() {
            self.peeked.one = self.lexer.next_token();
        }

        self.peeked.one.clone().ok_or(ParseError::UnexpectedEof)
    }

    pub(crate) fn slice_at(&self, range: Range<usize>) -> &str {
        &self.lexer.src[range]
    }

    pub(crate) fn peek_second(&mut self) -> Result<IndexedToken, ParseError> {
        if self.peeked.two.is_none() {
            self.peeked.two = self.lexer.next_token();
        }

        self.peeked.two.clone().ok_or(ParseError::UnexpectedEof)
    }

    pub(crate) fn peek(&mut self) -> Result<(IndexedToken, IndexedToken), ParseError> {
        Ok((self.peek_first()?, self.peek_second()?))
    }

    pub(crate) fn next_filter<T>(
        &mut self,
        f: impl FnOnce(Token) -> Option<T>,
    ) -> Result<Option<T>, ParseError> {
        let x = f(self.peek_first()?.0);
        if x.is_some() {
            self.next()?;
        }
        Ok(x)
    }

    pub(crate) fn next(&mut self) -> Result<(Token, Range<usize>), ParseError> {
        if self.peeked.one.is_none() {
            self.peeked.one = self.peeked.two.take();
        }

        self.peeked
            .one
            .take()
            .or_else(|| self.lexer.next_token())
            .ok_or(ParseError::UnexpectedEof)
    }

    pub(crate) fn expect(&mut self, tokens: &[Token]) -> Result<(), ParseError> {
        let given = self.next()?.0;
        if !tokens.contains(&given) {
            return Err(ParseError::Unexpected {
                expected: Expected::Tokens(tokens.to_vec()),
                given,
            });
        }

        Ok(())
    }

    pub(crate) fn matches(&mut self, first: Token, second: Token) -> bool {
        let Ok(((f, _), (s, _))) = self.peek() else {
            return false;
        };
        first == f && second == s
    }

    pub(crate) fn matches_first(&mut self, token: Token) -> bool {
        self.peek_first().map(|(t, _)| t == token).unwrap_or(false)
    }

    pub(crate) fn matches_next(&mut self, first: Token, second: Token) -> bool {
        if self.matches(first, second) {
            self.peeked = Peeked::new();
            true
        } else {
            false
        }
    }

    pub(crate) fn matches_next_first(&mut self, token: Token) -> bool {
        if self.matches_first(token) {
            self.peeked.one = None;
            true
        } else {
            false
        }
    }

    pub(crate) fn next_ident(&mut self) -> Result<&'src str, ParseError> {
        self.expect(&[Token::Ident])?;
        Ok(self.lexer.slice())
    }
}

pub(crate) struct Peeked {
    pub(crate) one: Option<IndexedToken>,
    pub(crate) two: Option<IndexedToken>,
}

impl Peeked {
    const fn new() -> Self {
        Self {
            one: None,
            two: None,
        }
    }
}
