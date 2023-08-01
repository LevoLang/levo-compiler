use core::fmt;
use std::error;

pub type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    kind: LexErrorKind,
}

impl LexError {
    pub fn kind(&self) -> LexErrorKind {
        self.kind
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexErrorKind {
    BadToken,
}

impl error::Error for LexError {}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            LexErrorKind::BadToken => write!(f, "Bad token encountered")
        }
    }
}

pub struct Token<'a> {
    kind: TokenKind<'a>,
    len: usize,
}

impl<'a> Token<'a> {
    pub fn kind(&self) -> TokenKind {
        self.kind
    }

    pub fn class(&self) -> TokenClass {
        self.kind.class()
    }

    pub fn is_eof(&self) -> bool { self.kind.is_eof() }

    pub fn is_literal(&self) -> bool { self.kind.is_literal() }

    pub fn len(&self) -> usize { self.len }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind<'a> {
    Eof,

    IntLiteral {
        text: &'a String,
    },

    Plus, // +
    Minus, // -
    Asterisk, // *
    Slash, // /
    Percent, // %

    OpenParen, // (
    ClosedParen, // )
}

impl<'a> TokenKind<'a> {
    pub fn class(&self) -> TokenClass {
        match self {
            Self::Eof => TokenClass::Miscellaneous,

            Self::IntLiteral { text: _ } => TokenClass::Literal,

            _ => TokenClass::Punctuation,
        }
    }

    pub fn is_eof(&self) -> bool {
        *self == Self::Eof
    }

    pub fn is_literal(&self) -> bool {
        self.class() == TokenClass::Literal
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenClass {
    Literal,
    Keyword,
    Identifier,
    Punctuation,
    Miscellaneous,
}
