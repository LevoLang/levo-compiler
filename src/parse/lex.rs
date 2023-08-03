// Imports

use core::fmt;
use std::error;

// Start

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

impl From<LexErrorKind> for LexError {
    fn from(value: LexErrorKind) -> Self {
        LexError { kind: value }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexErrorKind {
    BadToken,
    WrongClass,
    EOF,
}

impl error::Error for LexError {}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            LexErrorKind::BadToken => write!(f, "Bad token encountered"),
            LexErrorKind::WrongClass => write!(f, "Wrong token class encountered"),
            LexErrorKind::EOF => write!(f, "End of file reached"),
        }
    }
}

pub struct Token {
    kind: TokenKind,
    len: usize,
}

impl Token {
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn class(&self) -> TokenClass {
        self.kind.class()
    }

    pub fn is_literal(&self) -> bool {
        self.kind.is_literal()
    }

    pub fn len(&self) -> usize { self.len }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            TokenKind::Ident { name } => write!(f, "Ident: {}", name),
            TokenKind::IntLit { text } => write!(f, "IntLit: {}", text),
            TokenKind::Plus => write!(f, "Plus"),
            TokenKind::Minus => write!(f, "Minus"),
            TokenKind::Asterisk => write!(f, "Asterisk"),
            TokenKind::Slash => write!(f, "Slash"),
            TokenKind::Percent => write!(f, "Percent"),
            TokenKind::LeftParen => write!(f, "LeftParen"),
            TokenKind::RightParen => write!(f, "RightParen"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Ident { 
        name: String,
    },

    IntLit {
        text: String,
    },

    Plus, // +
    Minus, // -
    Asterisk, // *
    Slash, // /
    Percent, // %

    LeftParen, // (
    RightParen, // )
}

impl TokenKind {
    pub fn class(&self) -> TokenClass {
        match self {
            Self::IntLit { text: _ } => TokenClass::Lit,

            _ => TokenClass::Punc,
        }
    }

    pub fn is_literal(&self) -> bool {
        self.class() == TokenClass::Lit
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenClass {
    Lit,
    Keyword,
    Ident,
    Punc,
}

pub trait Lex {
    fn lex(&mut self) -> Result<Token>;
}

use std::iter::Peekable;

pub struct Lexer<I> where I: Iterator<Item = char> {
    cur: Peekable<I>,

    ident_buf: String,
}

pub fn create_lexer<I>(i: I) -> Lexer<I> where I: Iterator<Item = char> {
    Lexer { 
        cur: i.peekable(),

        ident_buf: String::with_capacity(32),
    }
}

impl<I> Lex for Lexer<I> where I: Iterator<Item = char> {
    fn lex(&mut self) -> Result<Token> {
        self.lex_token()
    }
}

impl <I> Lexer<I> where I: Iterator<Item = char> {
    fn lex_token(&mut self) -> Result<Token> {
        // For the sake of convenience, allow using TokenKind variants directly.
        use TokenKind::*;

        self.skip_whitespace();

        if let Some(ch) = self.cur.peek().copied() {
            match ch {
                '+' => {
                    self.cur.next();
                    Ok(Token { kind: Plus, len: 1 })
                },
                '-' => {
                    self.cur.next();
                    Ok(Token { kind: Minus, len: 1 })
                },
                '*' => {
                    self.cur.next();
                    Ok(Token { kind: Asterisk, len: 1 })
                },
                '/' => {
                    self.cur.next();
                    Ok(Token { kind: Slash, len: 1 })
                }
                '%' => {
                    self.cur.next();
                    Ok(Token { kind: Percent, len: 1 })
                },

                '(' => { self.cur.next();
                    Ok(Token { kind: LeftParen, len: 1 })
                },

                ')' => { self.cur.next();
                    Ok(Token { kind: RightParen, len: 1 })
                },

                'a'..='z' | 'A'..='Z' => self.lex_ident(),

                _ => todo!(),
            }
        } else {
            Err(LexError::from(LexErrorKind::EOF))
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.cur.peek().copied() {
            match ch {
                ' ' | '\n' | '\r' | '\t' => self.cur.next(),
                _ => break,
            };
        }
    }

    fn lex_ident(&mut self) -> Result<Token> {
        // For the sake of convenience, allow using TokenKind variants directly.
        use TokenKind::*;

        self.ident_buf.clear();
        assert_eq!(0, self.ident_buf.len());

        let push_char = |s: &mut Self, ch| {
            s.ident_buf.push(ch);
            s.cur.next();
        };

        loop {
            if let Some(ch) = self.cur.peek().copied() {
                match ch {
                    'a'..='z' | 'A'..='Z' | '_' => push_char(self, ch),
                    '0'..='9' if self.ident_buf.len() > 0 => push_char(self, ch),
                    ' ' | '\t' | '\n' | '.' | ',' | ';' | '(' | ')' | '[' | ']' => break,
                    _ if self.ident_buf.is_empty() && is_ident_start(ch) => push_char(self, ch),
                    _ if !self.ident_buf.is_empty() && is_ident_body(ch) => push_char(self, ch),
                    _ => break,
                }
            } else {
                return Err(LexError::from(LexErrorKind::EOF));
            }
        }
        
        if self.ident_buf.is_empty() {
            Err(LexError::from(LexErrorKind::WrongClass))
        } else {
            Ok(Token { kind: Ident { name: self.ident_buf.clone() }, len: self.ident_buf.len() })
        }
    }

}

fn is_ident_start(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false,
    }
}

fn is_ident_body(ch: char) -> bool {
    match ch {
        'a'..='z' | 'A'..='Z' | '_' => true,
        '0'..='9' => true,
        _ => false,
    }
}
