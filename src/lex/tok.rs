use std::fmt;

use crate::chars::{self, Delimiter, Whitespace};

use super::{Lexer, Tok};

pub struct Token {
    kind: TokenKind,
    len: u32,
}

impl Token {
    pub fn new(kind: TokenKind, len: u32) -> Self {
        Self {
            kind: kind,
            len: len,
        }
    }

    pub fn dummy() -> Self {
        Self {
            kind: TokenKind::Unknown,
            len: 0,
        }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }
}

impl Tok for Token {
    fn len(&self) -> u32 {
        self.len
    }

    fn is_trivia(&self) -> bool {
        self.kind().is_trivia()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Token { kind, len } = self;
        write!(f, "Token {{ kind: {kind}, len: {len} }}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Unknown,

    // identifier
    Ident,
    Underscore, // _

    // literal
    Lit {
        kind: LitKind,
        suffix_start: u32,
    },

    // punctuation
    Dot,       // .
    Comma,     // ,
    Semicolon, // ;
    Colon,     // :

    // operators
    Plus,     // +
    Minus,    // -
    Asterisk, // *
    Slash,    // /
    Percent,  // %

    // assignments
    Equals, // =

    // delimiters
    OpenDelim(Delimiter),
    CloseDelim(Delimiter),

    // trivia
    Whitespace(Whitespace),
    Comment {
        kind: CommentKind,
        style: CommentStyle,
        terminated: bool,
    },
}

impl TokenKind {
    pub fn is_trivia(&self) -> bool {
        use TokenKind::*;
        match self {
            Whitespace(_) | Comment { .. } => true,
            _ => false,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;

        match self {
            Unknown => write!(f, "Unknown"),

            Ident => write!(f, "Ident"),
            Underscore => write!(f, "Underscore"),

            Lit { kind, suffix_start } => {
                write!(f, "Lit: {{ kind: {kind}, suffix_start: {suffix_start} }}")
            }

            Dot => write!(f, "Dot"),
            Comma => write!(f, "Comma"),
            Semicolon => write!(f, "Semicolon"),
            Colon => write!(f, "Colon"),

            Plus => write!(f, "Plus"),
            Minus => write!(f, "Minus"),
            Asterisk => write!(f, "Asterisk"),
            Slash => write!(f, "Slash"),
            Percent => write!(f, "Percent"),

            Equals => write!(f, "Equals"),

            OpenDelim(kind) => write!(f, "OpenDelim({kind})"),
            CloseDelim(kind) => write!(f, "CloseDelim({kind})"),

            Whitespace(kind) => write!(f, "Whitespace({kind})"),
            Comment {
                kind,
                style,
                terminated,
            } => write!(
                f,
                "Comment: {{ kind: {kind}, style: {style}, terminated: {terminated} }}"
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LitKind {
    Int,
    Real,
}

impl fmt::Display for LitKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LitKind::Int => write!(f, "Int"),
            LitKind::Real => write!(f, "Real"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    Normal,
    Doc,
    InnerDoc,
}

impl fmt::Display for CommentKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CommentKind::Normal => write!(f, "Normal"),
            CommentKind::Doc => write!(f, "Doc"),
            CommentKind::InnerDoc => write!(f, "InnerDoc"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentStyle {
    Line,
    Block,
}

impl fmt::Display for CommentStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CommentStyle::Line => write!(f, "Line"),
            CommentStyle::Block => write!(f, "Block"),
        }
    }
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub(super) fn lex_token(&mut self) -> Option<Token> {
        use TokenKind::*;

        let start = self.cur;
        if let Some(c) = self.advance() {
            let kind = match c {
                // whitespace
                c if chars::is_whitespace(c) => self.scan_whitespace(c),

                // slash or comment
                '/' => match self.peek() {
                    Some('/') => {
                        self.advance();
                        self.scan_comment(CommentStyle::Line)
                    }
                    Some('*') => {
                        self.advance();
                        self.scan_comment(CommentStyle::Block)
                    }
                    _ => Slash,
                },

                // punc
                '.' => Dot,
                ',' => Comma,
                ';' => Semicolon,
                ':' => Colon,

                '+' => Plus,
                '-' => Minus,
                '*' => Asterisk,
                '%' => Percent,

                '=' => Equals,

                '(' => OpenDelim(Delimiter::Paren),
                ')' => CloseDelim(Delimiter::Paren),

                '{' => OpenDelim(Delimiter::Brace),
                '}' => CloseDelim(Delimiter::Brace),

                '[' => OpenDelim(Delimiter::Bracket),
                ']' => CloseDelim(Delimiter::Bracket),

                '0'..='9' => self.scan_num_lit(c),

                // ident or underscore
                c if chars::is_ident_start(c) => self.scan_ident(c),

                _ => Unknown,
            };

            let end = self.cur;
            Some(Token::new(kind, (end - start) as u32))
        } else {
            return None;
        }
    }

    fn scan_ident(&mut self, first: char) -> TokenKind {
        assert!(chars::is_ident_start(first));

        let start = self.cur - 1;
        while let Some(c) = self.peek() {
            if chars::is_ident_body(c) {
                self.advance();
            } else {
                break;
            }
        }

        let len = self.cur - start;
        if len == 1 && first == '_' {
            TokenKind::Underscore
        } else {
            TokenKind::Ident
        }
    }

    fn scan_num_lit(&mut self, first: char) -> TokenKind {
        assert!(chars::is_digit(first));

        let start = self.cur - 1;

        let mut radix = false;
        let mut exponent = false;

        while let Some(c) = self.peek() {
            match c {
                '0'..='9' => {
                    self.advance();
                    continue;
                }

                '.' if !exponent && !radix => {
                    self.advance();
                    if let Some(next) = self.peek() {
                        match next {
                            '0'..='9' => {
                                radix = true;
                                self.advance();
                            }
                            _ => {
                                self.shelf('.');
                                break;
                            }
                        }
                    }
                }

                c @ ('e' | 'E') if !exponent => {
                    self.advance();
                    if let Some(next) = self.peek() {
                        match next {
                            '0'..='9' => {
                                exponent = true;
                                self.advance();
                            }
                            _ => {
                                self.shelf(c);
                                break;
                            }
                        }
                    }
                }

                '_' => {
                    self.advance();
                }

                _ => break,
            }
        }

        let suffix_start: u32 = (self.cur - start) as u32;

        // suffix check
        if let Some(c) = self.peek() {
            if chars::is_ident_start(c) {
                self.advance();
                while let Some(c) = self.peek() {
                    if chars::is_ident_body(c) {
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        let kind = if radix || exponent {
            LitKind::Real
        } else {
            LitKind::Int
        };

        TokenKind::Lit { kind, suffix_start }
    }

    fn scan_whitespace(&mut self, first: char) -> TokenKind {
        assert!(chars::is_whitespace(first));

        let mut kind = Whitespace::from(first);
        // Handling the case where whitespace is CRLF
        if kind == Whitespace::CarRet {
            let Some(sec) = self.peek() else {
                return TokenKind::Whitespace(Whitespace::CarRet);
            };

            if Whitespace::from(sec) == Whitespace::LineFeed {
                kind = Whitespace::CarRetLineFeed;
                self.advance();
            }
        }

        while let Some(c) = self.peek() {
            if c == first {
                self.advance();
                if kind == Whitespace::CarRetLineFeed {
                    match self.peek() {
                        Some(sec) if Whitespace::from(sec) == Whitespace::LineFeed => {
                            self.advance();
                        }
                        _ => {
                            self.shelf(c);
                            break;
                        }
                    }
                } else if kind == Whitespace::CarRet {
                    if let Some(sec) = self.peek() {
                        if Whitespace::from(sec) == Whitespace::LineFeed {
                            self.shelf(c);
                            break;
                        }
                    }
                }
            } else {
                break;
            }
        }

        TokenKind::Whitespace(kind)
    }

    fn scan_comment(&mut self, style: CommentStyle) -> TokenKind {
        // This function assumes that we already have entered the comment area

        let kind = match self.peek() {
            Some('/') if style == CommentStyle::Line => CommentKind::Doc,
            Some('*') if style == CommentStyle::Block => CommentKind::Doc,
            Some('!') => CommentKind::InnerDoc,
            _ => CommentKind::Normal,
        };

        // Consume the designator character.
        if kind != CommentKind::Normal {
            self.advance();
        }

        // Empty comment block.
        if kind == CommentKind::Doc && style == CommentStyle::Block && self.peek() == Some('/') {
            self.advance();
            return TokenKind::Comment {
                kind: CommentKind::Normal,
                style: CommentStyle::Block,
                terminated: true,
            };
        }

        let mut last_char = None;
        let mut terminated = false;
        let mut nest: u32 = 1;
        while let Some(c) = self.advance() {
            if style == CommentStyle::Line && chars::is_newline(c) {
                terminated = true;
                last_char = Some(c);
                break;
            } else if style == CommentStyle::Block && c == '*' && self.peek() == Some('/') {
                self.advance();
                nest -= 1;

                if nest == 0 {
                    terminated = true;
                    break;
                }
            } else if style == CommentStyle::Block && c == '/' && self.peek() == Some('*') {
                self.advance();
                nest += 1;
            }
        }

        if style == CommentStyle::Line && last_char == Some('\r') && self.peek() == Some('\n') {
            self.advance();
        }

        TokenKind::Comment {
            kind: kind,
            style: style,
            terminated: terminated,
        }
    }
}
