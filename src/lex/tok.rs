use std::fmt;

use crate::chars;

use super::Lexer;

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

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn len(&self) -> u32 {
        self.len
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
    Ident(Ident),
    Underscore, // _

    // literal
    Lit(Lit),

    // punctuation
    Dot,        // .
    Comma,      // ,
    Semicolon,  // ;
    Colon,      // :

    // operators
    Plus,       // +
    Minus,      // -
    Asterisk,   // *
    Slash,      // /
    Percent,    // %

    // assignments
    Equals,     // =

    // delimiters
    OpenDelim(Delimiter),
    CloseDelim(Delimiter),

    // trivia
    Whitespace(WhitespaceKind),
    Comment {
        kind: CommentKind,
        style: CommentStyle,
        content: String,
        terminated: bool
    },

    // miscellaneous
    Bad {
        message: &'static str
    },
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;

        match self {
            Unknown => write!(f, "Unknown"),

            Ident(ident) => write!(f, "{ident}"),
            Underscore => write!(f, "Underscore"),

            Lit(lit) => write!(f, "{lit}"),

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
            Comment { kind, style, content, terminated}
                 => write!(f, "Comment: {{ kind: {kind}, style: {style}, message: \"{}\", \
                                terminated: {terminated} }}",
                                preview_content(content.trim())),

            Bad { message } => write!(f, "Bad: {{ message: \"{message} \" }}"),
        }
    }
}

fn preview_content(cont: &str) -> String {
    const MAX_CONTENT_LEN: usize = 40;

    if cont.len() < MAX_CONTENT_LEN {
        cont.to_owned()
    } else {
        cont.chars()
            .take(MAX_CONTENT_LEN - 3)
            .chain("...".chars())
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
}

impl Ident {
    pub fn new(name: String) -> Self {
        Self {
            name: name
        }
    }

    pub fn name(&self) -> &String {
        &self.name
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Ident { name } = self;
        write!(f, "Ident: {{ name: \"{name}\" }}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lit {
    kind: LitKind,
    text: String,
    prefix_end: u32,
    suffix_start: u32,
}

impl Lit {
    pub fn new(kind: LitKind,
               text: String,
               prefix_end: u32,
               suffix_start: u32) -> Self {
        Self {
            kind: kind,
            text: text,
            prefix_end: prefix_end,
            suffix_start: suffix_start,
        }
    }

    pub fn kind(&self) -> LitKind {
        self.kind
    }

    pub fn text(&self) -> &String {
        &self.text
    }

    pub fn suffix_start(&self) -> u32 {
        self.suffix_start
    }
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Lit { kind, text , .. } = self;
        write!(f, "Lit: {{ kind:: {kind}, text: \"{text}\" }}")
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    Paren,      // ( )
    Brace,      // { }
    Bracket,    // [ ]
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Delimiter::Paren => write!(f, "Paren"),
            Delimiter::Brace => write!(f, "Brace"),
            Delimiter::Bracket => write!(f, "Bracket"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WhitespaceKind {
    // space
    Space,
    Tab,

    // newline
    LineFeed,
    CarRet,
    CarRetLineFeed,
    FormFeed,
    VerTab,

    Other(char)
}

impl From<char> for WhitespaceKind {
    fn from(c: char) -> Self {
        use WhitespaceKind::*;

        match c {
            ' ' => Space,
            '\t' => Tab,

            '\n' => LineFeed,
            '\r' => CarRet,
            '\u{000B}' => VerTab,
            '\u{000C}' => FormFeed,

            _ => Other(c)
        }
    }
}

impl fmt::Display for WhitespaceKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WhitespaceKind::Space => write!(f, "' '"),
            WhitespaceKind::Tab => write!(f, "\\t"),
            WhitespaceKind::LineFeed => write!(f, "\\n"),
            WhitespaceKind::CarRet => write!(f, "\\r"),
            WhitespaceKind::CarRetLineFeed => write!(f, "\\r\\n"),
            WhitespaceKind::FormFeed => write!(f, "\\f"),
            WhitespaceKind::VerTab => write!(f, "\\v"),
            WhitespaceKind::Other(c) => write!(f, "Other({c})"),
        }
    }
}

impl <I> Lexer<I> where I: Iterator<Item = char> {
    pub(super) fn lex_token(&mut self) -> Option<Token> {
        use TokenKind::*;

        let start = self.cur;
        if let Some(first_char) = self.advance() {
            let kind = match first_char {
                // whitespace
                c if chars::is_whitespace(c) => self.scan_whitespace(c),

                // slash or comment
                '/' => match self.peek() {
                    Some('/') => {
                        self.advance();
                        self.scan_comment(CommentStyle::Line)
                    },
                    Some('*') => {
                        self.advance();
                        self.scan_comment(CommentStyle::Block) 
                    },
                    
                    _ => Slash,
                },

                // punc
                '.' => Dot,
                ',' => Comma,
                ';' => Semicolon,
                ':' => Colon,

                c @ ('+' | '-') if self.peek().is_some_and(chars::is_digit) => self.scan_num_lit(c),

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

                c @ '0'..='9' => self.scan_num_lit(c),

                // ident or underscore
                c if chars::is_ident_start(c) => self.scan_ident(c),

                _ => Unknown,
            };

            let end = self.cur;
            self.calibrate_buf();
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
            TokenKind::Ident(
                Ident::new(
                self.buf.iter()
                        .skip(start)
                        .take(len)
                        .collect()
                )
            )
        }
    }

    fn scan_num_lit(&mut self, _first: char) -> TokenKind {
        let start = self.cur - 1;

        let mut radix = false;
        let mut exponent = false;

        let just_had_radix = false;
        let just_had_exp = false;
        while let Some(c) = self.peek() {
            match c {
                '0'..='9' => { 
                    self.advance();
                    continue;
                },

                '.' if !exponent && !radix => {
                    radix = true;
                    self.advance();
                },

                'e' | 'E' if !exponent && !just_had_radix => {
                    exponent = true;
                    self.advance();
                }

                '_' if !just_had_exp && !just_had_radix => {
                    self.advance();
                }

                _ => break,
            }
        }

        let real = radix || exponent;
        let suffix_start = self.cur;

        let len = self.cur - start;
        let text = self.buf.iter()
                    .skip(start)
                    .take(len)
                    .collect();

        let kind = if real {
            LitKind::Real
        } else {
            LitKind::Int
        };

        TokenKind::Lit(
            Lit::new(
                kind,
                text,
                0,
                suffix_start as u32)
        )
    }

    fn scan_whitespace(&mut self, first: char) -> TokenKind {
        assert!(chars::is_whitespace(first));

        let mut kind = WhitespaceKind::from(first);
        // Handling the case where whitespace is CRLF
        if kind == WhitespaceKind::CarRet {
            let Some(sec) = self.peek() else {
                return TokenKind::Whitespace(WhitespaceKind::CarRet);
            };

            if WhitespaceKind::LineFeed == WhitespaceKind::from(sec) {
                kind = WhitespaceKind::CarRetLineFeed;
                self.advance();
            }
        }

        while let Some(c) = self.peek() {
            if c != first
                || (kind == WhitespaceKind::CarRetLineFeed // CRLF
                    && self.peek_nth(1) != Some('\n'))
                || (kind == WhitespaceKind::CarRet // CR but found CRLF
                    && self.peek_nth(1) == Some('\n')) {
                break;
            } else {
                if kind == WhitespaceKind::CarRetLineFeed {
                    self.advance_by(2);
                } else {
                    self.advance();
                }
            }
        }

        TokenKind::Whitespace(kind)
    }

    fn scan_comment(&mut self, style: CommentStyle) -> TokenKind {
        // This function assumes that we already have entered the comment area

        // Guard for empty block comments.
        if style == CommentStyle::Block
            && self.peek() == Some('*')
            && self.peek_nth(1) == Some('/') {
            return TokenKind::Comment {
                kind: CommentKind::Normal,
                style: CommentStyle::Block,
                content: String::new(),
                terminated: true
            }
        }

        let kind = match self.peek() {
            Some('/') if style == CommentStyle::Line => CommentKind::Doc,
            Some('*') if style == CommentStyle::Block => CommentKind::Doc,
            Some('!') => CommentKind::InnerDoc,
            _ => CommentKind::Normal,
        };

        if kind != CommentKind::Normal {
            self.advance();
        }

        let start = self.cur;

        let mut last_char: char = char::default();
        let mut terminated = false;
        let mut nest: u32 = 1;
        while let Some(c) = self.advance() {
            if style == CommentStyle::Line && chars::is_newline(c) {
                terminated = true;
                last_char = c;
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

        let len = if !terminated {
            self.cur - start
        } else if style == CommentStyle::Line {
            // Line comments include their end-of-line characters in their content, so if a line 
            // comment ends with CRLF, it should include the line feed at the end as well.
            if last_char == '\r' && self.peek() == Some('\n') {
                self.advance();
            }

            self.cur - start
        } else { // style == CommentStyle::Block
            self.cur - start - 2
        };

        TokenKind::Comment{
            kind: kind,
            style: style,
            content: self.buf.iter()
                                .skip(start)
                                .take(len)
                                .collect(),
            terminated: terminated,
        }
    }
}
