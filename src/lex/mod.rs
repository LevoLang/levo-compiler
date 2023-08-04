use std::cmp::min;
use std::fmt;

use crate::chars;

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

    //Ident(Ident),
    //Lit(Lit),

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
        match self {
            TokenKind::Unknown => write!(f, "Unknown"),

            TokenKind::Dot => write!(f, "Dot"),
            TokenKind::Comma => write!(f, "Comma"),
            TokenKind::Semicolon => write!(f, "Semicolon"),
            TokenKind::Colon => write!(f, "Colon"),

            TokenKind::Plus => write!(f, "Plus"),
            TokenKind::Minus => write!(f, "Minus"),
            TokenKind::Asterisk => write!(f, "Asterisk"),
            TokenKind::Slash => write!(f, "Slash"),
            TokenKind::Percent => write!(f, "Percent"),
            TokenKind::OpenDelim(kind) => write!(f, "OpenDelim({kind})"),
            TokenKind::CloseDelim(kind) => write!(f, "CloseDelim({kind})"),
            TokenKind::Whitespace(kind) => write!(f, "Whitespace({kind})"),
            TokenKind::Bad { message } => write!(f, "Bad: {{ message: \"{message} \" }}"),
            TokenKind::Comment { kind, style, content, terminated}
                 => write!(f, "Comment: {{ kind: {kind}, style: {style}, message: \"{}\", \
                                terminated: {terminated} }}",
                                preview_content(content.trim())),
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

pub trait Lex {
    fn lex(&mut self) -> Option<Token>;
}

pub struct Lexer<I: Iterator<Item = char>> {
    iter: I,

    buf: Vec<char>,
    cur: usize,
}

impl<I> Lexer<I> where I: Iterator<Item = char> {
    pub fn new(it: I) -> Self {
        Self {
            iter: it,

            buf: Vec::with_capacity(64),
            cur: 0,
        }
    }

    fn read(&mut self, amount: usize) -> bool {
        self.buf.reserve(amount);
        self.read_full()
    }

    fn read_full(&mut self) -> bool {
        let cap = self.buf.capacity();
        let len = self.buf.len();
        while let Some(c) = self.iter.next() {
            self.buf.push(c);
            if self.buf.len() >= cap {
                break;
            }
        }

        len != self.buf.len()
    }

    fn peek(&mut self) -> Option<char> {
        self.peek_nth(0)
    }

    fn peek_nth(&mut self, n: usize) -> Option<char> {
        let idx = self.cur + n;
        if idx >= self.buf.len() {
            self.read(idx - self.buf.len() + 1);
        }

        if idx < self.buf.len() {
            Some(self.buf[idx])
        } else {
            None
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.advance_by(1)
    }
    
    fn advance_by(&mut self, n: usize) -> Option<char> {
        let cur = self.cur;

        let dest = self.cur + n;
        if dest >= self.buf.len() {
            self.read(dest - self.buf.len() + 1);
        }

        if cur < self.buf.len() {
            self.cur = dest;
            Some(self.buf[cur])
        } else {
            None
        }
    }

    fn calibrate_buf(&mut self) {
        let cap = self.buf.capacity();
        if self.buf.len() == self.buf.capacity()
            && (self.cur != 0 && self.cur * 2 >= cap) {
            // The cursor is in the 2nd half of the buffer.
            let end = self.buf.len();
            let start = min(self.cur, end);

            self.buf.copy_within(start..end, 0);
            self.buf.truncate(end - start);
            self.cur = 0;

            self.read_full();
        }
    }
}

impl<I> Lex for Lexer<I> where I: Iterator<Item = char> {
    fn lex(&mut self) -> Option<Token> {
        use TokenKind::*;

        let start = self.cur;
        if let Some(first_char) = self.advance() {
            let kind = match first_char {
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

                '.' => Dot,
                ',' => Comma,
                ';' => Semicolon,
                ':' => Colon,

                '+' => Plus,
                '-' => Minus,
                '*' => Asterisk,
                '%' => Percent,
                
                '(' => OpenDelim(Delimiter::Paren),
                ')' => CloseDelim(Delimiter::Paren),
                
                '{' => OpenDelim(Delimiter::Brace),
                '}' => CloseDelim(Delimiter::Brace),
                
                '[' => OpenDelim(Delimiter::Bracket),
                ']' => CloseDelim(Delimiter::Bracket),

                c if chars::is_whitespace(c) => self.scan_whitespace(c),

                _ => Unknown,
            };

            let end = self.cur;
            self.calibrate_buf();
            Some(Token::new(kind, (end - start) as u32))
        } else {
            return None;
        }
    }
}

impl <I> Lexer<I> where I: Iterator<Item = char> {
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
