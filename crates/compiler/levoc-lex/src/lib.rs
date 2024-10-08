pub mod lexer;
pub mod util;

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

impl Token {
    fn new(kind: TokenKind, len: u32) -> Self {
        Self { kind, len }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Lit,

    // operators
    Plus,      // +
    Minus,     // -
    Asterisk,  // *
    Slash,     // /
    Percent,   // %
    Ampersand, // &
    Bar,       // |
    Bang,      // !

    // punctuation
    Dot,       // .
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;
    Eq,        // =
    Less,      // <
    Greater,   // >

    // delimiters
    OpenDelim(Delim),
    CloseDelim(Delim),

    Comment(Comment),
    Whitespace,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delim {
    Paren, // ( )
    Brace, // { }
    Brack, // [ ]
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Comment {
    pub kind: CommentKind,
    pub style: CommentStyle,
}

impl Comment {
    fn new(kind: CommentKind, style: CommentStyle) -> Self {
        Self { kind, style }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    Normal,
    Doc,
    InnerDoc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentStyle {
    Line,
    Block,
}
