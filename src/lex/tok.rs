use std::fmt;

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
