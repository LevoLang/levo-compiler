use std::fmt;

use crate::chars;

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
    Whitespace(WhitespaceKind),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    Paren,   // ( )
    Brace,   // { }
    Bracket, // [ ]
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
    Unknown,

    // single character
    Tab,             /* \u{0009} character tabulation */
    LineFeed,        /* \u{000A} line feed */
    VerTab,          /* \u{000B} line tabulation */
    FormFeed,        /* \u{000C} form feed */
    CarRet,          /* \u{000D} carriage return */
    Space,           /* \u{0020} space */
    NextLine,        /* \u{0085} next line */
    SpaceNoBrk,      /* \u{00A0} no-break space */
    SpaceOgham,      /* \u{1680} ogham space mark */
    EnQuad,          /* \u{2000} en quad */
    EmQuad,          /* \u{2001} em quad */
    EnSpace,         /* \u{2002} en space */
    EmSpace,         /* \u{2003} em space */
    ThirdPerEmSpace, /* \u{2004} three-per-em space */
    FourPerEmSpace,  /* \u{2005} four-per-em space */
    SixPerEmSpace,   /* \u{2006} six-per-em space */
    FigureSpace,     /* \u{2007} figure space */
    PuncSpace,       /* \u{2008} punctuation space */
    ThinSpace,       /* \u{2009} thin space */
    HairSpace,       /* \u{200A} hair space */
    LineSep,         /* \u{2028} line separator */
    ParSep,          /* \u{2029} paragraph separator */
    NarrowSpace,     /* \u{202F} narrow no-break space */
    MathSpace,       /* \u{205F} medium mathematical space */
    IdeoSpace,       /* \u{3000} ideographic space */

    // compound
    CarRetLineFeed, /* carriage return + line feed */
}

impl WhitespaceKind {
    pub fn as_char(&self) -> Option<char> {
        Some(match self {
            WhitespaceKind::Tab => '\t',
            WhitespaceKind::LineFeed => '\n',
            WhitespaceKind::VerTab => '\u{000B}',
            WhitespaceKind::FormFeed => '\u{000C}',
            WhitespaceKind::CarRet => '\r',
            WhitespaceKind::Space => ' ',
            WhitespaceKind::NextLine => '\u{0085}',
            WhitespaceKind::SpaceNoBrk => '\u{00A0}',
            WhitespaceKind::SpaceOgham => '\u{1680}',
            WhitespaceKind::EnQuad => '\u{2000}',
            WhitespaceKind::EmQuad => '\u{2001}',
            WhitespaceKind::EnSpace => '\u{2002}',
            WhitespaceKind::EmSpace => '\u{2003}',
            WhitespaceKind::Unknown => '\u{2004}',
            WhitespaceKind::ThirdPerEmSpace => '\u{2004}',
            WhitespaceKind::FourPerEmSpace => '\u{2005}',
            WhitespaceKind::SixPerEmSpace => '\u{2006}',
            WhitespaceKind::FigureSpace => '\u{2007}',
            WhitespaceKind::PuncSpace => '\u{2008}',
            WhitespaceKind::ThinSpace => '\u{2009}',
            WhitespaceKind::HairSpace => '\u{200A}',
            WhitespaceKind::LineSep => '\u{2028}',
            WhitespaceKind::ParSep => '\u{2029}',
            WhitespaceKind::NarrowSpace => '\u{202F}',
            WhitespaceKind::MathSpace => '\u{205F}',
            WhitespaceKind::IdeoSpace => '\u{3000}',
            WhitespaceKind::CarRetLineFeed => {
                return None;
            }
        })
    }
}

impl From<char> for WhitespaceKind {
    fn from(c: char) -> Self {
        match c {
            ' ' => Self::Space,
            '\r' => Self::CarRet,
            '\n' => Self::LineFeed,
            '\t' => Self::Tab,
            '\u{000B}' => Self::VerTab,
            '\u{000C}' => Self::FormFeed,
            '\u{0085}' => Self::NextLine,
            '\u{00A0}' => Self::SpaceNoBrk,
            '\u{1680}' => Self::SpaceOgham,
            '\u{2000}' => Self::EnQuad,
            '\u{2001}' => Self::EmQuad,
            '\u{2002}' => Self::EnSpace,
            '\u{2003}' => Self::EmSpace,
            '\u{2004}' => Self::ThirdPerEmSpace,
            '\u{2005}' => Self::FourPerEmSpace,
            '\u{2006}' => Self::SixPerEmSpace,
            '\u{2007}' => Self::FigureSpace,
            '\u{2008}' => Self::PuncSpace,
            '\u{2009}' => Self::ThinSpace,
            '\u{200A}' => Self::HairSpace,
            '\u{2028}' => Self::LineSep,
            '\u{2029}' => Self::ParSep,
            '\u{202F}' => Self::NarrowSpace,
            '\u{205F}' => Self::MathSpace,
            '\u{3000}' => Self::IdeoSpace,
            _ => Self::Unknown,
        }
    }
}

impl Into<Option<char>> for WhitespaceKind {
    fn into(self) -> Option<char> {
        self.as_char()
    }
}

impl fmt::Display for WhitespaceKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WhitespaceKind::Tab => write!(f, "\\t"),
            WhitespaceKind::LineFeed => write!(f, "\\n"),
            WhitespaceKind::VerTab => write!(f, "\\v"),
            WhitespaceKind::FormFeed => write!(f, "\\f"),
            WhitespaceKind::CarRet => write!(f, "\\r"),
            WhitespaceKind::Space => write!(f, "' '"),

            WhitespaceKind::CarRetLineFeed => write!(f, "\\r\\n"),
            c => write!(f, "{}", c.as_char().unwrap().escape_unicode()),
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

        let mut kind = WhitespaceKind::from(first);
        // Handling the case where whitespace is CRLF
        if kind == WhitespaceKind::CarRet {
            let Some(sec) = self.peek() else {
                return TokenKind::Whitespace(WhitespaceKind::CarRet);
            };

            if WhitespaceKind::from(sec) == WhitespaceKind::LineFeed {
                kind = WhitespaceKind::CarRetLineFeed;
                self.advance();
            }
        }

        while let Some(c) = self.peek() {
            if c == first {
                self.advance();
                if kind == WhitespaceKind::CarRetLineFeed {
                    match self.peek() {
                        Some(sec) if WhitespaceKind::from(sec) == WhitespaceKind::LineFeed => {
                            self.advance();
                        }
                        _ => {
                            self.shelf(c);
                            break;
                        }
                    }
                } else if kind == WhitespaceKind::CarRet {
                    if let Some(sec) = self.peek() {
                        if WhitespaceKind::from(sec) == WhitespaceKind::LineFeed {
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
