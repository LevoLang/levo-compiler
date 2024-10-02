use std::str::Chars;

use chars::{
    is_digit, is_ident_body, is_ident_start, is_newline, is_whitespace, Delim, Whitespace,
};

pub mod chars;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}
impl Token {
    pub fn new(kind: TokenKind, len: u32) -> Self {
        Self { kind, len }
    }
}

#[derive(Debug)]
pub enum TokenKind {
    // identifier
    Ident,
    Underscore, // _

    // literals
    Lit,

    // punctuation
    Dot,       // .
    Comma,     // ,
    Colon,     // :
    Semicolon, // ;

    // operators
    Plus,     // +
    Minus,    // -
    Asterisk, // *
    Slash,    // /
    Percent,  // %
    Bang,     // !
    Less,     // <
    Greater,  // >

    // assignments
    Eq, // =

    // delimiters
    OpenDelim(Delim),
    CloseDelim(Delim),

    // trivia
    Comment(Comment),
    Whitespace,
    Unknown,
}

#[derive(Debug)]
pub struct Comment {
    pub kind: CommentKind,
    pub style: CommentStyle,
    pub terminated: bool,
}
impl Comment {
    pub fn new(kind: CommentKind, style: CommentStyle, terminated: bool) -> Self {
        Self {
            kind,
            style,
            terminated,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CommentKind {
    Normal,
    Doc,
    InnerDoc,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CommentStyle {
    Line,
    Block,
}

pub trait Lex {
    fn next_token(&mut self) -> Option<Token>;
}

pub struct Cursor<'a> {
    iter: Chars<'a>,
    remaining: usize,
}

impl Cursor<'_> {
    pub fn new<'a>(input: &'a str) -> Cursor<'a> {
        Cursor {
            iter: input.chars(),
            remaining: input.len(),
        }
    }

    fn first(&self) -> Option<char> {
        self.iter.clone().next()
    }

    fn second(&self) -> Option<char> {
        let mut iter = self.iter.clone();
        iter.next();
        iter.next()
    }

    pub fn advance(&mut self) -> Option<char> {
        self.iter.next()
    }

    pub fn advance_nth(&mut self, n: usize) -> Option<char> {
        self.iter.nth(n)
    }

    pub fn pos_within_token(&mut self) -> u32 {
        (self.remaining - self.iter.as_str().len()) as u32
    }

    pub fn reset_pos_within_token(&mut self) {
        self.remaining = self.iter.as_str().len();
    }
}

impl Cursor<'_> {
    fn identifier(&mut self, underscore_start: bool) -> TokenKind {
        let mut is_wildcard = underscore_start;
        while self.first().is_some_and(|c| is_ident_body(c)) {
            self.advance();
            is_wildcard = false;
        }

        if is_wildcard {
            TokenKind::Underscore
        } else {
            TokenKind::Ident
        }
    }

    fn literal(&mut self) -> TokenKind {
        // first numeric chunk
        while self.first().is_some_and(|c| is_digit(c) || c == '_') {
            self.advance();
        }

        // radix and second numeric chunk
        if self.first() == Some('.') {
            match self.second() {
                Some(c) if is_digit(c) => {
                    self.advance_nth(1);
                    while self.first().is_some_and(|c| is_digit(c) || c == '_') {
                        self.advance();
                    }
                }
                _ => return TokenKind::Lit,
            }
        }

        // exponent and third numeric chunk
        match self.first() {
            Some('e') | Some('E') => match self.second() {
                Some(c) if is_digit(c) => {
                    self.advance_nth(1);
                    while self.first().is_some_and(|c| is_digit(c)) {
                        self.advance();
                    }
                }
                _ => {}
            },
            _ => {}
        }

        // suffix identifier
        match self.first() {
            Some(c) if is_ident_start(c) => {
                self.advance();
                while self.first().is_some_and(|c| is_ident_body(c)) {
                    self.advance();
                }
            }
            _ => {}
        }

        TokenKind::Lit
    }

    fn whitespace(&mut self) -> TokenKind {
        while self.first().is_some_and(|c| is_whitespace(c)) {
            self.advance();
        }

        TokenKind::Whitespace
    }

    fn comment(&mut self, style: CommentStyle) -> TokenKind {
        let kind = match self.advance_nth(1) {
            Some('*') if style == CommentStyle::Block => CommentKind::Doc,
            Some('/') if style == CommentStyle::Line => CommentKind::Doc,
            Some('!') => CommentKind::InnerDoc,
            _ => CommentKind::Normal,
        };

        let comment = if style == CommentStyle::Block
            && kind == CommentKind::Doc
            && self.first() == Some('/')
        {
            // The comment '/**/' is considered an empty normal block comment, but at this point it
            // will be recognized as an ill-formed doc comment, so it must be handled specially.
            self.advance();
            Comment::new(CommentKind::Normal, CommentStyle::Block, true)
        } else {
            let mut nesting_depth = 1;
            let mut terminated = true;
            loop {
                match self.advance() {
                    Some('/') if style == CommentStyle::Block => {
                        if let Some('*') = self.advance() {
                            nesting_depth += 1;
                        }
                    }
                    Some('*') if style == CommentStyle::Block => {
                        if let Some('/') = self.advance() {
                            nesting_depth -= 1;
                            if nesting_depth == 0 {
                                break;
                            }
                        }
                    }
                    Some(c) if style == CommentStyle::Line && is_newline(c) => {
                        if Whitespace::from(c) == Whitespace::CarRet {
                            match self.first() {
                                Some(c) if Whitespace::from(c) == Whitespace::LineFeed => {
                                    self.advance();
                                }
                                _ => {}
                            }
                        };
                        break;
                    }
                    None => {
                        terminated = false;
                        break;
                    }
                    _ => {}
                }
            }

            Comment::new(kind, style, terminated)
        };

        TokenKind::Comment(comment)
    }
}

impl Lex for Cursor<'_> {
    fn next_token(&mut self) -> Option<Token> {
        let Some(cur) = self.advance() else {
            return None;
        };

        let kind = match cur {
            // line comment, block comment or slash
            '/' => match self.first() {
                Some('/') => self.comment(CommentStyle::Line),
                Some('*') => self.comment(CommentStyle::Block),
                _ => TokenKind::Minus,
            },

            // whitespace
            c if is_whitespace(c) => self.whitespace(),

            // int literal
            c if is_digit(c) => self.literal(),

            // identifier, underscore
            c if is_ident_start(c) => self.identifier(c == '_'),

            // one-letter symbols
            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Asterisk,
            '%' => TokenKind::Percent,
            '<' => TokenKind::Less,
            '!' => TokenKind::Bang,
            '>' => TokenKind::Greater,
            '=' => TokenKind::Eq,
            '(' => TokenKind::OpenDelim(Delim::Paren),
            ')' => TokenKind::CloseDelim(Delim::Paren),
            '{' => TokenKind::OpenDelim(Delim::Brace),
            '}' => TokenKind::CloseDelim(Delim::Brace),
            '[' => TokenKind::OpenDelim(Delim::Brack),
            ']' => TokenKind::CloseDelim(Delim::Brack),

            _ => TokenKind::Unknown,
        };

        let token = Token::new(kind, self.pos_within_token());
        self.reset_pos_within_token();
        Some(token)
    }
}
