use std::str::Chars;

use crate::{util::CharExt, Comment, CommentKind, CommentStyle, Delim, Token, TokenKind};

pub struct Lexer<'a> {
    chars: Chars<'a>,
    cur: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars(),
            cur: 0,
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.cur += 1;
        self.chars.next()
    }

    fn advance_while<P>(&mut self, predicate: P)
    where
        P: Fn(char) -> bool,
    {
        while self.peek().is_some_and(|c| predicate(c)) {
            self.advance();
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.chars.clone().next()
    }
}

impl Lexer<'_> {
    fn whitespace(&mut self) -> TokenKind {
        self.advance_while(|c| c.is_whitespace());
        TokenKind::Whitespace
    }

    fn comment(&mut self, style: CommentStyle) -> TokenKind {
        self.advance();
        let kind = match self.peek() {
            Some('/') if matches!(style, CommentStyle::Line) => CommentKind::Doc,
            Some('*') if matches!(style, CommentStyle::Block) => CommentKind::Doc,
            Some('!') => CommentKind::InnerDoc,
            _ => CommentKind::Normal,
        };

        while let Some(c) = self.advance() {
            match c {
                c if matches!(style, CommentStyle::Line) => {
                    if c.is_newline() {
                        if c == '\r' && matches!(self.peek(), Some('\n')) {
                            // CRLF line ending in windows
                            self.advance();
                        }
                        break;
                    }
                }
                '*' if matches!(style, CommentStyle::Block) => {
                    if matches!(self.advance(), Some('/')) {
                        break;
                    }
                }

                _ => {}
            }
        }

        let comment = Comment::new(kind, style);
        TokenKind::Comment(comment)
    }

    fn ident(&mut self) -> TokenKind {
        self.advance_while(|c| c.is_ident_body());
        TokenKind::Ident
    }

    fn num_lit(&mut self) -> TokenKind {
        self.advance_while(|c| c.is_ascii_digit() || c == '_');

        // fraction
        if let Some('.') = self.peek() {
            self.advance();
            self.advance_while(|c| c.is_ascii_digit() || c == '_');
        }

        // exponent
        if let Some('e') | Some('E') = self.peek() {
            self.advance();
            self.advance_while(|c| c.is_ascii_digit() || c == '_');
        }

        // suffix
        if self.peek().is_some_and(|c| c.is_ident_start()) {
            self.advance_while(|c| c.is_ident_body());
        }

        TokenKind::Lit
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        let start = self.cur;

        let kind = match self.advance()? {
            c if c.is_whitespace() => self.whitespace(),
            c if c.is_ident_start() => self.ident(),
            c if c.is_ascii_digit() => self.num_lit(),

            // slash or comment
            '/' => match self.peek() {
                Some('/') => self.comment(CommentStyle::Line),
                Some('*') => self.comment(CommentStyle::Block),
                _ => TokenKind::Slash,
            },

            // single character tokens
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Asterisk,
            '%' => TokenKind::Percent,
            '&' => TokenKind::Ampersand,
            '|' => TokenKind::Bar,
            '!' => TokenKind::Bang,
            '<' => TokenKind::Less,
            '>' => TokenKind::Greater,

            '.' => TokenKind::Dot,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            '=' => TokenKind::Eq,

            '(' => TokenKind::OpenDelim(Delim::Paren),
            ')' => TokenKind::CloseDelim(Delim::Paren),
            '{' => TokenKind::OpenDelim(Delim::Brace),
            '}' => TokenKind::CloseDelim(Delim::Brace),
            '[' => TokenKind::OpenDelim(Delim::Brack),
            ']' => TokenKind::CloseDelim(Delim::Brack),
            _ => TokenKind::Unknown,
        };

        let len = self.cur - start;
        Some(Token::new(kind, len as u32))
    }
}
