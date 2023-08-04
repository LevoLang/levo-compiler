use std::cmp::min;

use crate::chars;

mod tok;
pub use tok::*;

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
