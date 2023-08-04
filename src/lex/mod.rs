use std::cmp::min;

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
        self.lex_token()
    }
}
