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

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter: iter,

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

    #[inline]
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

    #[inline]
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
        if self.buf.len() == self.buf.capacity() && (self.cur != 0 && self.cur * 2 >= cap) {
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

impl<I: Iterator<Item = char>> Lex for Lexer<I> {
    fn lex(&mut self) -> Option<Token> {
        self.lex_token()
    }
}

impl<I: Iterator<Item = char>> IntoIterator for Lexer<I> {
    type Item = Token;

    type IntoIter = TokenIter<Lexer<I>>;

    fn into_iter(self) -> TokenIter<Lexer<I>> {
        TokenIter::new(self)
    }
}

pub struct TokenIter<L: Lex> {
    lexer: L,
}

impl<L: Lex> TokenIter<L> {
    pub(self) fn new(lexer: L) -> Self {
        Self { lexer: lexer }
    }

    pub fn skip_trivia(self) -> SkipTrivia<Self> {
        SkipTrivia::new(self)
    }
}

impl<L: Lex> Iterator for TokenIter<L> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.lexer.lex()
    }
}

pub struct SkipTrivia<I: Iterator<Item = Token>> {
    iter: I,
}

impl<I: Iterator<Item = Token>> SkipTrivia<I> {
    pub(self) fn new(iter: I) -> Self {
        Self { iter: iter }
    }
}

impl<I: Iterator<Item = Token>> Iterator for SkipTrivia<I> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            return match self.iter.next() {
                Some(token) => {
                    if token.is_trivia() {
                        continue;
                    } else {
                        Some(token)
                    }
                }
                None => None,
            };
        }
    }
}
