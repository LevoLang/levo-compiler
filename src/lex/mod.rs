use std::iter::Peekable;
use std::marker::PhantomData;

mod tok;
pub use tok::*;

pub trait Tok {
    fn len(&self) -> u32;

    fn is_trivia(&self) -> bool;
}

pub trait Lex {
    type Token: Tok;

    fn lex(&mut self) -> Option<Self::Token>;
}

pub struct Lexer<I: Iterator<Item = char>> {
    iter: Peekable<I>,
    cur: usize,
    shelf: Option<char>,
}

impl<I: Iterator<Item = char>> Lexer<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter: iter.peekable(),
            cur: 0,
            shelf: None,
        }
    }

    #[inline]
    fn advance(&mut self) -> Option<char> {
        match self.shelf.take() {
            None => self.iter.next(),
            v => v,
        }.and_then(|c| {
            self.cur += c.len_utf8();
            Some(c)
        })
    }
    
    #[inline]
    fn peek(&mut self) -> Option<char> {
        match self.shelf {
            None => self.iter.peek().copied(),
            v => v,
        }
    }

    #[inline]
    fn shelf(&mut self, c: char) -> Option<char> {
        self.cur -= c.len_utf8();
        self.shelf.replace(c)
    }
}

impl<I: Iterator<Item = char>> Lex for Lexer<I> {
    type Token = Token;

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

    pub fn skip_trivia(self) -> SkipTrivia<L, Self> {
        SkipTrivia::new(self)
    }
}

impl<L: Lex> Iterator for TokenIter<L> {
    type Item = L::Token;

    fn next(&mut self) -> Option<L::Token> {
        self.lexer.lex()
    }
}

pub struct SkipTrivia<L: Lex, I: Iterator<Item = L::Token>> {
    iter: I,
    _phantom: PhantomData<L>,
}

impl<L: Lex, I: Iterator<Item = L::Token>> SkipTrivia<L, I> {
    pub(self) fn new(iter: I) -> Self {
        Self {
            iter: iter,
            _phantom: PhantomData::default(),
        }
    }
}

impl<L: Lex, I: Iterator<Item = L::Token>> Iterator for SkipTrivia<L, I> {
    type Item = L::Token;

    fn next(&mut self) -> Option<L::Token> {
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
