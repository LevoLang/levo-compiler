use levoc_lex::{lexer::Lexer, Token, TokenKind};

mod expr;
mod stmt;

pub struct Parser<'a> {
    _source: &'a str,
    lexer: Lexer<'a>,
    token: Option<Token>,
    cur: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut res = Self {
            _source: source,
            lexer: Lexer::new(source),
            token: None,
            cur: 0,
        };

        let _ = res.consume();
        res
    }

    pub(crate) fn consume(&mut self) -> Option<&Token> {
        self.token
            .as_ref()
            .inspect(|token| self.cur += token.len as usize);

        self.token = loop {
            match self.lexer.next() {
                Some(token) => {
                    if matches!(token.kind, TokenKind::Whitespace | TokenKind::Comment(_)) {
                        self.cur += token.len as usize;
                    } else {
                        break Some(token);
                    }
                }
                None => break None,
            }
        };

        self.token.as_ref()
    }
}
