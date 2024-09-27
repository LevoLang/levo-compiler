use crate::{
    ast::{tok::Token, Expr, Item, Stmt},
    error::Error,
    lex::Lex,
};

pub struct Parser<L: Lex> {
    lexer: L,
}

impl<L: Lex> Parser<L> {
    pub fn parse_item(&mut self) -> Item {
        todo!()
    }

    pub fn parse_stmt(&mut self) -> Stmt {
        todo!()
    }

    pub fn parse_expr(&mut self) -> Result<Expr, Error> {
        let Some(token) = self.next_token() else {
            return Err(Error::EndOfTok);
        };

        match token.kind() {
            
        }

        todo!()
    }

    pub fn next_token(&mut self) -> Option<Token> {
        todo!()
    }
}
