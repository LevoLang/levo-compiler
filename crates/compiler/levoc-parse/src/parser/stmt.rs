use levoc_lex::TokenKind;

use crate::{
    ast::{Stmt, StmtKind},
    error::{ParseError, ParseErrorKind},
    Span,
};

use super::Parser;

impl Parser<'_> {
    pub fn stmt(&mut self) -> Option<Result<Stmt, ParseError>> {
        /*
         *  statement:
         *      ';' |
         *      expression ';';
         */
        let start = self.cur;
        let token = self.token.as_ref()?;
        let (kind, span) = match token.kind {
            TokenKind::Semicolon => (
                StmtKind::Empty,
                Span::new(start, self.cur + token.len as usize),
            ),
            _ => {
                let expr = match self.expr() {
                    Some(Ok(expr)) => expr,
                    Some(Err(e)) => return Some(Err(e)),
                    None => return Some(Err(ParseError::new(ParseErrorKind::UnsureExprStmtEmpty))),
                };

                match self.token.as_ref() {
                    Some(token) if matches!(token.kind, TokenKind::Semicolon) => (
                        StmtKind::Expr(Box::new(expr)),
                        Span::new(start, self.cur + token.len as usize),
                    ),
                    _ => {
                        return Some(Err(ParseError::new(
                            ParseErrorKind::UnsureExprStmtNoSemicolon,
                        )))
                    }
                }
            }
        };

        let _ = self.consume();
        Some(Ok(Stmt::new(kind, span)))
    }
}
