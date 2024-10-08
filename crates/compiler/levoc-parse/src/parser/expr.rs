use levoc_lex::{Delim, TokenKind};

use crate::{
    ast::{BinOp, BinOpKind, Expr, ExprKind, UnOp, UnOpKind},
    error::{ParseError, ParseErrorKind},
    Span,
};

use super::Parser;

impl Parser<'_> {
    pub fn expr(&mut self) -> Option<Result<Expr, ParseError>> {
        /*
         *  expression:
         *      operation_expression;
         */
        self.op_expr()
    }

    pub fn op_expr(&mut self) -> Option<Result<Expr, ParseError>> {
        /*
         *  operation_expression:
         *      unary_expression '+' operation_expression |
         *      unary_expression '-' operation_expression |
         *      unary_expression '*' operation_expression |
         *      unary_expression '/' operation_expression |
         *      unary_expression '%' operation_expression |
         *      unary_expression;
         */
        let start = self.cur;
        let left = match self.un_expr()? {
            Ok(expr) => expr,
            Err(e) => return Some(Err(e)),
        };

        let Some(token) = self.token.as_ref() else {
            return Some(Ok(left));
        };

        let kind = match token.kind {
            TokenKind::Plus => BinOpKind::Add,
            TokenKind::Minus => BinOpKind::Sub,
            TokenKind::Asterisk => BinOpKind::Mul,
            TokenKind::Slash => BinOpKind::Div,
            TokenKind::Percent => BinOpKind::Mod,
            _ => return Some(Ok(left)),
        };

        let op_span = Span::new(self.cur, self.cur + token.len as usize);

        let _ = self.consume();
        let right = match self.op_expr() {
            Some(Ok(expr)) => expr,
            Some(Err(e)) => return Some(Err(e)),
            None => return Some(Err(ParseError::new(ParseErrorKind::UnsureOpExprNoRight))),
        };

        let kind = {
            let left = Box::new(left);
            if let ExprKind::BinOp(right_op) = right.kind {
                if right_op.kind.priority() < kind.priority() {
                    let left_op = BinOp::new(kind, op_span, left, right_op.left);
                    let left_span = Span::new(start, left_op.right.span.end);
                    let left = Box::new(Expr::new(ExprKind::BinOp(left_op), left_span));
                    ExprKind::BinOp(BinOp::new(
                        right_op.kind,
                        right_op.op_span,
                        left,
                        right_op.right,
                    ))
                } else {
                    let right = Box::new(Expr::new(ExprKind::BinOp(right_op), right.span));
                    ExprKind::BinOp(BinOp::new(kind, op_span, left, right))
                }
            } else {
                ExprKind::BinOp(BinOp::new(kind, op_span, left, Box::new(right)))
            }
        };

        Some(Ok(Expr::new(kind, Span::new(start, self.cur))))
    }

    pub fn un_expr(&mut self) -> Option<Result<Expr, ParseError>> {
        /*
         *  unary_expression:
         *      '+' unary_expression |
         *      '-' unary_expression |
         *      '!' unary_expression |
         *      primitive_expression;
         */
        let start = self.cur;
        let token = self.token.as_ref()?;
        let kind = match token.kind {
            TokenKind::Plus => UnOpKind::Pos,
            TokenKind::Minus => UnOpKind::Neg,
            TokenKind::Bang => UnOpKind::Bang,
            _ => return self.prim_expr(),
        };

        let op_span = Span::new(start, self.cur + token.len as usize);

        let _ = self.consume();

        let expr = match self.un_expr() {
            Some(Ok(expr)) => expr,
            Some(Err(e)) => return Some(Err(e)),
            None => return Some(Err(ParseError::new(ParseErrorKind::UnsureUnExprEmpty))),
        };

        let un_op = UnOp::new(kind, op_span, Box::new(expr));
        Some(Ok(Expr::new(
            ExprKind::UnOp(un_op),
            Span::new(start, self.cur),
        )))
    }

    pub fn prim_expr(&mut self) -> Option<Result<Expr, ParseError>> {
        /*
         *  primitive_expression:
         *      literal |
         *      identifier |
         *      '(' expression ')';
         */
        let token = self.token.as_ref()?;
        let (kind, span) = match token.kind {
            TokenKind::Ident => (
                ExprKind::Ident,
                Span::new(self.cur, self.cur + token.len as usize),
            ),
            TokenKind::Lit => (
                ExprKind::Lit,
                Span::new(self.cur, self.cur + token.len as usize),
            ),
            TokenKind::OpenDelim(Delim::Paren) => {
                let start = self.cur;
                self.consume();
                let expr = match self.expr() {
                    Some(Ok(expr)) => Box::new(expr),
                    Some(Err(e)) => return Some(Err(e)),
                    None => {
                        return Some(Err(ParseError::new(ParseErrorKind::UnsureParenExprEmpty)))
                    }
                };

                match self.token.as_ref() {
                    Some(token) if matches!(token.kind, TokenKind::CloseDelim(Delim::Paren)) => (
                        ExprKind::Paren(expr),
                        Span::new(start, self.cur + token.len as usize),
                    ),
                    _ => return Some(Err(ParseError::new(ParseErrorKind::ParenExprUnclosed))),
                }
            }

            TokenKind::Semicolon => return None,
            TokenKind::CloseDelim(Delim::Paren) => return None,
            TokenKind::Unknown => todo!(),

            _ => todo!(),
        };

        let _ = self.consume();
        Some(Ok(Expr::new(kind, span)))
    }
}
