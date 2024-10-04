use error::{ParseError, ParseErrorKind};
use levo_lex::{chars::Delim, Cursor, Lex, Token, TokenKind};

pub mod error;

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
}

impl Stmt {
    fn new(kind: StmtKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    Empty,
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

impl Expr {
    fn new(kind: ExprKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Wildcard,

    UnOp(UnOp),
    BinOp(BinOp),

    Delim(Delim, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Lit {}

impl Lit {
    fn new() -> Self {
        Self {}
    }
}

#[derive(Debug, Clone)]
pub struct Ident {}

impl Ident {
    fn new() -> Self {
        Self {}
    }
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub kind: UnOpKind,
    pub expr: Box<Expr>,
}

impl UnOp {
    fn new(kind: UnOpKind, expr: Box<Expr>) -> Self {
        Self { kind, expr }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Pos, // +
    Neg, // -
    Not, // !
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

impl BinOp {
    fn new(kind: BinOpKind, left: Box<Expr>, right: Box<Expr>) -> Self {
        Self { kind, left, right }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

pub struct Lexer<'a> {
    cursor: Cursor<'a>,
    shelf: Option<Option<Token>>,
}

impl<'a> Lexer<'a> {
    fn new(cursor: Cursor<'a>) -> Self {
        Self {
            cursor,
            shelf: None,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.shelf.take() {
            Some(token) => token,
            None => self.cursor.next_token(),
        }
    }

    /*fn peek_token(&mut self) -> Option<&Token> {
        self.shelf
            .get_or_insert_with(|| self.cursor.next_token())
            .as_ref()
    }*/
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(cursor: Cursor<'a>) -> Self {
        Self {
            lexer: Lexer::new(cursor),
            token: None,
        }
    }

    pub fn parse(&mut self) -> Option<Result<Stmt, ParseError>> {
        self.consume_token();
        self.stmt()
    }

    pub fn consume_token(&mut self) {
        self.token = self.lexer.next_token();
    }

    pub fn skip_trivia(&mut self) {
        loop {
            match self.token.as_ref().map(|t| &t.kind) {
                Some(TokenKind::Whitespace) | Some(TokenKind::Comment(_)) => self.consume_token(),
                _ => break,
            }
        }
    }
}

impl Parser<'_> {
    fn stmt(&mut self) -> Option<Result<Stmt, ParseError>> {
        self.skip_trivia();
        let token = self.token.as_ref()?;

        let kind = if let TokenKind::Semicolon = token.kind {
            StmtKind::Empty
        } else {
            let expr = match self.expr() {
                Some(Ok(expr)) => Box::new(expr),
                Some(Err(e)) => return Some(Err(e)),
                None => return Some(Err(ParseError::new(ParseErrorKind::BadStmt))),
            };

            self.skip_trivia();
            if let Some(token) = self.token.as_ref() {
                if let TokenKind::Semicolon = token.kind {
                    StmtKind::Expr(expr)
                } else {
                    return Some(Err(ParseError::new(ParseErrorKind::BadStmt)));
                }
            } else {
                return Some(Err(ParseError::new(ParseErrorKind::BadStmt)));
            }
        };

        Some(Ok(Stmt::new(kind)))
    }

    fn expr(&mut self) -> Option<Result<Expr, ParseError>> {
        let token = self.token.as_ref()?;
        if let TokenKind::CloseDelim(_) | TokenKind::Semicolon = token.kind {
            return None;
        }

        self.unary_expr()
    }

    fn unary_expr(&mut self) -> Option<Result<Expr, ParseError>> {
        let token = self.token.as_ref()?;

        let kind = if let TokenKind::Plus = token.kind {
            UnOpKind::Pos
        } else if let TokenKind::Minus = token.kind {
            UnOpKind::Neg
        } else if let TokenKind::Bang = token.kind {
            UnOpKind::Neg
        } else {
            return self.bin_expr();
        };

        self.consume_token();
        self.skip_trivia();
        match self.expr() {
            Some(Ok(expr)) => {
                let un_op = UnOp::new(kind, Box::new(expr));
                let kind = ExprKind::UnOp(un_op);
                Some(Ok(Expr::new(kind)))
            }
            Some(Err(e)) => Some(Err(e)),
            None => Some(Err(ParseError::new(ParseErrorKind::BadExpr))),
        }
    }

    fn bin_expr(&mut self) -> Option<Result<Expr, ParseError>> {
        self.add_expr()
    }

    fn add_expr(&mut self) -> Option<Result<Expr, ParseError>> {
        let expr = match self.mul_expr()? {
            Ok(expr) => expr,
            Err(e) => return Some(Err(e)),
        };

        let Some(token) = self.token.as_ref() else {
            return Some(Ok(expr));
        };

        let kind = if let TokenKind::Plus = token.kind {
            BinOpKind::Add
        } else if let TokenKind::Minus = token.kind {
            BinOpKind::Sub
        } else {
            return Some(Ok(expr));
        };

        self.consume_token();
        self.skip_trivia();
        let Some(right) = self.add_expr() else {
            return Some(Err(ParseError::new(ParseErrorKind::BadExpr)));
        };

        let left = Box::new(expr);
        let right = match right {
            Ok(expr) => Box::new(expr),
            Err(e) => return Some(Err(e)),
        };

        let bin_op = BinOp::new(kind, left, right);
        let kind = ExprKind::BinOp(bin_op);
        Some(Ok(Expr::new(kind)))
    }

    fn mul_expr(&mut self) -> Option<Result<Expr, ParseError>> {
        let expr = match self.prim_expr()? {
            Ok(expr) => expr,
            Err(e) => return Some(Err(e)),
        };

        self.consume_token();
        self.skip_trivia();
        let Some(token) = self.token.as_ref() else {
            return Some(Ok(expr));
        };

        let kind = if let TokenKind::Asterisk = token.kind {
            BinOpKind::Mul
        } else if let TokenKind::Slash = token.kind {
            BinOpKind::Div
        } else if let TokenKind::Percent = token.kind {
            BinOpKind::Mod
        } else {
            return Some(Ok(expr));
        };

        self.consume_token();
        self.skip_trivia();
        let Some(right) = self.mul_expr() else {
            return Some(Err(ParseError::new(ParseErrorKind::BadExpr)));
        };

        let left = Box::new(expr);
        let right = match right {
            Ok(expr) => Box::new(expr),
            Err(e) => return Some(Err(e)),
        };

        let bin_op = BinOp::new(kind, left, right);
        let kind = ExprKind::BinOp(bin_op);
        Some(Ok(Expr::new(kind)))
    }

    fn prim_expr(&mut self) -> Option<Result<Expr, ParseError>> {
        let token = self.token.as_ref()?;
        let kind = if let TokenKind::Lit = token.kind {
            let lit = Lit::new();
            ExprKind::Lit(lit)
        } else if let TokenKind::Ident = token.kind {
            let ident = Ident::new();
            ExprKind::Ident(ident)
        } else if let TokenKind::Underscore = token.kind {
            ExprKind::Wildcard
        } else if let TokenKind::OpenDelim(open) = token.kind {
            self.consume_token();
            self.skip_trivia();
            return self.delim_expr(open);
        } else {
            return None;
        };

        Some(Ok(Expr::new(kind)))
    }

    fn delim_expr(&mut self, open: Delim) -> Option<Result<Expr, ParseError>> {
        let expr = match self.expr() {
            Some(Ok(expr)) => Box::new(expr),
            Some(Err(e)) => return Some(Err(e)),
            None => return Some(Err(ParseError::new(ParseErrorKind::EmptyDelimExpr))),
        };

        let Some(token) = self.token.as_ref() else {
            return Some(Err(ParseError::new(ParseErrorKind::NoCloseDelim)));
        };

        if let TokenKind::CloseDelim(close) = token.kind {
            if open == close {
                let kind = ExprKind::Delim(open, expr);
                Some(Ok(Expr::new(kind)))
            } else {
                Some(Err(ParseError::new(ParseErrorKind::DelimNoMatch(
                    open, close,
                ))))
            }
        } else {
            Some(Err(ParseError::new(ParseErrorKind::BadExpr)))
        }
    }
}

/*

statement:
    SEMICOLON |
    expression_statement;

expression_statement:
    expression SEMICOLON;

expression:
    unary_expression;

unary_expression:
    + expression |
    - expression |
    binary_expression;

binary_expression:
    additive_expression;

additive_expression:
    multiplicative_expression |
    multiplicative_expression + additive_expression |
    multiplicative_expression - additive_expression;

multiplicative_expression:
    primitive_expression |
    primitive_expression * multiplicative_expression;

primitive_expression:
    literal |
    identifier |
    delimited_expression;

delimited_expression:
    ( expression );
*/
