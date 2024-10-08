use crate::Span;

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Self { kind, span }
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
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Ident,
    Lit,

    UnOp(UnOp),
    BinOp(BinOp),

    Paren(Box<Expr>),
    Block(Box<Block>),
}

#[derive(Debug, Clone)]
pub struct UnOp {
    pub kind: UnOpKind,
    pub op_span: Span,

    pub expr: Box<Expr>,
}

impl UnOp {
    pub fn new(kind: UnOpKind, op_span: Span, expr: Box<Expr>) -> Self {
        Self {
            kind,
            op_span,
            expr,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOpKind {
    Pos,  // +
    Neg,  // -
    Bang, // !
}

#[derive(Debug, Clone)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub op_span: Span,

    pub left: Box<Expr>,
    pub right: Box<Expr>,
}
impl BinOp {
    pub fn new(kind: BinOpKind, op_span: Span, left: Box<Expr>, right: Box<Expr>) -> Self {
        Self {
            kind,
            op_span,
            left,
            right,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOpKind {
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOpPriority {
    Add,
    Mul,
}

impl BinOpKind {
    pub fn priority(&self) -> BinOpPriority {
        match self {
            BinOpKind::Add | BinOpKind::Sub => BinOpPriority::Add,
            BinOpKind::Mul | BinOpKind::Div | BinOpKind::Mod => BinOpPriority::Mul,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}
