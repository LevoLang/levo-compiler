pub mod tok;
use self::tok::TrivToken;

pub struct Symbol(String);

pub struct Span {
    start: u32,
    end: u32,
}

pub struct Item {
    kind: ItemKind,
    span: Span,
}

pub enum ItemKind {
    Stmt(Box<Stmt>),
}

pub struct Stmt {
    kind: StmtKind,
    span: Span,
}

pub enum StmtKind {
    Expr(Box<Expr>, TrivToken),
}

pub struct Expr {
    kind: ExprKind,
    span: Span,
}

pub enum ExprKind {
    Lit(Lit),
    BinOp(BinOp),
    Assign(Assign),
}

pub struct Lit {
    sym: Symbol,

    token: TrivToken,
}

pub struct Assign {
    left: Box<Expr>,
    right: Box<Expr>,

    assign_token: TrivToken,
}

pub struct BinOp {
    left: Box<Expr>,
    right: Box<Expr>,
    kind: BinOpKind,

    op_token: TrivToken,
}

pub enum BinOpKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
}
