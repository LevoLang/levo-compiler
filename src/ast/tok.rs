use super::Span;

use crate::chars::Whitespace;

pub struct Token {
    kind: TokenKind,
    span: Span,
}

pub enum TokenKind {
    Unknown,

    Ident(Ident),
}

pub struct Ident {
    name: u64,
}

pub struct TrivToken {
    token: Token,
    trivias: TriviaList,
    span: Span,
}

pub struct TriviaList {
    list: Vec<Trivia>,
    trail_start: usize,
}

pub struct Trivia {
    kind: TriviaKind,
    span: Span,
}

pub enum TriviaKind {
    Whitespace(Whitespace),
    Comment(Comment),
}

pub struct Comment {
    span: Span,
    kind: CommentKind,
    style: CommentStyle,
}

pub enum CommentKind {
    Normal,
    Doc,
    InnerDoc,
}

pub enum CommentStyle {
    Line,
    Block,
}
