#[derive(Debug, Clone)]
pub struct ParseError {
    pub kind: ParseErrorKind,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    /// A ```parenthesized expression``` has no closing ```)```.
    ParenExprUnclosed,

    // A ```operation_expression``` has no right expression on the right.
    UnsureOpExprNoRight,
    /// A ```unary_expression``` has empty inner ```expression```.
    UnsureUnExprEmpty,
    /// A ```parenthesized_expression``` has empty inner ```expression```.
    UnsureParenExprEmpty,
    /// An ```expression_statement``` has empty inner ```expression```.
    UnsureExprStmtEmpty,
    /// An ```expression_statement``` has no trailing semicolon.
    UnsureExprStmtNoSemicolon,
}
