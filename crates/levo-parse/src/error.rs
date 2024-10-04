use std::{error, fmt};

use levo_lex::chars::Delim;

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    // statement errors
    BadStmt,

    // expression errors
    BadExpr,
    EmptyDelimExpr,
    NoCloseDelim,
    DelimNoMatch(Delim, Delim),
}

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
}

impl ParseError {
    pub fn new(kind: ParseErrorKind) -> Self {
        Self { kind }
    }
}

impl error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ParseErrorKind::BadStmt => write!(f, "bad statement"),
            ParseErrorKind::BadExpr => write!(f, "bad expression"),
            ParseErrorKind::EmptyDelimExpr => write!(f, "empty delimited expression"),
            ParseErrorKind::NoCloseDelim => write!(f, "no closing delimiter"),
            ParseErrorKind::DelimNoMatch(open, close) => {
                write!(f, "delimiters do not match: '{open:?}', '{close:?}'")
            }
        }
    }
}
