pub mod ast;
pub mod error;
pub mod parser;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        debug_assert!(end >= start, "span has start after its end");
        Self { start, end }
    }

    pub fn len(&self) -> usize {
        self.start - self.end
    }
}
