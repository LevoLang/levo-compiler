#[derive(Debug, Clone)]
pub enum Error {
    EndOfTok,
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::EndOfTok => write!(f, "{}", "end of token stream"),
        }
    }
}
