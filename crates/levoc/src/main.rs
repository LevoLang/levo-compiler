use levo_lex::Cursor;
use levo_parse::{error::ParseError, Parser};

fn main() -> Result<(), ParseError> {
    let text = r"(x + 10) * z;";
    let cursor = Cursor::new(text);
    let mut parser = Parser::new(cursor);

    while let Some(stmt) = parser.parse() {
        println!("{:?}", stmt?);
    }

    Ok(())
}
