use levo_lex::Cursor;
use levo_parse::{error::ParseError, Parser};

const TEXTS: [&str; 4] = [
    r"1 + 2; x + y; 12 + x;",
    "(((x)));",
    "                   x              ;",
    " x+y    *z / 12 + y % 13;",
];

#[test]
fn str_parse_test() -> Result<(), ParseError> {
    for (num, text) in TEXTS.iter().enumerate() {
        println!("==========");
        println!("Test #{}:", num);
        let cursor = Cursor::new(text);
        let mut parser = Parser::new(cursor);
        while let Some(stmt) = parser.parse() {
            println!("{:?}", stmt?);
        }
    }

    Ok(())
}
