use levoc_parse::parser::Parser;

fn main() {
    let input = "54;a;(((x)));(1 * 2) + 3;";
    let mut parser = Parser::new(input);

    while let Some(ast) = parser.stmt() {
        println!("{ast:?}");
    }
}
