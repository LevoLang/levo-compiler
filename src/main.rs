fn main() {
    use levoc::parse::lex::*;

    let mut lexer = create_lexer("a+b*(c-d)".chars());
    while let Ok(tok) = lexer.lex() {
        println!("{}", tok);
    }
}
