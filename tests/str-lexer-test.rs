#[test]
fn str_lexer_test() {
    use levoc::parse::lex::{Lex, Lexer};

    let texts = [
        "abc32 + bec_ * (c232d - d_3u2s)",
    ];

    for (num, text) in texts.iter().enumerate() {
        println!("==========");
        println!("Test #{}:", num.to_string());
        let mut lexer = Lexer::new(text.chars());
        while let Ok(tok) = lexer.lex() {
            println!("{}", tok);
        }
    }
}
