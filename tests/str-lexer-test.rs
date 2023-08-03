const TEXTS: [&str; 2] = [
// simple expression
"abc32 + bec_ * (c232d - d_3u2s)",

// comments
"// This is a line comment

/* This is a block comment */
a + b

/* Nested comments /* SHOULD */ work */
+ (x - z)"
];

#[test]
fn str_lexer_test() {
    use levoc::lex::{Lex, Lexer};

    for (num, text) in TEXTS.iter().enumerate() {
        println!("==========");
        println!("Test #{}:", num.to_string());
        let mut lexer = Lexer::new(text.chars());
        while let Ok(tok) = lexer.lex() {
            println!("{}", tok);
        }
    }
}
