const TEXTS: [&str; 3] = [
// simple expression
r"abc32 + bec_ * (c232d - d_3u2s)",

// comments
r"// This is a line comment

/* This is a block comment */
a + b

/* Nested comments /* SHOULD */ work */
+ (x - z)",

"/* This is a whitespace test */\n\r\n\t\t       \t\t\n\n\n\r\n\r\n\r\n\r\r\r\n"
];

#[test]
fn str_lexer_test() {
    use levoc::lex::{Lex, Lexer};

    for (num, text) in TEXTS.iter().enumerate() {
        println!("==========");
        println!("Test #{}:", num);
        let mut lexer = Lexer::new(text.chars());
        while let Some(tok) = lexer.lex() {
            println!("{}", tok);
        }
    }
}
