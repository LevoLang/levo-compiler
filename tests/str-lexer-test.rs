const TEXTS: [&str; 4] = [
// simple expression
r"abc32 + bec_ * (c232d - d_3u2s)",

// comments
r"// This is a line comment

/* This is a block comment */
a + b

/**/

/* Nested comments /* SHOULD */ work */
+ (x - z)

/* A very long comment so hello there how are you what are you doing im fine thanks for asking this
is just to see if the buffer extends correctly or not okay i think this is enough now bye */

// This line comment should be counted as non-terminated because it has no line-terminator at the end",

// comments #2
r"/* Second commend test */

/// Doc line comment
/// This is, lexically, a different comment but parser should mix them together well

/**
 * Doc block comment
 */

/*!
 * Inner doc block comment
 */

//! Inner doc line comment

/* This is a non-terminated block comment because it has no ending",

// whitespace
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
