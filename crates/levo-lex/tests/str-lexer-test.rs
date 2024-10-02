use levo_lex::Lex;

const TEXTS: [&str; 5] = [
    // 0: simple expression
    r"abc32 + bec_ * (c232d - d_3u2s)",
    // 1: comments
    r"// This is a line comment

/* This is a block comment */
a + b

/**/

/* Nested comments /* SHOULD */ work */
+ (x - z)

/* A very long comment so hello there how are you what are you doing im fine thanks for asking this
is just to see if the buffer extends correctly or not okay i think this is enough now bye */

// This line comment should be counted as non-terminated because it has no line-terminator at the end",
    // 2: comments #2
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
    // 3: whitespace
    "/* This is a whitespace test */\n\r\n\t\t       \t\t\n\n\n\r\n\r\n\r\n\r\r\r\n",
    // 4: identifiers and literals
    r"i = 34;
35.4;
32 + 3e12;
r+35.43;
3.3.5.4;
3_335_s;
3_.3sx;
3e10.3;
3e.x;
3.x;",
];

#[test]
fn str_lexer_test() {
    use levo_lex::Cursor;
    for (num, text) in TEXTS.iter().enumerate() {
        println!("==========");
        println!("Test #{}:", num);
        let mut lexer = Cursor::new(text);
        while let Some(tok) = lexer.next_token() {
            println!("{:?}", tok);
        }
    }
}
