use crate::lexer::Lexer;

#[test]
fn test_lexer() {
    let input = "hello 123.5e7f32";
    let lexer = Lexer::new(input);

    for token in lexer {
        println!("{token:?}");
    }
}
