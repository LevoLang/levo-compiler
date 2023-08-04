pub fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}

pub fn is_newline(c: char) -> bool {
    c == '\r'           // U+000D carriage return
    || c == '\n'        // U+000A line feed
    || c == '\u{0085}'  // U+0085 next line
    || c == '\u{2028}'  // U+2028 line separator
    || c == '\u{2029}'  // U+2029 paragraph separator
}
