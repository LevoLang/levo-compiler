#[inline]
pub fn is_ident_start(c: char) -> bool {
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

#[inline]
pub fn is_ident_body(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

#[inline]
pub fn is_digit(c: char) -> bool {
    matches!(c, '0'..='9')
}

#[inline]
pub fn is_whitespace(c: char) -> bool {
    c.is_whitespace()
}

#[inline]
pub fn is_newline(c: char) -> bool {
    c == '\r'           // U+000D carriage return
    || c == '\n'        // U+000A line feed
    || c == '\u{0085}'  // U+0085 next line
    || c == '\u{2028}'  // U+2028 line separator
    || c == '\u{2029}'  // U+2029 paragraph separator
}
