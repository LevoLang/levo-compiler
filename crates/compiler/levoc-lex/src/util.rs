use unicode_xid::UnicodeXID;

pub trait CharExt {
    fn is_ident_start(&self) -> bool;

    fn is_ident_body(&self) -> bool;

    fn is_newline(&self) -> bool;
}

impl CharExt for char {
    fn is_ident_start(&self) -> bool {
        *self == '_' || self.is_xid_start()
    }

    fn is_ident_body(&self) -> bool {
        self.is_xid_continue()
    }

    fn is_newline(&self) -> bool {
        matches!(
            *self,
            '\n'       | /* line feed */
            '\r'       | /* carriage return */
            '\u{000B}' | /* vertical tab */
            '\u{000C}' | /* form feed */
            '\u{0085}' | /* next line */
            '\u{2028}' | /* line separator */
            '\u{2029}', /* paragraph separator */
        )
    }
}
