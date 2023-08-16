use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Delimiter {
    Paren,   // ( )
    Brace,   // { }
    Bracket, // [ ]
}

impl fmt::Display for Delimiter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Delimiter::Paren => write!(f, "Paren"),
            Delimiter::Brace => write!(f, "Brace"),
            Delimiter::Bracket => write!(f, "Bracket"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Whitespace {
    Unknown,

    // single character
    Tab,             /* \u{0009} character tabulation */
    LineFeed,        /* \u{000A} line feed */
    VerTab,          /* \u{000B} line tabulation */
    FormFeed,        /* \u{000C} form feed */
    CarRet,          /* \u{000D} carriage return */
    Space,           /* \u{0020} space */
    NextLine,        /* \u{0085} next line */
    SpaceNoBrk,      /* \u{00A0} no-break space */
    SpaceOgham,      /* \u{1680} ogham space mark */
    EnQuad,          /* \u{2000} en quad */
    EmQuad,          /* \u{2001} em quad */
    EnSpace,         /* \u{2002} en space */
    EmSpace,         /* \u{2003} em space */
    ThirdPerEmSpace, /* \u{2004} three-per-em space */
    FourPerEmSpace,  /* \u{2005} four-per-em space */
    SixPerEmSpace,   /* \u{2006} six-per-em space */
    FigureSpace,     /* \u{2007} figure space */
    PuncSpace,       /* \u{2008} punctuation space */
    ThinSpace,       /* \u{2009} thin space */
    HairSpace,       /* \u{200A} hair space */
    LineSep,         /* \u{2028} line separator */
    ParSep,          /* \u{2029} paragraph separator */
    NarrowSpace,     /* \u{202F} narrow no-break space */
    MathSpace,       /* \u{205F} medium mathematical space */
    IdeoSpace,       /* \u{3000} ideographic space */

    // compound
    CarRetLineFeed, /* carriage return + line feed */
}

impl Whitespace {
    pub fn as_char(self) -> Option<char> {
        Some(match self {
            Whitespace::Tab => '\t',
            Whitespace::LineFeed => '\n',
            Whitespace::VerTab => '\u{000B}',
            Whitespace::FormFeed => '\u{000C}',
            Whitespace::CarRet => '\r',
            Whitespace::Space => ' ',
            Whitespace::NextLine => '\u{0085}',
            Whitespace::SpaceNoBrk => '\u{00A0}',
            Whitespace::SpaceOgham => '\u{1680}',
            Whitespace::EnQuad => '\u{2000}',
            Whitespace::EmQuad => '\u{2001}',
            Whitespace::EnSpace => '\u{2002}',
            Whitespace::EmSpace => '\u{2003}',
            Whitespace::Unknown => '\u{2004}',
            Whitespace::ThirdPerEmSpace => '\u{2004}',
            Whitespace::FourPerEmSpace => '\u{2005}',
            Whitespace::SixPerEmSpace => '\u{2006}',
            Whitespace::FigureSpace => '\u{2007}',
            Whitespace::PuncSpace => '\u{2008}',
            Whitespace::ThinSpace => '\u{2009}',
            Whitespace::HairSpace => '\u{200A}',
            Whitespace::LineSep => '\u{2028}',
            Whitespace::ParSep => '\u{2029}',
            Whitespace::NarrowSpace => '\u{202F}',
            Whitespace::MathSpace => '\u{205F}',
            Whitespace::IdeoSpace => '\u{3000}',
            Whitespace::CarRetLineFeed => {
                return None;
            }
        })
    }

    pub fn is_newline(&self) -> bool {
        matches!(
            self,
            Whitespace::CarRetLineFeed
                | Whitespace::LineFeed
                | Whitespace::CarRet
                | Whitespace::FormFeed
                | Whitespace::VerTab
                | Whitespace::NextLine
                | Whitespace::LineSep
                | Whitespace::ParSep
        )
    }
}

impl From<char> for Whitespace {
    fn from(c: char) -> Self {
        match c {
            ' ' => Self::Space,
            '\r' => Self::CarRet,
            '\n' => Self::LineFeed,
            '\t' => Self::Tab,
            '\u{000B}' => Self::VerTab,
            '\u{000C}' => Self::FormFeed,
            '\u{0085}' => Self::NextLine,
            '\u{00A0}' => Self::SpaceNoBrk,
            '\u{1680}' => Self::SpaceOgham,
            '\u{2000}' => Self::EnQuad,
            '\u{2001}' => Self::EmQuad,
            '\u{2002}' => Self::EnSpace,
            '\u{2003}' => Self::EmSpace,
            '\u{2004}' => Self::ThirdPerEmSpace,
            '\u{2005}' => Self::FourPerEmSpace,
            '\u{2006}' => Self::SixPerEmSpace,
            '\u{2007}' => Self::FigureSpace,
            '\u{2008}' => Self::PuncSpace,
            '\u{2009}' => Self::ThinSpace,
            '\u{200A}' => Self::HairSpace,
            '\u{2028}' => Self::LineSep,
            '\u{2029}' => Self::ParSep,
            '\u{202F}' => Self::NarrowSpace,
            '\u{205F}' => Self::MathSpace,
            '\u{3000}' => Self::IdeoSpace,
            _ => Self::Unknown,
        }
    }
}

impl Into<Option<char>> for Whitespace {
    fn into(self) -> Option<char> {
        self.as_char()
    }
}

impl fmt::Display for Whitespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Whitespace::Tab => write!(f, "\\t"),
            Whitespace::LineFeed => write!(f, "\\n"),
            Whitespace::VerTab => write!(f, "\\v"),
            Whitespace::FormFeed => write!(f, "\\f"),
            Whitespace::CarRet => write!(f, "\\r"),
            Whitespace::Space => write!(f, "' '"),

            Whitespace::CarRetLineFeed => write!(f, "\\r\\n"),
            c => write!(f, "{}", c.as_char().unwrap().escape_unicode()),
        }
    }
}

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
