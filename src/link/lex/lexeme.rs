pub type LexList<'a> = &'a [(&'a [u8], Lexeme<'a>)];

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Lexeme<'a> {
    Eof,
    Fun,
    Name(&'a str),
    ParL,
    ParR,
    CurL,
    CurR,
    Int(i32),
    Plus,
    RawStr(&'a str),
    Semicolon,
    Comma,
    BraL,
    BraR,
    If,
    Do,
    Equal,
    Unknown,
}

impl Lexeme<'_> {
    pub fn show(self) -> &'static str {
        match self {
            Eof => "<eof>",
            Fun => "`fn`",
            Name(_) => "<name>",
            ParL => "`(`",
            ParR => "`)`",
            CurL => "`{`",
            CurR => "`}`",
            Int(_) => "<int>",
            Plus => "`+`",
            RawStr(_) => "<raw str>",
            Semicolon => "`;`",
            Comma => "`,`",
            BraL => "`[`",
            BraR => "`]`",
            Unknown => "<?>",
            If => "`if`",
            Do => "`do`",
            Equal => "`==`",
        }
    }
}

use Lexeme::*;
