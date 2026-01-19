use Lexeme::*;

pub type LexList<'a> = &'a [(&'a [u8], Lexeme<'a>)];

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Lexeme<'a> {
    Eof,
    Name(&'a str),
    ParL,
    ParR,
    CurL,
    CurR,
    Int(i64),
    Plus,
    RawStr(&'a str),
    Str(&'a str),
    Semicolon,
    Colon,
    Star,
    Comma,
    BraL,
    BraR,
    Equal2,
    Less,
    Dot,
    Equal,
    Unknown,
    Ampersand,
}

impl<'a> Lexeme<'a> {
    pub fn show(self) -> &'a str {
        match self {
            Eof => "<eof>",
            Name(n) => n,
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
            Equal2 => "`==`",
            Less => "`<`",
            Dot => "`.`",
            Equal => "`=`",
            Colon => "`:`",
            Star => "`*`",
            Str(_) => "<str>",
            Ampersand => "`&`",
        }
    }
}
