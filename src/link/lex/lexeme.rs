pub type LexList<'a> = &'a [(&'a [u8], Lexeme<'a>)];

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Lexeme<'a> {
    Struct,
    Eof,
    Fun,
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
    If,
    Do,
    Equal2,
    Extern,
    Else,
    Less,
    Dot,
    Let,
    Equal,
    Unknown,
}

impl Lexeme<'_> {
    pub fn show(self) -> &'static str {
        match self {
            Eof => "<eof>",
            Fun => "`fn`",
            Name(_) => "<your mom>",
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
            Equal2 => "`==`",
            Extern => "`extern`",
            Else => "`else`",
            Less => "`<`",
            Dot => "`.`",
            Let => "`let`",
            Equal => "`=`",
            Colon => "`:`",
            Star => "`*`",
            Struct => "`struct`",
            Str(_) => "<str>",
        }
    }
}

use Lexeme::*;
