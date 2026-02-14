use Lexeme::*;

pub type LexList<'a> = &'a [(&'a [u8], Lexeme<'a>)];

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Lexeme<'a> {
    Eof,
    Name(&'a str),
    ParL,
    At,
    ParR,
    CurL,
    CurR,
    Int(i64),
    PlusEqual,
    Plus,
    Str(&'a str),
    Char(u8),
    Semicolon,
    Colon,
    Star,
    Bar,
    Comma,
    BraL,
    BraR,
    Equal2,
    Less,
    More,
    Dot,
    Equal,
    Unknown,
    Ampersand,
    BangEqual,
    Mod,
    Slash,
    Minus,
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
            BangEqual => "`!=`",
            More => "`>`",
            Mod => "`%`",
            Slash => "`/`",
            Minus => "`-`",
            At => "`@`",
            Bar => "`|`",
            Char(c) => (c as char).to_string().leak(),
            PlusEqual => "`+=`",
        }
    }
}
