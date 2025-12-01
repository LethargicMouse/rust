use crate::link::lex::{
    Lex, Token,
    lexeme::{LexList, Lexeme::*},
};

impl<'a> Lex<'a> {
    pub fn int(&mut self) -> Option<Token<'a>> {
        let res = self.take_while(|c| c.is_ascii_digit());
        if res.is_empty() {
            None
        } else {
            let int = str::from_utf8(res).unwrap().parse().unwrap();
            let lexeme = Int(int);
            Some(self.token(lexeme, res.len()))
        }
    }

    pub fn name(&mut self) -> Option<Token<'a>> {
        if self
            .source
            .code
            .get(self.cursor)
            .is_none_or(|c| !is_name_first_char(*c))
        {
            return None;
        }
        let res = self.take_while(is_name_char);
        let lexeme = Name(str::from_utf8(res).unwrap());
        Some(self.token(lexeme, res.len()))
    }

    pub fn list(&mut self, list: LexList<'a>) -> Option<Token<'a>> {
        for (s, lexeme) in list {
            if self.source.code[self.cursor..].starts_with(s) {
                return Some(self.token(*lexeme, s.len()));
            }
        }
        None
    }
}

fn is_name_first_char(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_'
}

fn is_name_char(c: u8) -> bool {
    is_name_first_char(c) || c.is_ascii_digit()
}

pub const LIST: LexList = &[
    (b"fn", Fun),
    (b"(", ParL),
    (b")", ParR),
    (b"{", CurL),
    (b"}", CurR),
    (b"+", Plus),
];
