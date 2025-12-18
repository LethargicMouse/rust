use crate::{
    die,
    link::lex::{
        Lex, Token,
        error::Unclosed,
        lexeme::{
            LexList,
            Lexeme::{self, *},
        },
    },
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

    pub fn raw_str(&mut self) -> Option<Token<'a>> {
        self.skip();
        if self.source.code[self.cursor..].starts_with(b"r\"") {
            self.cursor += 2;
            let start = self.cursor;
            while self.cursor != self.source.code.len() && self.source.code[self.cursor] != b'\"' {
                self.cursor += 1;
            }
            if self.cursor == self.source.code.len() {
                self.cursor = start - 2;
                die(Unclosed(self.location(1)))
            }
            let res = &self.source.code[start..self.cursor];
            let lexeme = Lexeme::RawStr(str::from_utf8(res).unwrap());
            self.cursor -= res.len() + 4;
            let tok = self.token(lexeme, res.len() + 5);
            Some(tok)
        } else {
            None
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

    pub fn unknown(&mut self) -> Token<'a> {
        self.token(Unknown, 1)
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
    (b";", Semicolon),
    (b",", Comma),
    (b"[", BraL),
    (b"]", BraR),
    (b"if", If),
];
