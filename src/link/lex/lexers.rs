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

    pub fn str(&mut self) -> Option<Token<'a>> {
        if !self.source.code[self.cursor..].starts_with(b"\"") {
            return None;
        }
        self.cursor += 1;
        let start = self.cursor;
        while self.cursor != self.source.code.len() && self.source.code[self.cursor] != b'\"' {
            self.cursor += 1;
        }
        if self.cursor == self.source.code.len() {
            self.cursor = start - 1;
            die(Unclosed(self.location(1)))
        }
        let res = &self.source.code[start..self.cursor];
        let lexeme = Lexeme::Str(str::from_utf8(res).unwrap());
        self.cursor -= res.len() + 1;
        let tok = self.token(lexeme, res.len() + 2);
        Some(tok)
    }

    pub fn char(&mut self) -> Option<Token<'a>> {
        if !self.source.code[self.cursor..].starts_with(b"'") {
            return None;
        }
        let char = self.source.code[self.cursor + 1];
        if let Some(b'\'') = self.source.code.get(self.cursor + 2) {
            Some(self.token(Lexeme::Char(char), 3))
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
    (b"(", ParL),
    (b")", ParR),
    (b"{", CurL),
    (b"}", CurR),
    (b"+=", PlusEqual),
    (b"+", Plus),
    (b";", Semicolon),
    (b",", Comma),
    (b"[", BraL),
    (b"]", BraR),
    (b"==", Equal2),
    (b"=>", EqualMore),
    (b"=", Equal),
    (b"<", Less),
    (b".", Dot),
    (b":", Colon),
    (b"*=", StarEqual),
    (b"*", Star),
    (b"&&", Ampersand2),
    (b"&", Ampersand),
    (b"!=", BangEqual),
    (b">", More),
    (b"%", Mod),
    (b"/", Slash),
    (b"-=", MinusEqual),
    (b"-", Minus),
    (b"@", At),
    (b"||", Bar2),
    (b"|", Bar),
];
