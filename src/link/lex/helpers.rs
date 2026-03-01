use crate::{
    link::{
        ast::{Assign, BinOp, Binary, Expr},
        lex::{Lex, Token, lexeme::Lexeme},
    },
    location::Location,
};

impl<'a> Lex<'a> {
    pub fn location(&self, len: usize) -> Location<'a> {
        let source = self.source;
        let start = self.source.poses[self.cursor];
        let end = self.source.poses[self.cursor + len];
        Location { source, start, end }
    }

    pub fn skip(&mut self) {
        self.skip_space();
        while self.source.code[self.cursor..].starts_with(b"--")
            || self.source.code[self.cursor..].starts_with(b"//")
        {
            self.cursor += self.source.code[self.cursor..]
                .iter()
                .take_while(|c| **c != b'\n')
                .count();
            self.skip_space();
        }
    }

    fn skip_space(&mut self) {
        self.cursor += self.source.code[self.cursor..]
            .iter()
            .take_while(|c| c.is_ascii_whitespace())
            .count();
    }

    pub fn token(&mut self, lexeme: Lexeme<'a>, len: usize) -> Token<'a> {
        let location = self.location(len);
        self.cursor += len;
        Token { lexeme, location }
    }

    pub fn take_while(&mut self, f: impl Fn(u8) -> bool) -> &'a [u8] {
        let len = self.source.code[self.cursor..]
            .iter()
            .take_while(|c| f(**c))
            .count();
        &self.source.code[self.cursor..self.cursor + len]
    }
}

pub fn op_assign<'a>(
    location: Location<'a>,
    left: Expr<'a>,
    op: BinOp,
    right: Expr<'a>,
) -> Expr<'a> {
    let to = left.clone();
    Assign {
        expr: Binary {
            location,
            left,
            op,
            right,
        }
        .into(),
        to,
        location,
    }
    .into()
}
