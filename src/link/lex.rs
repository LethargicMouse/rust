mod error;
mod helpers;
pub mod lexeme;
pub use lexeme::Lexeme;
mod lexers;

use crate::{
    link::lex::{
        lexeme::{LexList, Lexeme::*},
        lexers::LIST,
    },
    location::Location,
    source::Source,
};

pub fn lex(source: &'_ Source) -> Vec<Token<'_>> {
    Lex::new(source).run(LIST)
}

struct Lex<'a> {
    source: &'a Source,
    cursor: usize,
}

impl<'a> Lex<'a> {
    fn new(source: &'a Source) -> Self {
        Self { source, cursor: 0 }
    }

    fn run(mut self, list: LexList<'a>) -> Vec<Token<'a>> {
        let mut res = Vec::new();
        self.skip();
        while self.cursor < self.source.code.len() {
            let tok = self
                .list(list)
                .or_else(|| self.raw_str())
                .or_else(|| self.str())
                .or_else(|| self.name())
                .or_else(|| self.int())
                .unwrap_or_else(|| self.unknown());
            res.push(tok);
            if matches!(res.last().unwrap().lexeme, Unknown) {
                break;
            }
            self.skip();
        }
        self.cursor -= 1;
        res.push(self.token(Eof, 1));
        res
    }
}

pub struct Token<'a> {
    pub lexeme: Lexeme<'a>,
    pub location: Location<'a>,
}
