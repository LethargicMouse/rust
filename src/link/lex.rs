mod helpers;
pub mod lexeme;
mod lexers;

use std::fmt::Display;

use crate::{
    die::Mortal,
    link::lex::{
        lexeme::{
            LexList,
            Lexeme::{self, *},
        },
        lexers::LIST,
    },
    location::Location,
    source::Source,
};

pub fn lex(code: &'_ Source) -> Vec<Token<'_>> {
    Lex::new(code).run(LIST)
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
                .or_else(|| self.name())
                .or_else(|| self.int())
                .or_die_with(|_| self.error());
            res.push(tok);
            self.skip();
        }
        self.cursor -= 1;
        res.push(self.token(Eof, 1));
        res
    }
}

struct Error<'a>(Location<'a>);

impl Display for Error<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "! error lexing {}\n--! unexpected token", self.0)
    }
}

pub struct Token<'a> {
    pub lexeme: Lexeme<'a>,
    pub location: Location<'a>,
}
