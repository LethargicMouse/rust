mod expr;
mod item;
mod literal;

use crate::{
    Location,
    link::{
        ast::{Ast, Item},
        lex::lexeme::Lexeme::{self, *},
        parse::{Fail, Parse},
    },
};

pub type Parser<'a, T> = fn(&mut Parse<'a>) -> Result<T, Fail>;

impl<'a> Parse<'a> {
    pub fn ast(&mut self) -> Result<Ast<'a>, Fail> {
        let mut funs = Vec::new();
        let mut externs = Vec::new();
        while let Some(item) = self.maybe(Self::item) {
            match item {
                Item::Fun(fun) => funs.push(fun),
                Item::Extern(name) => externs.push(name),
            }
        }
        self.expect(Eof)?;
        Ok(Ast { funs, externs })
    }

    pub fn expect(&mut self, lexeme: Lexeme) -> Result<(), Fail> {
        self.expect_(lexeme).or_else(|_| self.fail(lexeme.show()))
    }

    fn expect_(&mut self, lexeme: Lexeme) -> Result<(), Fail> {
        if self.tokens[self.cursor].lexeme == lexeme {
            self.cursor += 1;
            Ok(())
        } else {
            Err(Fail)
        }
    }

    fn next(&self) -> Lexeme<'a> {
        self.tokens[self.cursor].lexeme
    }

    fn here(&self) -> Location<'a> {
        self.tokens[self.cursor].location
    }
}
