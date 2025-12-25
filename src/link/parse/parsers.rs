mod expr;
mod item;
mod literal;

use crate::{
    Location,
    link::{
        ast::{Ast, FunType, Item, Type},
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
                Item::Extern(extrn) => externs.push(extrn),
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

    fn typ(&mut self) -> Result<Type<'a>, Fail> {
        self.either(&[
            |p| Ok(p.name(false)?.into()),
            |p| Ok(p.fun_type_()?.into()),
            |p| {
                p.expect_(Star)?;
                Ok(Type::Ptr(Box::new(p.typ()?)))
            },
        ])
        .or_else(|_| self.fail("type"))
    }

    fn fun_type_(&mut self) -> Result<FunType<'a>, Fail> {
        self.expect_(Fun)?;
        self.expect(ParL)?;
        let params = self.sep(Self::typ).collect();
        self.expect(ParR)?;
        let ret_type = self.maybe(Self::typ).unwrap_or(Type::Unit);
        Ok(FunType { params, ret_type })
    }

    fn param(&mut self) -> Result<(&'a str, Type<'a>), Fail> {
        self.either(&[
            |p| {
                let name = p.name(true)?;
                p.expect(Colon)?;
                let typ = p.typ()?;
                Ok((name, typ))
            },
            |p| {
                let name = p.name(true)?;
                Ok((name, Type::Name(name)))
            },
        ])
    }
}
