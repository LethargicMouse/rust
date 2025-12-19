mod expr;
mod literal;

use crate::{
    Location,
    link::{
        ast::{self, Ast, Item, Literal},
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

    fn item(&mut self) -> Result<Item<'a>, Fail> {
        self.either(&[
            |p| Ok(Item::Fun(p.fun_()?)),
            |p| Ok(Item::Extern(p.extrn_()?)),
        ])
        .or_else(|_| self.fail("item"))
    }

    fn extrn_(&mut self) -> Result<&'a str, Fail> {
        self.expect_(Extern)?;
        let name = self.name()?;
        self.expect(Semicolon)?;
        Ok(name)
    }

    fn fun_(&mut self) -> Result<ast::Fun<'a>, Fail> {
        self.expect_(Fun)?;
        let name = self.name()?;
        self.expect(ParL)?;
        let params = self.sep(Self::name);
        self.expect(ParR)?;
        self.expect(CurL)?;
        let stmts = self.many(|p| {
            let expr = p.expr()?;
            p.expect(Semicolon)?;
            Ok(expr)
        });
        let ret = self.maybe(Self::expr).unwrap_or(Literal::Unit.into());
        self.expect(CurR)?;
        Ok(ast::Fun {
            params,
            stmts,
            ret,
            name,
        })
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
