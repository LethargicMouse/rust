mod expr;
mod literal;

use crate::link::{
    ast::{self, Ast, Literal},
    lex::lexeme::Lexeme::{self, *},
    parse::{Fail, Parse},
};

pub type Parser<'a, T> = fn(&mut Parse<'a>) -> Result<T, Fail>;

impl<'a> Parse<'a> {
    pub fn ast(&mut self) -> Result<Ast<'a>, Fail> {
        let funs = self.many(Self::fun);
        self.expect(Eof)?;
        Ok(Ast { funs })
    }

    fn fun(&mut self) -> Result<(&'a str, ast::Fun<'a>), Fail> {
        self.expect(Fun)?;
        let name = self.name()?;
        self.expect(ParL)?;
        self.expect(ParR)?;
        self.expect(CurL)?;
        let stmts = self.many(|p| {
            let expr = p.expr()?;
            p.expect(Semicolon)?;
            Ok(expr)
        });
        let ret = self.maybe(Self::expr).unwrap_or(Literal::Unit.into());
        self.expect(CurR)?;
        Ok((name, ast::Fun { stmts, ret }))
    }

    fn expect(&mut self, lexeme: Lexeme) -> Result<(), Fail> {
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
}
