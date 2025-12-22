use crate::link::{
    ast::{self, Item},
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    pub fn item(&mut self) -> Result<Item<'a>, Fail> {
        self.either(&[
            |p| Ok(Item::Fun(p.fun_()?)),
            |p| Ok(Item::Extern(p.extrn_()?)),
        ])
        .or_else(|_| self.fail("item"))
    }

    fn extrn_(&mut self) -> Result<&'a str, Fail> {
        self.expect_(Extern)?;
        let name = self.name(true)?;
        self.expect(Semicolon)?;
        Ok(name)
    }

    fn fun_(&mut self) -> Result<ast::Fun<'a>, Fail> {
        self.expect_(Fun)?;
        let name = self.name(true)?;
        self.expect(ParL)?;
        let params = self.sep(|p| p.name(true));
        self.expect(ParR)?;
        let body = self.block_or_do()?;
        Ok(ast::Fun { params, body, name })
    }
}
