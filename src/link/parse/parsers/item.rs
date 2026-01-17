use crate::link::{
    ast::{self, Extern, FunType, Header, Item, Prime, Struct},
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    pub fn item(&mut self) -> Result<Item<'a>, Fail> {
        self.either(&[
            |p| Ok(p.fun_()?.into()),
            |p| Ok(p.extrn_()?.into()),
            |p| Ok(p.struct_()?.into()),
        ])
        .or_else(|_| self.fail("item"))
    }

    fn struct_(&mut self) -> Result<(&'a str, Struct<'a>), Fail> {
        self.expect_(Name("struct"))?;
        let name = self.name(true)?;
        self.expect(CurL)?;
        let fields = self.sep(Self::field).collect();
        self.expect(CurR)?;
        Ok((name, Struct { fields }))
    }

    fn extrn_(&mut self) -> Result<Extern<'a>, Fail> {
        self.expect_(Name("extern"))?;
        let name = self.name(true)?;
        self.expect(Colon)?;
        let typ = self.typ()?;
        self.expect(Semicolon)?;
        Ok(Extern { name, typ })
    }

    fn fun_(&mut self) -> Result<ast::Fun<'a>, Fail> {
        let header = self.header_()?;
        let body = self.block_or_do()?;
        Ok(ast::Fun { header, body })
    }

    fn header_(&mut self) -> Result<Header<'a>, Fail> {
        self.expect_(Name("fn"))?;
        let name = self.name(true)?;
        self.expect(ParL)?;
        let mut params = Vec::new();
        let mut type_params = Vec::new();
        for param in self.sep(Self::field) {
            params.push(param.name);
            type_params.push(param.typ);
        }
        self.expect(ParR)?;
        let ret_type = self.maybe(Self::typ).unwrap_or(Prime::Unit.into());
        let typ = FunType {
            params: type_params,
            ret: ret_type,
        };
        Ok(Header { name, params, typ })
    }
}
