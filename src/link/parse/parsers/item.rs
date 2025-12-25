use crate::link::{
    ast::{self, Extern, FunType, Header, Item, Struct, Type},
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

    fn struct_(&mut self) -> Result<Struct<'a>, Fail> {
        self.expect_(Struct)?;
        let name = self.name(true)?;
        self.expect(CurL)?;
        self.expect(CurR)?;
        Ok(Struct { name })
    }

    fn extrn_(&mut self) -> Result<Extern<'a>, Fail> {
        self.expect_(Extern)?;
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
        self.expect_(Fun)?;
        let name = self.name(true)?;
        self.expect(ParL)?;
        let mut params = Vec::new();
        let mut type_params = Vec::new();
        let mut type_params_locations = Vec::new();
        for (param, typ, location) in self.sep(Self::param) {
            params.push(param);
            type_params.push(typ);
            type_params_locations.push(location);
        }
        self.expect(ParR)?;
        let ret_type = Type::Unit;
        let typ = FunType {
            params: type_params,
            ret_type,
        };
        Ok(Header {
            name,
            params,
            typ,
            type_params_locations,
        })
    }
}
