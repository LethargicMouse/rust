use crate::link::{
    ast::{
        self, Const, Extern, FunType, Header, Impl, Item, Prime, Struct, Trait, Type, TypeAlias,
    },
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    pub fn item(&mut self) -> Result<Item<'a>, Fail> {
        self.either(&[
            |p| Ok(p.fun_()?.into()),
            |p| Ok(p.extern_()?.into()),
            |p| Ok(p.struct_()?.into()),
            |p| Ok(p.type_alias_()?.into()),
            |p| Ok(p.trait_()?.into()),
            |p| Ok(p.impl_()?.into()),
            |p| Ok(p.const_()?.into()),
        ])
        .or_else(|_| self.fail("item"))
    }

    fn const_(&mut self) -> Result<Const<'a>, Fail> {
        self.expect_(Name("let"))?;
        let name = self.name(false)?;
        self.expect(Colon)?;
        let typ = self.typ()?;
        self.expect(Equal)?;
        let expr = self.expr(0)?;
        self.expect(Semicolon)?;
        Ok(Const { name, typ, expr })
    }

    fn impl_(&mut self) -> Result<Impl<'a>, Fail> {
        self.expect_(Name("impl"))?;
        let lame = self.lame(true)?;
        self.expect(Name("for"))?;
        let typ = self.typ()?;
        self.expect(CurL)?;
        let funs = self.many(Self::fun_);
        self.expect(CurR)?;
        Ok(Impl { lame, typ, funs })
    }

    fn trait_(&mut self) -> Result<(&'a str, Trait<'a>), Fail> {
        self.expect_(Name("trait"))?;
        let name = self.name(true)?;
        self.expect(CurL)?;
        let headers = self.many(|p| {
            let res = p.header_()?;
            p.expect(Semicolon)?;
            Ok(res)
        });
        self.expect(CurR)?;
        Ok((name, Trait { headers }))
    }

    fn type_alias_(&mut self) -> Result<TypeAlias<'a>, Fail> {
        self.expect_(Name("type"))?;
        let name = self.name(true)?;
        self.expect(Equal)?;
        let typ = self.typ()?;
        self.expect(Semicolon)?;
        Ok(TypeAlias { name, typ })
    }

    fn struct_(&mut self) -> Result<(&'a str, Struct<'a>), Fail> {
        self.expect_(Name("struct"))?;
        let name = self.name(true)?;
        let generics = self.maybe(Self::generics).unwrap_or_default();
        self.expect(CurL)?;
        let fields = self.sep(Self::field).collect();
        self.expect(CurR)?;
        Ok((name, Struct { generics, fields }))
    }

    fn extern_(&mut self) -> Result<Extern<'a>, Fail> {
        self.expect_(Name("extern"))?;
        let header = self.header()?;
        self.expect(Semicolon)?;
        let name = header.lame.name;
        let typ = header.typ;
        Ok(Extern { name, typ })
    }

    fn fun_(&mut self) -> Result<ast::Fun<'a>, Fail> {
        let header = self.header_()?;
        let body = self.block_or_do()?;
        Ok(ast::Fun { header, body })
    }

    fn header(&mut self) -> Result<Header<'a>, Fail> {
        self.maybe(Self::header_)
            .ok_or(Fail)
            .or_else(|_| self.fail("fn"))
    }

    fn header_(&mut self) -> Result<Header<'a>, Fail> {
        self.expect_(Name("fn"))?;
        let lame = self.lame(true)?;
        let location = lame.location;
        let generics = self.maybe(Self::generics).unwrap_or_default();
        self.expect(ParL)?;
        let mut params = Vec::new();
        let mut type_params = Vec::new();
        for param in self.sep(Self::field) {
            params.push(param.name);
            type_params.push(param.typ);
        }
        self.expect(ParR)?;
        Ok(Header {
            lame,
            params,
            typ: FunType {
                generics,
                params: type_params,
                ret: self
                    .maybe(Self::typ)
                    .unwrap_or_else(|| Type::Prime(Prime::Unit, self.here())),
                location,
            },
        })
    }
}
