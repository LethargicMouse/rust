use crate::link::{
    ast::{
        self, Const, Enum, Extern, FunType, Header, Impl, Item, Prime, Struct, Trait, Type,
        TypeAlias, Variant,
    },
    lex::Lexeme::*,
    parse::{Parse, Result, error::Fail},
};

impl<'a> Parse<'a> {
    pub fn item(&mut self) -> Result<Item<'a>> {
        self.either(&[
            |p| Ok(p.fun_()?.into()),
            |p| Ok(p.extern_()?.into()),
            |p| Ok(p.struct_()?.into()),
            |p| Ok(p.type_alias_()?.into()),
            |p| Ok(p.trait_()?.into()),
            |p| Ok(p.impl_()?.into()),
            |p| Ok(p.const_()?.into()),
            |p| Ok(p.enum_()?.into()),
        ])
        .or_else(|_| self.fail("item"))
    }

    fn enum_(&mut self) -> Result<(&'a str, Enum<'a>)> {
        self.expect_(Name("enum"))?;
        let name = self.name(true)?;
        let generics = self.maybe(Self::generics).unwrap_or_default();
        self.expect(CurL)?;
        let variants = self.sep(Self::variant).collect();
        self.expect(CurR)?;
        Ok((name, Enum { generics, variants }))
    }

    fn variant(&mut self) -> Result<Variant<'a>> {
        let lame = self.lame(true)?;
        let value = self.maybe(|p| {
            p.expect(ParL)?;
            let typ = p.typ()?;
            p.expect(ParR)?;
            Ok(typ)
        });
        Ok(Variant { lame, value })
    }

    fn const_(&mut self) -> Result<Const<'a>> {
        self.expect_(Name("let"))?;
        let lame = self.lame(false)?;
        self.expect(Colon)?;
        let typ = self.typ()?;
        self.expect(Equal)?;
        let expr = self.expr(0)?;
        self.expect(Semicolon)?;
        Ok(Const { lame, typ, expr })
    }

    fn impl_(&mut self) -> Result<Impl<'a>> {
        self.expect_(Name("impl"))?;
        let lame = self.lame(true)?;
        self.expect(Name("for"))?;
        let typ = self.typ()?;
        self.expect(CurL)?;
        let funs = self.many(Self::fun_);
        self.expect(CurR)?;
        Ok(Impl { lame, typ, funs })
    }

    fn trait_(&mut self) -> Result<(&'a str, Trait<'a>)> {
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

    fn type_alias_(&mut self) -> Result<TypeAlias<'a>> {
        self.expect_(Name("type"))?;
        let name = self.name(true)?;
        self.expect(Equal)?;
        let typ = self.typ()?;
        self.expect(Semicolon)?;
        Ok(TypeAlias { name, typ })
    }

    fn struct_(&mut self) -> Result<(&'a str, Struct<'a>)> {
        self.expect_(Name("struct"))?;
        let name = self.name(true)?;
        let generics = self.maybe(Self::generics).unwrap_or_default();
        self.expect(CurL)?;
        let fields = self.sep(Self::field).collect();
        self.expect(CurR)?;
        Ok((name, Struct { generics, fields }))
    }

    fn extern_(&mut self) -> Result<Extern<'a>> {
        self.expect_(Name("extern"))?;
        let header = self.header()?;
        self.expect(Semicolon)?;
        let lame = header.lame;
        let typ = header.typ;
        Ok(Extern { lame, typ })
    }

    fn fun_(&mut self) -> Result<ast::Fun<'a>> {
        let header = self.header_()?;
        let body = self.block_or_do()?;
        Ok(ast::Fun { header, body })
    }

    fn header(&mut self) -> Result<Header<'a>> {
        self.maybe(Self::header_)
            .ok_or(Fail)
            .or_else(|_| self.fail("fn"))
    }

    fn header_(&mut self) -> Result<Header<'a>> {
        self.expect_(Name("fn"))?;
        let lame = self.lame(true)?;
        let location = lame.location;
        let generics = self.maybe(Self::generics).unwrap_or_default();
        self.expect(ParL)?;
        let mut params = Vec::new();
        let mut type_params = Vec::new();
        for param in self.sep(Self::field) {
            params.push(param.lame);
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
