mod expr;
mod item;
mod literal;

use std::collections::HashMap;

use crate::{
    Location,
    link::{
        ast::{Ast, Field, FunType, Item, Prime, Type},
        lex::lexeme::Lexeme::{self, *},
        parse::{Fail, Parse},
    },
};

pub type Parser<'a, T> = fn(&mut Parse<'a>) -> Result<T, Fail>;

impl<'a> Parse<'a> {
    pub fn ast(&mut self) -> Result<Ast<'a>, Fail> {
        let begin = self.here();
        let mut funs = Vec::new();
        let mut externs = Vec::new();
        let mut structs = HashMap::new();
        let mut type_aliases = HashMap::new();
        let mut traits = Vec::new();
        while let Some(item) = self.maybe(Self::item) {
            match item {
                Item::Fun(fun) => funs.push(*fun),
                Item::Extern(extrn) => externs.push(extrn),
                Item::Struct(name, r#struct) => {
                    structs.insert(name, r#struct);
                }
                Item::TypeAlias(type_alias) => {
                    type_aliases.insert(type_alias.name, type_alias.typ);
                }
                Item::Trait(trait_) => traits.push(trait_),
            }
        }
        self.expect(Eof)?;
        Ok(Ast {
            begin,
            funs,
            externs,
            structs,
            type_aliases,
            traits,
        })
    }

    pub fn expect(&mut self, lexeme: Lexeme<'a>) -> Result<(), Fail> {
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
            |p| {
                let location = p.here();
                p.unit_()?;
                Ok(Type::Prime(Prime::Unit, location))
            },
            |p| Ok(p.fun_type_()?.into()),
            |p| {
                let location = p.here();
                p.expect_(Star)?;
                let typ = p.typ()?;
                let other = typ.location();
                Ok(Type::Ptr(Box::new(typ), location.combine(other)))
            },
            |p| {
                let location = p.here();
                p.expect_(Ampersand)?;
                let typ = p.typ()?;
                let other = typ.location();
                Ok(Type::Ref(Box::new(typ), location.combine(other)))
            },
            |p| {
                let lame = p.lame(false)?;
                p.expect(Less)?;
                let generics = p.sep(Self::typ).collect();
                p.expect(More)?;
                Ok(Type::Name(lame, generics))
            },
            |p| Ok(p.lame(false)?.into()),
        ])
        .or_else(|_| self.fail("type"))
    }

    fn fun_type_(&mut self) -> Result<FunType<'a>, Fail> {
        let location = self.here();
        self.expect_(Name("fn"))?;
        let generics = self.maybe(Self::generics).unwrap_or_default();
        self.expect(ParL)?;
        let params = self.sep(Self::typ).collect();
        self.expect(ParR)?;
        let ret = self
            .maybe(Self::typ)
            .unwrap_or_else(|| Type::Prime(Prime::Unit, self.here()));
        Ok(FunType {
            params,
            ret,
            location,
            generics,
        })
    }

    fn generics(&mut self) -> Result<Vec<&'a str>, Fail> {
        self.expect(Less)?;
        let res: Vec<_> = self.sep(|p| p.name(true)).collect();
        self.expect(More)?;
        Ok(res)
    }

    fn field(&mut self) -> Result<Field<'a>, Fail> {
        self.either(&[
            |p| {
                let name = p.name(true)?;
                p.expect(Colon)?;
                let typ = p.typ()?;
                Ok(Field { name, typ })
            },
            |p| {
                let typ = p.typ()?;
                let name = typ.get_name();
                Ok(Field { name, typ })
            },
        ])
    }
}
