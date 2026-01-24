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
        let mut funs = Vec::new();
        let mut externs = Vec::new();
        let mut structs = HashMap::new();
        while let Some(item) = self.maybe(Self::item) {
            match item {
                Item::Fun(fun) => funs.push(fun),
                Item::Extern(extrn) => externs.push(extrn),
                Item::Struct(name, r#struct) => {
                    structs.insert(name, r#struct);
                }
            }
        }
        self.expect(Eof)?;
        Ok(Ast {
            funs,
            externs,
            structs,
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
                let prime = p.prime_()?;
                Ok(Type::Prime(prime, location))
            },
            |p| Ok(p.fun_type_()?.into()),
            |p| {
                let location = p.here();
                p.expect_(Star)?;
                let typ = p.typ()?;
                Ok(Type::Ptr(Box::new(typ), location))
            },
            |p| Ok(p.lame(false)?.into()),
        ])
        .or_else(|_| self.fail("type"))
    }

    fn prime_(&mut self) -> Result<Prime, Fail> {
        self.either(&[
            |p| p.expect_(Name("i32")).map(|_| Prime::I32),
            |p| p.expect_(Name("u8")).map(|_| Prime::U8),
            |p| p.expect_(Name("u64")).map(|_| Prime::U64),
            |p| p.expect_(Name("bool")).map(|_| Prime::Bool),
            |p| p.unit_().map(|_| Prime::Unit),
        ])
    }

    fn fun_type_(&mut self) -> Result<FunType<'a>, Fail> {
        let location = self.here();
        self.expect_(Name("fn"))?;
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
        })
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
                let lame = p.lame(true)?;
                Ok(Field {
                    name: lame.name,
                    typ: Type::Name(lame),
                })
            },
        ])
    }
}
