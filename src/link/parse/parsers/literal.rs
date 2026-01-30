use crate::link::{
    ast::{Lame, Literal, Type},
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    pub fn literal_(&mut self) -> Result<Literal<'a>, Fail> {
        self.either(&[
            Self::unit_,
            Self::bool_,
            |p| Ok(Literal::Int(p.int_()?)),
            |p| Ok(Literal::Str(p.str_()?)),
            |p| Ok(Literal::Size(p.size_()?)),
        ])
    }

    fn size_(&mut self) -> Result<Type<'a>, Fail> {
        self.expect_(At)?;
        self.expect(Name("size"))?;
        self.typ()
    }

    pub fn unit_(&mut self) -> Result<Literal<'a>, Fail> {
        self.expect_(ParL)?;
        self.expect(ParR)?;
        Ok(Literal::Unit)
    }

    fn bool_(&mut self) -> Result<Literal<'a>, Fail> {
        self.expect_(Name("true"))
            .map(|_| Literal::Bool(true))
            .or_else(|_| self.expect_(Name("false")).map(|_| Literal::Bool(false)))
    }

    pub fn name(&mut self, loud: bool) -> Result<&'a str, Fail> {
        if let Name(n) = self.next() {
            if Self::RESERVED.contains(&n) {
                if loud {
                    self.fail("name that is not reserved")
                } else {
                    Err(Fail)
                }
            } else {
                self.cursor += 1;
                Ok(n)
            }
        } else if loud {
            self.fail("name")
        } else {
            Err(Fail)
        }
    }

    pub fn int_(&mut self) -> Result<i64, Fail> {
        if let Int(n) = self.next() {
            self.cursor += 1;
            Ok(n)
        } else {
            Err(Fail)
        }
    }

    fn str_(&mut self) -> Result<&'a str, Fail> {
        if let Str(s) = self.next() {
            self.cursor += 1;
            Ok(s)
        } else {
            Err(Fail)
        }
    }

    pub fn lame(&mut self, loud: bool) -> Result<Lame<'a>, Fail> {
        let location = self.here();
        let name = self.name(loud)?;
        Ok(Lame { name, location })
    }
}
