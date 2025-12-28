use crate::link::{
    ast::{Lame, Literal},
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    pub fn literal_(&mut self) -> Result<Literal<'a>, Fail> {
        self.either(&[
            Self::unit_,
            |p| Ok(Literal::Int(p.int_()?)),
            |p| Ok(Literal::RawStr(p.raw_str_()?)),
        ])
    }

    pub fn unit_(&mut self) -> Result<Literal<'a>, Fail> {
        self.expect_(ParL)?;
        self.expect(ParR)?;
        Ok(Literal::Unit)
    }

    pub fn name(&mut self, loud: bool) -> Result<&'a str, Fail> {
        if let Name(n) = self.next() {
            self.cursor += 1;
            Ok(n)
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

    pub fn raw_str_(&mut self) -> Result<&'a str, Fail> {
        if let RawStr(s) = self.next() {
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
