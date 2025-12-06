use crate::link::{
    ast::Literal,
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

    pub fn name_(&mut self) -> Result<&'a str, Fail> {
        if let Name(n) = self.next() {
            self.cursor += 1;
            Ok(n)
        } else {
            Err(Fail)
        }
    }

    pub fn int_(&mut self) -> Result<i32, Fail> {
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
}
