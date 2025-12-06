use crate::link::{
    ast::Expr,
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    pub fn unit_(&mut self) -> Result<Expr<'a>, Fail> {
        self.expect_(ParL)?;
        self.expect(ParR)?;
        Ok(Expr::Unit)
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
