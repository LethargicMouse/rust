use crate::link::{
    ast::*,
    lex::lexeme::Lexeme::{self, *},
    parse::{Fail, Parse},
};

pub type Parser<'a, T> = fn(&mut Parse<'a>) -> Result<T, Fail>;

impl<'a> Parse<'a> {
    pub fn ast(&mut self) -> Result<Ast<'a>, Fail> {
        self.expect(Fun)?;
        self.expect(Name("main"))?;
        self.expect(ParL)?;
        self.expect(ParR)?;
        self.expect(CurL)?;
        let expr = self.expr()?;
        self.expect(CurR)?;
        Ok(Ast { expr })
    }

    fn expr_1(&mut self) -> Result<Expr<'a>, Fail> {
        self.either(&[
            Self::unit,
            |p| Ok(Expr::Int(p.int()?)),
            |p| Ok(Expr::Call(p.call()?)),
            |_| Ok(Expr::Unit),
        ])
    }

    fn expr(&mut self) -> Result<Expr<'a>, Fail> {
        let mut expr = self.expr_1()?;
        while let Some((op, right)) = self.maybe(|p| p.bin_postfix()) {
            expr = Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
            .into()
        }
        Ok(expr)
    }

    fn bin_postfix(&mut self) -> Result<(BinOp, Expr<'a>), Fail> {
        let op = self.bin_op()?;
        let expr = self.expr_1()?;
        Ok((op, expr))
    }

    fn bin_op(&mut self) -> Result<BinOp, Fail> {
        self.either(&[|p| {
            p.expect(Plus)?;
            Ok(BinOp::Plus)
        }])
    }

    fn call(&mut self) -> Result<Call<'a>, Fail> {
        let name = self.name()?;
        self.expect(ParL)?;
        let arg = Box::new(self.expr()?);
        self.expect(ParR)?;
        Ok(Call { name, arg })
    }

    fn unit(&mut self) -> Result<Expr<'a>, Fail> {
        self.expect(ParL)?;
        self.expect(ParR)?;
        Ok(Expr::Unit)
    }

    fn expect(&mut self, lexeme: Lexeme) -> Result<(), Fail> {
        if self.tokens[self.cursor].lexeme == lexeme {
            self.cursor += 1;
            Ok(())
        } else {
            self.fail(lexeme.show())
        }
    }

    fn name(&mut self) -> Result<&'a str, Fail> {
        if let Name(n) = self.tokens[self.cursor].lexeme {
            self.cursor += 1;
            Ok(n)
        } else {
            self.fail("name")
        }
    }

    fn int(&mut self) -> Result<i32, Fail> {
        if let Int(n) = self.tokens[self.cursor].lexeme {
            self.cursor += 1;
            Ok(n)
        } else {
            self.fail("int")
        }
    }
}
