use crate::link::{
    ast::{BinOp, Binary, Call, Expr},
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    fn expr_1(&mut self) -> Result<Expr<'a>, Fail> {
        self.either(&[
            |p| Ok(p.literal_()?.into()),
            |p| Ok(Expr::Call(p.call_()?)),
            |p| Ok(Expr::Var(p.name_()?)),
        ])
        .or_else(|_| self.fail("expression"))
    }

    pub fn expr(&mut self) -> Result<Expr<'a>, Fail> {
        let mut expr = self.expr_1()?;
        while let Some((op, right)) = self.maybe(|p| p.bin_postfix_()) {
            expr = Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
            .into()
        }
        Ok(expr)
    }

    fn bin_postfix_(&mut self) -> Result<(BinOp, Expr<'a>), Fail> {
        let op = self.bin_op_()?;
        let expr = self.expr_1()?;
        Ok((op, expr))
    }

    fn bin_op_(&mut self) -> Result<BinOp, Fail> {
        self.either(&[|p| {
            p.expect_(Plus)?;
            Ok(BinOp::Plus)
        }])
    }

    fn call_(&mut self) -> Result<Call<'a>, Fail> {
        let name = self.name_()?;
        self.expect(ParL)?;
        let args = self.sep(Self::expr);
        self.expect(ParR)?;
        Ok(Call { name, args })
    }
}
