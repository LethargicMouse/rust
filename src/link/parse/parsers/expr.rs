use crate::link::{
    ast::{BinOp, Binary, Call, Expr, Get, If, Postfix, Var},
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    fn expr_2(&mut self) -> Result<Expr<'a>, Fail> {
        self.either(&[
            |p| Ok(p.literal_()?.into()),
            |p| Ok(Expr::If(p.if_expr()?)),
            |p| Ok(Expr::Call(p.call_()?)),
            |p| Ok(Expr::Var(p.var()?)),
        ])
        .or_else(|_| self.fail("expression"))
    }

    fn var(&mut self) -> Result<Var<'a>, Fail> {
        let location = self.here();
        let name = self.name_()?;
        Ok(Var { name, location })
    }

    fn expr_1(&mut self) -> Result<Expr<'a>, Fail> {
        let mut res = self.expr_2()?;
        while let Some(postfix) = self.maybe(Self::postfix) {
            match postfix {
                Postfix::Get(index) => {
                    res = Get {
                        from: Box::new(res),
                        index: Box::new(index),
                    }
                    .into()
                }
            }
        }
        Ok(res)
    }

    fn postfix(&mut self) -> Result<Postfix<'a>, Fail> {
        self.either(&[|p| {
            p.expect_(BraL)?;
            let expr = p.expr()?;
            p.expect_(BraR)?;
            Ok(Postfix::Get(expr))
        }])
    }

    fn if_expr(&mut self) -> Result<If<'a>, Fail> {
        self.expect_(If)?;
        let condition = Box::new(self.expr()?);
        self.expect(Do)?;
        let then_expr = Box::new(self.expr()?);
        Ok(If {
            condition,
            then_expr,
        })
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
        self.either(&[
            |p| {
                p.expect_(Plus)?;
                Ok(BinOp::Plus)
            },
            |p| {
                p.expect_(Equal)?;
                Ok(BinOp::Equal)
            },
        ])
    }

    fn call_(&mut self) -> Result<Call<'a>, Fail> {
        let fun = self.var()?;
        self.expect_(ParL)?;
        let args = self.sep(Self::expr);
        self.expect(ParR)?;
        Ok(Call { fun, args })
    }
}
