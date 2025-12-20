use crate::link::{
    ast::{BinOp, Binary, Block, Call, Expr, Get, If, Literal, NameLoc, Postfix},
    lex::Lexeme::*,
    parse::{Parse, error::Fail},
};

impl<'a> Parse<'a> {
    fn expr_2(&mut self) -> Result<Expr<'a>, Fail> {
        self.either(&[
            |p| Ok(p.literal_()?.into()),
            |p| Ok(p.if_expr_()?.into()),
            |p| Ok(Expr::Call(p.call_()?)),
            |p| Ok(Expr::Var(p.var()?)),
        ])
        .or_else(|_| self.fail("expression"))
    }

    fn var(&mut self) -> Result<NameLoc<'a>, Fail> {
        let location = self.here();
        let name = self.name_()?;
        Ok(NameLoc { name, location })
    }

    fn expr_1(&mut self) -> Result<Expr<'a>, Fail> {
        let mut res = self.expr_2()?;
        while let Some(postfix) = self.maybe(Self::postfix) {
            match postfix {
                Postfix::Get(index) => res = Get { from: res, index }.into(),
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

    fn if_expr_(&mut self) -> Result<If<'a>, Fail> {
        self.expect_(If)?;
        let condition = self.expr()?;
        let then_expr = self.block_or_do()?;
        let else_expr = self
            .maybe(|p| {
                p.expect(Else)?;
                p.expr()
            })
            .unwrap_or(Literal::Unit.into());
        Ok(If {
            condition,
            then_expr,
            else_expr,
        })
    }

    pub fn block_or_do(&mut self) -> Result<Expr<'a>, Fail> {
        self.either(&[
            |p| {
                p.expect(Do)?;
                p.expr()
            },
            |p| Ok(p.block()?.into()),
        ])
    }

    fn block(&mut self) -> Result<Block<'a>, Fail> {
        self.block_().or_else(|_| self.fail(CurL.show()))
    }

    fn block_(&mut self) -> Result<Block<'a>, Fail> {
        self.expect_(CurL)?;
        let stmts = self.many(Self::stmt);
        let ret = self.maybe(Self::expr).unwrap_or(Literal::Unit.into());
        self.expect_(CurR)?;
        Ok(Block { stmts, ret })
    }

    pub fn expr(&mut self) -> Result<Expr<'a>, Fail> {
        let mut expr = self.expr_1()?;
        while let Some((op, right)) = self.maybe(|p| p.bin_postfix_()) {
            expr = Binary {
                left: expr,
                op,
                right,
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
            |p| p.expect_(Plus).map(|_| BinOp::Plus),
            |p| p.expect_(Equal).map(|_| BinOp::Equal),
            |p| p.expect_(Less).map(|_| BinOp::Less),
        ])
    }

    fn call_(&mut self) -> Result<Call<'a>, Fail> {
        let fun = self.var()?;
        self.expect_(ParL)?;
        let args = self.sep(Self::expr);
        self.expect(ParR)?;
        Ok(Call { fun, args })
    }

    pub fn stmt(&mut self) -> Result<Expr<'a>, Fail> {
        let expr = self.expr()?;
        if expr.needs_semicolon() {
            self.expect(Semicolon)?;
        }
        Ok(expr)
    }
}
