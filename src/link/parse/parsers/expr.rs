use crate::{
    Location,
    link::{
        ast::{
            Assign, BinOp, Binary, Block, Call, Expr, FieldExpr, Get, If, Let, Literal, Postfix,
        },
        lex::Lexeme::*,
        parse::{Parse, error::Fail},
    },
};

impl<'a> Parse<'a> {
    fn expr_2(&mut self) -> Result<Expr<'a>, Fail> {
        self.either(&[
            |p| {
                let location = p.here();
                Ok(Expr::Literal(p.literal_()?, location))
            },
            |p| Ok(p.if_expr_()?.into()),
            |p| Ok(p.let_expr_()?.into()),
            |p| Ok(Expr::Call(p.call(false)?)),
            |p| Ok(Expr::Var(p.lame(false)?)),
        ])
        .or_else(|_| self.fail("expression"))
    }

    fn let_expr_(&mut self) -> Result<Let<'a>, Fail> {
        let location = self.here();
        self.expect_(Name("let"))?;
        let name = self.name(true)?;
        self.expect(Equal)?;
        let expr = self.expr()?;
        Ok(Let {
            name,
            expr,
            location,
        })
    }

    fn expr_1(&mut self) -> Result<Expr<'a>, Fail> {
        let mut res = self.expr_2()?;
        while let Some(postfix) = self.maybe(Self::postfix_) {
            match postfix {
                Postfix::Get(index) => {
                    res = Get {
                        location: res.location().combine(index.location()),
                        from: res,
                        index,
                    }
                    .into()
                }
                Postfix::Call(mut call) => {
                    call.args.insert(0, res);
                    res = Expr::Call(call);
                }
                Postfix::Field(name, name_location) => {
                    res = FieldExpr {
                        from: res,
                        name,
                        name_location,
                    }
                    .into()
                }
                Postfix::Assign(expr) => {
                    res = Assign {
                        location: res.location().combine(expr.location()),
                        expr,
                        to: res,
                    }
                    .into()
                }
            }
        }
        Ok(res)
    }

    fn postfix_(&mut self) -> Result<Postfix<'a>, Fail> {
        self.either(&[
            |p| {
                p.expect_(BraL)?;
                let expr = p.expr()?;
                p.expect(BraR)?;
                Ok(Postfix::Get(expr))
            },
            |p| {
                p.expect_(Equal)?;
                Ok(Postfix::Assign(p.expr()?))
            },
            |p| {
                p.expect_(Dot)?;
                Ok(p.call(true)?.into())
            },
            |p| {
                p.expect_(Dot)?;
                let location = p.here();
                let name = p.name(true)?;
                Ok(Postfix::Field(name, location))
            },
        ])
    }

    fn if_expr_(&mut self) -> Result<If<'a>, Fail> {
        let location = self.here();
        self.expect_(Name("if"))?;
        let condition = self.expr()?;
        let then_expr = self.block_or_do()?;
        let else_expr = self
            .maybe(|p| {
                p.expect(Name("else"))?;
                p.expr()
            })
            .unwrap_or_else(|| self.implicit_unit());
        Ok(If {
            location,
            condition,
            then_expr,
            else_expr,
        })
    }

    fn implicit_unit(&self) -> Expr<'a> {
        Expr::Literal(Literal::Unit, self.here())
    }

    pub fn block_or_do(&mut self) -> Result<Expr<'a>, Fail> {
        self.either(&[
            |p| {
                p.expect(Name("do"))?;
                p.expr()
            },
            |p| Ok(p.block()?.into()),
        ])
    }

    fn block(&mut self) -> Result<Block<'a>, Fail> {
        self.maybe(Self::block_)
            .ok_or(Fail)
            .or_else(|_| self.fail(CurL.show()))
    }

    fn block_(&mut self) -> Result<Block<'a>, Fail> {
        self.expect_(CurL)?;
        let stmts = self.many(Self::stmt);
        let ret = self
            .maybe(Self::expr)
            .unwrap_or_else(|| self.implicit_unit());
        self.expect_(CurR)?;
        Ok(Block { stmts, ret })
    }

    pub fn expr(&mut self) -> Result<Expr<'a>, Fail> {
        let mut expr = self.expr_1()?;
        while let Some((op, op_location, right)) = self.maybe(|p| p.bin_postfix_()) {
            expr = Binary {
                left: expr,
                op,
                op_location,
                right,
            }
            .into()
        }
        Ok(expr)
    }

    fn bin_postfix_(&mut self) -> Result<(BinOp, Location<'a>, Expr<'a>), Fail> {
        let op = self.bin_op_()?;
        let location = self.here();
        let expr = self.expr_1()?;
        Ok((op, location, expr))
    }

    fn bin_op_(&mut self) -> Result<BinOp, Fail> {
        self.either(&[
            |p| p.expect_(Plus).map(|_| BinOp::Plus),
            |p| p.expect_(Equal2).map(|_| BinOp::Equal),
            |p| p.expect_(Less).map(|_| BinOp::Less),
        ])
    }

    fn call(&mut self, loud: bool) -> Result<Call<'a>, Fail> {
        let var = self.lame(loud)?;
        self.expect_(ParL)?;
        let args = self.sep(Self::expr).collect();
        self.expect(ParR)?;
        Ok(Call { var, args })
    }

    pub fn stmt(&mut self) -> Result<Expr<'a>, Fail> {
        let expr = self.expr()?;
        if expr.needs_semicolon() {
            self.expect(Semicolon)?;
        }
        Ok(expr)
    }
}
