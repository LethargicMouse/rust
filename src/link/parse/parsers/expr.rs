use crate::{
    Location,
    link::{
        ast::{
            Array, Assign, BinOp, Binary, Block, Call, Cast, Expr, FieldExpr, Get, If, Let, Loop,
            New, NewField, Postfix, Ref, Return,
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
            |p| Ok(p.loop_expr_()?.into()),
            |p| Ok(p.ref_expr_()?.into()),
            |p| Ok(p.ret_()?.into()),
            |p| Ok(p.block_()?.into()),
            Self::par_expr,
            |p| Ok(p.new_expr_()?.into()),
            |p| Ok(p.call(false)?.into()),
            |p| Ok(p.lame(false)?.into()),
            |p| Ok(p.array_()?.into()),
        ])
        .or_else(|_| self.fail("expression"))
    }

    fn array_(&mut self) -> Result<Array<'a>, Fail> {
        let location = self.here();
        self.expect_(BraL)?;
        let elems = self.sep(|p| p.expr(0)).collect();
        let location = location.combine(self.here());
        self.expect(BraR)?;
        Ok(Array { elems, location })
    }

    fn par_expr(&mut self) -> Result<Expr<'a>, Fail> {
        self.expect_(ParL)?;
        let res = self.expr(0)?;
        self.expect(ParR)?;
        Ok(res)
    }

    fn ret_(&mut self) -> Result<Return<'a>, Fail> {
        let location = self.here();
        self.expect_(Name("return"))?;
        let expr = self
            .maybe(|p| p.expr(0))
            .unwrap_or_else(|| self.implicit_unit(self.here()));
        Ok(Return { expr, location })
    }

    fn ref_expr_(&mut self) -> Result<Ref<'a>, Fail> {
        let location = self.here();
        self.expect_(Ampersand)?;
        let expr = self.expr_1()?;
        Ok(Ref { expr, location })
    }

    fn loop_expr_(&mut self) -> Result<Loop<'a>, Fail> {
        self.expect_(Name("loop"))?;
        let body = self.expr(0)?;
        Ok(Loop { body })
    }

    fn new_expr_(&mut self) -> Result<New<'a>, Fail> {
        let lame = self.lame(false)?;
        self.expect(CurL)?;
        let fields = self.sep(Self::new_field).collect();
        self.expect(CurR)?;
        Ok(New { lame, fields })
    }

    fn new_field(&mut self) -> Result<NewField<'a>, Fail> {
        self.either(&[
            |p| {
                let lame = p.lame(true)?;
                p.expect(Colon)?;
                let expr = p.expr(0)?;
                Ok(NewField { lame, expr })
            },
            |p| {
                let lame = p.lame(true)?;
                let expr = lame.into();
                Ok(NewField { lame, expr })
            },
        ])
    }

    fn let_expr_(&mut self) -> Result<Let<'a>, Fail> {
        let location = self.here();
        self.expect_(Name("let"))?;
        let name = self.name(true)?;
        let typ = self.maybe(|p| {
            p.expect(Colon)?;
            p.typ()
        });
        self.expect(Equal)?;
        let expr = self.expr(0)?;
        Ok(Let {
            name,
            typ,
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
                        expr: res,
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
                Postfix::Cast(typ) => {
                    res = Cast {
                        location: res.location().combine(typ.location()),
                        expr: res,
                        typ,
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
                let expr = p.expr(0)?;
                p.expect(BraR)?;
                Ok(Postfix::Get(expr))
            },
            |p| {
                p.expect_(Equal)?;
                Ok(Postfix::Assign(p.expr(0)?))
            },
            |p| {
                p.expect_(Name("as"))?;
                Ok(Postfix::Cast(p.typ()?))
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
        self.expect(ParL)?;
        let condition = self.expr(0)?;
        self.expect(ParR)?;
        let then_expr = self.expr(0)?;
        let else_expr = self
            .maybe(|p| {
                p.expect(Name("else"))?;
                p.expr(0)
            })
            .unwrap_or_else(|| self.implicit_unit(location));
        Ok(If {
            location,
            condition,
            then_expr,
            else_expr,
        })
    }

    fn implicit_unit(&self, location: Location<'a>) -> Expr<'a> {
        Expr::ImplicitUnit(location)
    }

    pub fn block_or_do(&mut self) -> Result<Block<'a>, Fail> {
        self.either(&[Self::block, |p| {
            p.expect(Name("do"))?;
            Ok(p.expr(0)?.into())
        }])
    }

    fn block(&mut self) -> Result<Block<'a>, Fail> {
        self.maybe(Self::block_)
            .ok_or(Fail)
            .or_else(|_| self.fail(CurL.show()))
    }

    fn block_(&mut self) -> Result<Block<'a>, Fail> {
        let location = self.here();
        self.expect_(CurL)?;
        let mut stmts = self.many(Self::stmt);
        let mut last_semi_location = None;
        if let Some(expr) = stmts.last()
            && expr.needs_semicolon()
        {
            self.cursor -= 1;
            last_semi_location = Some(self.here());
            self.cursor += 1;
        }
        let ret = self.maybe(|p| p.expr(0)).unwrap_or_else(|| {
            if stmts.last().is_some_and(|e| !e.needs_semicolon()) {
                stmts.pop().unwrap()
            } else {
                self.implicit_unit(location)
            }
        });
        self.expect_(CurR)?;
        Ok(Block {
            stmts,
            last_semi_location,
            ret,
        })
    }

    pub fn expr(&mut self, min_priority: u8) -> Result<Expr<'a>, Fail> {
        let mut expr = self.expr_1()?;
        while let Some((op, right)) = self.maybe(|p| p.bin_postfix_(min_priority)) {
            expr = Binary {
                location: expr.location().combine(right.location()),
                left: expr,
                op,
                right,
            }
            .into()
        }
        Ok(expr)
    }

    fn bin_postfix_(&mut self, min_priority: u8) -> Result<(BinOp, Expr<'a>), Fail> {
        let op = self.bin_op_(min_priority)?;
        let expr = self.expr(min_priority.max(op.priority() + 1))?;
        Ok((op, expr))
    }

    fn bin_op_(&mut self, min_priority: u8) -> Result<BinOp, Fail> {
        let res = self.either(&[
            |p| p.expect_(Plus).map(|_| BinOp::Plus),
            |p| p.expect_(Equal2).map(|_| BinOp::Equal),
            |p| p.expect_(Less).map(|_| BinOp::Less),
            |p| p.expect_(BangEqual).map(|_| BinOp::NotEqual),
            |p| p.expect_(Mod).map(|_| BinOp::Mod),
            |p| p.expect_(Slash).map(|_| BinOp::Div),
            |p| p.expect_(Ampersand).map(|_| BinOp::And),
            |p| p.expect_(Minus).map(|_| BinOp::Subtract),
            |p| p.expect_(Star).map(|_| BinOp::Multiply),
        ])?;
        if res.priority() >= min_priority {
            Ok(res)
        } else {
            Err(Fail)
        }
    }

    fn call(&mut self, loud: bool) -> Result<Call<'a>, Fail> {
        let lame = self.lame(loud)?;
        self.expect_(ParL)?;
        let args = self.sep(|p| p.expr(0)).collect();
        self.expect(ParR)?;
        Ok(Call { lame, args })
    }

    pub fn stmt(&mut self) -> Result<Expr<'a>, Fail> {
        let expr = self.expr(0)?;
        if expr.needs_semicolon() {
            self.expect(Semicolon)?;
        }
        Ok(expr)
    }
}
