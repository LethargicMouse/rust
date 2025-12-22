use crate::{
    die,
    link::{
        Context,
        analyse::error::NotDeclared,
        asg,
        ast::{BinOp, Binary, Block, Call, Expr, Fun, Get, If, Let, Literal, NameLoc},
    },
};

pub struct Analyse<'a, 'b> {
    context: &'b mut Context<'a, ()>,
}

impl<'a, 'b> Analyse<'a, 'b> {
    pub fn new(context: &'b mut Context<'a, ()>) -> Self {
        context.new_layer();
        Self { context }
    }

    pub fn run(mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        for param in &fun.params {
            self.context.insert(param, ());
        }
        let params = fun.params;
        let body = self.expr(fun.body);
        asg::Fun { params, body }
    }

    fn expr(&mut self, expr: Expr<'a>) -> asg::Expr<'a> {
        match expr {
            Expr::Call(call) => self.call(call).into(),
            Expr::Binary(binary) => self.binary(*binary).into(),
            Expr::Literal(literal) => self.literal(literal).into(),
            Expr::Var(name) => self.var(name),
            Expr::If(if_expr) => self.if_expr(*if_expr).into(),
            Expr::Get(get) => self.get(*get),
            Expr::Block(block) => self.block(*block).into(),
            Expr::Let(let_expr) => self.let_expr(*let_expr).into(),
        }
    }

    fn let_expr(&mut self, let_expr: Let<'a>) -> asg::Let<'a> {
        let name = let_expr.name;
        let expr = self.expr(let_expr.expr);
        self.context.insert(name, ());
        asg::Let { name, expr }
    }

    fn block(&mut self, block: Block<'a>) -> asg::Block<'a> {
        let stmts = block.stmts.into_iter().map(|e| self.expr(e)).collect();
        let ret = self.expr(block.ret);
        asg::Block { stmts, ret }
    }

    fn get(&mut self, get: Get<'a>) -> asg::Expr<'a> {
        let from = self.expr(get.from);
        let index = self.expr(get.index);
        asg::Expr::Deref(Box::new(
            asg::Binary {
                left: from,
                op: asg::BinOp::Add,
                right: asg::Binary {
                    left: index,
                    op: asg::BinOp::Multiply,
                    right: asg::Literal::Int(8).into(),
                }
                .into(),
            }
            .into(),
        ))
    }

    fn if_expr(&mut self, if_expr: If<'a>) -> asg::If<'a> {
        let condition = self.expr(if_expr.condition);
        let then_expr = self.expr(if_expr.then_expr);
        let else_expr = self.expr(if_expr.else_expr);
        asg::If {
            condition,
            then_expr,
            else_expr,
        }
    }

    fn call(&mut self, call: Call<'a>) -> asg::Call<'a> {
        let args = call.args.into_iter().map(|e| self.expr(e)).collect();
        let name = call.fun.name;
        self.var(call.fun);
        asg::Call { name, args }
    }

    fn var(&self, var: NameLoc<'a>) -> asg::Expr<'a> {
        if self.context.get(var.name).is_none() {
            die(NotDeclared {
                location: var.location,
                kind: "item",
                name: var.name,
            })
        }
        asg::Expr::Var(var.name)
    }

    fn binary(&mut self, binary: Binary<'a>) -> asg::Binary<'a> {
        let left = self.expr(binary.left);
        let right = self.expr(binary.right);
        let op = self.bin_op(binary.op);
        asg::Binary { left, op, right }
    }

    fn bin_op(&self, bin_op: BinOp) -> asg::BinOp {
        match bin_op {
            BinOp::Plus => asg::BinOp::Add,
            BinOp::Equal => asg::BinOp::Equal,
            BinOp::Less => asg::BinOp::Less,
        }
    }

    fn literal(&self, literal: Literal<'a>) -> asg::Literal<'a> {
        match literal {
            Literal::Unit => asg::Literal::Int(0),
            Literal::Int(i) => asg::Literal::Int(i),
            Literal::RawStr(s) => asg::Literal::Str(s),
        }
    }
}
