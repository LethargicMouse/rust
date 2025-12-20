mod error;

use crate::{
    die,
    link::{
        Asg, Context,
        analyse::error::NotDeclared,
        asg,
        ast::{Ast, BinOp, Binary, Block, Call, Expr, Fun, Get, If, Literal, NameLoc},
    },
};

pub fn analyse(ast: Ast) -> Asg {
    Analyse::new().run(ast)
}

struct Analyse<'a> {
    context: Context<'a, ()>,
}

impl<'a> Analyse<'a> {
    fn new() -> Self {
        let context = Context::new();
        Self { context }
    }

    fn run(mut self, ast: Ast<'a>) -> Asg<'a> {
        for extrn in ast.externs {
            self.context.insert(extrn, ());
        }
        for fun in &ast.funs {
            self.context.insert(fun.name, ());
        }
        let funs = ast
            .funs
            .into_iter()
            .map(|f| (f.name, self.fun(f)))
            .collect();
        Asg { funs }
    }

    fn fun(&mut self, fun: Fun<'a>) -> asg::Fun<'a> {
        AnalyseFun::new(&mut self.context).run(fun)
    }
}

struct AnalyseFun<'a, 'b> {
    context: &'b mut Context<'a, ()>,
}

impl<'a, 'b> AnalyseFun<'a, 'b> {
    fn new(context: &'b mut Context<'a, ()>) -> Self {
        context.new_layer();
        Self { context }
    }

    fn run(self, fun: Fun<'a>) -> asg::Fun<'a> {
        for param in &fun.params {
            self.context.insert(param, ());
        }
        let params = fun.params;
        let body = self.expr(fun.body);
        asg::Fun { params, body }
    }

    fn expr(&self, expr: Expr<'a>) -> asg::Expr<'a> {
        match expr {
            Expr::Call(call) => self.call(call).into(),
            Expr::Binary(binary) => self.binary(*binary).into(),
            Expr::Literal(literal) => self.literal(literal).into(),
            Expr::Var(name) => self.var(name),
            Expr::If(if_expr) => self.if_expr(*if_expr).into(),
            Expr::Get(get) => self.get(*get),
            Expr::Block(block) => self.block(*block).into(),
        }
    }

    fn block(&self, block: Block<'a>) -> asg::Block<'a> {
        let stmts = block.stmts.into_iter().map(|e| self.expr(e)).collect();
        let ret = Box::new(self.expr(block.ret));
        asg::Block { stmts, ret }
    }

    fn get(&self, get: Get<'a>) -> asg::Expr<'a> {
        let from = self.expr(get.from);
        let index = self.expr(get.index);
        asg::Expr::Deref(Box::new(
            asg::Binary {
                left: Box::new(from),
                right: Box::new(
                    asg::Binary {
                        left: Box::new(index),
                        right: Box::new(asg::Literal::Int(8).into()),
                        op: asg::BinOp::Multiply,
                    }
                    .into(),
                ),
                op: asg::BinOp::Add,
            }
            .into(),
        ))
    }

    fn if_expr(&self, if_expr: If<'a>) -> asg::If<'a> {
        let condition = Box::new(self.expr(if_expr.condition));
        let then_expr = Box::new(self.expr(if_expr.then_expr));
        let else_expr = Box::new(self.expr(if_expr.else_expr));
        asg::If {
            condition,
            then_expr,
            else_expr,
        }
    }

    fn call(&self, call: Call<'a>) -> asg::Call<'a> {
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

    fn binary(&self, binary: Binary<'a>) -> asg::Binary<'a> {
        let left = Box::new(self.expr(binary.left));
        let right = Box::new(self.expr(binary.right));
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
